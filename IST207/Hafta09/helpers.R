# =========================================================
# AHP — Multi-Person, Multi-Layer (Per-person + Group)
# - Per-person local weights & CI/CR at every node (sum to 1)
# - Group path via geometric-mean aggregation (AJGM)
# - Global probabilities per-person (AIP) and group
# - RI = 0 (n<=2); RI = (1.98*(n-2))/n otherwise
# - Eigen or Column method for local priorities
# =========================================================

`%||%` <- function(a,b) if (is.null(a)) b else a

# ---------- math helpers ----------
.is_square <- function(M) is.matrix(M) && nrow(M) == ncol(M)
.is_positive <- function(M) all(M > 0)
.is_reciprocal <- function(M, tol = 1e-8) {
  all(abs(M * t(M) - 1) < tol) && all(abs(diag(M) - 1) < tol)
}
.normalize_columns <- function(M) sweep(M, 2, colSums(M), "/")

.principal_eigen_weights <- function(M) {
  ev <- eigen(M)
  i  <- which.max(Re(ev$values))
  w  <- Re(ev$vectors[, i])
  w  <- abs(w); w / sum(w)
}
.column_method_weights <- function(M) {
  N <- .normalize_columns(M)
  w <- rowMeans(N); w / sum(w)
}
.lambda_max_via_w <- function(M, w) sum((M %*% w) / w) / length(w)

.consistency <- function(M, w = NULL) {
  n <- nrow(M)
  # if (is.null(w)) {
  #   # use your current local-weights method to get w; keep sum(w)=1
  #   w <- .principal_eigen_weights(M)  # or .column_method_weights(M) if you prefer
  # }
  
  w <- M/ matrix(rep(colSums(M),nrow(M)),ncol=ncol(M),byrow = T)
  
  w <- rowMeans(w)
  w <- as.numeric(w) / sum(w)         # ensure normalization (safety)
  
  # --- lambda, CI, RI, CR WITHOUT eigenvalues ---
  Aw <- as.numeric(M %*% w)
  lambda_max <- sum(Aw)                      # since sum(w)=1
  CI  <- if (n > 2) (lambda_max - n) / (n - 1) else 0
  RI  <- if (n <= 2) 0 else (1.98 * (n - 2)) / n
  CR  <- if (RI > 0) CI / RI else 0
  
  list(lambda_max = lambda_max, CI = CI, RI = RI, CR = CR, n = n)
}



# ---------- input coercion & aggregation ----------
.as_person_list <- function(x) {
  if (is.matrix(x)) {
    list(x)
  } else if (is.list(x) && all(vapply(x, is.matrix, logical(1)))) {
    x
  } else if (is.array(x) && length(dim(x)) == 3) {
    lapply(seq_len(dim(x)[3]), function(p) x[,,p])
  } else {
    stop("`matrix` must be: a matrix, a list of matrices, or a 3D array [n,n,P].")
  }
}

.geomean_mats <- function(mats) {
  n <- nrow(mats[[1]])
  if (!all(vapply(mats, function(M) .is_square(M) && nrow(M)==n && ncol(M)==n, logical(1))))
    stop("All person matrices must share the same square size.")
  if (!all(vapply(mats, .is_positive, logical(1)))) stop("All entries must be > 0.")
  logsum <- Reduce(`+`, lapply(mats, log))
  G <- exp(logsum / length(mats))
  diag(G) <- 1
  G <- (G + 1/t(G)) / 2; diag(G) <- 1 # tidy reciprocity
  G
}

.solve_one <- function(M, method = c("eigen","column")) {
  method <- match.arg(method)
  w_e <- .principal_eigen_weights(M)
  cons <- .consistency(M, w_e)                 # consistency via eigen path (standard)
  w <- if (method == "eigen") w_e else .column_method_weights(M)
  list(weights = w, consistency = cons, normalized = .normalize_columns(M))
}

# ---------- PATH COLLECTION (records numeric factors for explicit formulas) ----------
# Returns a data.frame with columns:
#   alt, contrib (numeric), product_str (e.g., "0.833*0.545"), factors (list-column)
.collect_paths <- function(node,
                           who = c("group","person_1"),
                           prefix_factors = numeric(0),
                           digits = 3) {
  who <- as.character(who)
  fmt <- function(x) formatC(x, digits = digits, format = "f", drop0trailing = TRUE)
  
  get_local <- function(nd) {
    if (startsWith(who, "person_")) nd$per_person[[who]]$weights else nd$group$weights
  }
  
  # Terminal node: its local weights are over alternatives
  if (is.null(node$children) || length(node$children) == 0) {
    wloc <- get_local(node)
    alts <- names(wloc)
    out <- lapply(alts, function(a) {
      fac <- c(prefix_factors, as.numeric(wloc[[a]]))
      data.frame(
        alt = a,
        contrib = prod(fac),
        product_str = paste(fmt(fac), collapse = "*"),
        stringsAsFactors = FALSE
      )
    })
    return(do.call(rbind, out))
  }
  
  # Internal node: recurse over children and extend factors with the edge weight
  wloc <- get_local(node)
  chn  <- names(node$children)
  wloc <- wloc[chn]
  out  <- list()
  for (ch in chn) {
    out[[length(out) + 1]] <-
      .collect_paths(node$children[[ch]],
                     who = who,
                     prefix_factors = c(prefix_factors, as.numeric(wloc[[ch]])),
                     digits = digits)
  }
  do.call(rbind, out)
}

# Sum contributions by alternative
.sum_by_alt <- function(df) {
  tapply(df$contrib, df$alt, sum)
}


# Summarize contributions by alternative
.sum_by_alt <- function(df) {
  tapply(df$contrib, df$alt, sum)
}


# ---------- AHP Path Plotter (full or names-only) ----------
ahp_plot_paths <- function(tree,
                           who = c("group","person_1"),
                           file = NULL,
                           digits = 3,
                           main_title = "AHP Paths",
                           layout = c("compact","expanded"),
                           # visibility toggles
                           names_only = FALSE,     # <--- NEW: show only node/alt names
                           show_cr    = TRUE,
                           # sizes
                           cr_digits = 3,
                           cex_title = 1.2,
                           cex_node  = 1.0,    # node labels (Goal, etc.)
                           cex_alt   = 0.95,   # alt labels (A,B,C)
                           cex_prob  = 0.85,   # edge probabilities
                           cex_contr = 0.85,   # path contributions
                           cex_eq    = 0.9,    # equations block
                           cex_cr    = 0.8,    # CR label
                           # paddings
                           pad_node  = 0.012,
                           pad_alt   = 0.010,
                           gap_cr    = 0.012,
                           # equation placement
                           eq_x = 0.62,
                           eq_y_offset = 0.05) {
  
  who    <- as.character(who)
  layout <- match.arg(layout)
  
  # --- if names_only, hide everything else ---
  if (isTRUE(names_only)) {
    show_cr    <- FALSE
    draw_probs <- FALSE
    draw_contr <- FALSE
    draw_eq    <- FALSE
  } else {
    draw_probs <- TRUE
    draw_contr <- TRUE
    draw_eq    <- TRUE
  }
  
  fmt    <- function(x) formatC(x, digits = digits, format = "f", drop0trailing = TRUE)
  fmt_cr <- function(x) formatC(x, digits = cr_digits, format = "f", drop0trailing = TRUE)
  
  # ---- data prep (only needed if we’ll draw equations) ----
  if (draw_eq) {
    paths_df <- .collect_paths(tree, who = who, prefix_factors = numeric(0), digits = digits)
    totals   <- .sum_by_alt(paths_df); totals[is.na(totals)] <- 0
    totals   <- totals / sum(totals)
    alt_names <- names(totals)
    by_alt <- split(paths_df, paths_df$alt)
    eq_line <- vapply(alt_names, function(a) {
      if (is.null(by_alt[[a]]) || NROW(by_alt[[a]]) == 0L)
        sprintf("P(%s) = %s", a, fmt(0))
      else {
        parts <- paste(by_alt[[a]]$product_str, collapse = " + ")
        sprintf("P(%s) = %s = %s", a, parts, fmt(totals[[a]]))
      }
    }, character(1))
    finals_line <- paste(sprintf("P(%s) = %s", alt_names, fmt(totals[alt_names])),
                         collapse = "     ")
  } else {
    totals <- NULL
    alt_names <- NULL
    eq_line <- character(0)
    finals_line <- ""
  }
  
  # ---- plotting setup ----
  if (!is.null(file)) { png(file, width = 1400, height = 950, res = 160); on.exit(dev.off(), add = TRUE) }
  par(mar = c(4.5, 2, 4, 2))
  plot.new(); plot.window(xlim = c(0,1), ylim = c(0,1))
  title(main = sprintf("%s (%s) — %s", main_title, getOption("ahp.method","column"), who),
        cex.main = cex_title)
  
  # ---- coordinate utilities ----
  count_leaves <- function(nd) if (is.null(nd$children) || !length(nd$children)) 1 else sum(vapply(nd$children, count_leaves, numeric(1)))
  total_leaves <- max(1, count_leaves(tree))
  coords <- list()
  assign_coords <- function(nd, depth=0, left=0, right=total_leaves) {
    if (is.null(nd$children) || !length(nd$children)) { coords[[nd$node_name]] <<- c((left+right)/2, depth); return() }
    chs <- nd$children
    counts <- vapply(chs, count_leaves, numeric(1)); cumc <- c(0, cumsum(counts))
    for (i in seq_along(chs)) assign_coords(chs[[i]], depth+1, left+cumc[i], left+cumc[i+1])
    child_x <- vapply(chs, function(cn) coords[[cn$node_name]][1], numeric(1))
    coords[[nd$node_name]] <<- c(mean(child_x), depth)
  }
  assign_coords(tree, 0)
  max_depth <- max(vapply(coords, function(v) v[2], numeric(1)))
  to_xy <- function(nm) c(coords[[nm]][1]/(total_leaves+1), 1 - coords[[nm]][2]/(max_depth+3))
  
  # ---- helper drawing functions ----
  draw_label_box <- function(x,y,label,pad=0.01,cex=1){
    w <- strwidth(label,cex=cex)+2*pad
    h <- strheight("M",cex=cex)+2*pad
    rect(x-w/2,y-h/2,x+w/2,y+h/2)
    text(x,y,labels=label,cex=cex)
    list(xmin=x-w/2,xmax=x+w/2,ymin=y-h/2,ymax=y+h/2)
  }
  draw_text_bg <- function(x,y,label,cex=1,pad=0.004){
    w <- strwidth(label,cex=cex)+2*pad; h <- strheight("M",cex=cex)+2*pad
    rect(x-w/2,y-h/2,x+w/2,y+h/2,col="white",border=NA)
    text(x,y,labels=label,cex=cex)
  }
  draw_edge_label <- function(x1,y1,x2,y2,label,i,n,cex=1){
    mx <- (x1+x2)/2; my <- (y1+y2)/2
    dx <- x2-x1; dy <- y2-y1; L <- sqrt(dx*dx+dy*dy)+1e-9
    nx <- -dy/L; ny <- dx/L
    k <- (i - (n+1)/2)
    off <- 0.012 + 0.004*abs(k)
    draw_text_bg(mx + off*nx, my + off*ny, label, cex=cex, pad=0.003)
  }
  
  local_w <- function(nd) if (startsWith(who,"person_")) nd$per_person[[who]]$weights else nd$group$weights
  node_cr <- function(nd) if (startsWith(who,"person_")) nd$per_person[[who]]$consistency$CR else nd$group$consistency$CR
  
  term_y <- c()  # track alt box bottom for optional equation placement
  
  # ---- recursive drawing ----
  draw_node <- function(nd,prefix_prod=1,parent=NULL){
    xy <- to_xy(nd$node_name)
    box <- draw_label_box(xy[1],xy[2],nd$node_name,pad=pad_node,cex=cex_node)
    
    # CR label
    if(!names_only && show_cr && is.finite(node_cr(nd)))
      text(box$xmax+gap_cr,xy[2],labels=paste0("CR=",formatC(node_cr(nd), digits = cr_digits, format = "f", drop0trailing = TRUE)),
           adj=c(0,0.5),cex=cex_cr)
    
    # internal
    if(!is.null(nd$children) && length(nd$children)>0){
      wloc <- local_w(nd); chn <- names(nd$children); wloc <- wloc[chn]
      for(ch in seq_along(chn)){
        ch_nm <- chn[ch]; ch_xy <- to_xy(ch_nm)
        segments(xy[1],box$ymin,ch_xy[1],to_xy(ch_nm)[2]+0.02)
        if(!names_only)
          draw_edge_label(xy[1],box$ymin,ch_xy[1],to_xy(ch_nm)[2]+0.02,
                          formatC(wloc[[ch_nm]], digits = digits, format = "f", drop0trailing = TRUE),
                          ch,length(chn),cex=cex_prob)
        draw_node(nd$children[[ch_nm]],prefix_prod=prefix_prod*as.numeric(wloc[[ch_nm]]),parent=nd)
      }
      return()
    }
    
    # terminal
    wloc <- local_w(nd)
    span_max <- 0.26; margin <- 0.02
    span <- span_max
    if(!is.null(parent)){
      sib_names <- names(parent$children)
      idx <- match(nd$node_name,sib_names)
      x_here <- xy[1]
      x_left  <- if(!is.na(idx)&&idx>1)  to_xy(sib_names[idx-1])[1] else NA
      x_right <- if(!is.na(idx)&&idx<length(sib_names)) to_xy(sib_names[idx+1])[1] else NA
      left_gap  <- if(is.na(x_left))  Inf else (x_here-x_left)-margin
      right_gap <- if(is.na(x_right)) Inf else (x_right-x_here)-margin
      half_gap  <- max(0.06,0.45*min(left_gap,right_gap))
      span <- min(span_max,2*half_gap)
    }
    drop <- 0.18 + 0.5*strheight("M",cex=cex_alt)
    y_alt <- xy[2]-drop
    x_alts <- seq(xy[1]-span/2,xy[1]+span/2,length.out=length(wloc))
    
    for(i in seq_along(wloc)){
      alt <- names(wloc)[i]
      x2 <- x_alts[i]; y2 <- y_alt+0.02
      segments(xy[1],box$ymin,x2,y2)
      if(!names_only)
        draw_edge_label(xy[1],box$ymin,x2,y2,
                        formatC(wloc[[i]], digits = digits, format = "f", drop0trailing = TRUE),
                        i,length(wloc),cex=cex_prob)
      abox <- draw_label_box(x2,y_alt,alt,pad=pad_alt,cex=cex_alt)
      term_y <<- c(term_y, abox$ymin)
      if(!names_only && layout == "expanded"){
        contrib <- prefix_prod*as.numeric(wloc[[i]])
        draw_text_bg(x2,abox$ymin-0.014,formatC(contrib, digits = digits, format = "f", drop0trailing = TRUE),
                     cex=cex_contr,pad=0.003)
      }
    }
  }
  
  draw_node(tree,1,NULL)
  
  # ---- equations (optional) ----
  if (draw_eq) {
    usr <- par("usr")
    y_floor <- if(length(term_y)) min(term_y) else 0.12
    y_eq <- max(0.08, y_floor - eq_y_offset)
    draw_text_bg(eq_x, y_eq + 0.08, finals_line, cex=cex_eq, pad=0.005)
    draw_text_bg(eq_x, y_eq, paste(eq_line, collapse="\n"), cex=cex_eq, pad=0.005)
  }
  
  invisible(list(totals = totals))
}



# -------- recursive hierarchy (required by ahp_solve) --------
ahp_hierarchy <- function(node,
                          method = c("eigen","column"),
                          cr_threshold = 0.10,
                          check_reciprocity = TRUE,
                          node_name = "ROOT",
                          prefix_per_person = NULL,   # list length P, numeric
                          prefix_group = NULL) {      # numeric
  `%||%` <- function(a,b) if (is.null(a)) b else a
  .is_square <- function(M) is.matrix(M) && nrow(M) == ncol(M)
  .is_positive <- function(M) all(M > 0)
  .is_reciprocal <- function(M, tol = 1e-8) {
    all(abs(M * t(M) - 1) < tol) && all(abs(diag(M) - 1) < tol)
  }
  .normalize_columns <- function(M) sweep(M, 2, colSums(M), "/")
  .principal_eigen_weights <- function(M) {
    ev <- eigen(M); i <- which.max(Re(ev$values)); w <- Re(ev$vectors[, i]); w <- abs(w); w/sum(w)
  }
  .column_method_weights <- function(M) {
    N <- .normalize_columns(M); w <- rowMeans(N); w/sum(w)
  }
  .consistency <- function(M, w) {
    n <- nrow(M); w <- as.numeric(w)/sum(w)
    Aw <- as.numeric(M %*% w)
    lambda_max <- sum(Aw)
    CI <- if (n > 2) (lambda_max - n)/(n - 1) else 0
    RI <- if (n <= 2) 0 else (1.98 * (n - 2)) / n
    CR <- if (RI > 0) CI/RI else 0
    list(lambda_max=lambda_max, CI=CI, RI=RI, CR=CR, n=n)
  }
  .solve_one <- function(M, method = c("eigen","column")) {
    method <- match.arg(method)
    w <- if (method == "eigen") .principal_eigen_weights(M) else .column_method_weights(M)
    cons <- .consistency(M, w)
    list(weights = w, consistency = cons, normalized = .normalize_columns(M))
  }
  .as_person_list <- function(x) {
    if (is.matrix(x)) list(x)
    else if (is.list(x) && all(vapply(x, is.matrix, logical(1)))) x
    else if (is.array(x) && length(dim(x)) == 3) lapply(seq_len(dim(x)[3]), function(p) x[,,p])
    else stop("`matrix` must be: a matrix, a list of matrices, or a 3D array [n,n,P].")
  }
  .geomean_mats <- function(mats) {
    n <- nrow(mats[[1]])
    if (!all(vapply(mats, function(M) .is_square(M) && nrow(M)==n && ncol(M)==n, logical(1))))
      stop("All person matrices must share the same square size.")
    if (!all(vapply(mats, .is_positive, logical(1)))) stop("All entries must be > 0.")
    logsum <- Reduce(`+`, lapply(mats, log))
    G <- exp(logsum / length(mats)); diag(G) <- 1
    G <- (G + 1/t(G)) / 2; diag(G) <- 1
    G
  }
  
  method <- match.arg(method)
  
  mats_p <- .as_person_list(node$matrix)
  P <- length(mats_p)
  
  item_names <- rownames(mats_p[[1]]) %||% colnames(mats_p[[1]])
  if (is.null(item_names)) item_names <- paste0("I", seq_len(nrow(mats_p[[1]])))
  
  if (!all(vapply(mats_p, .is_square, logical(1)))) stop(sprintf("Node '%s': non-square matrix.", node_name))
  if (!all(vapply(mats_p, .is_positive, logical(1)))) stop(sprintf("Node '%s': non-positive entries.", node_name))
  if (check_reciprocity && !all(vapply(mats_p, .is_reciprocal, logical(1))))
    stop(sprintf("Node '%s': at least one person's matrix is not reciprocal.", node_name))
  
  per_person <- lapply(seq_len(P), function(p) {
    r <- .solve_one(mats_p[[p]], method = method)
    names(r$weights) <- item_names
    r
  })
  names(per_person) <- paste0("person_", seq_len(P))
  
  A_group <- .geomean_mats(mats_p)
  if (check_reciprocity && !.is_reciprocal(A_group))
    stop(sprintf("Node '%s': aggregated matrix is not reciprocal.", node_name))
  group_res <- .solve_one(A_group, method = method)
  names(group_res$weights) <- item_names
  
  has_children <- is.list(node$children) && length(node$children) > 0
  
  if (is.null(prefix_per_person)) {
    prefix_per_person <- lapply(seq_len(P), function(i) 1)
    names(prefix_per_person) <- names(per_person)
  }
  if (is.null(prefix_group)) prefix_group <- 1
  
  leaves_per_person <- lapply(seq_len(P), function(i) setNames(numeric(0), character(0)))
  names(leaves_per_person) <- names(per_person)
  leaves_group <- setNames(numeric(0), character(0))
  
  children_out <- NULL
  
  if (has_children) {
    child_names <- names(node$children)
    if (is.null(child_names)) stop(sprintf("Node '%s': children must be a named list.", node_name))
    if (!all(child_names %in% item_names))
      stop(sprintf("Node '%s': child names (%s) must match row/col names (%s).",
                   node_name, paste(child_names, collapse=", "), paste(item_names, collapse=", ")))
    
    per_person <- lapply(per_person, function(pp) { pp$weights <- pp$weights[child_names]; pp })
    group_res$weights <- group_res$weights[child_names]
    
    children_out <- node$children
    for (k in seq_along(children_out)) {
      cname <- child_names[k]
      child <- children_out[[k]]
      
      child_prefix_per_person <- lapply(seq_len(P), function(i) {
        as.numeric(prefix_per_person[[i]]) * per_person[[i]]$weights[[cname]]
      })
      names(child_prefix_per_person) <- names(per_person)
      child_prefix_group <- as.numeric(prefix_group) * group_res$weights[[cname]]
      
      sub <- ahp_hierarchy(child,
                           method = method, cr_threshold = cr_threshold,
                           check_reciprocity = check_reciprocity,
                           node_name = cname,
                           prefix_per_person = child_prefix_per_person,
                           prefix_group = child_prefix_group)
      
      for (i in seq_len(P)) {
        leaves_per_person[[i]] <- c(leaves_per_person[[i]], sub$leaves$per_person[[i]])
      }
      leaves_group <- c(leaves_group, sub$leaves$group)
      children_out[[k]] <- sub
    }
    
  } else {
    # terminal: accumulate leaves
    for (nm in item_names) {
      for (i in seq_len(P)) {
        val <- as.numeric(prefix_per_person[[i]]) * per_person[[i]]$weights[[nm]]
        cur <- leaves_per_person[[i]][nm]; if (is.na(cur)) cur <- 0
        leaves_per_person[[i]][nm] <- cur + val
      }
      val_g <- as.numeric(prefix_group) * group_res$weights[[nm]]
      cur_g <- leaves_group[nm]; if (is.na(cur_g)) cur_g <- 0
      leaves_group[nm] <- cur_g + val_g
    }
  }
  
  is_consistent_per_person <- vapply(per_person, function(pp) pp$consistency$CR <= cr_threshold, logical(1))
  is_consistent_group      <- group_res$consistency$CR <= cr_threshold
  
  list(
    node_name   = node_name,
    per_person  = per_person,
    group       = list(matrix = A_group, weights = group_res$weights,
                       consistency = group_res$consistency, normalized = group_res$normalized),
    is_consistent = list(per_person = is_consistent_per_person, group = is_consistent_group),
    children    = children_out,
    leaves      = list(per_person = leaves_per_person, group = leaves_group)
  )
}


# ---------- top-level ----------
ahp_solve <- function(model,
                      method = c("eigen","column"),
                      cr_threshold = 0.10,
                      check_reciprocity = TRUE,
                      plot = FALSE,
                      plot_targets = c("group","per_person"),
                      plot_prefix = "ahp_paths",
                      digits = 3) {
  method <- match.arg(method)
  tree <- ahp_hierarchy(model, method = method, cr_threshold = cr_threshold,
                        check_reciprocity = check_reciprocity, node_name = "Goal")
  
  # ---- aggregate duplicate names by sum, then normalize ----
  agg_sum <- function(v) {
    if (length(v) == 0) return(v)
    v[is.na(v)] <- 0
    out <- tapply(as.numeric(v), INDEX = names(v), FUN = sum)
    setNames(as.numeric(out), names(out))
  }
  
  per_person_globals <- lapply(tree$leaves$per_person, function(v) {
    v <- agg_sum(v); s <- sum(v); if (is.na(s) || s <= 0) v else v / s
  })
  group_global <- { v <- agg_sum(tree$leaves$group); s <- sum(v); if (is.na(s) || s <= 0) v else v / s }
  
  # ---- Optional plotting ----
  plot_files <- list()
  if (isTRUE(plot)) {
    # group
    if ("group" %in% plot_targets) {
      f <- paste0(plot_prefix, "_group.png")
      ahp_plot_paths(tree, who = "group", file = f, digits = digits,
                     main_title = sprintf("AHP Paths (%s)", method))
      plot_files$group <- f
    }
    # each person
    if ("per_person" %in% plot_targets) {
      for (nm in names(tree$per_person)) {
        f <- paste0(plot_prefix, "_", nm, ".png")
        ahp_plot_paths(tree, who = nm, file = f, digits = digits,
                       main_title = sprintf("AHP Paths (%s) — %s", method, nm))
        plot_files[[nm]] <- f
      }
    }
  }
  
  list(
    tree = tree,
    global = list(per_person = per_person_globals, group = group_global),
    method = method,
    cr_threshold = cr_threshold,
    plots = plot_files           # paths to PNGs (if any)
  )
}



# =========================================================
# AHP → MathJax HTML report WITH:
# - labeled matrices (row + col headers)
# - full normalization, row-weight, consistency steps (+)
# - copyable LaTeX blocks
# =========================================================
ahp_export_html <- function(tree,
                            who = c("group","person_1"),
                            file = "ahp_report.html",
                            digits = 3,
                            title  = "AHP Report",
                            show_copy_buttons = TRUE) {
  
  who <- as.character(who)
  
  # ---------- format helpers ----------
  fmt  <- function(x) formatC(x, digits = digits, format = "f", drop0trailing = TRUE)
  esc_html <- function(s) {
    s <- gsub("&","&amp;", s, fixed = TRUE)
    s <- gsub("<","&lt;",  s, fixed = TRUE)
    s <- gsub(">","&gt;",  s, fixed = TRUE)
    s
  }
  esc_tex_id <- function(s) gsub("_","\\_", s, fixed = TRUE)
  
  join_plus <- function(x) paste(fmt(as.numeric(x)), collapse = " + ")
  join_plus_terms <- function(coef, varnames) {
    stopifnot(length(coef) == length(varnames))
    paste(sprintf("%s\\,%s", fmt(coef), varnames), collapse = " + ")
  }
  
  # ---------- labeled matrix (row/col names outside) ----------
  bm_labeled <- function(M, label = NULL) {
    rn <- rownames(M); if (is.null(rn)) rn <- as.character(seq_len(nrow(M)))
    cn <- colnames(M); if (is.null(cn)) cn <- as.character(seq_len(ncol(M)))
    
    header <- paste(c("", paste0("\\text{", esc_tex_id(cn), "}")), collapse = " & ")
    body_rows <- apply(cbind(rn, M), 1, function(row) {
      lab <- paste0("\\text{", esc_tex_id(row[1]), "}")
      vals <- paste(fmt(as.numeric(row[-1])), collapse = " & ")
      paste(lab, vals, sep = " & ")
    })
    
    matrix_tex <- paste0(
      "\\begin{array}{c|", paste(rep("c", ncol(M)), collapse=""), "}\n",
      header, " \\\\\n\\hline\n",
      paste(body_rows, collapse = " \\\\\n"), "\n",
      "\\end{array}"
    )
    prefix <- if (!is.null(label)) paste0("\\text{", esc_tex_id(label), "} = ") else ""
    paste0("\\[", prefix, "\\left[", matrix_tex, "\\right]\\]")
  }
  
  # ---------- math helpers ----------
  normalize_columns <- function(A) sweep(A, 2, colSums(A), "/")
  local_w <- function(nd) if (startsWith(who,"person_")) nd$per_person[[who]]$weights else nd$group$weights
  get_matrix <- function(nd) {
    if (startsWith(who,"person_")) {
      N <- nd$per_person[[who]]$normalized
      if (!is.null(N)) { D <- diag(1/diag(N)); A <- as.matrix(N %*% D); diag(A) <- 1; return(A) }
      nd$group$matrix
    } else nd$group$matrix
  }
  consistency <- function(A, w) {
    n <- nrow(A); w <- as.numeric(w); w <- w/sum(w)
    Aw <- as.numeric(A %*% w)
    lambda <- sum(Aw)
    CI <- if (n > 2) (lambda - n)/(n-1) else 0
    RI <- if (n <= 2) 0 else (1.98*(n-2))/n
    CR <- if (RI > 0) CI/RI else 0
    list(Aw=Aw, lambda=lambda, CI=CI, RI=RI, CR=CR, n=n)
  }
  
  # collect paths (if helpers exist)
  paths <- if (exists(".collect_paths", mode="function")) {
    df <- .collect_paths(tree, who = who, prefix_factors = numeric(0), digits = digits)
    totals <- .sum_by_alt(df); totals[is.na(totals)] <- 0; totals <- totals / sum(totals)
    list(df=df, totals=totals)
  } else NULL
  
  # ---------- HTML builders ----------
  block <- function(title_html, raw_latex_vec) {
    raw <- paste(raw_latex_vec, collapse = "\n")
    c(
      sprintf("<h4>%s</h4>", title_html),
      raw_latex_vec,
      if (show_copy_buttons)
        "<div class='copyrow'><button class='copybtn' onclick='copyNext(this)'>Copy LaTeX</button></div>" else NULL,
      sprintf("<pre class='latex'>%s</pre>", esc_html(raw))
    )
  }
  
  # ---------- main recursion ----------
  node_to_html <- function(nd, level = 1) {
    nm <- esc_tex_id(nd$node_name)
    A  <- get_matrix(nd)
    N  <- normalize_columns(A)
    w  <- local_w(nd)
    if (!is.null(rownames(A))) { w <- w[rownames(A)]; names(w) <- rownames(A) }
    
    rn <- rownames(A); if (is.null(rn)) rn <- as.character(seq_len(nrow(A)))
    cn <- colnames(A); if (is.null(cn)) cn <- as.character(seq_len(ncol(A)))
    
    rmN <- rowMeans(N); w_col <- rmN / sum(rmN)
    
    # --- beginning matrix ---
    html <- c(
      sprintf("<h3>Node: %s</h3>", esc_html(nd$node_name)),
      block("Beginning matrix", c(bm_labeled(A, label = nd$node_name)))
    )
    
    # --- normalization (explicit +) ---
    norm_steps <- character(0)
    for (j in seq_len(ncol(A))) {
      col_j <- A[, j]
      S <- sum(col_j)
      num_line <- paste0("\\(S_{", esc_tex_id(cn[j]), "} = ",
                         paste(sprintf("a_{%s%s}", seq_along(col_j), j), collapse = " + "),
                         " = ", join_plus(col_j), " = ", fmt(S), "\\)")
      ij_lines <- sapply(seq_len(nrow(A)), function(i) {
        paste0("\\(N_{", esc_tex_id(rn[i]), ",", esc_tex_id(cn[j]), "} = \\frac{a_{", i, j, "}}{S_{", esc_tex_id(cn[j]),
               "}} = \\frac{", fmt(A[i,j]), "}{", fmt(S), "} = ", fmt(N[i,j]), "\\)")
      })
      norm_steps <- c(norm_steps, num_line, ij_lines)
    }
    html <- c(html, block("Calculation to normalisation", norm_steps))
    
    # --- normalized matrix ---
    html <- c(html, block("Normalized matrix", c(bm_labeled(N, label = paste0(nd$node_name, "_norm")))))
    
    # --- row weights ---
    rw_steps <- sapply(seq_len(nrow(N)), function(i) {
      inner_sum <- join_plus(N[i, ])
      paste0("\\(w_{", esc_tex_id(rn[i]), "} = \\tfrac{1}{", ncol(N), "}(",
             inner_sum, ") = ", fmt(rmN[i]), "\\)")
    })
    html <- c(html,
              block("Calculation to Row weights", rw_steps),
              block("Local weights (row mean of normalized columns)", c(paste0("\\[w = \\begin{bmatrix}", paste(fmt(w_col), collapse = " & "), "\\end{bmatrix}^T\\]")))
    )
    
    # --- consistency (+ signs) ---
    cons <- consistency(A, w_col)
    w_sym <- if (!is.null(names(w_col))) paste0("w_{", esc_tex_id(names(w_col)), "}") else paste0("w_{", seq_along(w_col), "}")
    Aw_lines <- sapply(seq_len(nrow(A)), function(i) {
      terms <- join_plus_terms(A[i, ], w_sym)
      paste0("\\((Aw)_{", esc_tex_id(rn[i]), "} = ", terms, " = ", fmt(cons$Aw[i]), "\\)")
    })
    lambda_terms <- paste(fmt(cons$Aw), collapse = " + ")
    ci_line <- paste0("\\[CI = \\tfrac{\\lambda_{\\max}-n}{n-1} = \\tfrac{", fmt(cons$lambda), " - ", nrow(A), "}{", nrow(A)-1, "} = ", fmt(cons$CI), "\\]")
    ri_line <- paste0("\\[RI = ",
                      if (cons$n<=2) "0" else paste0("\\tfrac{1.98(n-2)}{n} = \\tfrac{1.98(", cons$n, "-2)}{", cons$n, "}"),
                      " = ", fmt(cons$RI), "\\]")
    cr_line <- paste0("\\[CR = ", if (cons$RI>0) paste0("\\tfrac{CI}{RI} = ", fmt(cons$CR)) else "0", "\\]")
    
    html <- c(html,
              block("Consistency check",
                    c(Aw_lines,
                      paste0("\\[\\lambda_{\\max} = ", lambda_terms, " = ", fmt(cons$lambda), "\\]"),
                      ci_line, ri_line, cr_line))
    )
    
    # --- local probabilities ---
    labs <- names(w_col); if (is.null(labs)) labs <- paste0("I", seq_along(w_col))
    probs_line <- paste(paste0(esc_tex_id(labs), ":\\; ", fmt(w_col[labs])), collapse = "\\quad ")
    html <- c(html, block("Local probabilities", c(paste0("\\[", probs_line, "\\]"))))
    
    # recurse
    if (!is.null(nd$children) && length(nd$children) > 0)
      for (ch in names(nd$children)) html <- c(html, node_to_html(nd$children[[ch]], level + 1))
    
    html
  }
  
  # ---------- assemble ----------
  styles <- "
  <style>
    body{font-family: system-ui,-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif; line-height:1.45; max-width:980px; margin:24px auto; padding:0 16px;}
    h2{margin:0 0 8px;}
    h3{margin:18px 0 8px; border-bottom:1px solid #eee; padding-bottom:6px;}
    h4{margin:14px 0 6px;}
    pre.latex{background:#f7f7f9; border:1px solid #eee; padding:10px; overflow:auto;}
    .copyrow{display:flex; justify-content:flex-end; margin:4px 0;}
    .copybtn{font-size:12px; padding:4px 8px; cursor:pointer; border:1px solid #999; background:#fff;}
  </style>"
  script <- "
  <script>
    function copyNext(btn){
      const pre = btn.parentElement.nextElementSibling;
      if (!pre) return;
      const txt = pre.innerText;
      navigator.clipboard.writeText(txt).then(()=>{
        const old = btn.textContent; btn.textContent='Copied!';
        setTimeout(()=>{btn.textContent=old;},900);
      });
    }
  </script>"
  
  head <- paste0(
    "<!doctype html><html><head><meta charset='utf-8'><title>", esc_html(title), "</title>",
    styles, script,
    "<script src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js' id='MathJax-script'></script>",
    "</head><body>"
  )
  
  header <- sprintf("<h2>%s — %s</h2>", esc_html(title),
                    if (!is.null(getOption("ahp.method"))) paste0("method: ", esc_html(getOption("ahp.method"))) else "")
  
  body <- node_to_html(tree, 1)
  
  # final section
  final_block <- character(0)
  if (!is.null(paths)) {
    totals <- paths$totals; df <- paths$df
    by_alt <- split(df, df$alt)
    finals_vec <- paste(paste0("P(", esc_tex_id(names(totals)), ") = ", fmt(totals)), collapse = "\\quad ")
    eq_lines <- vapply(names(totals), function(a) {
      parts <- paste(by_alt[[a]]$product_str, collapse = " + ")
      paste0("P(", esc_tex_id(a), ") = ", parts, " = ", fmt(totals[[a]]))
    }, character(1))
    winner <- names(totals)[which.max(totals)]
    
    final_block <- c(
      "<hr/>",
      "<h3>Global probabilities & decision</h3>",
      block("Global probability vector", c(paste0("\\[", finals_vec, "\\]"))),
      block("Path-sum equations", paste(paste0("\\(", eq_lines, "\\)"), collapse = "\n")),
      sprintf("<p><b>Decision:</b> choose <span>\\(%s\\)</span> with probability %s.</p>",
              esc_html(winner), fmt(totals[[winner]]))
    )
  }
  
  html <- c(head, header, body, final_block, "</body></html>")
  writeLines(html, con = file)
  invisible(normalizePath(file))
}




# Convenience: solve + export in one call
ahp_export_html_from_model <- function(model,
                                       who = "group",
                                       file = "ahp_report.html",
                                       method = c("column","eigen"),
                                       digits = 3,
                                       title = "AHP Report",
                                       cr_threshold = 0.10,
                                       check_reciprocity = TRUE) {
  method <- match.arg(method)
  options(ahp.method = method)
  res <- ahp_solve(model, method = method, cr_threshold = cr_threshold,
                   check_reciprocity = check_reciprocity)
  ahp_export_html(res$tree, who = who, file = file, digits = digits, title = title)
}


# =========================================================
# AHP → RAW LaTeX (copyable) and .tex exporter
# =========================================================

ahp_collect_latex <- function(tree, who = c("group","person_1"), digits = 3, title = "AHP Report") {
  who <- as.character(who)
  
  fmt  <- function(x) formatC(x, digits = digits, format = "f", drop0trailing = TRUE)
  esc  <- function(s) { s <- gsub("\\\\", "\\\\textbackslash{}", s); gsub("_","\\_", s, fixed = TRUE) }
  
  bm <- function(M) {
    rows <- apply(M, 1, function(r) paste(fmt(r), collapse = " & "))
    paste0("\\[\\begin{bmatrix}\n", paste(rows, collapse = " \\\\\n"), "\n\\end{bmatrix}\\]")
  }
  rowvec <- function(v) paste(fmt(as.numeric(v)), collapse = " \\; ")
  vecT <- function(v, name = NULL) {
    inner <- rowvec(v)
    if (length(name)) paste0("\\[", esc(name), " = \\begin{bmatrix} ", inner, " \\end{bmatrix}^{\\!T}\\]")
    else paste0("\\[\\begin{bmatrix} ", inner, " \\end{bmatrix}^{\\!T}\\]")
  }
  
  normalize_columns <- function(A) sweep(A, 2, colSums(A), "/")
  
  # pull the matrix and local weights at a node
  local_w <- function(nd) if (startsWith(who,"person_")) nd$per_person[[who]]$weights else nd$group$weights
  get_matrix <- function(nd) {
    if (startsWith(who,"person_")) {
      N <- nd$per_person[[who]]$normalized
      if (!is.null(N)) {
        D <- diag(1/diag(N))
        A <- as.matrix(N %*% D); diag(A) <- 1; return(A)
      }
      nd$group$matrix
    } else nd$group$matrix
  }
  
  # consistency via Aw and lambda = sum(Aw)
  consistency <- function(A, w) {
    n <- nrow(A); w <- as.numeric(w); w <- w/sum(w)
    Aw <- as.numeric(A %*% w)
    lambda <- sum(Aw)
    CI <- if (n > 2) (lambda - n)/(n-1) else 0
    RI <- if (n <= 2) 0 else (1.98*(n-2))/n
    CR <- if (RI > 0) CI/RI else 0
    list(Aw=Aw, lambda=lambda, CI=CI, RI=RI, CR=CR, n=n)
  }
  
  # Path equations for final section
  path_part <- {
    if (!exists(".collect_paths", mode = "function")) NULL else {
      df <- .collect_paths(tree, who = who, prefix_factors = numeric(0), digits = digits)
      totals <- .sum_by_alt(df); totals[is.na(totals)] <- 0; totals <- totals / sum(totals)
      list(df = df, totals = totals)
    }
  }
  
  # -------- Node → LaTeX --------
  node_tex <- function(nd, level = 1) {
    nm <- esc(nd$node_name)
    A  <- get_matrix(nd)
    N  <- normalize_columns(A)
    w  <- local_w(nd)
    if (!is.null(rownames(A))) { w <- w[rownames(A)]; names(w) <- rownames(A) }
    rmN <- rowMeans(N); w_col <- rmN / sum(rmN)
    
    # Column-sum lines
    colS <- colSums(A)
    sumlines <- paste0("\\(\\sum_i a_{i", seq_len(ncol(A)), "} = ", fmt(colS), "\\)")
    # Row-weight lines
    rnames <- rownames(N); if (is.null(rnames)) rnames <- as.character(seq_len(nrow(N)))
    rw_steps <- sapply(seq_len(nrow(N)), function(i) {
      paste0("\\(w_{", esc(rnames[i]), "} = \\tfrac{1}{", ncol(N), "}(",
             rowvec(N[i,]), ") = ", fmt(rmN[i]), "\\)")
    })
    
    cons <- consistency(A, w_col)
    Aw_lines <- sapply(seq_len(nrow(A)), function(i) {
      paste0("\\((Aw)_{", i, "} = ", rowvec(A[i,] * w_col), " = ", fmt(cons$Aw[i]), "\\)")
    })
    
    labs <- names(w_col); if (is.null(labs)) labs <- paste0("I", seq_along(w_col))
    loc_line <- paste(paste0(esc(labs), ":\\; ", fmt(w_col[labs])), collapse = "\\quad ")
    
    c(
      sprintf("\\subsection*{Node: %s}", nm),
      "\\paragraph{Beginning matrix}", bm(A),
      "\\paragraph{Calculation to normalisation}", paste(sumlines, collapse = " \\\\ "),
      "\\paragraph{Normalized matrix}", bm(N),
      "\\paragraph{Calculation to row weights}", paste(rw_steps, collapse = " \\\\ "),
      "\\paragraph{Local weights}", vecT(w_col, "w"),
      "\\paragraph{Consistency check}",
      paste(Aw_lines, collapse = " \\\\ "),
      paste0("\\[\\lambda_{\\max} = \\sum_i (Aw)_i = ", fmt(cons$lambda), "\\]"),
      paste0("\\[CI = \\tfrac{\\lambda_{\\max}-n}{n-1} = \\tfrac{", fmt(cons$lambda), " - ", nrow(A), "}{", nrow(A)-1, "} = ", fmt(cons$CI), "\\]"),
      paste0("\\[RI = ", if (cons$n<=2) "0" else paste0("\\tfrac{1.98(n-2)}{n} = \\tfrac{1.98(", cons$n, "-2)}{", cons$n, "}"),
             " = ", fmt(cons$RI), "\\]"),
      paste0("\\[CR = ", if (cons$RI>0) paste0("\\tfrac{CI}{RI} = ", fmt(cons$CR)) else "0", "\\]"),
      "\\paragraph{Local probabilities}",
      paste0("\\[", loc_line, "\\]")
    )
  }
  
  # DFS
  walk <- function(nd, lvl = 1) {
    out <- node_tex(nd, lvl)
    if (!is.null(nd$children) && length(nd$children) > 0) {
      for (k in names(nd$children)) out <- c(out, walk(nd$children[[k]], lvl + 1))
    }
    out
  }
  
  body <- c(sprintf("\\section*{%s}", esc(title)), walk(tree, 1))
  
  # Final global section
  if (!is.null(path_part)) {
    totals <- path_part$totals; df <- path_part$df
    by_alt <- split(df, df$alt)
    # vector line
    finals_vec <- paste(paste0("P(", esc(names(totals)), ") = ", fmt(totals)), collapse = "\\quad ")
    # explicit equations
    eq_lines <- vapply(names(totals), function(a) {
      parts <- paste(by_alt[[a]]$product_str, collapse = " + ")
      paste0("P(", esc(a), ") = ", parts, " = ", fmt(totals[[a]]))
    }, character(1))
    winner <- names(totals)[which.max(totals)]
    body <- c(
      body,
      "\\section*{Global probabilities and decision}",
      paste0("\\[", finals_vec, "\\]"),
      paste(paste0("\\(", eq_lines, "\\)"), collapse = " \\\\ "),
      paste0("\\paragraph{Decision} Choose \\(", esc(winner), "\\) with probability ", fmt(totals[[winner]]), ".")
    )
  }
  
  paste(c(body, ""), collapse = "\n")
}

ahp_export_latex <- function(tree,
                             who = c("group","person_1"),
                             file = "ahp_report.tex",
                             standalone = TRUE,
                             digits = 3,
                             title = "AHP Report") {
  who <- as.character(who)
  body <- ahp_collect_latex(tree, who = who, digits = digits, title = title)
  
  if (!standalone) {
    writeLines(body, file)
    return(invisible(normalizePath(file)))
  }
  
  preamble <- c(
    "\\documentclass[11pt]{article}",
    "\\usepackage[margin=1in]{geometry}",
    "\\usepackage{amsmath,amssymb,bm}",
    "\\usepackage{newtxtext,newtxmath}",
    "\\setlength{\\parskip}{6pt}",
    "\\setlength{\\parindent}{0pt}",
    "\\begin{document}"
  )
  end <- "\\end{document}"
  
  writeLines(c(preamble, body, end), file)
  invisible(normalizePath(file))
}

