# ============================================================
`%||%` <- function(a,b) if (is.null(a)) b else a


export_payoff_html_transport <- function(
    file, P, p = NULL,
    type = c("gain","loss"),
    # labels
    row_label       = "Sipariş Miktarı",
    col_group_label = "İstem Miktarı",
    prob_label      = "Olasılık",
    ev_label        = "Beklenen Değer (EV)",
    # display toggles
    show_probs_row  = TRUE,
    show_ev_col     = TRUE,
    # marking toggles
    mark_ml = TRUE,     # mark most-likely column (+ best cell)
    mark_ev = TRUE,     # mark EV-best row (+ EV cell)
    # digits
    digits_cell = 0,
    digits_prob = 3,
    digits_ev   = 0
){
  type <- match.arg(type)
  
  A <- as.matrix(P)
  alt_names   <- rownames(A); if (is.null(alt_names))   alt_names   <- paste0("O", seq_len(nrow(A)))
  state_names <- colnames(A); if (is.null(state_names)) state_names <- paste0("S", seq_len(ncol(A)))
  rownames(A) <- alt_names; colnames(A) <- state_names
  
  # probabilities (uniform if missing)
  if (is.null(p)) {
    p_use <- rep(1/ncol(A), ncol(A))
  } else {
    p_use <- as.numeric(p); s <- sum(p_use)
    if (is.na(s) || s <= 0) stop("`p` must sum to a positive value.")
    if (abs(s - 1) > 1e-8) p_use <- p_use / s
  }
  names(p_use) <- state_names
  
  EV <- if (show_ev_col) as.numeric(A %*% p_use) else NULL
  if (!is.null(EV)) names(EV) <- alt_names
  
  # ---- which to mark ----
  j_ml <- if (mark_ml) which.max(p_use) else NA_integer_
  i_ml <- if (mark_ml) {
    col_vals <- A[, j_ml]
    if (type == "gain") which.max(col_vals) else which.min(col_vals)
  } else NA_integer_
  
  i_ev <- if (mark_ev && !is.null(EV)) {
    if (type == "gain") which.max(EV) else which.min(EV)
  } else NA_integer_
  
  # ---- helpers ----
  fmt_num <- function(x, d) {
    vapply(x, function(xx) if (is.na(xx)) "" else
      formatC(xx, digits = d, format = "f", big.mark = " ", decimal.mark = "."),
      character(1))
  }
  
  css <- "
  <style>
    :root{
      --grid:#333; --head-bg:#f0f0f0; --lbl-bg:#fafafa;
      --lilac-bg:#F5F3FF;  --lilac-br:#7C3AED;   /* EV row */
      --green-bg:#ECFDF5;  --green-br:#16A34A;  /* ML col */
      --amber-bg:#FFF7ED;  --amber-br:#F59E0B;  /* best cell */
      --prob-bg:#FCFCFC;   --evcol-bg:#F8F9FF;  /* EV col */
    }
    body { font-family:'Segoe UI', Roboto, Arial, sans-serif; margin:18px; color:#111; }
    table.tt { border-collapse:collapse; border:1.5px solid var(--grid); margin-bottom:14px; }
    table.tt th, table.tt td { border:1px solid var(--grid); padding:6px 10px; }
    thead th { background:var(--head-bg); font-weight:700; text-align:center; }
    td.lbl { font-weight:700; background:var(--lbl-bg); white-space:nowrap; }
    td.num { text-align:right; }
    .corner { background:var(--head-bg); font-weight:700; text-align:center; }
    .prob   { background:var(--prob-bg); font-size:12px; }
    .evh    { background:var(--head-bg); }
    .evc    { background:var(--evcol-bg); font-weight:700; }

    /* ----- MARKS ----- */

    /* full green band for ML column */
    .mark-col-hdr { background:var(--green-bg) !important; border-bottom:3px solid var(--green-br) !important; }
    .ml-col       { background:var(--green-bg) !important; border-left:3px solid var(--green-br) !important; border-right:3px solid var(--green-br) !important; }

    /* purple framed lilac row for EV best */
    .mark-row td  { background:var(--lilac-bg) !important; }
    .mark-row td.lbl { border-left:2px solid var(--lilac-br) !important; }
    .mark-row td   { border-top:2px solid var(--lilac-br) !important; border-bottom:2px solid var(--lilac-br) !important; }
    .mark-row td:last-child { border-right:2px solid var(--lilac-br) !important; }

    /* amber best cell *inside* ML column, keep outer green borders via inset box-shadow */
    .best-ml { background:var(--amber-bg) !important; font-weight:700; box-shadow: inset 0 0 0 3px var(--amber-br); }

    /* EV winning cell */
    .mark-ev-cell { background:var(--amber-bg) !important; font-weight:700; box-shadow: inset 0 0 0 3px var(--amber-br); }

    caption { caption-side: top; text-align:left; font-weight:700; margin:0 0 8px 0; }
  </style>"
  
  # header
  thead_top <- paste0(
    "<tr>",
    sprintf("<th class='corner' rowspan='2'>%s</th>", row_label),
    sprintf("<th colspan='%d'>%s</th>", ncol(A), col_group_label),
    if (!is.null(EV)) sprintf("<th class='evh' rowspan='2'>%s</th>", ev_label) else "",
    "</tr>"
  )
  th_states <- paste(vapply(seq_along(state_names), function(j){
    cls <- if (!is.na(j_ml) && j==j_ml && mark_ml) " class='mark-col-hdr'" else ""
    sprintf("<th%s>%s</th>", cls, state_names[j])
  }, character(1)), collapse = "")
  thead_mid <- paste0("<tr>", th_states, "</tr>")
  
  prob_row <- if (show_probs_row) {
    c("<tr class='prob'>",
      sprintf("<td class='lbl'>%s</td>", prob_label),
      paste(sprintf("<td class='num'>%s</td>", fmt_num(p_use, digits_prob)), collapse = ""),
      if (!is.null(EV)) "<td class='evc'></td>" else "",
      "</tr>")
  } else character(0)
  
  # body
  body_rows <- unlist(lapply(seq_len(nrow(A)), function(i){
    row_cls <- if (!is.na(i_ev) && i==i_ev && mark_ev) " class='mark-row'" else ""
    open <- paste0("<tr", row_cls, ">")
    
    cells <- paste(vapply(seq_len(ncol(A)), function(j){
      cls <- "num"
      if (!is.na(j_ml) && j==j_ml && mark_ml) cls <- paste(cls, "ml-col")
      if (!is.na(i_ml) && !is.na(j_ml) && i==i_ml && j==j_ml && mark_ml) cls <- paste(cls, "best-ml")
      sprintf("<td class='%s'>%s</td>", cls, fmt_num(A[i, j], digits_cell))
    }, character(1)), collapse = "")
    
    ev_cell <- if (!is.null(EV)) {
      cls <- "num evc"
      if (!is.na(i_ev) && i==i_ev && mark_ev) cls <- paste(cls, "mark-ev-cell")
      sprintf("<td class='%s'>%s</td>", cls, fmt_num(EV[i], digits_ev))
    } else ""
    
    c(open, sprintf("<td class='lbl'>%s</td>", alt_names[i]), cells, ev_cell, "</tr>")
  }))
  
  html <- c(
    "<!DOCTYPE html>",
    "<html lang='tr'><head><meta charset='utf-8'/>",
    "<title>Payoff</title>", css, "</head><body>",
    "<table class='tt'>",
    "<caption>Payoff Tablosu</caption>",
    "<thead>", thead_top, thead_mid, "</thead>",
    "<tbody>", prob_row, body_rows, "</tbody>",
    "</table>",
    "</body></html>"
  )
  
  con <- file(file, open = "wb"); on.exit(close(con), add = TRUE)
  writeLines(html, con, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/"))
}



plot_decision_tree_classic <- function(branch_costs, state_labels, probs, payoffs_matrix,
                                       decision_label = "K", chance_labels = NULL,
                                       main = "Decision tree (classic layout)",
                                       # layout parameters
                                       cost_perp   = 0.020,
                                       state_perp  = 0.015,
                                       ev_v_offset = 0.040,
                                       payoff_dx   = 0.015,
                                       # font sizes
                                       cex_main   = 1.2,
                                       cex_nodes  = 1.05,
                                       cex_cost   = 0.95,
                                       cex_state  = 0.90,
                                       cex_payoff = 0.95,
                                       # cost markers
                                       draw_cost_marks = FALSE,
                                       cost_mark_len   = 0.020,
                                       cost_mark_gap   = 0.010,
                                       # pruning and highlighting
                                       prune_by = c("expected_value","most_likely","expected_regret","minimax_regret"),
                                       type     = c("gain","loss"),
                                       most_likely_mode = c("first","any","average"),
                                       circle_metric = c("ev","criterion"),
                                       circle_metric_label = TRUE,     # EV=/ML=/ER=/MR=
                                       circle_digits = 2,
                                       show_circle_metric = TRUE,      # <- NEW: show/hide text above chance node
                                       color_best   = "black",
                                       color_pruned = "grey40",
                                       lwd_best     = 1.8,
                                       lwd_pruned   = 1.1,
                                       draw_prune_marks = TRUE,
                                       prune_mark_len = 0.012,
                                       prune_mark_gap = 0.006,
                                       # export
                                       file = NULL, width = 9, height = 6, dpi = 300,
                                       # summary under figure
                                       show_summary = TRUE, summary_cex = 0.9
) {
  `%||%` <- function(a,b) if (is.null(a)) b else a
  prune_by <- match.arg(prune_by)
  type     <- match.arg(type)
  most_likely_mode <- match.arg(most_likely_mode)
  circle_metric <- match.arg(circle_metric)
  
  P <- as.matrix(payoffs_matrix)
  k <- nrow(P); m <- ncol(P)
  
  # --- in classic layout, "chance_labels" really represent ALTERNATIVE labels ---
  alt_labels <- chance_labels %||% rownames(P) %||% paste0("A", seq_len(k))
  stopifnot(length(state_labels) == m)
  
  # normalize probabilities
  if (!is.null(probs)) { 
    s <- sum(probs) 
    if (abs(s-1)>1e-8) probs <- probs/s 
  }
  
  # regret matrix
  state_best <- apply(P, 2, if (type=="gain") max else min)
  best_mat   <- matrix(state_best, nrow=k, ncol=m, byrow=TRUE)
  Reg <- if (type=="gain") best_mat - P else P - best_mat
  
  # Expected Value vector
  EV_vec <- if (!is.null(probs)) as.numeric(P %*% probs) else rep(NA_real_, k)
  names(EV_vec) <- alt_labels
  
  # Scores for pruning (we always maximize 'scores')
  scores <- switch(
    prune_by,
    expected_value = {
      if (is.null(probs)) stop("expected_value pruning requires `probs`.")
      if (type=="gain") EV_vec else -EV_vec
    },
    most_likely = {
      if (is.null(probs)) stop("most_likely pruning requires `probs`.")
      ml <- which(probs == max(probs))
      
      metric <- switch(
        most_likely_mode,
        first = {
          P[, ml[1]]
        },
        any = {
          if (type == "gain") {
            apply(P[, ml, drop = FALSE], 1, max)
          } else {
            apply(P[, ml, drop = FALSE], 1, min)
          }
        },
        average = {
          q <- probs[ml]/sum(probs[ml])
          as.numeric(P[, ml, drop = FALSE] %*% q)
        }
      )
      if (type=="gain") metric else -metric
    },
    expected_regret = {
      if (is.null(probs)) stop("expected_regret pruning requires `probs`.")
      -as.numeric(Reg %*% probs)
    },
    minimax_regret  = -apply(Reg, 1, max)
  )
  
  # --- which alternative / branches are "best" overall? ---
  best_pairs_global <- NULL  # (i,j) pairs to highlight at state-branch level
  
  if (prune_by == "most_likely") {
    # 1) Most-likely states (ties allowed)
    max_p <- max(probs)
    ml_states <- which(abs(probs - max_p) < 1e-12)
    
    # 2) Payoffs restricted to most-likely states
    ml_values <- P[, ml_states, drop = FALSE]
    
    # 3) For gain: highest payoff; for loss: lowest payoff (within ML states)
    if (type == "gain") {
      best_val <- max(ml_values)
      best_pairs <- which(abs(ml_values - best_val) < 1e-12, arr.ind = TRUE)
    } else {
      best_val <- min(ml_values)
      best_pairs <- which(abs(ml_values - best_val) < 1e-12, arr.ind = TRUE)
    }
    
    # 4) Map local state indices back to global state indices
    best_idx <- unique(best_pairs[, "row"])
    best_pairs_global <- cbind(
      i = best_pairs[, "row"],
      j = ml_states[best_pairs[, "col"]]
    )
    
  } else {
    best_idx <- which(scores == max(scores))
  }
  
  # Values to show above circles
  shown_vals <- switch(
    circle_metric,
    ev = EV_vec,
    criterion = switch(
      prune_by,
      expected_value  = EV_vec,
      most_likely     = {
        if (type == "gain") scores else -scores
      },
      expected_regret = -scores,
      minimax_regret  = -scores
    )
  )
  
  metric_prefix <- switch(
    prune_by,
    expected_value  = "EV=",
    most_likely     = "ML=",
    expected_regret = "ER=",
    minimax_regret  = "MR="
  )
  
  shown_labels <- if (circle_metric_label)
    paste0(metric_prefix, formatC(shown_vals, digits=circle_digits, format="f"))
  else
    formatC(shown_vals, digits=circle_digits, format="f")
  
  # --- Handle export device ---
  if (!is.null(file)) {
    ext <- tools::file_ext(file)
    if (ext == "png")      png(file, width=width, height=height, units="in", res=dpi)
    else if (ext == "pdf") pdf(file, width=width, height=height)
    else if (ext == "svg") svg(file, width=width, height=height)
    else stop("Unsupported file extension. Use .png, .pdf or .svg")
    on.exit(dev.off(), add=TRUE)
  } else if (is.null(dev.list())) {
    dev.new(width=width, height=height)
  }
  
  # --- Plotting ---
  bottom_mar <- if (show_summary) 3.8 else 1.5
  op <- par(mar=c(bottom_mar, 1.3, 2.5, 1.5)); on.exit(par(op), add=TRUE)
  
  plot.new(); plot.window(xlim=c(0,1), ylim=c(0,1))
  title(main, cex.main=cex_main)
  
  # Helpers --------------------------------------------------------
  draw_box <- function(x,y,lab,shape=c("square","circle"),cex=1,col="black",lwd=1.6){
    shape <- match.arg(shape)
    if (shape=="square"){
      w <- strwidth(lab,cex=cex)+0.03; h <- strheight("Mg",cex=cex)+0.02
      rect(x-w/2,y-h/2,x+w/2,y+h/2,lwd=lwd,border=col)
      text(x,y,lab,cex=cex,col=col)
      return(list(type="square",w=w,h=h))
    } else {
      r <- 0.015
      symbols(x,y,circles=r,inches=FALSE,add=TRUE,lwd=lwd,fg=col)
      if (!is.null(lab) && nzchar(lab)) {
        text(x,y,lab,cex=cex,col=col)
      }
      return(list(type="circle",r=r))
    }
  }
  edge <- function(x1,y1,x2,y2,lwd=1.2,col="black") segments(x1,y1,x2,y2,lwd=lwd,col=col)
  text_perp <- function(x1,y1,x2,y2,label,offset=0.015,cex=0.9,col="black"){
    mx <- (x1 + x2)/2; my <- (y1 + y2)/2
    dx <- x2 - x1; dy <- y2 - y1; L <- sqrt(dx*dx + dy*dy) + 1e-12
    nx <- -dy / L; ny <-  dx / L
    text(mx + offset*nx, my + offset*ny, labels=label, cex=cex, col=col)
  }
  hatch <- function(x,y,len=0.02,gap=0.010,col="black",lwd=1.2){
    segments(x-len,y+gap,x,y,col=col,lwd=lwd)
    segments(x-len,y-gap,x,y,col=col,lwd=lwd)
  }
  
  # coordinates
  x_dec <- 0.08; x_after <- 0.23; x_ch <- 0.30; x_mid <- 0.56; x_leaf <- 0.90
  y_seq <- seq(0.85,0.15,length.out=k); y_dec <- mean(y_seq)
  
  dec_geom <- draw_box(x_dec,y_dec,decision_label,"square",cex=cex_nodes)
  x_dec_right <- x_dec + dec_geom$w/2
  
  # -------------------- main drawing loop -------------------------
  for(i in seq_len(k)){
    yi <- y_seq[i]
    is_best_alt <- i %in% best_idx
    
    # Alternative-level color (K -> circle, circle outline, metric label)
    col_i <- if(is_best_alt) color_best else color_pruned
    lwd_i <- if(is_best_alt) lwd_best else lwd_pruned
    
    # branch from decision node to alt
    edge(x_dec_right,y_dec,0.23,yi,lwd=lwd_i,col=col_i)
    
    # --- classic layout -> label alternative on decision branch ---
    text_perp(x_dec_right, y_dec, 0.23, yi,
              label = alt_labels[i],
              offset = cost_perp*1.4,   # a bit further than cost label
              cex = cex_cost, col = col_i)
    
    # cost on branch (if any), slightly other side of the edge
    if(!is.null(branch_costs))
      text_perp(x_dec_right,y_dec,0.23,yi,
                label=branch_costs[i],
                offset=-cost_perp,cex=cex_cost,col=col_i)
    
    if(draw_cost_marks)
      hatch((x_dec_right+0.23)/2,(y_dec+yi)/2,len=cost_mark_len,gap=cost_mark_gap,col=col_i,lwd=lwd_i)
    
    # chance node (circle) WITHOUT alt text inside (classic representation)
    ch_geom <- draw_box(x_ch,yi,NULL,"circle",cex=cex_nodes,col=col_i,lwd=lwd_i)
    x_ch_left  <- x_ch - ch_geom$r
    x_ch_right <- x_ch + ch_geom$r
    edge(0.23,yi,x_ch_left,yi,lwd=lwd_i,col=col_i)
    
    # metric above circle (EV= / ML= / ER= / MR=) – now OPTIONAL
    if (show_circle_metric) {
      text(x_ch, yi + ev_v_offset, labels = shown_labels[i], cex = cex_cost, col = col_i)
    }
    
    if(!is_best_alt && draw_prune_marks){
      xm <- (0.23 + x_ch_left)/2
      hatch(xm+0.006, yi, len=prune_mark_len, gap=prune_mark_gap, col=col_i, lwd=lwd_i)
      hatch(xm-0.006, yi, len=prune_mark_len, gap=prune_mark_gap, col=col_i, lwd=lwd_i)
    }
    
    # state branches leaving this chance node
    y_states <- seq(yi + 0.08, yi - 0.08, length.out = m)
    for(j in seq_len(m)){
      yj <- y_states[j]
      
      # --- color per state-branch ---
      if (prune_by == "most_likely") {
        is_best_branch <- FALSE
        if (!is.null(best_pairs_global)) {
          is_best_branch <- any(best_pairs_global[, "i"] == i &
                                  best_pairs_global[, "j"] == j)
        }
        col_ij <- if (is_best_branch) color_best else color_pruned
        lwd_ij <- if (is_best_branch) lwd_best else lwd_pruned
      } else {
        col_ij <- col_i
        lwd_ij <- lwd_i
      }
      
      edge(x_ch_right, yi, x_mid, yj, lwd=lwd_ij, col=col_ij)
      text_perp(x_ch_right, yi, x_mid, yj,
                sprintf("%s(%.1f)", state_labels[j], probs[j]),
                offset=state_perp, cex=cex_state, col=col_ij)
      edge(x_mid, yj, x_leaf, yj, lwd=lwd_ij, col=col_ij)
      text(x_leaf + payoff_dx, yj,
           labels = formatC(P[i,j], digits = 0, format = "f"),
           cex = cex_payoff, adj = c(0,0.5), col = col_ij)
    }
  }
  
  # ---- Summary under figure ----
  if (show_summary) {
    best_str <- paste(alt_labels[best_idx], collapse = ", ")
    metrics_line <- paste(sprintf("%s: %s", alt_labels, shown_labels), collapse = "   |   ")
    rule_name <- switch(
      prune_by,
      expected_value="Expected Value",
      most_likely="Most Likely",
      expected_regret="Expected Regret",
      minimax_regret="Minimax Regret"
    )
    head_line <- sprintf("Rule: %s (%s)  —  Best: %s",
                         rule_name, if (type=="gain") "gain/max" else "loss/min", best_str)
    
    mtext(head_line,  side = 1, line = 2.2, cex = summary_cex, adj = 0.5)
    mtext(metrics_line, side = 1, line = 0.9, cex = summary_cex, adj = 0.5)
  }
  
  invisible(list(
    best_index = best_idx,
    best_alt   = alt_labels[best_idx],
    best_pairs = best_pairs_global,
    shown      = shown_labels
  ))
}


make_terminal <- function(payoff, label = NULL) {
  list(
    type   = "terminal",
    label  = label,
    payoff = payoff
  )
}

make_chance <- function(label = NULL, branches) {
  # branches: list(list(label="S1", prob=0.3, node=...), ...)
  list(
    type     = "chance",
    label    = label,
    branches = branches
  )
}

make_decision <- function(label = "K", branches) {
  # branches: list(list(label="A", cost=0, node=...), ...)
  list(
    type     = "decision",
    label    = label,
    branches = branches
  )
}

validate_tree <- function(node, path = "root") {
  if (!is.list(node))
    stop("Node at ", path, " is not a list.")
  
  if (is.null(node$type))
    stop("Node at ", path, " has no 'type' element. ",
         "Ensure you used make_terminal / make_chance / make_decision.")
  
  if (!(node$type %in% c("terminal","chance","decision")))
    stop("Node at ", path, " has invalid type='", node$type, "'.")
  
  if (node$type == "terminal") {
    if (is.null(node$payoff))
      stop("Terminal node at ", path, " has no 'payoff'.")
    return(invisible(TRUE))
  }
  
  # chance or decision -> must have branches
  if (is.null(node$branches) || !is.list(node$branches))
    stop("Node at ", path, " has no valid 'branches' list.")
  
  for (i in seq_along(node$branches)) {
    b <- node$branches[[i]]
    if (is.null(b$node))
      stop("Branch ", i, " at ", path, " has no 'node'. ",
           "Branches must be like list(label=.., prob/cost=.., node=<child_node>).")
    validate_tree(b$node, paste0(path, " -> branch[", i, "]"))
  }
  
  invisible(TRUE)
}

plot_decision_tree_recursive <- function(tree,
                                         main = "Decision tree",
                                         type = c("gain","loss"),
                                         show_payoffs = TRUE, # görsel parametreler
                                         show_circle_metric   = TRUE,
                                         circle_digits        = 2,
                                         payoff_digits = 0,
                                         prob_digits = 2,
                                         circle_metric_label  = TRUE,
                                         color_best   = "black",
                                         color_pruned = "black",
                                         lwd_best     = 1.8,
                                         lwd_pruned   = 1.2,
                                         cex_main   = 1.3,
                                         cex_nodes  = 1.2,
                                         cex_edge   = 1.0,
                                         cex_payoff = 1.05,
                                         dx = NULL,              # otomatik, max derinliğe göre
                                         ev_v_offset = 0.05,
                                         state_perp  = 0.05,
                                         payoff_dx  = 0.02,      # yaprakları sağa kaydırma
                                         highlight_strategy = TRUE, # TRUE: strateji, FALSE: tek path
                                         # export
                                         file   = NULL,
                                         width  = 8,
                                         height = 4.5,
                                         dpi    = 300,
                                         # --- YENİ: EV denklemleri için HTML export ---
                                         html_file  = NULL,
                                         html_title = main,
                                         big_tree_mode = FALSE) {
  
  type <- match.arg(type)
  
  # Küçük yardımcı
  `%||%` <- function(a,b) if (is.null(a)) b else a
  
  #------------------------------------------------------------
  # 0) Yapı kontrolü
  #------------------------------------------------------------
  validate_tree(tree)
  
  node_type <- function(node) {
    t <- node$type
    if (is.null(t) || length(t) == 0)
      stop("Encountered node without 'type'. Did you construct the tree correctly?")
    t
  }
  
  #------------------------------------------------------------
  # 1) EV hesaplama (bottom–up)
  #------------------------------------------------------------
  compute_ev_branch <- function(branch) {
    branch$node <- compute_ev(branch$node)
    branch
  }
  
  compute_ev <- function(node) {
    t <- node_type(node)
    
    if (t == "terminal") {
      node$ev <- node$payoff
      return(node)
    }
    
    if (t == "chance") {
      node$branches <- lapply(node$branches, compute_ev_branch)
      evs   <- vapply(node$branches, function(b) b$node$ev, numeric(1))
      probs <- vapply(node$branches, function(b) b$prob,  numeric(1))
      s <- sum(probs); if (abs(s - 1) > 1e-8) probs <- probs / s
      node$ev <- sum(probs * evs)
      return(node)
    }
    
    if (t == "decision") {
      node$branches <- lapply(node$branches, compute_ev_branch)
      evs <- vapply(node$branches, function(b) b$node$ev, numeric(1))
      node$ev <- if (type == "gain") max(evs) else min(evs)
      return(node)
    }
    
    stop("Unknown node type: ", t)
  }
  
  tree <- compute_ev(tree)
  
  #------------------------------------------------------------
  # 2) Renklendirme: strateji mi, tek path mi?
  #------------------------------------------------------------
  
  # global tek path (her düğümde en iyi dal, diğerleri kırmızı)
  mark_best_path <- function(node) {
    t <- node_type(node)
    
    if (t == "terminal") {
      node$on_best_path <- TRUE
      return(node)
    }
    
    if (t %in% c("chance","decision")) {
      evs <- vapply(node$branches, function(b) b$node$ev, numeric(1))
      best_idx <- if (type == "gain") which.max(evs) else which.min(evs)
      
      for (i in seq_along(node$branches)) {
        if (i == best_idx) {
          node$branches[[i]]$on_best <- TRUE
          node$branches[[i]]$node    <- mark_best_path(node$branches[[i]]$node)
        } else {
          node$branches[[i]]$on_best <- FALSE
        }
      }
      node$on_best_path <- TRUE
      return(node)
    }
    
    stop("Unknown node type in mark_best_path: ", t)
  }
  
  # seçilen stratejiyi boyama:
  # - karar düğümlerinde sadece en iyi dal
  # - şans düğümlerinde tüm dallar
  mark_strategy <- function(node) {
    t <- node_type(node)
    
    if (t == "terminal") {
      node$on_best_path <- TRUE
      return(node)
    }
    
    if (t == "decision") {
      evs <- vapply(node$branches, function(b) b$node$ev, numeric(1))
      best_idx <- if (type == "gain") which.max(evs) else which.min(evs)
      
      for (i in seq_along(node$branches)) {
        if (i == best_idx) {
          node$branches[[i]]$on_best <- TRUE
          node$branches[[i]]$node    <- mark_strategy(node$branches[[i]]$node)
        } else {
          node$branches[[i]]$on_best <- FALSE
        }
      }
      node$on_best_path <- TRUE
      return(node)
    }
    
    if (t == "chance") {
      # tüm olası sonuçlar stratejinin parçası
      for (i in seq_along(node$branches)) {
        node$branches[[i]]$on_best <- TRUE
        node$branches[[i]]$node    <- mark_strategy(node$branches[[i]]$node)
      }
      node$on_best_path <- TRUE
      return(node)
    }
    
    stop("Unknown node type in mark_strategy: ", t)
  }
  
  if (highlight_strategy) {
    tree <- mark_strategy(tree)
  } else {
    tree <- mark_best_path(tree)
  }
  
  #------------------------------------------------------------
  # 3) Y konumları
  #------------------------------------------------------------
  assign_y_positions <- function(node, leaf_index = 0L) {
    t <- node_type(node)
    
    if (t == "terminal") {
      if (big_tree_mode) {
        leaf_index <- leaf_index + 5L
      } else {
        leaf_index <- leaf_index + 1L
      }
      node$y <- leaf_index
      return(list(node = node, leaf_index = leaf_index))
    }
    
    ys <- numeric(length(node$branches))
    for (i in seq_along(node$branches)) {
      res <- assign_y_positions(node$branches[[i]]$node, leaf_index)
      node$branches[[i]]$node <- res$node
      leaf_index <- res$leaf_index
      ys[i] <- res$node$y
    }
    node$y <- mean(ys)
    list(node = node, leaf_index = leaf_index)
  }
  
  res_layout <- assign_y_positions(tree, 0L)
  tree <- res_layout$node
  
  collect_y <- function(node) {
    ys <- node$y
    t <- node_type(node)
    if (t != "terminal") {
      for (b in node$branches) ys <- c(ys, collect_y(b$node))
    }
    ys
  }
  
  all_y <- collect_y(tree)
  min_y0 <- min(all_y); max_y0 <- max(all_y)
  
  # ilk dal yukarıda, son dal aşağıda olsun diye ters ölçek
  scale_y <- function(y) {
    if (max_y0 == min_y0) return(0.5)
    
    if (big_tree_mode) {
      # ilk yaprak yukarıda, son yaprak aşağıda
      return(0.98 - (y - min_y0) / (max_y0 - min_y0) * 0.96)
    } else {
      return(0.9  - (y - min_y0) / (max_y0 - min_y0) * 0.8)
    }
  }
  
  
  
  apply_y_scale <- function(node) {
    node$y <- scale_y(node$y)
    t <- node_type(node)
    if (t != "terminal") {
      node$branches <- lapply(node$branches, function(b) {
        b$node <- apply_y_scale(b$node)
        b
      })
    }
    node
  }
  
  tree <- apply_y_scale(tree)
  
  #------------------------------------------------------------
  # 4) X konumları
  #------------------------------------------------------------
  compute_depth <- function(node) {
    t <- node_type(node)
    if (t == "terminal") return(0L)
    1L + max(vapply(node$branches, function(b) compute_depth(b$node), integer(1)))
  }
  
  depth_max <- compute_depth(tree)
  
  if (is.null(dx)) {
    dx <- 0.8 / max(1L, depth_max)   # kök ~0.07, en derin seviye ~0.87
  }
  
  assign_x <- function(node, depth = 0L) {
    node$x <- 0.07 + depth * dx
    t <- node_type(node)
    if (t != "terminal") {
      node$branches <- lapply(node$branches, function(b) {
        b$node <- assign_x(b$node, depth + 1L)
        b
      })
    }
    node
  }
  
  tree <- assign_x(tree)
  
  #------------------------------------------------------------
  # 5) Grafik device
  #------------------------------------------------------------
  if (!is.null(file)) {
    ext <- tools::file_ext(file)
    if (ext == "png")      png(file, width=width, height=height, units="in", res=dpi)
    else if (ext == "pdf") pdf(file, width=width, height=height)
    else if (ext == "svg") svg(file, width=width, height=height)
    else stop("Unsupported file extension. Use .png, .pdf or .svg")
    on.exit(dev.off(), add=TRUE)
  } else if (is.null(dev.list())) {
    dev.new(width = width, height = height)
  }
  
  op <- par(mar = c(1.5, 1.3, 2.5, 1.5)); on.exit(par(op), add = TRUE)
  plot.new()
  plot.window(xlim = c(0, 1), ylim = c(0, 1))
  title(main, cex.main = cex_main)
  
  #------------------------------------------------------------
  # 6) Geometri yardımcıları
  #------------------------------------------------------------
  decision_half_extents <- function(label) {
    w <- strwidth(label, cex = cex_nodes) + 0.03
    h <- strheight("Mg", cex = cex_nodes) + 0.02
    c(rx = w/2, ry = h/2)
  }
  
  draw_square <- function(x,y,lab,cex=1,col="black",lwd=1.6) {
    w <- strwidth(lab, cex=cex) + 0.03
    h <- strheight("Mg", cex=cex) + 0.02
    rect(x-w/2, y-h/2, x+w/2, y+h/2, border=col, lwd=lwd)
    text(x, y, lab, cex=cex, col=col)
  }
  
  NODE_R_CHANCE <- 0.020
  
  draw_circle <- function(x, y, col, lwd) {
    symbols(x, y, circles = NODE_R_CHANCE, inches = FALSE,
            add = TRUE, lwd = lwd, fg = col)
  }
  
  edge <- function(x1,y1,x2,y2,lwd=1.2,col="black") {
    segments(x1,y1,x2,y2,lwd=lwd,col=col)
  }
  
  text_perp <- function(x1,y1,x2,y2,label,offset=0.015,cex=0.9,col="black") {
    mx <- (x1 + x2)/2; my <- (y1 + y2)/2
    dx <- x2 - x1; dy <- y2 - y1
    L <- sqrt(dx*dx + dy*dy) + 1e-12
    nx <- -dy / L; ny <-  dx / L
    text(mx + offset*nx, my + offset*ny, labels=label, cex=cex, col=col)
  }
  
  clip_segment_to_nodes <- function(px, py, ptype, plabel,
                                    cx, cy, ctype, clabel) {
    dx <- cx - px
    dy <- cy - py
    L  <- sqrt(dx*dx + dy*dy)
    if (L < 1e-6) L <- 1e-6
    ux <- dx / L
    uy <- dy / L
    
    # parent
    if (ptype == "decision") {
      he <- decision_half_extents(plabel)
      tx <- he["rx"] / abs(ux)
      ty <- he["ry"] / abs(uy)
      t_parent <- min(tx, ty)
    } else if (ptype == "chance") {
      t_parent <- NODE_R_CHANCE
    } else {
      t_parent <- 0
    }
    
    # child
    if (ctype == "decision") {
      he <- decision_half_extents(clabel)
      tx <- he["rx"] / abs(ux)
      ty <- he["ry"] / abs(uy)
      t_child <- min(tx, ty)
    } else if (ctype == "chance") {
      t_child <- NODE_R_CHANCE
    } else {
      t_child <- 0
    }
    
    x1 <- px + t_parent * ux
    y1 <- py + t_parent * uy
    x2 <- cx - t_child * ux
    y2 <- cy - t_child * uy
    
    list(x1=x1, y1=y1, x2=x2, y2=y2)
  }
  
  #------------------------------------------------------------
  # 7) Çizim
  #------------------------------------------------------------
  draw_node <- function(node) {
    t <- node_type(node)
    
    # terminal yaprak
    if (t == "terminal") {
      col_leaf <- if (isTRUE(node$on_best_path)) color_best else color_pruned
      if (show_payoffs) {
      text(node$x + payoff_dx, node$y,
           labels = formatC(node$payoff, digits = payoff_digits, format = "f"),
           cex = cex_payoff, adj = c(0,0.5), col = col_leaf)
      }
      return()
    }
    
    col_node <- if (isTRUE(node$on_best_path)) color_best else color_pruned
    lwd_node <- if (isTRUE(node$on_best_path)) lwd_best   else lwd_pruned
    
    # düğüm şekli
    if (t == "decision") {
      draw_square(node$x, node$y, node$label,
                  cex = cex_nodes, col = col_node, lwd = lwd_node)
    } else if (t == "chance") {
      draw_circle(node$x, node$y, col = col_node, lwd = lwd_node)
      if (show_circle_metric) {
        lab_val <- if (circle_metric_label) {
          paste0("EV=", formatC(node$ev, digits = circle_digits, format = "f"))
        } else {
          formatC(node$ev, digits = circle_digits, format = "f")
        }
        text(node$x,
             node$y + NODE_R_CHANCE + ev_v_offset,
             labels = lab_val,
             cex    = cex_edge,
             col    = col_node)
      }
    }
    
    # dallar ve alt düğümler
    for (b in node$branches) {
      child   <- b$node
      child_t <- node_type(child)
      
      col_edge <- if (isTRUE(b$on_best)) color_best else color_pruned
      lwd_edge <- if (isTRUE(b$on_best)) lwd_best   else lwd_pruned
      
      clipped <- clip_segment_to_nodes(
        px = node$x, py = node$y, ptype = t,       plabel = node$label %||% "",
        cx = child$x, cy = child$y, ctype = child_t, clabel = child$label %||% ""
      )
      
      edge(clipped$x1, clipped$y1, clipped$x2, clipped$y2,
           lwd = lwd_edge, col = col_edge)
      
      if (t == "decision") {
        if (!is.null(b$label)) {
          text_perp(clipped$x1, clipped$y1, clipped$x2, clipped$y2,
                    label = b$label,
                    offset = state_perp, cex = cex_edge, col = col_edge)
        }
        if (!is.null(b$cost) && b$cost != 0) {
          text_perp(clipped$x1, clipped$y1, clipped$x2, clipped$y2,
                    label = paste0("c=", b$cost),
                    offset = -state_perp, cex = cex_edge, col = col_edge)
        }
      } else if (t == "chance") {
        if (!is.null(b$prob)) {
          fmt <- paste0("p=%.", prob_digits, "f")
          prob_str <- sprintf(fmt, b$prob)
        } else {
          prob_str <- ""
        }
        lab <- paste(c(b$label, prob_str), collapse = " ")
        lab <- trimws(lab)
        if (nchar(lab) > 0) {
          text_perp(clipped$x1, clipped$y1, clipped$x2, clipped$y2,
                    label = lab,
                    offset = state_perp, cex = cex_edge, col = col_edge)
        }
      }
      
      draw_node(child)
    }
  }
  
  draw_node(tree)
  
  #------------------------------------------------------------
  # 8) EV denklemlerini RAW LaTeX olarak HTML'e yaz
  #------------------------------------------------------------
  if (!is.null(html_file)) {
    
    latex_label <- function(x) {
      x <- x %||% ""
      gsub("_", "\\\\_", x)
    }
    
    collect_equations <- function(node, lines = character()) {
      t <- node_type(node)
      if (t == "terminal") return(lines)
      
      for (b in node$branches)
        lines <- collect_equations(b$node, lines)
      
      lab_tex <- latex_label(node$label)
      
      if (t == "chance") {
        probs <- vapply(node$branches, function(b) b$prob, numeric(1))
        vals  <- vapply(node$branches, function(b) {
          ch <- b$node
          if (node_type(ch) == "terminal") ch$payoff else ch$ev
        }, numeric(1))
        
        terms <- sprintf("%.4g * %s",
                         probs,
                         formatC(vals, digits = 6, format = "g"))
        rhs <- paste(terms, collapse = " + ")
        
        eq <- sprintf("EV(%s) = %s = %s",
                      lab_tex,
                      rhs,
                      formatC(node$ev, digits = 6, format = "g"))
        
        lines <- c(lines, eq)
      }
      
      if (t == "decision") {
        vals <- vapply(node$branches, function(b) b$node$ev, numeric(1))
        op <- if (type == "gain") "max" else "min"
        rhs <- paste(formatC(vals, digits = 6, format = "g"), collapse=", ")
        
        eq <- sprintf("EV(%s) = %s(%s) = %s",
                      lab_tex,
                      op,
                      rhs,
                      formatC(node$ev, digits = 6, format = "g"))
        
        lines <- c(lines, eq)
      }
      
      lines
    }
    
    eqs <- collect_equations(tree)
    
    html_lines <- c(
      "<!DOCTYPE html>",
      "<html><head>",
      sprintf("<title>%s (Raw LaTeX EV)</title>", html_title),
      "</head><body>",
      "<h2>RAW LaTeX EV Equations</h2>",
      "<pre>",
      paste(eqs, collapse="\n\n"),
      "</pre>",
      "</body></html>"
    )
    
    con <- file(html_file, open = "wb")
    writeLines(html_lines, con, useBytes = TRUE)
    close(con)
  }
  
  invisible(tree)
}