# =========================================================
# Transport Solver & Renderers (full, single file)
# - Balanced/unbalanced handling (dummy row/col)
# - Initial methods: northwest, rowmin, colmin, leastcost, vam
# - Optimizers: none, modi (u-v), stepping, both
# - Trace support with per-step snapshots
# - Degenerate basics (0-valued basic cells) highlighted in 50_* packages
# - Objective: "min" (default) or "max" (profit maximization)
# - HTML generators in a unified visual style
# - Example hooks at bottom (commented)
# =========================================================

.eps <- 1e-9

# ---------- helpers ----------
.html_escape <- function(x){
  x <- gsub("&","&amp;",x,fixed=TRUE)
  x <- gsub("<","&lt;",x,fixed=TRUE)
  x <- gsub(">","&gt;",x,fixed=TRUE); x
}
.mk_names <- function(prefix, k) sprintf("%s %d", prefix, seq_len(k))
.transport_css_bigM <- function(){
  paste0(
    "<style>
.tbl-sim{border-collapse:collapse;margin:10px 0;font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;font-size:14px}
.tbl-sim caption{caption-side:top;font-weight:bold;margin-bottom:6px;text-align:left}
.tbl-sim th,.tbl-sim td{border:1px solid #dee2e6;padding:6px 10px;text-align:center}
.tbl-sim thead th{background:#f1f3f5}
.tbl-sim th:first-child{background:#fafafa;text-align:left}
.tbl-sim .muted{color:#adb5bd}.tbl-sim .sub{font-size:11px;color:#6c757d}
.tbl-sim .bold{font-weight:bold}
.tbl-sim .alloc{background:#fff3cd;border:2px solid #f0ad4e}
.tbl-sim .rhs{background:#e7f1fb}
.tbl-sim .na{color:#adb5bd}
.tbl-sim .neg{color:#b00020;font-weight:600}
.tbl-sim .minneg{background:#fdecea;border:2px solid #f5c2c7}
.tbl-sim .basic{background:#eef9f1}
.tbl-sim .degen{background:#eaf5ff;border:2px dashed #1a73e8} /* x=0 temel hücre */
.tbl-sim tr.exh-row td:not(.alloc), .tbl-sim tr.exh-row th { background:#fdecea; }
.tbl-sim td.exh-col:not(.alloc),    .tbl-sim th.exh-col    { background:#fdecea; }
.pivot{background:#e8f0fe !important; box-shadow: inset 0 0 0 3px #1a73e8}
.plus{background:#f1faf1 !important; box-shadow: inset 0 0 0 3px #2e7d32}
.minus{background:#fff1f1 !important; box-shadow: inset 0 0 0 3px #c62828}
.cellwrap{position:relative;min-width:76px;height:46px}
.viol { background:#fdecea; border:2px solid #f5c2c7 }  /* generic violation */
.pivot{ background:#e8f0fe !important; box-shadow: inset 0 0 0 3px #1a73e8; }
.cbox{position:absolute;top:6px;right:6px;border:1px solid #adb5bd;border-radius:6px;padding:1px 6px;font-size:12px;color:#495057}
.uvchip{display:inline-block;padding:1px 6px;border:1px solid #adb5bd;border-radius:6px;font-size:12px;margin-left:6px;color:#495057;background:#f8f9fa}
</style>"
  )
}
.transport_labels_default <- function(){
  list(
    initial_title   = "Başlangıç Tablosu",
    allocation_title= "Dağıtım – %METHOD%",
    total_cost      = "Toplam Maliyet",
    row_prefix      = "Kaynak", col_prefix = "Hedef",
    row_names       = NULL,     col_names  = NULL,
    supply_label    = "Arz",    demand_label = "Talep"
  )
}
.fnum <- function(v){
  if (isTRUE(all.equal(v, round(v)))) sprintf("%d", as.integer(round(v))) else sprintf("%g", v)
}


.modi_violation_masks <- function(R, objective = c("min","max"), tol = 1e-12){
  objective <- match.arg(objective)
  if (!is.matrix(R)) R <- as.matrix(R)
  viol <- matrix(FALSE, nrow(R), ncol(R))
  best <- matrix(FALSE, nrow(R), ncol(R))
  if (all(is.na(R))) return(list(viol=viol, best=best, best_val=NA_real_))
  if (objective == "max"){
    viol    <- (!is.na(R)) & (R >  tol)
    bestVal <- suppressWarnings(max(R, na.rm = TRUE))
    if (is.finite(bestVal) && bestVal > tol) best <- (!is.na(R)) & (abs(R - bestVal) <= tol)
  } else {
    viol    <- (!is.na(R)) & (R < -tol)
    bestVal <- suppressWarnings(min(R, na.rm = TRUE))
    if (is.finite(bestVal) && bestVal < -tol) best <- (!is.na(R)) & (abs(R - bestVal) <= tol)
  }
  list(viol = viol, best = best, best_val = bestVal)
}



# =========================================================
# Transportation Solver (Balanced/Unbalanced) with objective
# =========================================================
solve_transport <- function(cost, supply, demand,
                            method   = c("northwest","rowmin","colmin","leastcost","vam"),
                            optimize = c("none","modi","stepping","both"),
                            objective = c("min","max"),
                            trace = FALSE,
                            allow_unbalanced = TRUE,
                            dummy_row_cost = 0,
                            dummy_col_cost = 0,
                            drop_dummy_in_output = TRUE) {
  method   <- match.arg(method)
  optimize <- match.arg(optimize)
  objective <- match.arg(objective)
  
  C0_input <- as.matrix(cost)
  s_in <- as.numeric(supply)
  d_in <- as.numeric(demand)
  m0 <- nrow(C0_input); n0 <- ncol(C0_input)
  if (length(s_in) != m0) stop("length(supply) must equal nrow(cost)")
  if (length(d_in) != n0) stop("length(demand) must equal ncol(cost)")
  
  # Objective transform: for maximization, do NOT negate costs (we’ll handle in MODI rule)
  is_max <- (objective == "max")
  C0 <- C0_input + 0  # keep original signs; MODI will pick rule by objective
  
  # ---------- Unbalanced handling ----------
  sumS <- sum(s_in); sumD <- sum(d_in)
  augmented <- list(added = NULL, dummy_index = NA_integer_, original_m = m0, original_n = n0)
  
  C <- C0; s0 <- s_in; d0 <- d_in
  if (abs(sumS - sumD) > 1e-9) {
    if (!allow_unbalanced) stop("Problem unbalanced ve allow_unbalanced=FALSE.")
    if (sumS > sumD) {
      add <- sumS - sumD
      if (!(length(dummy_col_cost) %in% c(1, m0))) stop("dummy_col_cost skalar ya da uzunluk = nrow(cost) olmalı.")
      dummy_col <- rep(dummy_col_cost, length.out = m0)
      C <- cbind(C, dummy_col)
      d0 <- c(d0, add)
      augmented$added <- "col"; augmented$dummy_index <- ncol(C)
    } else {
      add <- sumD - sumS
      if (!(length(dummy_row_cost) %in% c(1, n0))) stop("dummy_row_cost skalar ya da uzunluk = ncol(cost) olmalı.")
      dummy_row <- rep(dummy_row_cost, length.out = n0)
      C <- rbind(C, dummy_row)
      s0 <- c(s0, add)
      augmented$added <- "row"; augmented$dummy_index <- nrow(C)
    }
  }
  
  m <- nrow(C); n <- ncol(C)
  s <- s0 + 0; d <- d0 + 0
  alloc <- matrix(0, m, n)
  active_rows <- rep(TRUE, m)
  active_cols <- rep(TRUE, n)
  
  steps <- list(); k_step <- 0
  push <- function(stage, i=NULL, j=NULL, shipped=NULL, note=NULL, extras=list()){
    if (!trace) return(invisible())
    k_step <<- k_step + 1
    steps[[length(steps) + 1]] <<- list(
      k = k_step, stage = stage, i = i, j = j, x_ij = shipped,
      alloc = alloc + 0, s = s + 0, d = d + 0,
      cost_so_far = sum(alloc * C),
      info = c(list(note = note), extras)
    )
  }
  
  do_alloc <- function(i, j, stage, note=NULL, extras=list()){
    x <- min(s[i], d[j])
    if (x < .eps) {
      if (s[i] <= .eps) { s[i] <<- 0; active_rows[i] <<- FALSE }
      if (d[j] <= .eps) { d[j] <<- 0; active_cols[j] <<- FALSE }
    } else {
      alloc[i,j] <<- alloc[i,j] + x
      s[i] <<- s[i] - x
      d[j] <<- d[j] - x
      if (s[i] <= .eps) { s[i] <<- 0; active_rows[i] <<- FALSE }
      if (d[j] <= .eps) { d[j] <<- 0; active_cols[j] <<- FALSE }
      push(stage, i, j, x, note, extras)
    }
    invisible(x)
  }
  
  choose_best_cell <- function(){
    rs <- which(active_rows); cs <- which(active_cols)
    if (!length(rs) || !length(cs)) return(NULL)
    cand <- as.matrix(expand.grid(i = rs, j = cs))
    if (nrow(cand) == 0) return(NULL)
    feasible <- cand[pmin(s[cand[,1]], d[cand[,2]]) > .eps, , drop = FALSE]
    if (nrow(feasible) == 0) return(NULL)
    vals <- C[cbind(feasible[,1], feasible[,2])]
    k <- if (is_max) which.max(vals) else which.min(vals)
    c(feasible[k,1], feasible[k,2])
  }
  
  # ---------- Initial methods ----------
  if (method == "northwest"){
    i <- 1; j <- 1
    while (any(active_rows) && any(active_cols)){
      do_alloc(i, j, stage = "initial", note = "NW corner")
      exr <- !active_rows[i]; exc <- !active_cols[j]
      if (exr && exc){ if (j < n) j <- j + 1 else i <- i + 1
      } else if (exr){ i <- i + 1
      } else if (exc){ j <- j + 1 }
      while (i <= m && !active_rows[i]) i <- i + 1
      while (j <= n && !active_cols[j]) j <- j + 1
      if (i > m || j > n) break
    }
  } else if (method == "rowmin"){
    r <- 1
    while (any(active_rows) && any(active_cols)){
      if (!active_rows[r]) { ar <- which(active_rows); if (!length(ar)) break; r <- ar[1] }
      repeat {
        if (!(active_rows[r] && any(active_cols))) break
        cols <- which(active_cols); cols <- cols[ d[cols] > .eps ]
        if (!length(cols)) break
        j <- {
          vals <- C[r, cols]
          cols[ if (is_max) which.max(vals) else which.min(vals) ]
        }
        moved <- do_alloc(r, j, stage="initial", note = if (is_max) "Row-max" else "Row-min")
        if (moved <= .eps || !active_rows[r]) break
      }
      ar <- which(active_rows); if (!length(ar)) break; r <- ar[1]
    }
  } else if (method == "colmin"){
    cidx <- 1
    while (any(active_rows) && any(active_cols)){
      if (!active_cols[cidx]) { ac <- which(active_cols); if (!length(ac)) break; cidx <- ac[1] }
      repeat {
        if (!(active_cols[cidx] && any(active_rows))) break
        rows <- which(active_rows); rows <- rows[ s[rows] > .eps ]
        if (!length(rows)) break
        i <- {
          vals <- C[rows, cidx]
          rows[ if (is_max) which.max(vals) else which.min(vals) ]
        }
        moved <- do_alloc(i, cidx, stage="initial",note = if (is_max) "Col-max" else "Col-min")
        if (moved <= .eps || !active_cols[cidx]) break
      }
      ac <- which(active_cols); if (!length(ac)) break; cidx <- ac[1]
    }
  } else if (method == "leastcost"){
    repeat {
      if (!(any(active_rows) && any(active_cols))) break
      ij <- choose_best_cell(); if (is.null(ij)) break
      moved <- do_alloc(ij[1], ij[2], stage="initial", note = if (is_max) "Most-profit" else "Least-cost")
      if (moved <= .eps) break
    }
  } else if (method == "vam"){
    # --- helpers for penalties (two best values) ---
    pen_row <- function(i){
      cols <- which(active_cols); cols <- cols[d[cols] > .eps]
      if (length(cols) >= 2L) {
        v <- sort(C[i, cols], decreasing = is_max)
        v[1] - v[2]
      } else if (length(cols) == 1L) 0 else NA_real_
    }
    pen_col <- function(j){
      rows <- which(active_rows); rows <- rows[s[rows] > .eps]
      if (length(rows) >= 2L) {
        v <- sort(C[rows, j], decreasing = is_max)
        v[1] - v[2]
      } else if (length(rows) == 1L) 0 else NA_real_
    }
    
    # choose best cell over a set of (i,j) candidate pairs
    pick_best_from_pairs <- function(pairs){
      if (is.null(pairs) || !nrow(pairs)) return(NULL)
      feas <- pairs[pmin(s[pairs[,1]], d[pairs[,2]]) > .eps, , drop = FALSE]
      if (!nrow(feas)) return(NULL)
      vals <- C[cbind(feas[,1], feas[,2])]
      k <- if (is_max) which.max(vals) else which.min(vals)
      c(feas[k,1], feas[k,2])
    }
    # global best among all active cells
    pick_global_best <- function(){
      rs <- which(active_rows); cs <- which(active_cols)
      if (!length(rs) || !length(cs)) return(NULL)
      cand <- as.matrix(expand.grid(i = rs, j = cs))
      pick_best_from_pairs(cand)
    }
    
    tol <- 1e-12
    while (any(active_rows) && any(active_cols)){
      rset <- which(active_rows); cset <- which(active_cols)
      
      # current-step penalties (BEFORE table)
      rpen_now <- if (length(rset)) { x <- sapply(rset, pen_row); names(x) <- paste0("r", rset); x } else numeric(0)
      cpen_now <- if (length(cset)) { x <- sapply(cset, pen_col); names(x) <- paste0("c", cset); x } else numeric(0)
      
      mr <- suppressWarnings(max(rpen_now, na.rm = TRUE))
      mc <- suppressWarnings(max(cpen_now, na.rm = TRUE))
      
      rows_max <- rset[ which(!is.na(rpen_now) & abs(rpen_now - mr) <= tol) ]
      cols_max <- cset[ which(!is.na(cpen_now) & abs(cpen_now - mc) <= tol) ]
      
      # >>> UNION(rows_max, cols_max) <<<
      cand_pairs <- NULL
      if (length(rows_max)) {
        cand_pairs <- rbind(cand_pairs, cbind(i = rep(rows_max, each = length(cset)), j = cset))
      }
      if (length(cols_max)) {
        cand_pairs <- rbind(cand_pairs, cbind(i = rep(rset, each = length(cols_max)), j = cols_max))
      }
      if (!is.null(cand_pairs)) {
        cand_pairs <- unique(cand_pairs)
      }
      
      ij <- pick_best_from_pairs(cand_pairs)
      if (is.null(ij)) ij <- pick_global_best()
      if (is.null(ij)) break
      
      moved <- do_alloc(
        ij[1], ij[2],
        stage = "initial",
        note  = sprintf("VAM tie Δ*=%s → union(rows, cols) then global %s",
                        ifelse(is.finite(max(mr, mc)), max(mr, mc), "NA"),
                        if (is_max) "max" else "min"),
        extras = list(rpen = rpen_now, cpen = cpen_now)
      )
      if (moved <= .eps) break
      
      # next-step penalties (AFTER table)
      rset2 <- which(active_rows); cset2 <- which(active_cols)
      rpen_next <- if (length(rset2)) { x <- sapply(rset2, pen_row); names(x) <- paste0("r", rset2); x } else numeric(0)
      cpen_next <- if (length(cset2)) { x <- sapply(cset2, pen_col); names(x) <- paste0("c", cset2); x } else numeric(0)
      
      if (trace && length(steps)) {
        steps[[length(steps)]]$info$rpen_next <- rpen_next
        steps[[length(steps)]]$info$cpen_next <- cpen_next
      }
    }
  }
  
  
  
  
  # ---------- feasibility check ----------
  if (any(abs(rowSums(alloc) - s0) > 1e-8) ||
      any(abs(colSums(alloc) - d0) > 1e-8)) {
    stop("İç hata: Başlangıç tahsisi fizibil değil (augment edilmiş örnek).")
  }
  
  # ---------- Common helpers (MODI + Stepping) ----------
  is_basic <- alloc > 0
  
  connect_components <- function(){
    repeat {
      row_vis <- rep(FALSE, m); col_vis <- rep(FALSE, n)
      start_row <- which(rowSums(is_basic) > 0)[1]
      if (is.na(start_row)) break
      q_rows <- c(start_row); row_vis[start_row] <- TRUE
      while (length(q_rows)){
        i <- q_rows[1]; q_rows <- q_rows[-1]
        js <- which(is_basic[i, ])
        if (length(js)){
          new_cols <- js[!col_vis[js]]
          col_vis[new_cols] <- TRUE
          for (j in new_cols){
            iset <- which(is_basic[, j])
            new_rows <- iset[!row_vis[iset]]
            if (length(new_rows)) { row_vis[new_rows] <- TRUE; q_rows <- c(q_rows, new_rows) }
          }
        }
      }
      if (all(row_vis) && all(col_vis)) break
      best <- NULL; bestc <- Inf
      for (i in 1:m) for (j in 1:n) {
        if (is_basic[i,j]) next
        connects <- (row_vis[i] && !col_vis[j]) || (!row_vis[i] && col_vis[j])
        if (connects && C[i,j] < bestc){ best <- c(i,j); bestc <- C[i,j] }
      }
      if (is.null(best)) break
      is_basic[best[1], best[2]] <<- TRUE
    }
  }
  
  find_loop <- function(ie, je){
    rows_to_cols <- lapply(seq_len(m), function(i) which(is_basic[i, ]))
    cols_to_rows <- lapply(seq_len(n), function(j) which(is_basic[, j]))
    start  <- ie; target <- m + je; N <- m + n
    prev    <- rep(NA_integer_, N); visited <- rep(FALSE, N)
    q <- integer(0); head <- 1L
    q <- c(q, start); visited[start] <- TRUE; prev[start] <- 0L
    while (head <= length(q)) {
      node <- q[head]; head <- head + 1L
      if (node <= m) {
        for (j in rows_to_cols[[node]]) {
          v <- m + j
          if (!visited[v]) { visited[v] <- TRUE; prev[v] <- node; q <- c(q, v) }
        }
      } else {
        j <- node - m
        for (i in cols_to_rows[[j]]) {
          if (!visited[i]) { visited[i] <- TRUE; prev[i] <- node; q <- c(q, i) }
        }
      }
      if (visited[target]) break
    }
    if (!visited[target]) return(NULL)
    nodes <- integer(0); cur <- target
    while (cur != 0L && !is.na(cur)) { nodes <- c(cur, nodes); cur <- prev[cur] }
    loop <- matrix(c(ie, je), ncol = 2L, byrow = TRUE)
    if (length(nodes) >= 2L){
      for (k in seq_len(length(nodes) - 1L)) {
        a <- nodes[k]; b <- nodes[k + 1L]
        if (a <= m && b >  m) { i <- a;     j <- b - m } else
          if (a >  m && b <= m) { i <- b;     j <- a - m } else next
        if (!(i == ie && j == je)) loop <- rbind(loop, c(i, j))
      }
    }
    loop
  }
  apply_cycle <- function(loop_path, enter_i, enter_j){
    signs <- rep(c(1, -1), length.out = nrow(loop_path))
    minus <- loop_path[signs == -1, , drop = FALSE]
    if (nrow(minus) == 0) return(list(theta = 0, left = NULL, minus = minus))
    theta <- suppressWarnings(min(alloc[cbind(minus[,1], minus[,2])], na.rm = TRUE))
    if (!is.finite(theta)) theta <- 0
    if (theta <= .eps) { return(list(theta = 0, left = NULL, minus = minus)) }
    for (k in seq_len(nrow(loop_path))){
      i <- loop_path[k,1]; j <- loop_path[k,2]
      alloc[i,j] <<- alloc[i,j] + signs[k] * theta
      if (abs(alloc[i,j]) < 1e-12) alloc[i,j] <<- 0
    }
    zero_hits <- minus[ alloc[cbind(minus[,1], minus[,2])] <= (.eps + 1e-15), , drop = FALSE ]
    if (!nrow(zero_hits)) {
      zero_hits <- minus[ order(minus[,1], minus[,2]), , drop = FALSE ][1,,drop=FALSE]
    } else {
      zero_hits <- zero_hits[ order(zero_hits[,1], zero_hits[,2]), , drop = FALSE ][1,,drop=FALSE]
    }
    is_basic[enter_i, enter_j] <<- TRUE
    is_basic[ zero_hits[1,1], zero_hits[1,2] ] <<- FALSE
    list(theta = theta, left = zero_hits)
  }
  
  # ---------- MODI (objective-aware R selection) ----------
  run_modi <- function(){
    compute_uv <- function(){
      u <- rep(NA_real_, m); v <- rep(NA_real_, n); u[1] <- 0
      dlog <- list(); changed <- TRUE
      while (changed){
        changed <- FALSE
        for (i in 1:m) for (j in 1:n) if (is_basic[i,j]){
          if (!is.na(u[i]) && is.na(v[j])){
            v[j] <- C[i,j] - u[i]; changed <- TRUE
            dlog[[length(dlog)+1]] <- list(kind="v", i=i, j=j,
                                           eq=sprintf("v_%d = c_%d,%d - u_%d = %g - %g = %g", j, i, j, i, C[i,j], u[i], v[j]))
          } else if (is.na(u[i]) && !is.na(v[j])){
            u[i] <- C[i,j] - v[j]; changed <- TRUE
            dlog[[length(dlog)+1]] <- list(kind="u", i=i, j=j,
                                           eq=sprintf("u_%d = c_%d,%d - v_%d = %g - %g = %g", i, i, j, j, C[i,j], v[j], u[i]))
          }
        }
      }
      list(u=u, v=v, log=dlog)
    }
    reduced_costs <- function(u, v){
      R <- matrix(NA_real_, m, n)
      for (i in 1:m) for (j in 1:n) if (!is_basic[i,j]) {
        if (!is.na(u[i]) && !is.na(v[j])) R[i,j] <- C[i,j] - u[i] - v[j]
      }
      R
    }
    connect_components()
    max_iter <- 3000; iter <- 0
    tol <- 1e-12
    repeat {
      iter <- iter + 1; if (iter > max_iter){ warning("MODI max_iter aşıldı; durduruldu."); break }
      uv <- compute_uv(); R  <- reduced_costs(uv$u, uv$v)
      
      if (is_max){
        # NEW: maximization → choose HIGHEST POSITIVE R
        maxR <- suppressWarnings(max(R, na.rm = TRUE))
        push("modi-uv", shipped=0,
             note=sprintf("u–v hesap; max(R)=%s", ifelse(is.finite(maxR), sprintf("%g", maxR), "NA")),
             extras=list(u=uv$u, v=uv$v, uv_log=uv$log, reduced=R, basis=(is_basic+0)))
        if (!is.finite(maxR) || maxR <= tol) break
        cand <- which(!is.na(R) & R >= (maxR - tol), arr.ind = TRUE)
      } else {
        # minimization → choose MOST NEGATIVE R (classic)
        minR <- suppressWarnings(min(R, na.rm = TRUE))
        push("modi-uv", shipped=0,
             note=sprintf("u–v hesap; min(R)=%s", ifelse(is.finite(minR), sprintf("%g", minR), "NA")),
             extras=list(u=uv$u, v=uv$v, uv_log=uv$log, reduced=R, basis=(is_basic+0)))
        if (!is.finite(minR) || minR >= -tol) break
        cand <- which(!is.na(R) & R <= (minR + tol), arr.ind = TRUE)
      }
      
      best <- NULL; best_loop <- NULL; best_theta <- 0; loops <- vector("list", nrow(cand))
      for (k in seq_len(nrow(cand))){
        ie <- cand[k,1]; je <- cand[k,2]
        loop <- find_loop(ie, je); loops[[k]] <- loop
        if (is.null(loop) || !is.matrix(loop) || nrow(loop) < 4L) next
        signs <- rep(c(1,-1), length.out = nrow(loop))
        minus <- loop[signs == -1, , drop = FALSE]; if (!nrow(minus)) next
        th <- suppressWarnings(min(alloc[cbind(minus[,1], minus[,2])], na.rm = TRUE))
        if (is.finite(th) && th > best_theta + 1e-15) { best_theta <- th; best <- c(ie,je); best_loop <- loop }
      }
      if (!is.null(best_loop) && best_theta > .eps){
        res <- apply_cycle(best_loop, best[1], best[2])
        rcval <- if (is_max) R[best[1], best[2]] else R[best[1], best[2]]
        push("modi", best[1], best[2], res$theta,
             note = sprintf("MODI enter (%d,%d), R=%g (%s)",
                            best[1], best[2], rcval, if (is_max) "maximizasyon→pozitif" else "minimizasyon→negatif"),
             extras = list(u=uv$u, v=uv$v, reduced=R, basis=(is_basic+0)))
        next
      }
      # Degeneracy swap fallback
      swapped <- FALSE
      for (kk in seq_along(loops)){
        loop <- loops[[kk]]; if (is.null(loop) || !is.matrix(loop) || nrow(loop) < 4L) next
        signs <- rep(c(1,-1), length.out = nrow(loop))
        minus <- loop[signs == -1, , drop = FALSE]; if (!nrow(minus)) next
        idx_r <- minus[,1]; idx_c <- minus[,2]
        ok_bounds <- idx_r >= 1 & idx_r <= m & idx_c >= 1 & idx_c <= n
        if (!all(ok_bounds)) next
        minus_is_basic <- is_basic[cbind(idx_r, idx_c)]
        minus_is_zero  <- alloc[cbind(idx_r, idx_c)] <= (.eps + 1e-15)
        cand_leavers   <- cbind(idx_r, idx_c)[minus_is_basic & minus_is_zero, , drop = FALSE]
        if (!nrow(cand_leavers)) next
        lv <- cand_leavers[order(cand_leavers[,1], cand_leavers[,2]), , drop = FALSE][1,,drop=FALSE]
        ent_i <- cand[kk,1]; ent_j <- cand[kk,2]
        if (is.na(ent_i) || is.na(ent_j)) next
        is_basic[ent_i, ent_j] <<- TRUE
        is_basic[lv[1,1], lv[1,2]] <<- FALSE
        push("modi", ent_i, ent_j, 0,
             note = sprintf("MODI degenerate swap: enter (%d,%d), leave (%d,%d)", ent_i, ent_j, lv[1,1], lv[1,2]),
             extras = list(u=uv$u, v=uv$v, reduced=R, basis=(is_basic+0)))
        swapped <- TRUE; break
      }
      if (!swapped){ warning("MODI: degeneracy but no valid basis swap found; stopping."); break }
    }
  }
  
  # ---------- Stepping-Stone ----------
  run_stepping <- function(){
    connect_components()
    max_iter <- 3000; iter <- 0
    prev_hash <- paste(alloc, collapse=",")
    repeat {
      iter <- iter + 1; if (iter > max_iter) { warning("Stepping max_iter aşıldı; durduruldu."); break }
      best_delta <- 0; best_cell  <- NULL; best_loop  <- NULL
      for (ie in 1:m) for (je in 1:n){
        if (alloc[ie,je] > 0) next
        loop <- find_loop(ie, je); if (is.null(loop)) next
        signs <- rep(c(1,-1), length.out = nrow(loop))
        delta <- sum(signs * C[cbind(loop[,1], loop[,2])])
        if (delta < best_delta - 1e-12){ best_delta <- delta; best_cell <- c(ie,je); best_loop <- loop }
      }
      if (is.null(best_cell)) break
      res <- apply_cycle(best_loop, best_cell[1], best_cell[2]); th  <- res$theta
      if (th <= .eps) { warning("Stepping theta≈0; durduruldu."); break }
      push("stepping", best_cell[1], best_cell[2], th,
           note = sprintf("Stepping enter (%d,%d), Δ=%g", best_cell[1], best_cell[2], best_delta))
      cur_hash <- paste(alloc, collapse=",")
      if (identical(cur_hash, prev_hash)) { warning("Stepping stagnasyon; durduruldu."); break }
      prev_hash <- cur_hash
    }
  }
  
  if (optimize %in% c("modi","both"))      run_modi()
  if (optimize %in% c("stepping","both"))  run_stepping()
  
  # ---------- Output (drop dummy if requested) ----------
  alloc_full <- alloc; cost_full  <- C; supply_full <- s0; demand_full <- d0
  alloc_out <- alloc_full; cost_out  <- cost_full; supply_out <- supply_full; demand_out <- demand_full
  if (!is.null(augmented$added) && drop_dummy_in_output) {
    if (augmented$added == "col"){
      j <- augmented$dummy_index; alloc_out  <- alloc_out[, -j, drop = FALSE]; cost_out   <- cost_out[,  -j, drop = FALSE]; demand_out <- demand_out[-j]
    } else if (augmented$added == "row"){
      i <- augmented$dummy_index; alloc_out  <- alloc_out[-i, , drop = FALSE]; cost_out   <- cost_out[ -i, , drop = FALSE]; supply_out <- supply_out[-i]
    }
  }
  
  total_val <- sum(alloc_full * cost_full)  # for max this is still total profit since C were original signs
  
  list(
    allocation = alloc_out,
    total_cost = total_val,
    method     = method,
    optimized  = optimize != "none",
    optimizer  = optimize,
    cost       = cost_out,
    supply     = supply_out,
    demand     = demand_out,
    augmented  = augmented,
    objective  = objective,
    steps      = if (trace) steps else NULL
  )
}

# ----- REPLACE your current method_title() with this -----
method_title <- function(m, objective = c("min","max")){
  objective <- match.arg(objective)
  mm <- tolower(m)
  if (objective == "max"){
    switch(mm,
           "northwest" = "Kuzeybatı Köşe (NW)",
           "rowmin"    = "Satır-Max (Row Max)",
           "colmin"    = "Sütun-Max (Col Max)",
           "leastcost" = "En Yüksek Kâr (Most Profit)",
           "vam"       = "Vogel Yaklaşımı (VAM)",
           toupper(m)
    )
  } else {
    switch(mm,
           "northwest" = "Kuzeybatı Köşe (NW)",
           "rowmin"    = "Satır-Min (Row Min)",
           "colmin"    = "Sütun-Min (Col Min)",
           "leastcost" = "En Düşük Maliyet (Least Cost)",
           "vam"       = "Vogel Yaklaşımı (VAM)",
           toupper(m)
    )
  }
}

opt_title <- function(o){
  switch(tolower(o),
         "none"     = "Başlangıç",
         "modi"     = "MODI (u–v)",
         "stepping" = "Stepping-Stone",
         "both"     = "MODI + Stepping",
         toupper(o)
  )
}



# ---------- Export RAW LaTeX of total cost ----------
transport_export_latex_equation_html <- function(cost, alloc, file,
                                                 title = "LaTeX – Toplam Maliyet Eşitliği",
                                                 render_math = FALSE,
                                                 mathjax_src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"){
  C <- as.matrix(cost); A <- as.matrix(alloc)
  total_cost <- sum(C * A)
  .esc <- function(x){ x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE); gsub(">","&gt;",x,fixed=TRUE) }
  nz <- which(A > 0, arr.ind = TRUE)
  if (nrow(nz) == 0){
    eq <- "\\[ Z = \\sum_{i=1}^{m} \\sum_{j=1}^{n} c_{ij} x_{ij} \\]"
  } else {
    sym_terms <- character(nrow(nz)); num_terms <- character(nrow(nz))
    for (k in seq_len(nrow(nz))){
      i <- nz[k,1]; j <- nz[k,2]
      sym_terms[k] <- sprintf("c_{%d,%d} x_{%d,%d}", i, j, i, j)
      num_terms[k] <- sprintf("%s\\cdot %s", .fnum(C[i,j]), .fnum(A[i,j]))
    }
    eq <- sprintf("\\[\\begin{aligned}
Z &= \\sum_{i=1}^{m}\\sum_{j=1}^{n} c_{ij} x_{ij} \\\\
  &= %s \\\\
  &= %s \\\\
  &= \\mathbf{%s}
\\end{aligned}\\]",
                  paste(sym_terms, collapse = " + "),
                  paste(num_terms, collapse = " + "),
                  .fnum(total_cost))
  }
  html <- c("<!doctype html>","<html><head><meta charset='utf-8'><title>LaTeX Equation</title>",
            "<style>body{font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:20px}h3{margin:0 0 8px 0}.raw{white-space:pre-wrap;background:#f8f9fa;border:1px solid #e9ecef;padding:12px;border-radius:6px;font-family:ui-monospace,Consolas,Monaco,monospace}.note{color:#6c757d;font-size:13px;margin-top:8px}</style>")
  if (render_math){
    html <- c(html, sprintf("<script id='MathJax-script' async src='%s'></script>", .esc(mathjax_src)))
  }
  html <- c(html, "</head><body>",
            sprintf("<h3>%s</h3>", .esc(title)),
            sprintf("<div class='raw'>%s</div>", .esc(eq)))
  if (render_math){ html <- c(html, "<h4>Önizleme (MathJax)</h4>", eq) }
  html <- c(html, "<div class='note'>Bu dosya <b>ham</b> LaTeX içerir.</div>","</body></html>")
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  writeLines(html, file)
  invisible(eq)
}
# ---------- Cost-only & Initial tables ----------
transport_initial_costs_only_html <- function(cost,
                                              labels = .transport_labels_default(),
                                              include_css = TRUE,
                                              table_class = "tbl-sim",
                                              title = "Birim Maliyet Tablosu (Sadece Maliyet + xᵢⱼ)",
                                              digits = NULL) {
  C <- as.matrix(cost); m <- nrow(C); n <- ncol(C)
  fmt <- if (is.null(digits)) function(x) sprintf("%g", x)
  else function(x) formatC(x, format = "f", digits = digits)
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  extra_css <- "
  <style>
    .tbl-sim .cellwrap{position:relative;min-width:72px;height:48px}
    .tbl-sim .cbox{position:absolute;top:6px;right:6px;
                   border:1px solid #adb5bd;border-radius:6px;
                   padding:1px 6px;font-size:12px;color:#495057}
    .tbl-sim .xvar{position:absolute;left:8px;bottom:6px;font-weight:600}
  </style>"
  html <- c()
  if (include_css) html <- c(html, .transport_css_bigM(), extra_css)
  html <- c(html, sprintf("<table class=\"%s\">", .html_escape(table_class)))
  html <- c(html, sprintf("<caption>%s</caption>", .html_escape(title)))
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", .html_escape(col_names[j])))
  html <- c(html, "</tr></thead><tbody>")
  for (i in seq_len(m)) {
    html <- c(html, "<tr>", sprintf("<th>%s</th>", .html_escape(row_names[i])))
    for (j in seq_len(n)) {
      cell <- sprintf(
        "<div class='cellwrap'>
           <div class='cbox'>%s</div>
           <div class='xvar'>x<sub>%d%d</sub></div>
         </div>", fmt(C[i,j]), i, j)
      html <- c(html, sprintf("<td>%s</td>", cell))
    }
    html <- c(html, "</tr>")
  }
  html <- c(html, "</tbody></table>")
  paste(html, collapse = "\n")
}

transport_initial_html <- function(cost, supply, demand,
                                   labels = .transport_labels_default(),
                                   include_css = TRUE, table_class = "tbl-sim",
                                   title_override = NULL,
                                   digits = NULL){
  C <- as.matrix(cost); m <- nrow(C); n <- ncol(C)
  fmt <- if (is.null(digits)) function(x) sprintf("%g", x)
  else function(x) formatC(x, format = "f", digits = digits)
  
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  title <- if (is.null(title_override)) lbl$initial_title else title_override
  
  extra_css <- "
  <style>
    .tbl-sim .cellwrap{position:relative;min-width:72px;height:48px}
    .tbl-sim .cbox{position:absolute;top:6px;right:6px;
                   border:1px solid #adb5bd;border-radius:6px;
                   padding:1px 6px;font-size:12px;color:#495057}
    .tbl-sim .xvar{position:absolute;left:8px;bottom:6px;font-weight:600}
  </style>"
  
  html <- c()
  if (include_css) html <- c(html, .transport_css_bigM(), extra_css)
  
  html <- c(html, sprintf("<table class=\"%s\">", .html_escape(table_class)))
  html <- c(html, sprintf("<caption>%s</caption>", .html_escape(title)))
  
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", .html_escape(col_names[j])))
  html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", .html_escape(lbl$supply_label)))
  
  for (i in seq_len(m)){
    html <- c(html, "<tr>", sprintf("<th>%s</th>", .html_escape(row_names[i])))
    for (j in seq_len(n)){
      cell <- sprintf(
        "<div class='cellwrap'>
           <div class='cbox'>%s</div>
           <div class='xvar'>x<sub>%d%d</sub></div>
         </div>", fmt(C[i,j]), i, j)
      html <- c(html, sprintf("<td>%s</td>", cell))
    }
    html <- c(html, sprintf("<td class='rhs bold'>%g</td>", supply[i]), "</tr>")
  }
  
  html <- c(html, sprintf("<tr><th>%s</th>", .html_escape(lbl$demand_label)))
  for (j in seq_len(n)) html <- c(html, sprintf("<td class='rhs bold'>%g</td>", demand[j]))
  html <- c(html, sprintf("<td class='rhs bold'>%g</td>", sum(demand)),
            "</tr></tbody></table>")
  paste(html, collapse = "\n")
}

# ---------- Step (before/after) with exhausted row/col tint + optional VAM Δ ----------
transport_result_from_alloc_step_html <- function(cost, alloc,
                                                  supply_total, demand_total,
                                                  title = "Ara Adım",
                                                  labels = .transport_labels_default(),
                                                  include_css = TRUE,
                                                  table_class = "tbl-sim",
                                                  row_penalties = NULL,   # Δ(satır)
                                                  col_penalties = NULL) { # Δ(sütun)
  C <- as.matrix(cost); A <- as.matrix(alloc)
  m <- nrow(C); n <- ncol(C)
  row_used <- rowSums(A); col_used <- colSums(A)
  total_cost <- sum(A * C)
  H <- function(x){x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE); gsub(">","&gt;",x,fixed=TRUE)}
  
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else paste0(lbl$row_prefix," ",seq_len(m))
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else paste0(lbl$col_prefix," ",seq_len(n))
  
  # compute exhausted bands once
  eps <- 1e-12
  exh_rows <- which(abs(row_used - supply_total) <= eps)
  exh_cols <- which(abs(col_used - demand_total) <= eps)
  
  # safe getters (blank out if exhausted)
  get_row_pen <- function(i){
    if (i %in% exh_rows) return("")  # hide for exhausted row
    if (is.null(row_penalties) || length(row_penalties)==0) return("")
    nm <- names(row_penalties); key <- paste0("r", i)
    if (!is.null(nm) && key %in% nm) return(row_penalties[[key]])
    if (i <= length(row_penalties)) return(row_penalties[i])
    ""
  }
  get_col_pen <- function(j){
    if (j %in% exh_cols) return("")  # hide for exhausted column
    if (is.null(col_penalties) || length(col_penalties)==0) return("")
    nm <- names(col_penalties); key <- paste0("c", j)
    if (!is.null(nm) && key %in% nm) return(col_penalties[[key]])
    if (j <= length(col_penalties)) return(col_penalties[j])
    ""
  }
  
  
  have_row_pen <- !is.null(row_penalties) && length(row_penalties) > 0
  have_col_pen <- !is.null(col_penalties) && length(col_penalties) > 0
  
  eps <- 1e-12
  exh_rows <- which(abs(row_used - supply_total) <= eps)
  exh_cols <- which(abs(col_used - demand_total) <= eps)
  
  html <- c()
  if (include_css) html <- c(html, .transport_css_bigM())
  html <- c(html, sprintf("<table class=\"%s\">", H(table_class)))
  html <- c(html, sprintf("<caption>%s (%s = <b>%g</b>)</caption>",
                          H(title), H(lbl$total_cost), total_cost))
  
  # header
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)) {
    cls <- if (j %in% exh_cols) " class='exh-col'" else ""
    html <- c(html, sprintf("<th%s>%s</th>", cls, H(col_names[j])))
  }
  html <- c(html, sprintf("<th class='rhs'>%s</th>", H(lbl$supply_label)))
  if (have_row_pen) html <- c(html, "<th class='rhs'>Δ (satır)</th>")
  html <- c(html, "</tr></thead><tbody>")
  
  # body
  for (i in seq_len(m)) {
    row_cls <- if (i %in% exh_rows) " class='exh-row'" else ""
    html <- c(html, sprintf("<tr%s>", row_cls))
    html <- c(html, sprintf("<th>%s</th>", H(row_names[i])))
    for (j in seq_len(n)) {
      td_cls <- if (j %in% exh_cols) "exh-col" else ""
      if (A[i,j] > 0){
        cell <- sprintf("<div class='bold'>%g</div><div class='sub'>(c=%g)</div>", A[i,j], C[i,j])
        html <- c(html, sprintf("<td class='alloc %s'>%s</td>", td_cls, cell))
      } else {
        cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(c=%g)</div>", C[i,j])
        html <- c(html, sprintf("<td class='%s'>%s</td>", td_cls, cell))
      }
    }
    html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                            row_used[i], supply_total[i]))
    if (have_row_pen) {
      v <- get_row_pen(i)
      html <- c(html, sprintf("<td class='rhs'>%s</td>", H(as.character(v))))
    }
    html <- c(html, "</tr>")
  }
  
  # demand row
  html <- c(html, sprintf("<tr><th>%s</th>", H(lbl$demand_label)))
  for (j in seq_len(n)) {
    cls <- if (j %in% exh_cols) " class='rhs bold exh-col'" else " class='rhs bold'"
    html <- c(html, sprintf("<td%s>%g <span class='sub'>(%g)</span></td>",
                            cls, col_used[j], demand_total[j]))
  }
  html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                          sum(A), sum(supply_total)))
  if (have_row_pen) html <- c(html, "<td class='rhs'></td>")
  html <- c(html, "</tr>")
  
  # Δ (sütun) row
  if (have_col_pen) {
    html <- c(html, "<tr><th>Δ (sütun)</th>")
    for (j in seq_len(n)) {
      v <- get_col_pen(j)
      cls <- if (j %in% exh_cols) " class='rhs exh-col'" else " class='rhs'"
      html <- c(html, sprintf("<td%s>%s</td>", cls, H(as.character(v))))
    }
    html <- c(html, "<td class='rhs'></td>")
    if (have_row_pen) html <- c(html, "<td class='rhs'></td>")
    html <- c(html, "</tr>")
  }
  
  html <- c(html, "</tbody></table>")
  paste(html, collapse = "\n")
}


# --- Step-by-step renderer with VAM Δ shown for the *next* step (objective-aware) ---
render_trace_steps <- function(tr, dataset_title, outdir, method_label,
                               labels_for_steps = .transport_labels_default(),
                               max_steps = 2000,
                               show_titles = FALSE, show_notes = FALSE) {
  if (is.null(tr$steps) || !length(tr$steps)) return(invisible())
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  .H <- function(x){x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE); gsub(">","&gt;",x,fixed=TRUE)}
  
  idx_file <- file.path(outdir, sprintf("30_%s_adimlar_index.html", method_label))
  lines <- c("<html><head><meta charset='utf-8'><title>Adımlar</title></head><body>",
             if (show_titles) sprintf("<h2>%s – Adımlar</h2>", .H(dataset_title)) else "",
             "<ol>")
  
  C <- tr$cost; supply_total <- tr$supply; demand_total <- tr$demand
  n_written <- 0L
  
  for (t in seq_along(tr$steps)) {
    if (n_written >= max_steps) break
    st <- tr$steps[[t]]
    f_before <- sprintf("30_%s_step_%03d_before.html", method_label, t)
    f_after  <- sprintf("31_%s_step_%03d_after.html",  method_label, t)
    
    A_after  <- st$alloc + 0
    A_before <- A_after
    if (!is.null(st$i) && !is.null(st$j) && !is.null(st$x_ij) &&
        is.finite(st$x_ij) && st$x_ij > 0) {
      ii <- as.integer(st$i); jj <- as.integer(st$j); x <- as.numeric(st$x_ij)
      if (ii >= 1 && ii <= nrow(A_before) && jj >= 1 && jj <= ncol(A_before)) {
        A_before[ii, jj] <- max(0, A_before[ii, jj] - x)
      }
    }
    
    step_title <- paste0("Adım ", st$k,
                         if (isTRUE(show_notes) && !is.null(st$info$note))
                           paste0(" — ", st$stage, ": ", st$info$note)
                         else paste0(" — ", st$stage))
    
    # BEFORE: show current Δ
    rpen_now  <- st$info$rpen;      if (length(rpen_now)  == 0) rpen_now  <- NULL
    cpen_now  <- st$info$cpen;      if (length(cpen_now)  == 0) cpen_now  <- NULL
    
    cat(
      transport_result_from_alloc_step_html(
        cost = C, alloc = A_before,
        supply_total = supply_total, demand_total = demand_total,
        title = paste0(step_title, " (öncesi)"),
        labels = labels_for_steps, include_css = TRUE,
        row_penalties = rpen_now, col_penalties = cpen_now
      ),
      file = file.path(outdir, f_before)
    )
    
    # AFTER: show next-step Δ
    rpen_next <- st$info$rpen_next; if (length(rpen_next) == 0) rpen_next <- NULL
    cpen_next <- st$info$cpen_next; if (length(cpen_next) == 0) cpen_next <- NULL
    
    cat(
      transport_result_from_alloc_step_html(
        cost = C, alloc = A_after,
        supply_total = supply_total, demand_total = demand_total,
        title = paste0(step_title, " (sonrası)"),
        labels = labels_for_steps, include_css = TRUE,
        row_penalties = rpen_next, col_penalties = cpen_next
      ),
      file = file.path(outdir, f_after)
    )
    
    lines <- c(lines, sprintf("<li>Adım %d: <a href='%s'>öncesi</a> · <a href='%s'>sonrası</a></li>",
                              st$k, f_before, f_after))
    n_written <- n_written + 1L
  }
  
  lines <- c(lines, "</ol>",
             if (n_written >= max_steps) "<p><em>max_steps sınırı nedeniyle kesildi.</em></p>" else "",
             "</body></html>")
  writeLines(lines, idx_file)
  invisible(TRUE)
}




# ===== MODI u–v / R renderers (40/41–45/46–47/50) =====
## ===== Fallback helpers (only defined if absent) ===========================
if (!exists(".html_escape", mode="function")) {
  .html_escape <- function(x){
    x <- gsub("&","&amp;",x,fixed=TRUE)
    x <- gsub("<","&lt;",x,fixed=TRUE)
    x <- gsub(">","&gt;",x,fixed=TRUE)
    x
  }
}
if (!exists(".transport_labels_default", mode="function")) {
  .transport_labels_default <- function(){
    list(
      initial_title   = "Başlangıç Tablosu",
      allocation_title= "Dağıtım",
      total_cost      = "Toplam Maliyet",
      row_prefix      = "Kaynak", col_prefix = "Hedef",
      row_names       = NULL,     col_names  = NULL,
      supply_label    = "Arz",    demand_label = "Talep"
    )
  }
}
if (!exists(".transport_css_bigM", mode="function")) {
  .transport_css_bigM <- function(){
    paste0(
      "<style>
      .tbl-sim{border-collapse:collapse;margin:10px 0;font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;font-size:14px}
      .tbl-sim caption{caption-side:top;font-weight:bold;margin-bottom:6px;text-align:left}
      .tbl-sim th,.tbl-sim td{border:1px solid #dee2e6;padding:6px 10px;text-align:center}
      .tbl-sim thead th{background:#f1f3f5}
      .tbl-sim th:first-child{background:#fafafa;text-align:left}
      .tbl-sim .muted{color:#adb5bd}.tbl-sim .sub{font-size:11px;color:#6c757d}
      .tbl-sim .bold{font-weight:bold}
      .tbl-sim .alloc{background:#fff3cd;border:2px solid #f0ad4e}
      .tbl-sim .rhs{background:#e7f1fb}
      .tbl-sim .neg{color:#b00020;font-weight:600}
      .tbl-sim .minneg{background:#fdecea;border:2px solid #f5c2c7}
      .tbl-sim .basic{background:#eef9f1}
      .tbl-sim .na{color:#adb5bd}
      .uvwrap{display:flex; gap:16px; align-items:flex-start; margin:8px 0}
      .uvbox{min-width:220px}
      .uvbox table{width:100%}
      .eqs{font-family:ui-monospace,Consolas,monospace; background:#f8f9fa; padding:10px; border:1px solid #e9ecef; border-radius:6px}
      </style>"
    )
  }
}
if (!exists(".mk_names", mode="function")) {
  .mk_names <- function(prefix, k) sprintf("%s %d", prefix, seq_len(k))
}

## ===== MODI u–v snapshot renderer =========================================
# Writes one page per MODI "modi-uv" snapshot:
#   40_<method>_modi_index.html          (index)
#   40_<method>_modi_it_###.html         (per-step page with u, v, and R table)
render_modi_uv_tables <- function(tr, outdir, method_label,
                                  labels_for_steps = .transport_labels_default(),
                                  include_css = TRUE) {
  if (is.null(tr$steps) || !length(tr$steps)) return(invisible())
  
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  base_css <- if (include_css) .transport_css_bigM() else ""
  
  idx_path <- file.path(outdir, sprintf("40_%s_modi_index.html", method_label))
  idx <- c("<html><head><meta charset='utf-8'><title>MODI u–v</title>",
           base_css, "</head><body>",
           sprintf("<h2>%s – MODI u–v Hesap Adımları</h2>",
                   .html_escape(if (!is.null(tr$method)) toupper(tr$method) else method_label)),
           "<ol>")
  
  C <- tr$cost
  m <- nrow(C); n <- ncol(C)
  lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  
  snap_id <- 0L
  for (st in tr$steps) {
    if (!identical(st$stage, "modi-uv")) next
    snap_id <- snap_id + 1L
    
    u <- st$info$u; v <- st$info$v; R <- st$info$reduced
    basis <- st$info$basis
    if (is.null(basis)) basis <- (st$alloc > 0) + 0
    
    # find minimum negative R to highlight
    obj <- if (!is.null(tr$objective)) tr$objective else "min"
    vm <- .modi_violation_masks(R, objective = obj, tol = 1e-12)
    
    cap <- if (!is.null(st$info$note)) st$info$note else "u–v hesap"
    step_title <- sprintf("Adım %d — %s", st$k, cap)
    
    # ---------- build page ----------
    fname <- sprintf("40_%s_modi_it_%03d.html", method_label, snap_id)
    H <- .html_escape
    html <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>",
              sprintf("<h3>%s</h3>", H(step_title)),
              "<div class='uvwrap'>")
    
    # u table
    html <- c(html, "<div class='uvbox'>",
              "<table class='tbl-sim'><thead><tr><th>i</th><th>u<sub>i</sub></th></tr></thead><tbody>")
    for (i in seq_len(m)) {
      val <- if (is.na(u[i])) "<span class='na'>&ndash;</span>" else sprintf("%g", u[i])
      html <- c(html, sprintf("<tr><th>%s</th><td>%s</td></tr>", H(row_names[i]), val))
    }
    html <- c(html, "</tbody></table></div>")
    
    # v table
    html <- c(html, "<div class='uvbox'>",
              "<table class='tbl-sim'><thead><tr><th>j</th><th>v<sub>j</sub></th></tr></thead><tbody>")
    for (j in seq_len(n)) {
      val <- if (is.na(v[j])) "<span class='na'>&ndash;</span>" else sprintf("%g", v[j])
      html <- c(html, sprintf("<tr><th>%s</th><td>%s</td></tr>", H(col_names[j]), val))
    }
    html <- c(html, "</tbody></table></div>")
    
    html <- c(html, "</div>") # /uvwrap
    
    # Reduced-cost matrix with basis marker
    html <- c(html,
              "<table class='tbl-sim'><thead><tr><th></th>")
    for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", H(col_names[j])))
    html <- c(html, "</tr></thead><tbody>")
    for (i in seq_len(m)) {
      html <- c(html, sprintf("<tr><th>%s</th>", H(row_names[i])))
      for (j in seq_len(n)) {
        cls <- character(0)
        is_degen <- (!is.null(basis) && basis[i,j]==1 && abs((st$alloc + 0)[i,j]) <= 1e-12)
        if (is_degen)       cls <- c(cls, "degen")
        if (vm$viol[i,j]) cls <- c(cls, "viol")
        if (vm$best[i,j]) cls <- c(cls, "pivot")  
        if (basis[i,j] == 1) cls <- c(cls, "basic")
        val <- if (is.na(R[i,j])) "<span class='na'>&ndash;</span>" else sprintf("%g", R[i,j])
        sub <- sprintf("<div class='sub'>(c=%g%s)</div>", C[i,j], if (basis[i,j]==1) ", basic" else "")
        html <- c(html, sprintf("<td class='%s'>%s%s</td>", paste(cls, collapse=" "), val, sub))
      }
      html <- c(html, "</tr>")
    }
    html <- c(html, "</tbody></table>",
              "</body></html>")
    
    writeLines(html, file.path(outdir, fname))
    idx <- c(idx, sprintf("<li>Adım %d: <a href='%s'>u–v ve R</a></li>", st$k, fname))
  }
  
  idx <- c(idx, "</ol></body></html>")
  writeLines(idx, idx_path)
  invisible(TRUE)
}
###############################################################################
## Minimal fallbacks (won’t overwrite if you already have them)
###############################################################################
if (!exists(".html_escape", mode="function")) {
  .html_escape <- function(x){
    x <- gsub("&","&amp;",x,fixed=TRUE)
    x <- gsub("<","&lt;",x,fixed=TRUE)
    x <- gsub(">","&gt;",x,fixed=TRUE)
    x
  }
}
if (!exists(".mk_names", mode="function")) {
  .mk_names <- function(prefix, k) sprintf("%s %d", prefix, seq_len(k))
}
if (!exists(".transport_labels_default", mode="function")) {
  .transport_labels_default <- function(){
    list(
      initial_title   = "Başlangıç Tablosu",
      allocation_title= "Dağıtım",
      total_cost      = "Toplam Maliyet",
      row_prefix      = "Kaynak", col_prefix = "Hedef",
      row_names       = NULL,     col_names  = NULL,
      supply_label    = "Arz",    demand_label = "Talep"
    )
  }
}
if (!exists(".transport_css_bigM", mode="function")) {
  .transport_css_bigM <- function(){
    paste0(
      "<style>
      .tbl-sim{border-collapse:collapse;margin:10px 0;font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;font-size:14px}
      .tbl-sim caption{caption-side:top;font-weight:bold;margin-bottom:6px;text-align:left}
      .tbl-sim th,.tbl-sim td{border:1px solid #dee2e6;padding:6px 10px;text-align:center}
      .tbl-sim thead th{background:#f1f3f5}
      .tbl-sim th:first-child{background:#fafafa;text-align:left}
      .tbl-sim .muted{color:#adb5bd}.tbl-sim .sub{font-size:11px;color:#6c757d}
      .tbl-sim .bold{font-weight:bold}
      .tbl-sim .alloc{background:#fff3cd;border:2px solid #f0ad4e}
      .tbl-sim .rhs{background:#e7f1fb}
      .tbl-sim .na{color:#adb5bd}
      .tbl-sim .neg{color:#b00020;font-weight:600}
      .tbl-sim .minneg{background:#fdecea;border:2px solid #f5c2c7}
      .tbl-sim .basic{background:#eef9f1}
      .tbl-sim .degen{background:#eaf5ff;border:2px dashed #1a73e8} /* 0 değerli temel hücre */
      .uvwrap{display:flex; gap:16px; align-items:flex-start; margin:8px 0}
      .uvbox{min-width:220px}
      .uvbox table{width:100%}
      .eqs{font-family:ui-monospace,Consolas,monospace; background:#f8f9fa; padding:10px; border:1px solid #e9ecef; border-radius:6px}
      .uvchip{display:inline-block;padding:1px 6px;border:1px solid #adb5bd;border-radius:6px;font-size:12px;margin-left:6px;color:#495057;background:#f8f9fa}
      .pivot{background:#e8f0fe; box-shadow: inset 0 0 0 3px #1a73e8}
      .plus{background:#f1faf1; box-shadow: inset 0 0 0 3px #2e7d32}
      .minus{background:#fff1f1; box-shadow: inset 0 0 0 3px #c62828}
      .cellwrap{position:relative;min-width:76px;height:46px}
      .cbox{position:absolute;top:6px;right:6px;border:1px solid #adb5bd;border-radius:6px;padding:1px 6px;font-size:12px;color:#495057}
      </style>"
    )
  }
}
.mf <- function(x){
  if (is.na(x)) return("NA")
  if (isTRUE(all.equal(x, round(x), tol=1e-12))) return(sprintf("%d", as.integer(round(x))))
  sprintf("%g", x)
}

###############################################################################
## Exporters (fallbacks) for LaTeX logs you referenced in your pipeline
###############################################################################
if (!exists("transport_export_latex_uv_eqs_from_step", mode="function")) {
  transport_export_latex_uv_eqs_from_step <- function(u, v, C, uv_log, file,
                                                      title="LaTeX – Türetilen eşitlikler (u–v)",
                                                      render_math=FALSE,
                                                      mathjax_src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"){
    .f <- function(z) if (isTRUE(all.equal(z, round(z)))) sprintf("%d", as.integer(round(z))) else sprintf("%g", z)
    lines <- character(0)
    if (!is.null(uv_log) && length(uv_log)){
      for (e in uv_log){
        i <- e$i; j <- e$j
        if (identical(e$kind,"v"))
          lines <- c(lines, sprintf("v_{%d} = c_{%d,%d} - u_{%d} = %s - %s = %s", j, i, j, i, .f(C[i,j]), .f(u[i]), .f(v[j])))
        if (identical(e$kind,"u"))
          lines <- c(lines, sprintf("u_{%d} = c_{%d,%d} - v_{%d} = %s - %s = %s", i, i, j, j, .f(C[i,j]), .f(v[j]), .f(u[i])))
      }
    }
    if (!length(lines)) lines <- "% Bu adımda u–v türetimi yok."
    latex <- sprintf("\\[\\begin{aligned}\n%s \\\\\n\\end{aligned}\\]\n", paste(lines, collapse=" \\\\\n"))
    H <- .html_escape
    html <- c("<!doctype html><html><head><meta charset='utf-8'><title>LaTeX u–v</title>",
              "<style>body{font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:20px}.raw{white-space:pre-wrap;background:#f8f9fa;border:1px solid #e9ecef;padding:12px;border-radius:6px;font-family:ui-monospace,Consolas,Monaco,monospace}</style>")
    if (render_math) html <- c(html, sprintf("<script id='MathJax-script' async src='%s'></script>", H(mathjax_src)))
    html <- c(html, "</head><body>", sprintf("<h3>%s</h3>", H(title)),
              sprintf("<div class='raw'>%s</div>", H(latex)))
    if (render_math) html <- c(html, "<h4>Önizleme</h4>", latex)
    html <- c(html, "</body></html>")
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(html, file)
    invisible(latex)
  }
}
if (!exists("transport_export_latex_rij_from_step", mode="function")) {
  transport_export_latex_rij_from_step <- function(cost, u, v, file,
                                                   title="LaTeX – R_{ij} Hesapları",
                                                   only_defined=TRUE,
                                                   render_math=FALSE,
                                                   mathjax_src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"){
    C <- as.matrix(cost); m <- nrow(C); n <- ncol(C)
    .f <- function(z) if (isTRUE(all.equal(z, round(z)))) sprintf("%d", as.integer(round(z))) else sprintf("%g", z)
    lines <- character(0)
    for (i in seq_len(m)) for (j in seq_len(n)) {
      if (is.na(u[i]) || is.na(v[j])){
        if (!only_defined) lines <- c(lines, sprintf("R_{%d,%d} = c_{%d,%d} - u_{%d} - v_{%d}\\;\\text{ (tanımsız)}", i,j,i,j,i,j))
      } else {
        Rij <- C[i,j] - u[i] - v[j]
        lines <- c(lines, sprintf("R_{%d,%d} = c_{%d,%d} - u_{%d} - v_{%d} = %s - %s - %s = %s",
                                  i,j,i,j,i,j,.f(C[i,j]),.f(u[i]),.f(v[j]),.f(Rij)))
      }
    }
    if (!length(lines)) lines <- "% Bu adımda R_ij üretilemedi."
    latex <- sprintf("\\[\\begin{aligned}\n%s \\\\\n\\end{aligned}\\]\n", paste(lines, collapse=" \\\\\n"))
    H <- .html_escape
    html <- c("<!doctype html><html><head><meta charset='utf-8'><title>LaTeX R_ij</title>",
              "<style>body{font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:20px}.raw{white-space:pre-wrap;background:#f8f9fa;border:1px solid #e9ecef;padding:12px;border-radius:6px;font-family:ui-monospace,Consolas,Monaco,monospace}</style>")
    if (render_math) html <- c(html, sprintf("<script id='MathJax-script' async src='%s'></script>", H(mathjax_src)))
    html <- c(html, "</head><body>", sprintf("<h3>%s</h3>", H(title)),
              sprintf("<div class='raw'>%s</div>", H(latex)))
    if (render_math) html <- c(html, "<h4>Önizleme</h4>", latex)
    html <- c(html, "</body></html>")
    dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
    writeLines(html, file)
    invisible(latex)
  }
}

###############################################################################
## 1) MODI split — 41_uv.html, 42_eqs.html, 43_reduced.html (+ index)
###############################################################################
render_modi_split <- function(tr, outdir, method_label,
                              labels_for_steps = .transport_labels_default(),
                              include_css = TRUE,
                              show_titles = TRUE){
  if (is.null(tr$steps) || !length(tr$steps)) return(invisible())
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  C <- tr$cost; m <- nrow(C); n <- ncol(C)
  lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
  rn <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  cn <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  css <- if (include_css) .transport_css_bigM() else ""
  idx <- c("<html><head><meta charset='utf-8'><title>MODI adımları</title>", css, "</head><body>",
           sprintf("<h2>%s – MODI u–v adımları</h2>", .html_escape(method_label)), "<ol>")
  snap <- 0L
  for (st in tr$steps){
    if (!identical(st$stage, "modi-uv")) next
    snap <- snap + 1L
    u <- st$info$u; v <- st$info$v; R <- st$info$reduced
    basis <- st$info$basis; if (is.null(basis)) basis <- (st$alloc > 0) + 0
    
    obj <- if (!is.null(tr$objective)) tr$objective else "min"
    vm <- .modi_violation_masks(R, objective = obj, tol = 1e-12)
    
    cap <- if (!is.null(st$info$note)) st$info$note else "u–v hesap"
    title <- sprintf("Adım %d — %s", st$k, cap)
    ## 41: u–v tables
    f_uv <- file.path(outdir, sprintf("41_%s_it_%03d_uv.html", method_label, snap))
    html <- c("<html><head><meta charset='utf-8'>", css, "</head><body>")
    if (show_titles) html <- c(html, sprintf("<h3>%s — u–v tabloları</h3>", .html_escape(title)))
    html <- c(html, "<div class='uvwrap'>",
              "<div class='uvbox'><table class='tbl-sim'><thead><tr><th>i</th><th>u<sub>i</sub></th></tr></thead><tbody>")
    for (i in seq_len(m)) html <- c(html, sprintf("<tr><th>%s</th><td>%s</td></tr>", .html_escape(rn[i]), if (is.na(u[i])) "<span class='na'>&ndash;</span>" else sprintf("%g",u[i])))
    html <- c(html, "</tbody></table></div>",
              "<div class='uvbox'><table class='tbl-sim'><thead><tr><th>j</th><th>v<sub>j</sub></th></tr></thead><tbody>")
    for (j in seq_len(n)) html <- c(html, sprintf("<tr><th>%s</th><td>%s</td></tr>", .html_escape(cn[j]), if (is.na(v[j])) "<span class='na'>&ndash;</span>" else sprintf("%g",v[j])))
    html <- c(html, "</tbody></table></div></div></body></html>")
    writeLines(html, f_uv)
    
    ## 42: derived equations
    f_eq <- file.path(outdir, sprintf("42_%s_it_%03d_eqs.html", method_label, snap))
    transport_export_latex_uv_eqs_from_step(u, v, C, st$info$uv_log, file=f_eq,
                                            title=sprintf("%s — Türetilen eşitlikler", title),
                                            render_math=FALSE)
    
    ## 43: R table
    f_r <- file.path(outdir, sprintf("43_%s_it_%03d_reduced.html", method_label, snap))
    H <- .html_escape
    html <- c("<html><head><meta charset='utf-8'>", css, "</head><body>")
    if (show_titles) html <- c(html, sprintf("<h3>%s — İndirgenmiş maliyet matrisi R</h3>", H(title)))
    html <- c(html, "<table class='tbl-sim'><thead><tr><th></th>")
    for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", H(cn[j])))
    html <- c(html, "</tr></thead><tbody>")
    for (i in seq_len(m)){
      html <- c(html, sprintf("<tr><th>%s</th>", H(rn[i])))
      for (j in seq_len(n)){
        cls <- character(0)
        if (basis[i,j]==1) cls <- c(cls, "basic")
        if (vm$viol[i,j])  cls <- c(cls, "viol")
        if (vm$best[i,j])  cls <- c(cls, "pivot")
        val <- if (is.na(R[i,j])) "<span class='na'>&ndash;</span>" else sprintf("%g",R[i,j])
        sub <- sprintf("<div class='sub'>(c=%g%s)</div>", C[i,j], if (basis[i,j]==1) ", basic" else "")
        html <- c(html, sprintf("<td class='%s'>%s%s</td>", paste(cls, collapse=" "), val, sub))
      }
      html <- c(html, "</tr>")
    }
    html <- c(html, "</tbody></table></body></html>")
    writeLines(html, f_r)
    
    idx <- c(idx, sprintf("<li>Adım %d: <a href='%s'>u–v</a> · <a href='%s'>eşitlikler</a> · <a href='%s'>R</a></li>",
                          st$k, basename(f_uv), basename(f_eq), basename(f_r)))
  }
  idx <- c(idx, "</ol></body></html>")
  writeLines(idx, file.path(outdir, sprintf("45_%s_modi_split_index.html", method_label)))
  invisible(TRUE)
}

###############################################################################
## 2) MODI u/v on final — overlay + R table + transition (before/after)
###############################################################################
render_modi_uv_on_final <- function(tr, final_res, outdir,
                                    method_label,
                                    labels_for_steps = .transport_labels_default(),
                                    final_href = NULL,
                                    render_math = FALSE) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  C <- tr$cost; m <- nrow(C); n <- ncol(C)
  lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
  rn <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  cn <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  H <- .html_escape; css <- .transport_css_bigM(); eps <- 1e-9
  
  # Helpers
  find_loop <- function(is_basic, ie, je){
    rows_to_cols <- lapply(seq_len(m), function(i) which(is_basic[i, ]))
    cols_to_rows <- lapply(seq_len(n), function(j) which(is_basic[, j]))
    start <- ie; target <- m + je; N <- m + n
    prev <- rep(NA_integer_, N); visited <- rep(FALSE, N)
    q <- integer(0); head <- 1L
    q <- c(q, start); visited[start] <- TRUE; prev[start] <- 0L
    while (head <= length(q)) {
      node <- q[head]; head <- head + 1L
      if (node <= m) {
        for (j in rows_to_cols[[node]]) {
          v <- m + j
          if (!visited[v]) { visited[v] <- TRUE; prev[v] <- node; q <- c(q, v) }
        }
      } else {
        j <- node - m
        for (i in cols_to_rows[[j]]) {
          if (!visited[i]) { visited[i] <- TRUE; prev[i] <- node; q <- c(q, i) }
        }
      }
      if (visited[target]) break
    }
    if (!visited[target]) return(NULL)
    nodes <- integer(0); cur <- target
    while (cur != 0L && !is.na(cur)) { nodes <- c(cur, nodes); cur <- prev[cur] }
    loop <- matrix(c(ie, je), ncol=2, byrow=TRUE)
    if (length(nodes) >= 2L){
      for (k in seq_len(length(nodes)-1L)){
        a <- nodes[k]; b <- nodes[k+1L]
        if (a <= m && b > m) { i <- a; j <- b - m
        } else if (a > m && b <= m) { i <- b; j <- a - m
        } else next
        if (!(i == ie && j == je)) loop <- rbind(loop, c(i,j))
      }
    }
    loop
  }
  render_uv_alloc_table <- function(A, u, v, caption, file){
    html <- c("<html><head><meta charset='utf-8'>", css, "</head><body>",
              sprintf("<table class='tbl-sim'><caption>%s</caption>", H(caption)),
              "<thead><tr><th></th>")
    for (j in seq_len(n)) html <- c(html, sprintf("<th>%s<br><span class='sub'>v<sub>%d</sub>=%s</span></th>", H(cn[j]), j, ifelse(is.na(v[j]),"&ndash;",sprintf("%g",v[j]))))
    html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", H(lbl$supply_label)))
    for (i in seq_len(m)){
      html <- c(html, "<tr>", sprintf("<th>%s<br><span class='sub'>u<sub>%d</sub>=%s</span></th>", H(rn[i]), i, ifelse(is.na(u[i]),"&ndash;",sprintf("%g",u[i]))))
      for (j in seq_len(n)){
        if (A[i,j] > 0) {
          cell <- sprintf("<div class='bold'>%g</div><div class='sub'>(c=%g)</div>", A[i,j], C[i,j])
          html <- c(html, sprintf("<td class='alloc'>%s</td>", cell))
        } else {
          html <- c(html, sprintf("<td><div class='muted'>&ndash;</div><div class='sub'>(c=%g)</div></td>", C[i,j]))
        }
      }
      html <- c(html, sprintf("<td class='rhs bold'>%g</td></tr>", sum(A[i, ])))
    }
    html <- c(html, sprintf("<tr><th>%s</th>", H(lbl$demand_label)))
    for (j in seq_len(n)) html <- c(html, sprintf("<td class='rhs bold'>%g</td>", sum(A[,j])))
    html <- c(html, sprintf("<td class='rhs bold'>%g</td></tr>", sum(A)), "</tbody></table></body></html>")
    writeLines(html, file)
  }
  render_R_table <- function(u,v,file){
    R <- matrix(NA_real_, m, n)
    for (i in seq_len(m)) for (j in seq_len(n)) if (!is.na(u[i]) && !is.na(v[j])) R[i,j] <- C[i,j]-u[i]-v[j]
    obj <- if (!is.null(tr$objective)) tr$objective else "min"
    vm  <- .modi_violation_masks(R, objective = obj, tol = 1e-12)    
    html <- c("<html><head><meta charset='utf-8'>", css, "</head><body>",
              "<table class='tbl-sim'><thead><tr><th></th>")
    for (j in seq_len(n)) html <- c(html, sprintf("<th>%s<br><span class='sub'>v<sub>%d</sub>=%s</span></th>", H(cn[j]), j, ifelse(is.na(v[j]),"&ndash;",sprintf("%g",v[j]))))
    html <- c(html, "</tr></thead><tbody>")
    for (i in seq_len(m)){
      html <- c(html, "<tr>", sprintf("<th>%s<br><span class='sub'>u<sub>%d</sub>=%s</span></th>", H(rn[i]), i, ifelse(is.na(u[i]),"&ndash;",sprintf("%g",u[i]))))
      for (j in seq_len(n)){
        val <- R[i,j]; cls <- character(0)
        if (vm$viol[i,j]) cls <- c(cls, "viol")
        if (vm$best[i,j]) cls <- c(cls, "pivot")
        stamp <- if (is.na(val)) "<span class='na'>&ndash;</span>" else sprintf("%g",val)
        html <- c(html, sprintf("<td class='%s'><div class='cbox'>c=%g</div>%s</td>", paste(cls, collapse=" "), C[i,j], stamp))
      }
      html <- c(html, "</tr>")
    }
    html <- c(html, "</tbody></table></body></html>")
    writeLines(html, file)
  }
  render_transition <- function(A_after, A_before, ie, je, loop_path, file){
    signs <- rep(c(1, -1), length.out = nrow(loop_path))
    plus  <- loop_path[signs ==  1, , drop = FALSE]
    minus <- loop_path[signs == -1, , drop = FALSE]
    
    # local helpers so we never hit "could not find function"
    is_pivot <- function(i, j) (i == ie) && (j == je)
    in_plus  <- if (nrow(plus))  function(i, j) any(plus[,1]  == i & plus[,2]  == j) else function(...) FALSE
    in_minus <- if (nrow(minus)) function(i, j) any(minus[,1] == i & minus[,2] == j) else function(...) FALSE
    
    html <- c("<html><head><meta charset='utf-8'>", .transport_css_bigM(), "</head><body>",
              "<table class='tbl-sim'><thead><tr><th></th>")
    
    m <- nrow(A_after); n <- ncol(A_after)
    H <- .html_escape; C <- tr$cost  # or pass C in args if you prefer
    
    # column headers (adjust cn source to your context)
    cn <- .mk_names(.transport_labels_default()$col_prefix, n)
    rn <- .mk_names(.transport_labels_default()$row_prefix, m)
    
    for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", H(cn[j])))
    html <- c(html, "</tr></thead><tbody>")
    
    for (i in seq_len(m)){
      html <- c(html, "<tr>", sprintf("<th>%s</th>", H(rn[i])))
      for (j in seq_len(n)){
        cls <- character(0)
        if (is_pivot(i,j))       cls <- c(cls, "pivot")
        else if (in_plus(i,j))   cls <- c(cls, "plus")
        else if (in_minus(i,j))  cls <- c(cls, "minus")
        
        val    <- A_after[i,j]
        before <- A_before[i,j]
        
        inner <- if (is_pivot(i,j) || in_plus(i,j) || in_minus(i,j)) {
          sprintf("<div class='bold'>%g</div><div class='sub'>(%g→%g)</div>", val, before, val)
        } else if (val > 0) {
          sprintf("<div class='bold'>%g</div>", val)
        } else {
          "<div class='muted'>&ndash;</div>"
        }
        
        html <- c(html,
                  sprintf("<td class='%s'><div class='cellwrap'><div class='cbox'>c=%g</div>%s</div></td>",
                          paste(cls, collapse=" "), C[i,j], inner))
      }
      html <- c(html, "</tr>")
    }
    
    html <- c(html, "</tbody></table></body></html>")
    writeLines(html, file)
  }
  
  
  # Pair each "modi-uv" with the next "modi" pivot step
  steps <- tr$steps
  items <- character(0); pair <- 0L
  for (i in seq_along(steps)){
    if (!identical(steps[[i]]$stage, "modi-uv")) next
    # find next pivot
    j <- i + 1L
    while (j <= length(steps) && !identical(steps[[j]]$stage, "modi")) j <- j + 1L
    if (j > length(steps)) break
    pair <- pair + 1L
    st_uv <- steps[[i]]; st_pv <- steps[[j]]
    u <- st_uv$info$u; v <- st_uv$info$v
    A_before <- st_uv$alloc + 0
    A_after  <- st_pv$alloc + 0
    basis_before <- if (!is.null(st_uv$info$basis)) st_uv$info$basis==1 else A_before>0
    ie <- as.integer(st_pv$i); je <- as.integer(st_pv$j)
    loop <- find_loop(basis_before, ie, je)
    
    f_uv <- file.path(outdir, sprintf("46_%s_it_%03d_uv_on_final.html", method_label, pair))
    f_R  <- file.path(outdir, sprintf("46_%s_it_%03d_R_on_final.html",  method_label, pair))
    f_b  <- file.path(outdir, sprintf("46_%s_it_%03d_transition.html",  method_label, pair))
    f_a  <- file.path(outdir, sprintf("46_%s_it_%03d_transition_after.html", method_label, pair))
    cap <- sprintf("(Toplam Maliyet = %g)%s", sum(A_before*C),
                   if (!is.null(final_href)) paste0(" – <a href='", H(final_href),"'>Nihai dağıtım</a>") else "")
    
    render_uv_alloc_table(A_before, u, v, caption=cap, file=f_uv)
    render_R_table(u, v, f_R)
    if (!is.null(loop) && nrow(loop)>=4){
      render_transition(A_before, A_before, ie, je, loop, f_b)   # before
      render_transition(A_after,  A_before, ie, je, loop, f_a)   # after
      items <- c(items, sprintf("<li>Adım %d: <a href='%s'>u/v+dağıtım</a> · <a href='%s'>R</a> · Geçiş: <a href='%s'>öncesi</a> / <a href='%s'>sonrası</a></li>",
                                pair, basename(f_uv), basename(f_R), basename(f_b), basename(f_a)))
    } else {
      render_transition(A_before, A_before, ie, je, matrix(c(ie,je),1,2), f_b)
      items <- c(items, sprintf("<li>Adım %d: <a href='%s'>u/v+dağıtım</a> · <a href='%s'>R</a> · Geçiş: <a href='%s'>öncesi</a></li>",
                                pair, basename(f_uv), basename(f_R), basename(f_b)))
    }
  }
  
  idx <- c("<html><head><meta charset='utf-8'>", css, "</head><body>",
           sprintf("<h2>%s – MODI (u/v, R, Geçiş)</h2>", H(method_label)),
           "<ol>", items, "</ol>", "</body></html>")
  writeLines(idx, file.path(outdir, sprintf("46_%s_uv_on_final_index.html", method_label)))
  invisible(TRUE)
}

###############################################################################
## 3) Alternate optima finder — R==0 non-basic cells at convergence
###############################################################################
render_modi_alternative_solutions <- function(tr, outdir, method_label,
                                              labels_for_steps = .transport_labels_default()){
  if (is.null(tr$steps) || !length(tr$steps)) return(invisible())
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  C <- tr$cost; m <- nrow(C); n <- ncol(C)
  lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
  rn <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  cn <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  css <- .transport_css_bigM(); H <- .html_escape
  
  # last MODI-UV snapshot (optimality check)
  last_uv <- NULL
  for (k in seq_along(tr$steps)) {
    st <- tr$steps[[k]]
    if (identical(st$stage, "modi-uv")) last_uv <- st
  }
  if (is.null(last_uv)) return(invisible())
  
  u <- last_uv$info$u; v <- last_uv$info$v
  A <- last_uv$alloc + 0
  basis <- if (!is.null(last_uv$info$basis)) last_uv$info$basis==1 else A>0
  
  # reduced costs
  R <- matrix(NA_real_, m, n)
  for (i in 1:m) for (j in 1:n) if (!is.na(u[i]) && !is.na(v[j])) R[i,j] <- C[i,j]-u[i]-v[j]
  zero_nb <- which(basis==FALSE & !is.na(R) & abs(R) <= 1e-12, arr.ind=TRUE)
  if (!nrow(zero_nb)) {
    idx <- c("<html><head><meta charset='utf-8'>", css, "</head><body>",
             sprintf("<h3>%s — Alternatif çözüm yok</h3>", H(method_label)),
             "<p>Optimal çözüm tekil görünüyor (R=0 olan temel-dışı hücre yok).</p>",
             "</body></html>")
    writeLines(idx, file.path(outdir, sprintf("47_%s_alternatives_index.html", method_label)))
    return(invisible(TRUE))
  }
  
  # loop finder (on current basis + candidate entering cell)
  find_loop <- function(is_basic, ie, je){
    isb <- is_basic
    isb[ie, je] <- TRUE
    rows_to_cols <- lapply(seq_len(m), function(i) which(isb[i, ]))
    cols_to_rows <- lapply(seq_len(n), function(j) which(isb[, j]))
    start <- ie; target <- m + je; N <- m + n
    prev <- rep(NA_integer_, N); visited <- rep(FALSE, N)
    q <- integer(0); head <- 1L
    q <- c(q, start); visited[start] <- TRUE; prev[start] <- 0L
    while (head <= length(q)) {
      node <- q[head]; head <- head + 1L
      if (node <= m) {
        for (jj in rows_to_cols[[node]]) {
          v <- m + jj
          if (!visited[v]) { visited[v] <- TRUE; prev[v] <- node; q <- c(q, v) }
        }
      } else {
        j <- node - m
        for (ii in cols_to_rows[[j]]) {
          if (!visited[ii]) { visited[ii] <- TRUE; prev[ii] <- node; q <- c(q, ii) }
        }
      }
      if (visited[target]) break
    }
    if (!visited[target]) return(NULL)
    nodes <- integer(0); cur <- target
    while (cur != 0L && !is.na(cur)) { nodes <- c(cur, nodes); cur <- prev[cur] }
    loop <- matrix(c(ie, je), ncol=2, byrow=TRUE)
    if (length(nodes) >= 2L) {
      for (k in seq_len(length(nodes)-1L)) {
        a <- nodes[k]; b <- nodes[k+1L]
        if (a <= m && b > m) { i <- a; j <- b - m
        } else if (a > m && b <= m) { i <- b; j <- a - m
        } else next
        if (!(i==ie && j==je)) loop <- rbind(loop, c(i,j))
      }
    }
    loop
  }
  
  items <- character(0); alt_id <- 0L
  for (p in seq_len(nrow(zero_nb))){
    ie <- zero_nb[p,1]; je <- zero_nb[p,2]
    loop <- find_loop(basis, ie, je)
    if (is.null(loop) || nrow(loop) < 4L) next
    
    signs <- rep(c(1,-1), length.out=nrow(loop))
    minus <- loop[signs==-1,,drop=FALSE]
    theta <- min(A[cbind(minus[,1], minus[,2])], na.rm=TRUE)
    if (!is.finite(theta) || theta <= 0) next
    
    A_alt <- A
    for (k in seq_len(nrow(loop))){
      i <- loop[k,1]; j <- loop[k,2]
      A_alt[i,j] <- A_alt[i,j] + signs[k] * theta
      if (abs(A_alt[i,j]) < 1e-12) A_alt[i,j] <- 0
    }
    is_pivot <- function(i, j) (i == ie) && (j == je)
    
    alt_id <- alt_id + 1L
    f <- file.path(outdir, sprintf("47_%s_alt_%02d_i%d_j%d.html", method_label, alt_id, ie, je))
    html <- c("<html><head><meta charset='utf-8'>", css, "</head><body>",
              sprintf("<h3>Alternatif çözüm %d — giren hücre (%d,%d), θ=%g</h3>", alt_id, ie, je, theta),
              "<table class='tbl-sim'><thead><tr><th></th>")
    for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", H(cn[j])))
    html <- c(html, "</tr></thead><tbody>")
    for (i in seq_len(m)){
      html <- c(html, "<tr>", sprintf("<th>%s</th>", H(rn[i])))
      for (j in seq_len(n)){
        in_loop  <- any(loop[,1] == i & loop[,2] == j)
        cls <- character(0)
        if (A_alt[i,j] > 0) cls <- c(cls, "alloc")
        if (is_pivot(i,j))  cls <- c(cls, "pivot")
        if (in_loop) {
          if (any(minus[,1] == i & minus[,2] == j)) cls <- c(cls, "minus") else cls <- c(cls, "plus")
        }
        if (A_alt[i,j] > 0) {
          cell <- sprintf("<div class='bold'>%g</div><div class='sub'>(c=%g)</div>", A_alt[i,j], C[i,j])
        } else cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(c=%g)</div>", C[i,j])
        html <- c(html, sprintf("<td class='%s'>%s</td>", cls, cell))
      }
      html <- c(html, "</tr>")
    }
    html <- c(html, "</tbody></table>",
              sprintf("<p><b>Toplam maliyet:</b> %g (orijinalle aynı olmalı)</p>", sum(A_alt*C)),
              "</body></html>")
    writeLines(html, f)
    items <- c(items, sprintf("<li>R=0: (%d,%d) → <a href='%s'>alternatif çözüm</a></li>", ie, je, basename(f)))
  }
  
  idx <- c("<html><head><meta charset='utf-8'>", css, "</head><body>",
           sprintf("<h2>%s — Alternatif Optimumlar (R=0)</h2>", H(method_label)),
           "<ol>", items, "</ol>", "</body></html>")
  writeLines(idx, file.path(outdir, sprintf("47_%s_alternatives_index.html", method_label)))
  invisible(TRUE)
}

###############################################################################
## 4) MODI step packages — 50_* (u/v table, u/v+R table, LaTeX dumps)
###############################################################################
render_modi_step_packages <- function(tr, outdir, method_label,
                                      labels_for_steps = .transport_labels_default(),
                                      math_preview = FALSE){
  if (is.null(tr$steps) || !length(tr$steps)) return(invisible())
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  C <- tr$cost; m <- nrow(C); n <- ncol(C)
  lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
  rn <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  cn <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  css <- .transport_css_bigM(); H <- .html_escape
  
  idx <- c("<html><head><meta charset='utf-8'>", css, "</head><body>",
           sprintf("<h2>%s – MODI Paketleri</h2>", H(method_label)), "<ol>")
  snap <- 0L
  for (st in tr$steps){
    if (!identical(st$stage,"modi-uv")) next
    snap <- snap + 1L
    A <- st$alloc + 0; u <- st$info$u; v <- st$info$v; R <- st$info$reduced

    # NEW: ensure we always have a basis mask for this snapshot
    basis <- NULL
    if (!is.null(st$info) && !is.null(st$info$basis)) {
      basis <- st$info$basis
    }
    if (is.null(basis)) {
      # fallback: treat positive allocations as basic
      basis <- (A > 0) + 0
    }
    
    
    obj <- if (!is.null(tr$objective)) tr$objective else "min"
    vm <- .modi_violation_masks(R, objective = obj, tol = 1e-12)
    
    # (a) u/v only
    f_uv <- file.path(outdir, sprintf("50_%s_step_%03d_uv_table.html", method_label, snap))
    html <- c("<html><head><meta charset='utf-8'>", css, "</head><body>",
              "<table class='tbl-sim'><thead><tr><th></th>")
    for (j in 1:n) html <- c(html, sprintf("<th>%s<br><span class='uvchip'>v<sub>%d</sub>=%s</span></th>", H(cn[j]), j, ifelse(is.na(v[j]),"&ndash;",sprintf("%g",v[j]))))
    html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", H(lbl$supply_label)))
    for (i in 1:m){
      html <- c(html, "<tr>", sprintf("<th>%s<br><span class='uvchip'>u<sub>%d</sub>=%s</span></th>", H(rn[i]), i, ifelse(is.na(u[i]),"&ndash;",sprintf("%g",u[i]))))
      for (j in 1:n){
        # show allocation only (and degenerate basics), no R-based highlighting here
        is_degen <- (!is.null(basis) && basis[i,j] == 1 && abs(A[i,j]) <= 1e-12)
        td_cls   <- if (is_degen) "alloc degen" else if (A[i,j] > 0) "alloc" else ""
        
        if (A[i,j] > 0 || is_degen) {
          cell <- sprintf("<div class='big'>%g</div><div class='sub'>(c=%g)</div>", A[i,j], C[i,j])
        } else {
          cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(c=%g)</div>", C[i,j])
        }
        
        html <- c(html, sprintf("<td class='%s'>%s</td>", td_cls, cell))
      }
      
      html <- c(html, sprintf("<td class='rhs bold'>%g</td></tr>", sum(A[i,])))
    }
    html <- c(html, sprintf("<tr><th>%s</th>", H(lbl$demand_label)))
    for (j in 1:n) html <- c(html, sprintf("<td class='rhs bold'>%g</td>", sum(A[,j])))
    html <- c(html, sprintf("<td class='rhs bold'>%g</td></tr>", sum(A)),
              "</tbody></table></body></html>")
    writeLines(html, f_uv)
    
    # (b) u/v + R table with pivot highlight
    f_uvR <- file.path(outdir, sprintf("50_%s_step_%03d_uvR_table.html", method_label, snap))
    html <- c("<html><head><meta charset='utf-8'>", css, "</head><body>",
              "<table class='tbl-sim'><thead><tr><th></th>")
    for (j in 1:n) html <- c(html, sprintf("<th>%s</th>", H(cn[j])))
    html <- c(html, "</tr></thead><tbody>")
    for (i in 1:m){
      html <- c(html, "<tr>", sprintf("<th>%s<br><span class='sub'>u<sub>%d</sub>=%s</span></th>", H(rn[i]), i, ifelse(is.na(u[i]),"&ndash;",sprintf("%g",u[i]))))
      for (j in 1:n){
        r <- R[i,j]; cls <- character(0)
        is_degen <- (!is.null(basis) && basis[i,j]==1 && abs(A[i,j]) <= 1e-12)
        if (is_degen)     cls <- c(cls, "degen")
        if (A[i,j] > 0) cls <- c(cls, "alloc")
        if (vm$viol[i,j]) cls <- c(cls, "viol")
        if (vm$best[i,j]) cls <- c(cls, "pivot")       
        atop <- if (A[i,j] > 0 || is_degen) sprintf("<div class='big'>%g</div>", A[i,j]) else "<div class='muted'>&ndash;</div>"
        rline <- if (is.na(r)) "<div class='sub'>R=–</div>" else sprintf("<div class='sub'>R=%g</div>", r)
        html <- c(html, sprintf("<td class='%s'><div class='cellwrap'><div class='cbox'>c=%g</div>%s</div>%s</td>",
                                paste(cls, collapse=" "), C[i,j], atop, rline))
      }
      html <- c(html, "</tr>")
    }
    html <- c(html, "</tbody></table></body></html>")
    writeLines(html, f_uvR)
    
    # (c) raw LaTeX pages (derivations + all R_ij)
    f_uv_eqs <- file.path(outdir, sprintf("50_%s_step_%03d_uv_eqs.html", method_label, snap))
    transport_export_latex_uv_eqs_from_step(u=u, v=v, C=C, uv_log=st$info$uv_log,
                                            file=f_uv_eqs, title="u–v türetimleri (LaTeX – ham)", render_math=math_preview)
    
    f_rij <- file.path(outdir, sprintf("50_%s_step_%03d_rij_latex.html", method_label, snap))
    transport_export_latex_rij_from_step(cost=C, u=u, v=v, file=f_rij,
                                         title="R_{ij} hesap (LaTeX – ham)", render_math=math_preview)
    
    idx <- c(idx, sprintf("<li>Adım %d: <a href='%s'>u/v</a> · <a href='%s'>u/v+R</a> · LaTeX: <a href='%s'>u–v</a> · <a href='%s'>R_{ij}</a></li>",
                          st$k, basename(f_uv), basename(f_uvR), basename(f_uv_eqs), basename(f_rij)))
  }
  idx <- c(idx, "</ol></body></html>")
  writeLines(idx, file.path(outdir, sprintf("50_%s_modi_pkg_index.html", method_label)))
  invisible(TRUE)
}

# VAM step table with Δ(row) & Δ(col) bands (no overlap with x_ij cells)
transport_result_from_alloc_step_html_vam <- function(
    cost, alloc, supply_total, demand_total,
    rpen = NULL, cpen = NULL,
    objective = c("min","max"),
    title = "Ara Adım (VAM)",
    labels = .transport_labels_default(),
    include_css = TRUE,
    table_class = "tbl-sim"
){
  objective <- match.arg(objective)
  C <- as.matrix(cost); A <- as.matrix(alloc)
  m <- nrow(C); n <- ncol(C)
  row_used <- rowSums(A); col_used <- colSums(A)
  total_cost <- sum(A * C)
  H <- function(x){x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE); gsub(">","&gt;",x,fixed=TRUE)}
  
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  
  eps <- 1e-12
  exh_rows <- which(abs(row_used - supply_total) <= eps)
  exh_cols <- which(abs(col_used - demand_total) <= eps)
  
  # helpers to pick penalties from stored lists/vectors
  pick_named <- function(obj, key){
    if (is.null(obj)) return(NA_real_)
    if (is.list(obj)) {
      if (!is.null(names(obj)) && key %in% names(obj)) return(suppressWarnings(as.numeric(obj[[key]])))
      return(NA_real_)
    }
    if (is.atomic(obj)) {
      nm <- names(obj)
      if (!is.null(nm) && key %in% nm) return(suppressWarnings(as.numeric(obj[[key]])))
    }
    NA_real_
  }
  
  rpen_vec <- rep(NA_real_, m)
  if (!is.null(rpen)) for (i in seq_len(m)){
    key <- paste0("r", i); val <- pick_named(rpen, key)
    if (is.na(val) && length(rpen) >= i) { vv <- suppressWarnings(as.numeric(rpen[[i]])); if (is.finite(vv)) val <- vv }
    if (is.finite(val)) rpen_vec[i] <- val
  }
  cpen_vec <- rep(NA_real_, n)
  if (!is.null(cpen)) for (j in seq_len(n)){
    key <- paste0("c", j); val <- pick_named(cpen, key)
    if (is.na(val) && length(cpen) >= j) { vv <- suppressWarnings(as.numeric(cpen[[j]])); if (is.finite(vv)) val <- vv }
    if (is.finite(val)) cpen_vec[j] <- val
  }
  
  any_defined <- function(x) any(is.finite(x))
  active_rows <- setdiff(seq_len(m), exh_rows)
  active_cols <- setdiff(seq_len(n), exh_cols)
  
  # fallback: compute penalties if missing
  if (!any_defined(rpen_vec)) {
    for (i in active_rows) {
      cols <- active_cols
      if (length(cols) >= 2L) {
        vals <- sort(C[i, cols], decreasing = (objective=="max"))
        rpen_vec[i] <- abs(vals[1] - vals[2])
      }
    }
  }
  if (!any_defined(cpen_vec)) {
    for (j in active_cols) {
      rows <- active_rows
      if (length(rows) >= 2L) {
        vals <- sort(C[rows, j], decreasing = (objective=="max"))
        cpen_vec[j] <- abs(vals[1] - vals[2])
      }
    }
  }
  
  extra_css <- "
  <style>
    .tbl-sim .delta-hdr{background:#f8f9fa;font-weight:600}
    .tbl-sim .delta{background:#f6f7ff;font-weight:600}
    .tbl-sim .exh-col-h{background:#fdecea}
    .tbl-sim .exh-row-h{background:#fdecea}
  </style>"
  
  html <- c()
  if (include_css) html <- c(html, .transport_css_bigM(), extra_css)
  
  html <- c(html, sprintf("<table class=\"%s\">", H(table_class)))
  html <- c(html, sprintf("<caption>%s (%s = <b>%g</b>)</caption>",
                          H(title), H(lbl$total_cost), total_cost))
  
  # HEADER — move Arz before Δ(satır)
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)) {
    cls <- if (j %in% exh_cols) " class='exh-col-h'" else ""
    html <- c(html, sprintf("<th%s>%s</th>", cls, H(col_names[j])))
  }
  html <- c(html, sprintf("<th class='rhs'>%s</th>", H(lbl$supply_label)))  # Arz first
  html <- c(html, "<th class='delta-hdr'>Δ (satır)</th></tr></thead><tbody>")
  
  # ROWS
  for (i in seq_len(m)){
    row_cls <- if (i %in% exh_rows) " class='exh-row'" else ""
    lh_cls  <- if (i %in% exh_rows) " class='exh-row-h'" else ""
    html <- c(html, sprintf("<tr%s><th%s>%s</th>", row_cls, lh_cls, H(row_names[i])))
    for (j in seq_len(n)){
      td_cls <- if (j %in% exh_cols) "exh-col" else ""
      if (A[i,j] > 0){
        cell <- sprintf("<div class='bold'>%g</div><div class='sub'>(c=%g)</div>", A[i,j], C[i,j])
        html <- c(html, sprintf("<td class='alloc %s'>%s</td>", td_cls, cell))
      } else {
        cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(c=%g)</div>", C[i,j])
        html <- c(html, sprintf("<td class='%s'>%s</td>", td_cls, cell))
      }
    }
    # Arz (rhs) then Δ(satır)
    html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                            row_used[i], supply_total[i]))
    drow <- if (is.finite(rpen_vec[i])) sprintf("%g", rpen_vec[i]) else "<span class='na'>&ndash;</span>"
    html <- c(html, sprintf("<td class='delta'>%s</td></tr>", drow))
  }
  
  # DEMAND TOTALS row – keep order: columns, Arz, Δ-blank
  html <- c(html, sprintf("<tr><th>%s</th>", H(lbl$demand_label)))
  for (j in seq_len(n)) {
    cls <- if (j %in% exh_cols) " class='rhs bold exh-col'" else " class='rhs bold'"
    html <- c(html, sprintf("<td%s>%g <span class='sub'>(%g)</span></td>",
                            cls, col_used[j], demand_total[j]))
  }
  html <- c(html,
            sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                    sum(A), sum(supply_total)))
  html <- c(html, "<td class='delta'>&nbsp;</td></tr>")
  
  # Δ(sütun) row – last cell stays blank; Arz-blank placed before Δ
  html <- c(html, "<tr><th class='delta-hdr'>Δ (sütun)</th>")
  for (j in seq_len(n)) {
    dcol <- if (is.finite(cpen_vec[j])) sprintf("%g", cpen_vec[j]) else "<span class='na'>&ndash;</span>"
    html <- c(html, sprintf("<td class='delta'>%s</td>", dcol))
  }
  html <- c(html, "<td class='rhs'>&nbsp;</td><td class='delta'>&nbsp;</td></tr>")
  
  html <- c(html, "</tbody></table>")
  paste(html, collapse = "\n")
}

# --- Birim Kâr + x_ij + Arz/Talep kenarları (HTML) --------------------------
transport_initial_costs_xvars_with_margins_html <- function(
    cost, supply, demand,
    title = "Birim Kâr Tablosu (Sadece Kâr + xᵢⱼ)",
    labels = .transport_labels_default(),
    include_css = TRUE,
    table_class = "tbl-sim",
    digits = NULL,
    file = NULL
){
  C <- as.matrix(cost); m <- nrow(C); n <- ncol(C)
  if (length(supply) != m) stop("length(supply) nrow(cost) ile eşit olmalı.")
  if (length(demand) != n) stop("length(demand) ncol(cost) ile eşit olmalı.")
  fmt <- if (is.null(digits)) function(x) sprintf("%g", x)
  else function(x) formatC(x, format = "f", digits = digits)
  
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  rn  <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  cn  <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  H <- function(x){x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE); gsub(">","&gt;",x,fixed=TRUE)}
  
  extra_css <- "
  <style>
    .tbl-sim .cellwrap{position:relative;min-width:72px;height:48px}
    .tbl-sim .cbox{position:absolute;top:6px;right:6px;
                   border:1px solid #adb5bd;border-radius:6px;
                   padding:1px 6px;font-size:12px;color:#495057}
    .tbl-sim .xvar{position:absolute;left:8px;bottom:6px;font-weight:600}
    .tbl-sim .rhs{background:#e7f1fb}
  </style>"
  
  html <- c()
  if (include_css) html <- c(html, .transport_css_bigM(), extra_css)
  
  html <- c(html, sprintf("<table class=\"%s\">", H(table_class)))
  html <- c(html, sprintf("<caption>%s</caption>", H(title)))
  
  # başlık satırı
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", H(cn[j])))
  html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", H(lbl$supply_label)))
  
  # hücreler + satır sağında arz
  for (i in seq_len(m)) {
    html <- c(html, "<tr>", sprintf("<th>%s</th>", H(rn[i])))
    for (j in seq_len(n)) {
      cell <- sprintf(
        "<div class='cellwrap'>
           <div class='cbox'>%s</div>
           <div class='xvar'>x<sub>%d%d</sub></div>
         </div>", fmt(C[i,j]), i, j)
      html <- c(html, sprintf("<td>%s</td>", cell))
    }
    html <- c(html, sprintf("<td class='rhs bold'>%s</td>", fmt(supply[i])), "</tr>")
  }
  
  # alt satır: talep + toplam
  html <- c(html, sprintf("<tr><th>%s</th>", H(lbl$demand_label)))
  for (j in seq_len(n)) html <- c(html, sprintf("<td class='rhs bold'>%s</td>", fmt(demand[j])))
  html <- c(html, sprintf("<td class='rhs bold'>%s</td></tr>", fmt(sum(demand))), "</tbody></table>")
  
  out <- paste(html, collapse = "\n")
  if (!is.null(file)) { dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE); writeLines(out, file) }
  return(out)
}

# ----- ADD this helper somewhere with your exporters -----
export_initial_solution_latex <- function(tr, outdir, method_label,
                                          render_math = FALSE,
                                          labels = .transport_labels_default()){
  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  
  # find the last 'initial' snapshot (after the initial method finishes)
  A0 <- NULL
  if (!is.null(tr$steps) && length(tr$steps)){
    is_init <- vapply(tr$steps, function(s) identical(s$stage, "initial"), logical(1))
    if (any(is_init)) {
      last_i <- max(which(is_init))
      A0 <- tr$steps[[last_i]]$alloc + 0
    }
  }
  # fallback: if no trace/initial steps, use the returned allocation (only if no optimizer ran)
  if (is.null(A0)) A0 <- tr$allocation + 0
  
  C  <- tr$cost
  obj_word <- if (identical(tr$objective, "max")) "Toplam Kâr" else "Toplam Maliyet"
  ttl <- sprintf("LaTeX — Başlangıç Çözümü (%s, %s)", 
                 method_title(method_label, tr$objective), obj_word)
  
  file <- file.path(outdir, sprintf("20_%s_initial_latex.html", method_label))
  transport_export_latex_equation_html(cost = C, alloc = A0, file = file,
                                       title = ttl, render_math = render_math)
  invisible(file)
}
transport_result_from_alloc_html <- function(cost, alloc, 
                                             title = "Dağıtım", 
                                             labels = .transport_labels_default(), 
                                             include_css = TRUE, 
                                             table_class = "tbl-sim", basis = NULL) { 
  C <- as.matrix(cost); m <- nrow(C); n <- ncol(C) 
  supply <- rowSums(alloc); 
  demand <- colSums(alloc) 
  total_cost <- sum(alloc * C) 
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE) 
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m) 
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n) 
  html <- c(); if (include_css) 
  html <- c(html, .transport_css_bigM()) 
  html <- c(html, sprintf("<table class=\"%s\">", .html_escape(table_class))) 
  html <- c(html, sprintf("<caption>%s (%s = <b>%g</b>)</caption>", 
                          .html_escape(title), .html_escape(lbl$total_cost), total_cost)) 
  html <- c(html, "<thead><tr><th></th>") 
  for (j in seq_len(n)) 
    html <- c(html, sprintf("<th>%s</th>", .html_escape(col_names[j]))) 
  html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", 
                          .html_escape(lbl$supply_label))) 
  for (i in seq_len(m)){ html <- c(html, "<tr>", 
                                   sprintf("<th>%s</th>", .html_escape(row_names[i]))) 
  for (j in seq_len(n)){ is_degen <- (!is.null(basis) && basis[i,j]==1 && alloc[i,j]==0) 
  td_cls <- if (is_degen) "alloc degen" else if (alloc[i,j] > 0) "alloc" else "" 
  if (alloc[i,j] > 0 || is_degen){ cell <- sprintf("<div class='bold'>%g</div><div class='sub'>(c=%g)</div>", alloc[i,j], C[i,j]) 
  html <- c(html, sprintf("<td class='%s'>%s</td>", td_cls, cell)) } else { cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(c=%g)</div>", C[i,j]) 
  html <- c(html, sprintf("<td>%s</td>", cell)) } } 
  html <- c(html, sprintf("<td class='rhs bold'>%g</td>", supply[i]), "</tr>") } 
  html <- c(html, sprintf("<tr><th>%s</th>", .html_escape(lbl$demand_label))) 
  for (j in seq_len(n)) html <- c(html, sprintf("<td class='rhs bold'>%g</td>", demand[j])) 
  html <- c(html, sprintf("<td class='rhs bold'>%g</td></tr></tbody></table>", sum(supply))) 
  paste(html, collapse="\n") }
