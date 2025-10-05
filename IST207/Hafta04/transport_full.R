
# =========================================================
# Transport Solver & Renderers (full, single file)
# - Balanced/unbalanced handling (dummy row/col)
# - Initial methods: northwest, rowmin, colmin, leastcost, vam
# - Optimizers: none, modi (u-v), stepping, both
# - Trace support with per-step snapshots
# - Degenerate basics (0-valued basic cells) highlighted in 50_* packages
# - Objective: "min" (default) or "max" (profit maximization)
# - HTML generators in a unified visual style
# - Utilities to export RAW LaTeX for equations
# - Example at bottom that writes all outputs (including final MODI LaTeX)
# =========================================================

.eps <- 1e-9

# ---------- small helpers ----------
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
.degen{background: #f0f4ff; outline: 3px dashed #1a73e8}  /* degenerate basic cell (x=0) */
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
  
  # Objective transform: for maximization (profits), negate to minimize
  is_max <- (objective == "max")
  C0 <- if (is_max) -C0_input else C0_input
  
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
      return(invisible(0))
    }
    alloc[i,j] <<- alloc[i,j] + x
    s[i] <<- s[i] - x
    d[j] <<- d[j] - x
    if (s[i] <= .eps) { s[i] <<- 0; active_rows[i] <<- FALSE }
    if (d[j] <= .eps) { d[j] <<- 0; active_cols[j] <<- FALSE }
    push(stage, i, j, x, note, extras)
    invisible(x)
  }
  
  choose_least_cost_cell <- function(){
    rs <- which(active_rows); cs <- which(active_cols)
    if (!length(rs) || !length(cs)) return(NULL)
    cand <- as.matrix(expand.grid(i = rs, j = cs))
    if (nrow(cand) == 0) return(NULL)
    feasible <- cand[ pmin(s[cand[,1]], d[cand[,2]]) > .eps, , drop = FALSE]
    if (nrow(feasible) == 0) return(NULL)
    k <- which.min(C[ cbind(feasible[,1], feasible[,2]) ])
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
        j <- cols[ which.min(C[r, cols]) ]
        moved <- do_alloc(r, j, stage="initial", note="Row-min")
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
        i <- rows[ which.min(C[rows, cidx]) ]
        moved <- do_alloc(i, cidx, stage="initial", note="Col-min")
        if (moved <= .eps || !active_cols[cidx]) break
      }
      ac <- which(active_cols); if (!length(ac)) break; cidx <- ac[1]
    }
  } else if (method == "leastcost"){
    repeat {
      if (!(any(active_rows) && any(active_cols))) break
      ij <- choose_least_cost_cell(); if (is.null(ij)) break
      moved <- do_alloc(ij[1], ij[2], stage="initial", note="Least-cost")
      if (moved <= .eps) break
    }
  } else if (method == "vam"){
    pen_row <- function(i){
      cols <- which(active_cols); cols <- cols[d[cols] > .eps]
      if (length(cols) >= 2L) {
        v <- sort(C[i, cols], partial = 2L)[1:2]; v[2] - v[1]
      } else NA_real_
    }
    pen_col <- function(j){
      rows <- which(active_rows); rows <- rows[s[rows] > .eps]
      if (length(rows) >= 2L) {
        v <- sort(C[rows, j], partial = 2L)[1:2]; v[2] - v[1]
      } else NA_real_
    }
    pick_min_over <- function(rset, cset){
      if (!length(rset) || !length(cset)) return(NULL)
      cand <- as.matrix(expand.grid(i = rset, j = cset))
      feas <- cand[pmin(s[cand[,1]], d[cand[,2]]) > .eps, , drop = FALSE]
      if (!nrow(feas)) return(NULL)
      k <- which.min(C[cbind(feas[,1], feas[,2])]); c(feas[k,1], feas[k,2])
    }
    pick_global_min <- function(){ pick_min_over(which(active_rows), which(active_cols)) }
    tol <- 1e-12
    while (any(active_rows) && any(active_cols)){
      rset <- which(active_rows); cset <- which(active_cols)
      if (length(rset) == 1L || length(cset) == 1L){
        ij <- pick_global_min(); if (is.null(ij)) break
        do_alloc(ij[1], ij[2], stage="initial", note="VAM (tek satır/sütun) → global min")
        next
      }
      rpen <- sapply(rset, pen_row); cpen <- sapply(cset, pen_col)
      mr <- suppressWarnings(max(rpen, na.rm = TRUE))
      mc <- suppressWarnings(max(cpen, na.rm = TRUE))
      extras <- list(rpen = setNames(as.numeric(rpen), paste0("r", rset)),
                     cpen = setNames(as.numeric(cpen), paste0("c", cset)))
      if (is.finite(mr) && mr > mc + tol){
        tied_rows <- rset[ which(abs(rpen - mr) <= tol) ]
        ij <- pick_min_over(tied_rows, cset); if (is.null(ij)) ij <- pick_global_min()
        do_alloc(ij[1], ij[2], stage="initial", note=sprintf("VAM row Δ=%g → tied rows global min", mr), extras=extras)
      } else if (is.finite(mc) && mc > mr + tol){
        tied_cols <- cset[ which(abs(cpen - mc) <= tol) ]
        ij <- pick_min_over(rset, tied_cols); if (is.null(ij)) ij <- pick_global_min()
        do_alloc(ij[1], ij[2], stage="initial", note=sprintf("VAM col Δ=%g → tied cols global min", mc), extras=extras)
      } else {
        ij <- pick_global_min(); if (is.null(ij)) break
        do_alloc(ij[1], ij[2], stage="initial", note="VAM tie (row vs col) → global min", extras=extras)
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
  
  # ---------- MODI ----------
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
    repeat {
      iter <- iter + 1; if (iter > max_iter){ warning("MODI max_iter aşıldı; durduruldu."); break }
      uv <- compute_uv(); R  <- reduced_costs(uv$u, uv$v)
      minR <- suppressWarnings(min(R, na.rm = TRUE))
      push("modi-uv", shipped=0,
           note=sprintf("u–v hesap; min(R)=%g", ifelse(is.finite(minR), minR, NA_real_)),
           extras=list(u=uv$u, v=uv$v, uv_log=uv$log, reduced=R, basis=(is_basic+0)))
      if (!is.finite(minR) || minR >= -1e-9) break
      cand <- which(R <= (minR + 1e-12), arr.ind = TRUE)
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
        push("modi", best[1], best[2], res$theta,
             note = sprintf("MODI enter (%d,%d), red.cost=%g", best[1], best[2], minR),
             extras = list(u=uv$u, v=uv$v, reduced=R, basis=(is_basic+0)))
        next
      }
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
  
  # report total "cost" as profit if max
  total_val <- sum(alloc_full * cost_full)
  if (is_max) total_val <- -total_val
  
  list(
    allocation = alloc_out,
    total_cost = total_val,                 # if objective=max, this is total PROFIT
    method     = method,
    optimized  = optimize != "none",
    optimizer  = optimize,
    cost       = if (is_max) C0_input else cost_out,  # return original signs for display
    supply     = supply_out,
    demand     = demand_out,
    augmented  = augmented,
    objective  = objective,
    steps      = if (trace) steps else NULL
  )
}

# ---------- labels and titles ----------
method_title <- function(m){
  switch(tolower(m),
         "northwest" = "Kuzeybatı Köşe (NW)",
         "rowmin"    = "Satır-Min (Row Min)",
         "colmin"    = "Sütun-Min (Col Min)",
         "leastcost" = "En Düşük Maliyet (Least Cost)",
         "vam"       = "Vogel Yaklaşımı (VAM)",
         toupper(m)
  )
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

# ---------- Result HTML (allocation) ----------
transport_result_from_alloc_html <- function(cost, alloc, 
                                             title = "Dağıtım", 
                                             labels = .transport_labels_default(),
                                             include_css = TRUE,
                                             table_class = "tbl-sim",
                                             basis = NULL) {   # optional basic pattern to mark degenerates
  C <- as.matrix(cost); m <- nrow(C); n <- ncol(C)
  supply <- rowSums(alloc); demand <- colSums(alloc)
  total_cost <- sum(alloc * C)
  
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  
  html <- c(); if (include_css) html <- c(html, .transport_css_bigM())
  html <- c(html, sprintf("<table class=\"%s\">", .html_escape(table_class)))
  html <- c(html, sprintf("<caption>%s (%s = <b>%g</b>)</caption>",
                          .html_escape(title), .html_escape(lbl$total_cost), total_cost))
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", .html_escape(col_names[j])))
  html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", .html_escape(lbl$supply_label)))
  for (i in seq_len(m)){
    html <- c(html, "<tr>", sprintf("<th>%s</th>", .html_escape(row_names[i])))
    for (j in seq_len(n)){
      is_degen <- (!is.null(basis) && basis[i,j]==1 && alloc[i,j]==0)
      td_cls <- if (is_degen) "alloc degen" else if (alloc[i,j] > 0) "alloc" else ""
      if (alloc[i,j] > 0 || is_degen){
        cell <- sprintf("<div class='bold'>%g</div><div class='sub'>(m=%g)</div>", alloc[i,j], C[i,j])
        html <- c(html, sprintf("<td class='%s'>%s</td>", td_cls, cell))
      } else {
        cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(m=%g)</div>", C[i,j])
        html <- c(html, sprintf("<td>%s</td>", cell))
      }
    }
    html <- c(html, sprintf("<td class='rhs bold'>%g</td>", supply[i]), "</tr>")
  }
  html <- c(html, sprintf("<tr><th>%s</th>", .html_escape(lbl$demand_label)))
  for (j in seq_len(n)) html <- c(html, sprintf("<td class='rhs bold'>%g</td>", demand[j]))
  html <- c(html, sprintf("<td class='rhs bold'>%g</td></tr></tbody></table>", sum(supply)))
  paste(html, collapse="\n")
}

# ---------- Export RAW LaTeX of total cost (for an allocation) ----------
transport_export_latex_equation_html <- function(cost, alloc, file,
                                                 title = "LaTeX – Toplam Maliyet Eşitliği",
                                                 render_math = FALSE,
                                                 mathjax_src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"){
  C <- as.matrix(cost)
  A <- as.matrix(alloc)
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

# ---------- VAM/step traces & MODI packages (degenerate marking included) ----------

# Per-step MODI package writer (u/v + R + LaTeX)
render_modi_step_packages <- function(tr, outdir, method_label,
                                      labels_for_steps = .transport_labels_default(),
                                      math_preview = FALSE){
  if (is.null(tr$steps) || !length(tr$steps)) return(invisible())
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  base_css <- paste0(
    .transport_css_bigM(),
    "<style>
      .hdr{font-weight:600;margin:6px 0}
      .note{color:#6c757d;font-size:12px}
      .tbl-sim .uv { color:#495057; font-size:12px }
      .tbl-sim .neg { background:#fdecea; border:2px solid #f5c2c7 }
      .tbl-sim .rbox{font-size:12px;margin-top:2px}
      .tbl-sim .costpill{position:absolute;top:6px;right:6px;border:1px solid #adb5bd;border-radius:6px;padding:1px 6px;font-size:12px;color:#495057}
      .cellwrap{position:relative;min-width:84px;height:46px}
      .alloc .big{font-weight:700}
      .muted{color:#adb5bd}
      .pivot{ background:#e8f0fe !important; box-shadow: inset 0 0 0 3px #1a73e8; }
      .degen{ background:#f0f4ff !important; outline:3px dashed #1a73e8; }
    </style>"
  )
  
  # UV-only table
  write_uv_table <- function(step_id, st){
    C <- tr$cost; A <- st$alloc + 0
    u <- st$info$u; v <- st$info$v
    basis <- st$info$basis
    m <- nrow(C); n <- ncol(C)
    lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
    rn <- if (is.null(lbl$row_names)) .mk_names(lbl$row_prefix,m) else lbl$row_names
    cn <- if (is.null(lbl$col_names)) .mk_names(lbl$col_prefix,n) else lbl$col_names
    
    html <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>")
    html <- c(html, sprintf("<div class='hdr'>%s</div>", ""))
    html <- c(html, sprintf("<table class='tbl-sim'><thead><tr><th></th>"))
    for (j in 1:n) html <- c(html, sprintf("<th>%s<br><span class='uv'>v<sub>%d</sub> = %s</span></th>", .html_escape(cn[j]), j, ifelse(is.na(v[j]),"&ndash;",sprintf("%g",v[j]))))
    html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", .html_escape(lbl$supply_label)))
    
    for (i in 1:m){
      html <- c(html, "<tr>")
      html <- c(html, sprintf("<th>%s<br><span class='uv'>u<sub>%d</sub> = %s</span></th>", .html_escape(rn[i]), i, ifelse(is.na(u[i]),"&ndash;",sprintf("%g",u[i]))))
      for (j in 1:n){
        is_degen <- (!is.null(basis) && basis[i,j]==1 && A[i,j]==0)
        td_cls <- if (is_degen) "alloc degen" else if (A[i,j] > 0) "alloc" else ""
        if (A[i,j] > 0 || is_degen){
          cell <- sprintf("<div class='big'>%g</div><div class='sub'>(m=%g)</div>", A[i,j], C[i,j])
          html <- c(html, sprintf("<td class='%s'>%s</td>", td_cls, cell))
        } else {
          cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(m=%g)</div>", C[i,j])
          html <- c(html, sprintf("<td>%s</td>", cell))
        }
      }
      html <- c(html, sprintf("<td class='rhs bold'>%g</td>", sum(A[i,])), "</tr>")
    }
    html <- c(html, "<tr><th>", .html_escape(lbl$demand_label), "</th>")
    for (j in 1:n) html <- c(html, sprintf("<td class='rhs bold'>%g</td>", sum(A[,j])))
    html <- c(html, sprintf("<td class='rhs bold'>%g</td></tr></tbody></table>", sum(A)))
    html <- c(html, "</body></html>")
    file <- sprintf("50_%s_step_%03d_uv_table.html", method_label, step_id)
    writeLines(html, file.path(outdir, file))
    file
  }
  
  # UV+R table with pivot & degenerate marks
  write_uvR_table <- function(step_id, st){
    C <- tr$cost; A <- st$alloc + 0
    u <- st$info$u; v <- st$info$v; R <- st$info$reduced; basis <- st$info$basis
    m <- nrow(C); n <- ncol(C)
    lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
    rn <- if (is.null(lbl$row_names)) .mk_names(lbl$row_prefix,m) else lbl$row_names
    cn <- if (is.null(lbl$col_names)) .mk_names(lbl$col_prefix,n) else lbl$col_names
    
    minR <- suppressWarnings(min(R, na.rm = TRUE))
    is_pivot <- matrix(FALSE, m, n)
    if (is.finite(minR) && minR < 0){
      where <- which(!is.na(R) & abs(R - minR) <= 1e-12, arr.ind = TRUE)
      if (nrow(where)) is_pivot[cbind(where[,1], where[,2])] <- TRUE
    }
    
    html <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>")
    html <- c(html, "<table class='tbl-sim'><thead><tr><th></th>")
    for (j in 1:n){
      vtxt <- if (is.na(v[j])) "<span class='uv'>v<sub>j</sub> = –</span>"
      else sprintf("<span class='uv'>v<sub>%d</sub> = %g</span>", j, v[j])
      html <- c(html, sprintf("<th>%s<br>%s</th>", .html_escape(cn[j]), vtxt))
    }
    html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", .html_escape(lbl$supply_label)))
    
    for (i in 1:m){
      utxt <- if (is.na(u[i])) "<span class='uv'>u<sub>i</sub> = –</span>"
      else sprintf("<span class='uv'>u<sub>%d</sub> = %g</span>", i, u[i])
      html <- c(html, "<tr>", sprintf("<th>%s<br>%s</th>", .html_escape(rn[i]), utxt))
      for (j in 1:n){
        r <- R[i,j]
        cls <- character(0)
        if (!is.null(basis) && basis[i,j]==1 && A[i,j]==0) cls <- c(cls, "degen")
        if (A[i,j] > 0) cls <- c(cls, "alloc")
        if (!is.na(r) && r < 0) cls <- c(cls, "neg")
        if (is_pivot[i,j])       cls <- c(cls, "pivot")
        ctag <- sprintf("<div class='costpill'>c=%g</div>", C[i,j])
        atop <- if (A[i,j] > 0) sprintf("<div class='big'>%g</div>", A[i,j])
        else "<div class='muted'>&ndash;</div>"
        rline <- if (is.na(r)) "<div class='rbox'><span class='muted'>R=–</span></div>"
        else sprintf("<div class='rbox'>R=%g</div>", r)
        html <- c(html, sprintf("<td class='%s'><div class='cellwrap'>%s%s</div>%s</td>",
                                paste(cls, collapse=" "), ctag, atop, rline))
      }
      html <- c(html, sprintf("<td class='rhs bold'>%g</td></tr>", sum(A[i,])))
    }
    html <- c(html, sprintf("<tr><th>%s</th>", .html_escape(lbl$demand_label)))
    for (j in 1:n) html <- c(html, sprintf("<td class='rhs bold'>%g</td>", sum(A[,j])))
    html <- c(html, sprintf("<td class='rhs bold'>%g</td></tr>", sum(A)), "</tbody></table>")
    
    # also write LaTeX files (R_ij and u/v derivations) for this step
    latex_file <- sprintf("50_%s_step_%03d_rij_latex.html", method_label, step_id)
    transport_export_latex_rij_from_step <- function(cost, u, v, file,
                                                     title = "LaTeX – R_{ij} Hesapları",
                                                     only_defined = TRUE,
                                                     render_math = FALSE,
                                                     mathjax_src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"){
      C <- as.matrix(cost); m <- nrow(C); n <- ncol(C)
      R <- matrix(NA_real_, m, n)
      for (i in seq_len(m)) for (j in seq_len(n)) { if (!is.na(u[i]) && !is.na(v[j])) R[i,j] <- C[i,j] - u[i] - v[j] }
      lines <- character(0)
      for (i in seq_len(m)) for (j in seq_len(n)) {
        if (is.na(R[i,j]) && only_defined) next
        lhs <- sprintf("R_{%d,%d}", i, j)
        if (is.na(R[i,j])) {
          lines <- c(lines, sprintf("%s = c_{%d,%d} - u_{%d} - v_{%d} \\quad (\\text{tanımsız})", lhs, i, j, i, j))
        } else {
          lines <- c(lines, sprintf("%s = c_{%d,%d} - u_{%d} - v_{%d} = %s - %s - %s = %s",
                                    lhs, i, j, i, j, .fnum(C[i,j]), .fnum(u[i]), .fnum(v[j]), .fnum(R[i,j])))
        }
      }
      if (!length(lines)){ lines <- c("% Bu adımda hesaplanabilir R_{ij} yok (u veya v eksik).") }
      latex_block <- sprintf("\\[\\begin{aligned}\n%s\n\\end{aligned}\\]",
                             paste(paste0(lines, " \\\\"), collapse="\n"))
      css <- "<style>body{font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:20px}.raw{white-space:pre-wrap;background:#f8f9fa;border:1px solid #e9ecef;padding:12px;border-radius:6px;font-family:ui-monospace,Consolas,Monaco,monospace}</style>"
      html <- c("<!doctype html><html><head><meta charset='utf-8'><title>LaTeX R_ij</title>", css)
      if (render_math){ html <- c(html, sprintf("<script id='MathJax-script' async src='%s'></script>", .html_escape(mathjax_src))) }
      html <- c(html, "</head><body>",
                sprintf("<h3>%s</h3>", .html_escape(title)),
                sprintf("<div class='raw'>%s</div>", .html_escape(latex_block)))
      if (render_math){ html <- c(html, "<h4>Önizleme (MathJax)</h4>", latex_block) }
      html <- c(html, "</body></html>")
      dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
      writeLines(html, file); invisible(latex_block)
    }
    transport_export_latex_rij_from_step(C, u, v, file.path(outdir, latex_file))
    
    file <- sprintf("50_%s_step_%03d_uvR_table.html", method_label, step_id)
    writeLines(html, file.path(outdir, file))
    file
  }
  
  idx_path <- file.path(outdir, sprintf("50_%s_modi_pkg_index.html", method_label))
  idx <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>",
           sprintf("<h2>%s – MODI Paketleri</h2>", method_title(tr$method)),
           "<ol>")
  snap_id <- 0
  for (st in tr$steps){
    if (!(st$stage %in% c("modi-uv","modi"))) next
    if (st$stage != "modi-uv") next
    snap_id <- snap_id + 1
    f1 <- write_uv_table(snap_id, st)
    f2 <- write_uvR_table(snap_id, st)
    idx <- c(idx, sprintf("<li>Adım %d: <a href='%s'>u/v tablo</a> · <a href='%s'>u/v + R<sub>ij</sub> (+ dejenere)</a></li>", st$k, basename(f1), basename(f2)))
  }
  idx <- c(idx, "</ol></body></html>")
  writeLines(idx, idx_path)
  invisible(TRUE)
}

# ============================================================
# Cost-only initial table (optional helper)
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


# ---------- Başlangıç (input) tablosu ----------
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
  
  # extra CSS for cost box + x_ij label
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
  
  # header
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", .html_escape(col_names[j])))
  html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", .html_escape(lbl$supply_label)))
  
  # body
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
  
  # demand row
  html <- c(html, sprintf("<tr><th>%s</th>", .html_escape(lbl$demand_label)))
  for (j in seq_len(n)) html <- c(html, sprintf("<td class='rhs bold'>%g</td>", demand[j]))
  html <- c(html, sprintf("<td class='rhs bold'>%g</td>", sum(demand)),
            "</tr></tbody></table>")
  paste(html, collapse = "\n")
}