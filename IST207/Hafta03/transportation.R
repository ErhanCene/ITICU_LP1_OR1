.eps <- 1e-9


# =========================================================
# Transportation Solver (Balanced/Unbalanced)
# Methods  : "northwest" | "rowmin" | "colmin" | "leastcost" | "vam"
# Optimize : "none" | "modi" | "stepping" | "both"
# Trace    : TRUE → tüm hamle/pivotlar steps listesinde
# Unbalanced: otomatik dummy satır/sütun (+ maliyetleri ayarlanabilir)
# =========================================================
solve_transport <- function(cost, supply, demand,
                            method   = c("northwest","rowmin","colmin","leastcost","vam"),
                            optimize = c("none","modi","stepping","both"),
                            trace = FALSE,
                            allow_unbalanced = TRUE,
                            dummy_row_cost = 0,      # talep > arz → dummy ROW maliyeti (skalar veya uzunluk = ncol)
                            dummy_col_cost = 0,      # arz > talep → dummy COL maliyeti (skalar veya uzunluk = nrow)
                            drop_dummy_in_output = TRUE) {
  
  
  # cost   = C
  # supply = sup
  # demand = dem
  # method   = "northwest"
  # optimize = "modi"
  # trace    = TRUE
  # allow_unbalanced = TRUE
  # dummy_row_cost = 0      # talep > arz → dummy ROW maliyeti (skalar veya uzunluk = ncol)
  # dummy_col_cost = 0      # arz > talep → dummy COL maliyeti (skalar veya uzunluk = nrow)
  # drop_dummy_in_output = TRUE
  
  method   <- match.arg(method)
  optimize <- match.arg(optimize)
  
  C0 <- as.matrix(cost)
  s_in <- as.numeric(supply)
  d_in <- as.numeric(demand)
  m0 <- nrow(C0); n0 <- ncol(C0)
  
  if (length(s_in) != m0) stop("length(supply) must equal nrow(cost)")
  if (length(d_in) != n0) stop("length(demand) must equal ncol(cost)")
  
  # ---------- Unbalanced işlemi ----------
  sumS <- sum(s_in); sumD <- sum(d_in)
  augmented <- list(added = NULL, dummy_index = NA_integer_, original_m = m0, original_n = n0)
  
  C <- C0; s0 <- s_in; d0 <- d_in
  if (abs(sumS - sumD) > 1e-9) {
    if (!allow_unbalanced) stop("Problem unbalanced ve allow_unbalanced=FALSE.")
    if (sumS > sumD) {
      # DUMMY SÜTUN (fazla arzı emmek için)
      add <- sumS - sumD
      if (!(length(dummy_col_cost) %in% c(1, m0))) stop("dummy_col_cost skalar ya da uzunluk = nrow(cost) olmalı.")
      dummy_col <- rep(dummy_col_cost, length.out = m0)
      C <- cbind(C, dummy_col)
      d0 <- c(d0, add)
      augmented$added <- "col"; augmented$dummy_index <- ncol(C)
    } else {
      # DUMMY SATIR (talep fazlasını karşılamak için)
      add <- sumD - sumS
      if (!(length(dummy_row_cost) %in% c(1, n0))) stop("dummy_row_cost skalar ya da uzunluk = ncol(cost) olmalı.")
      dummy_row <- rep(dummy_row_cost, length.out = n0)
      C <- rbind(C, dummy_row)
      s0 <- c(s0, add)
      augmented$added <- "row"; augmented$dummy_index <- nrow(C)
    }
  }
  
  # ---------- Çalışma kopyaları ----------
  m <- nrow(C); n <- ncol(C)
  s <- s0 + 0; d <- d0 + 0
  alloc <- matrix(0, m, n)
  active_rows <- rep(TRUE, m)
  active_cols <- rep(TRUE, n)
  
  # ---------- Trace yardımcıları ----------
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
  
  # ---------- Yardımcılar ----------
  do_alloc <- function(i, j, stage, note=NULL, extras=list()){
    x <- min(s[i], d[j])
    if (x < .eps) {
      # clamp & inactivate if needed, but DO NOT push a step
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
  
  
  # ---------- Başlangıç yöntemleri ----------
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
        cols <- which(active_cols)
        cols <- cols[ d[cols] > .eps ]
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
        rows <- which(active_rows)
        rows <- rows[ s[rows] > .eps ]
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
      ij <- choose_least_cost_cell()
      if (is.null(ij)) break
      moved <- do_alloc(ij[1], ij[2], stage="initial", note="Least-cost")
      if (moved <= .eps) break
    }
  } else if (method == "vam"){
    pen_row <- function(i){
      cols <- which(active_cols)
      cols <- cols[d[cols] > .eps]                # feasible columns only
      if (length(cols) >= 2L) {
        v <- sort(C[i, cols], partial = 2L)[1:2]
        v[2] - v[1]
      } else NA_real_
    }
    pen_col <- function(j){
      rows <- which(active_rows)
      rows <- rows[s[rows] > .eps]                # feasible rows only
      if (length(rows) >= 2L) {
        v <- sort(C[rows, j], partial = 2L)[1:2]
        v[2] - v[1]
      } else NA_real_
    }
    
    # global cheapest among a given rows x cols submatrix, filtered to feasible cells
    pick_min_over <- function(rset, cset){
      if (!length(rset) || !length(cset)) return(NULL)
      cand <- as.matrix(expand.grid(i = rset, j = cset))
      feas <- cand[pmin(s[cand[,1]], d[cand[,2]]) > .eps, , drop = FALSE]
      if (!nrow(feas)) return(NULL)
      k <- which.min(C[cbind(feas[,1], feas[,2])])
      c(feas[k,1], feas[k,2])
    }
    
    # global cheapest over the whole active tableau
    pick_global_min <- function(){
      pick_min_over(which(active_rows), which(active_cols))
    }
    
    tol <- 1e-12
    
    while (any(active_rows) && any(active_cols)){
      rset <- which(active_rows); cset <- which(active_cols)
      
      # single row/col left → pure global min
      if (length(rset) == 1L || length(cset) == 1L){
        ij <- pick_global_min(); if (is.null(ij)) break
        do_alloc(ij[1], ij[2], stage="initial",
                 note="VAM (tek satır/sütun) → global min")
        next
      }
      
      rpen <- sapply(rset, pen_row)
      cpen <- sapply(cset, pen_col)
      mr <- suppressWarnings(max(rpen, na.rm = TRUE))
      mc <- suppressWarnings(max(cpen, na.rm = TRUE))
      
      extras <- list(rpen = setNames(as.numeric(rpen), paste0("r", rset)),
                     cpen = setNames(as.numeric(cpen), paste0("c", cset)))
      
      # Row wins (strictly)
      if (is.finite(mr) && mr > mc + tol){
        tied_rows <- rset[ which(abs(rpen - mr) <= tol) ]       # all rows with max Δ
        ij <- pick_min_over(tied_rows, cset)                    # global min within tied rows
        if (is.null(ij)) ij <- pick_global_min()
        do_alloc(ij[1], ij[2], stage="initial",
                 note=sprintf("VAM row Δ=%g → tied rows global min", mr), extras=extras)
        
        # Column wins (strictly)
      } else if (is.finite(mc) && mc > mr + tol){
        tied_cols <- cset[ which(abs(cpen - mc) <= tol) ]       # all cols with max Δ
        ij <- pick_min_over(rset, tied_cols)                    # global min within tied cols
        if (is.null(ij)) ij <- pick_global_min()
        do_alloc(ij[1], ij[2], stage="initial",
                 note=sprintf("VAM col Δ=%g → tied cols global min", mc), extras=extras)
        
        # Tie between mr and mc (within tol) → pure global min everywhere
      } else {
        ij <- pick_global_min(); if (is.null(ij)) break
        do_alloc(ij[1], ij[2], stage="initial",
                 note="VAM tie (row vs col) → global min", extras=extras)
      }
    }
  }
  
  
  
  
  
  # ---------- Başlangıç fizibilite kontrolü ----------
  if (any(abs(rowSums(alloc) - s0) > 1e-8) ||
      any(abs(colSums(alloc) - d0) > 1e-8)) {
    stop("İç hata: Başlangıç tahsisi fizibil değil (augment edilmiş örnek).")
  }
  
  # ---------- İyileştirme yardımcıları (MODI + Stepping ortak) ----------
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
    # Build adjacency from current basis (is_basic)
    rows_to_cols <- lapply(seq_len(m), function(i) which(is_basic[i, ]))
    cols_to_rows <- lapply(seq_len(n), function(j) which(is_basic[, j]))
    
    # BFS from row-node ie to col-node (m + je)
    start  <- ie
    target <- m + je
    N <- m + n
    prev    <- rep(NA_integer_, N)   # predecessor node
    visited <- rep(FALSE, N)
    
    q <- integer(0); head <- 1L
    q <- c(q, start); visited[start] <- TRUE; prev[start] <- 0L
    
    while (head <= length(q)) {
      node <- q[head]; head <- head + 1L
      
      if (node <= m) {
        # row node -> all basic columns in that row
        for (j in rows_to_cols[[node]]) {
          v <- m + j
          if (!visited[v]) {
            visited[v] <- TRUE; prev[v] <- node; q <- c(q, v)
            if (v == target) break
          }
        }
      } else {
        # col node -> all basic rows in that column
        j <- node - m
        for (i in cols_to_rows[[j]]) {
          if (!visited[i]) {
            visited[i] <- TRUE; prev[i] <- node; q <- c(q, i)
          }
        }
      }
      if (visited[target]) break
    }
    
    if (!visited[target]) return(NULL)  # no cycle possible
    
    # Reconstruct node path row ie -> ... -> col je
    nodes <- integer(0); cur <- target
    while (cur != 0L && !is.na(cur)) { nodes <- c(cur, nodes); cur <- prev[cur] }
    
    # Convert node path to loop of (i,j) corners, starting with entering (ie,je)
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
    # signs: + - + - ... starting at entering cell
    signs <- rep(c(1, -1), length.out = nrow(loop_path))
    minus <- loop_path[signs == -1, , drop = FALSE]
    if (nrow(minus) == 0) return(list(theta = 0, left = NULL, minus = minus))
    
    # step size (theta) = min allocation on minus corners
    theta <- suppressWarnings(min(alloc[cbind(minus[,1], minus[,2])], na.rm = TRUE))
    if (!is.finite(theta)) theta <- 0
    
    if (theta <= .eps) {
      # degenerate: no movement possible; caller may do a degenerate swap
      return(list(theta = 0, left = NULL, minus = minus))
    }
    
    # apply +theta/-theta along the loop
    for (k in seq_len(nrow(loop_path))){
      i <- loop_path[k,1]; j <- loop_path[k,2]
      alloc[i,j] <<- alloc[i,j] + signs[k] * theta
      if (abs(alloc[i,j]) < 1e-12) alloc[i,j] <<- 0
    }
    
    # choose leaving basic among minus corners that hit zero (lexicographic)
    zero_hits <- minus[ alloc[cbind(minus[,1], minus[,2])] <= (.eps + 1e-15), , drop = FALSE ]
    if (!nrow(zero_hits)) {
      # extremely rare numerics; pick the smallest minus anyway
      zero_hits <- minus[ order(minus[,1], minus[,2]), , drop = FALSE ][1,,drop=FALSE]
    } else {
      zero_hits <- zero_hits[ order(zero_hits[,1], zero_hits[,2]), , drop = FALSE ][1,,drop=FALSE]
    }
    
    # update basis: enter (enter_i,enter_j), leave selected zero minus
    is_basic[enter_i, enter_j] <<- TRUE
    is_basic[ zero_hits[1,1], zero_hits[1,2] ] <<- FALSE
    
    list(theta = theta, left = zero_hits)
  }
  
  
  # ---------- MODI ----------
  run_modi <- function(){
    # --- compute u, v and keep a derivation log of each equality used ---
    compute_uv <- function(){
      u <- rep(NA_real_, m); v <- rep(NA_real_, n); u[1] <- 0
      dlog <- list()  # pretty equations we used to derive u/v
      changed <- TRUE
      while (changed){
        changed <- FALSE
        for (i in 1:m) for (j in 1:n) if (is_basic[i,j]){
          if (!is.na(u[i]) && is.na(v[j])){
            v[j] <- C[i,j] - u[i]; changed <- TRUE
            dlog[[length(dlog)+1]] <- list(
              kind="v", i=i, j=j,
              eq=sprintf("v_%d = c_%d,%d - u_%d = %g - %g = %g", j, i, j, i, C[i,j], u[i], v[j])
            )
          } else if (is.na(u[i]) && !is.na(v[j])){
            u[i] <- C[i,j] - v[j]; changed <- TRUE
            dlog[[length(dlog)+1]] <- list(
              kind="u", i=i, j=j,
              eq=sprintf("u_%d = c_%d,%d - v_%d = %g - %g = %g", i, i, j, j, C[i,j], v[j], u[i])
            )
          }
        }
      }
      list(u=u, v=v, log=dlog)
    }
    
    # reduced costs only for NON-BASIC cells
    reduced_costs <- function(u, v){
      R <- matrix(NA_real_, m, n)
      for (i in 1:m) for (j in 1:n) if (!is_basic[i,j]) {
        if (!is.na(u[i]) && !is.na(v[j])) R[i,j] <- C[i,j] - u[i] - v[j]
      }
      R
    }
    
    connect_components()  # keep the current basis connectivity (no padding)
    
    max_iter <- 3000
    iter <- 0
    
    repeat {
      iter <- iter + 1
      if (iter > max_iter){ warning("MODI max_iter aşıldı; durduruldu."); break }
      
      uv <- compute_uv()
      R  <- reduced_costs(uv$u, uv$v)
      
      if (all(is.na(R))) {
        # log snapshot even if nothing computable
        push("modi-uv", shipped=0, note="u–v hesap (R yok)",
             extras=list(u=uv$u, v=uv$v, uv_log=uv$log, reduced=R, basis=(is_basic+0)))
        break
      }
      
      minR <- suppressWarnings(min(R, na.rm = TRUE))
      
      # ---- Log THIS iteration's u, v, and R as an HTML-friendly snapshot ----
      push("modi-uv", shipped=0,
           note=sprintf("u–v hesap; min(R)=%g", ifelse(is.finite(minR), minR, NA_real_)),
           extras=list(u=uv$u, v=uv$v, uv_log=uv$log, reduced=R, basis=(is_basic+0)))
      
      if (!is.finite(minR) || minR >= -1e-9) {
        # optimal within tolerance
        break
      }
      
      # All most-negative candidates
      cand <- which(R <= (minR + 1e-12), arr.ind = TRUE)
      if (is.null(nrow(cand)) || nrow(cand) == 0L){
        warning("MODI: no entering-cell candidates; stopping.")
        break
      }
      
      # Choose the candidate with largest positive theta
      best <- NULL; best_loop <- NULL; best_theta <- 0
      loops <- vector("list", nrow(cand))
      for (k in seq_len(nrow(cand))){
        ie <- cand[k,1]; je <- cand[k,2]
        loop <- find_loop(ie, je); loops[[k]] <- loop
        if (is.null(loop) || !is.matrix(loop) || nrow(loop) < 4L) next
        signs <- rep(c(1,-1), length.out = nrow(loop))
        minus <- loop[signs == -1, , drop = FALSE]
        if (!nrow(minus)) next
        th <- suppressWarnings(min(alloc[cbind(minus[,1], minus[,2])], na.rm = TRUE))
        if (is.finite(th) && th > best_theta + 1e-15) {
          best_theta <- th; best <- c(ie,je); best_loop <- loop
        }
      }
      
      # Case A: a positive-theta pivot exists
      if (!is.null(best_loop) && best_theta > .eps){
        res <- apply_cycle(best_loop, best[1], best[2])
        push("modi", best[1], best[2], res$theta,
             note = sprintf("MODI enter (%d,%d), red.cost=%g", best[1], best[2], minR),
             extras = list(u=uv$u, v=uv$v, reduced=R, basis=(is_basic+0)))
        next
      }
      
      # Case B: degeneracy — try a safe basis swap (no allocation change)
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
        swapped <- TRUE
        break
      }
      
      if (!swapped){
        warning("MODI: degeneracy but no valid basis swap found; stopping.")
        break
      }
    }
  }
  
  
  
  # ---------- Stepping-Stone ----------
  run_stepping <- function(){
    connect_components()
    max_iter <- 3000
    iter <- 0
    prev_hash <- paste(alloc, collapse=",")
    
    repeat {
      iter <- iter + 1
      if (iter > max_iter) { warning("Stepping max_iter aşıldı; durduruldu."); break }
      
      best_delta <- 0
      best_cell  <- NULL
      best_loop  <- NULL
      
      for (ie in 1:m) for (je in 1:n){
        if (alloc[ie,je] > 0) next
        loop <- find_loop(ie, je); if (is.null(loop)) next
        signs <- rep(c(1,-1), length.out = nrow(loop))
        delta <- sum(signs * C[cbind(loop[,1], loop[,2])])
        if (delta < best_delta - 1e-12){
          best_delta <- delta; best_cell <- c(ie,je); best_loop <- loop
        }
      }
      
      if (is.null(best_cell)) break
      
      # NOTE: apply_cycle() returns a LIST now
      res <- apply_cycle(best_loop, best_cell[1], best_cell[2])
      th  <- res$theta
      
      if (th <= .eps) { warning("Stepping theta≈0; durduruldu."); break }
      
      push("stepping", best_cell[1], best_cell[2], th,
           note = sprintf("Stepping enter (%d,%d), Δ=%g", best_cell[1], best_cell[2], best_delta))
      
      cur_hash <- paste(alloc, collapse=",")
      if (identical(cur_hash, prev_hash)) { warning("Stepping stagnasyon; durduruldu."); break }
      prev_hash <- cur_hash
    }
  }
  
  
  
  # ---------- İyileştirme çalıştır ----------
  if (optimize %in% c("modi","both"))      run_modi()
  if (optimize %in% c("stepping","both"))  run_stepping()
  
  # ---------- Çıktı: dummy'yi gerekirse düşür ----------
  alloc_full <- alloc
  cost_full  <- C
  supply_full <- s0
  demand_full <- d0
  
  alloc_out <- alloc_full
  cost_out  <- cost_full
  supply_out <- supply_full
  demand_out <- demand_full
  
  if (!is.null(augmented$added) && drop_dummy_in_output) {
    if (augmented$added == "col"){
      j <- augmented$dummy_index
      alloc_out  <- alloc_out[, -j, drop = FALSE]
      cost_out   <- cost_out[,  -j, drop = FALSE]
      demand_out <- demand_out[-j]
    } else if (augmented$added == "row"){
      i <- augmented$dummy_index
      alloc_out  <- alloc_out[-i, , drop = FALSE]
      cost_out   <- cost_out[ -i, , drop = FALSE]
      supply_out <- supply_out[-i]
    }
  }
  
  list(
    allocation = alloc_out,
    total_cost = sum(alloc_full * cost_full),  # toplam maliyet (dummy dahil hesap)
    method     = method,
    optimized  = optimize != "none",
    optimizer  = optimize,
    cost       = cost_out,
    supply     = supply_out,
    demand     = demand_out,
    augmented  = augmented,
    steps      = if (trace) steps else NULL
  )
}

# ============================================================
# HTML RENDER (BigM stili, TR) + DEMO (UNBALANCED destekli)
#  -> solve_transport() fonksiyonunuzun güncel sürümü yüklü olmalı
# ============================================================

# ---------- Helpers (CSS & utils) ----------
.html_escape <- function(x){
  x <- gsub("&","&amp;",x,fixed=TRUE)
  x <- gsub("<","&lt;",x,fixed=TRUE)
  x <- gsub(">","&gt;",x,fixed=TRUE); x
}
.mk_names <- function(prefix, k) sprintf("%s%d", prefix, seq_len(k))
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

# ---------- Dummy adını otomatik ekleyen etiketleyici ----------
labels_with_dummy <- function(labels, augmented, drop_dummy_in_output,
                              m_after, n_after,
                              dummy_row_name="(Dummy Kaynak)",
                              dummy_col_name="(Dummy Hedef)"){
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  if (is.null(augmented$added) || drop_dummy_in_output) return(lbl)
  
  if (augmented$added == "row"){
    rn <- if (is.null(lbl$row_names)) .mk_names(lbl$row_prefix, m_after) else {
      # ensure length m_after; if short, extend then insert name at exact index
      x <- lbl$row_names
      length(x) <- m_after; if (is.na(x[augmented$dummy_index])) x[augmented$dummy_index] <- dummy_row_name
      x
    }
    lbl$row_names <- rn
  } else if (augmented$added == "col"){
    cn <- if (is.null(lbl$col_names)) .mk_names(lbl$col_prefix, n_after) else {
      x <- lbl$col_names
      length(x) <- n_after; if (is.na(x[augmented$dummy_index])) x[augmented$dummy_index] <- dummy_col_name
      x
    }
    lbl$col_names <- cn
  }
  lbl
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


# ---------- Sonuç tablosu (hazır tahsisten) ----------
transport_result_from_alloc_html <- function(cost, alloc, 
                                             title = "Dağıtım", 
                                             labels = .transport_labels_default(),
                                             include_css = TRUE,
                                             table_class = "tbl-sim") {
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
      if (alloc[i,j] > 0){
        cell <- sprintf("<div class='bold'>%g</div><div class='sub'>(m=%g)</div>", alloc[i,j], C[i,j])
        html <- c(html, sprintf("<td class='alloc'>%s</td>", cell))
      } else {
        cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(m=%g)</div>", C[i,j])
        html <- c(html, sprintf("<td>%s</td>", cell))
      }
    }
    html <- c(html, sprintf("<td class='rhs bold'>%g</td>", supply[i]), "</tr>")
  }
  html <- c(html, sprintf("<tr><th>%s</th>", .html_escape(lbl$demand_label)))
  for (j in seq_len(n)) html <- c(html, sprintf("<td class='rhs bold'>%g</td>", demand[j]))
  html <- c(html, sprintf("<td class='rhs bold'>%g</td>", sum(supply)), "</tr></tbody></table>")
  paste(html, collapse="\n")
}

# ---------- Adım sayfası (mevcut / toplam) ----------
transport_result_from_alloc_step_html <- function(cost, alloc,
                                                  supply_total, demand_total,
                                                  title = "Ara Adım",
                                                  labels = .transport_labels_default(),
                                                  include_css = TRUE,
                                                  table_class = "tbl-sim") {
  C <- as.matrix(cost); m <- nrow(C); n <- ncol(C)
  supply_cur <- rowSums(alloc); demand_cur <- colSums(alloc)
  total_cur  <- sum(alloc);     total_all  <- sum(supply_total)
  total_cost <- sum(alloc * C)
  
  # Which rows/cols are exhausted at THIS step?
  eps <- 1e-12
  exh_rows <- which(abs(supply_cur - supply_total) <= eps)
  exh_cols <- which(abs(demand_cur - demand_total) <= eps)
  
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  
  html <- c()
  if (include_css) {
    html <- c(html,
              .transport_css_bigM(),
              "<style>
        /* Light-red tint for exhausted rows/cols; keep .alloc (yellow) dominant */
        .tbl-sim tr.exh-row td:not(.alloc), .tbl-sim tr.exh-row th { background:#fdecea; }
        .tbl-sim td.exh-col:not(.alloc),   .tbl-sim th.exh-col   { background:#fdecea; }
      </style>"
    )
  }
  
  html <- c(html, sprintf("<table class=\"%s\">", .html_escape(table_class)))
  html <- c(html, sprintf("<caption>%s (%s = <b>%g</b>)</caption>",
                          .html_escape(title), .html_escape(lbl$total_cost), total_cost))
  
  # Header
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)) {
    cls <- if (j %in% exh_cols) " class='exh-col'" else ""
    html <- c(html, sprintf("<th%s>%s</th>", cls, .html_escape(col_names[j])))
  }
  html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", .html_escape(lbl$supply_label)))
  
  # Body
  for (i in seq_len(m)) {
    row_cls <- if (i %in% exh_rows) " class='exh-row'" else ""
    html <- c(html, sprintf("<tr%s>", row_cls))
    html <- c(html, sprintf("<th>%s</th>", .html_escape(row_names[i])))
    for (j in seq_len(n)) {
      td_cls <- if (j %in% exh_cols) "exh-col" else ""
      if (alloc[i,j] > 0) {
        cell <- sprintf("<div class='bold'>%g</div><div class='sub'>(m=%g)</div>", alloc[i,j], C[i,j])
        html <- c(html, sprintf("<td class='alloc %s'>%s</td>", td_cls, cell))
      } else {
        cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(m=%g)</div>", C[i,j])
        html <- c(html, sprintf("<td class='%s'>%s</td>", td_cls, cell))
      }
    }
    html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                            supply_cur[i], supply_total[i]), "</tr>")
  }
  
  # Demand row
  html <- c(html, sprintf("<tr><th>%s</th>", .html_escape(lbl$demand_label)))
  for (j in seq_len(n)) {
    cls <- if (j %in% exh_cols) " class='rhs bold exh-col'" else " class='rhs bold'"
    html <- c(html, sprintf("<td%s>%g <span class='sub'>(%g)</span></td>",
                            cls, demand_cur[j], demand_total[j]))
  }
  html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                          total_cur, total_all), "</tr></tbody></table>")
  
  paste(html, collapse = "\n")
}


# ---------- Başlık yardımcıları ----------
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

# Overwrite your old helper with this one:
save_alloc_table <- function(res, title, fname, labels,
                             dummy_row_name="(Dummy Kaynak)",
                             dummy_col_name="(Dummy Hedef)") {
  # res is the *list* returned by solve_transport()
  # keep dummy visible + labeled
  lbl2 <- labels_with_dummy(
    labels = labels,
    augmented = res$augmented,
    drop_dummy_in_output = FALSE,                # we want to SHOW dummy
    m_after = nrow(res$cost),
    n_after = ncol(res$cost),
    dummy_row_name = dummy_row_name,
    dummy_col_name = dummy_col_name
  )
  cat(
    transport_result_from_alloc_html(
      cost  = res$cost,          # augmented cost (matches allocation)
      alloc = res$allocation,    # augmented allocation
      title = title,
      labels = lbl2
    ),
    file = file.path(outdir, fname)
  )
}


render_trace_steps <- function(tr, dataset_title, outdir, method_label,
                               labels_for_steps, max_steps = 2000,
                               show_titles = FALSE, show_notes = FALSE){
  
  idx_path <- file.path(outdir, sprintf("30_%s_adimlar_index.html", method_label))
  idx <- c("<html><head><meta charset='utf-8'><title>Adımlar</title></head><body>",
           if (show_titles) sprintf("<h2>%s – Adımlar (Başlangıç + İyileştirme)</h2>", dataset_title) else "",
           "<ol>")
  
  prev_alloc_hash <- NULL
  step_count <- 0
  show_pen <- identical(tolower(method_label), "vam")  # <- penalties only for VAM
  
  .vam_penalties <- function(C, row_used, col_used, supply_total, demand_total){
    m <- nrow(C); n <- ncol(C); eps <- 1e-12
    active_rows <- which(row_used < supply_total - eps)
    active_cols <- which(col_used < demand_total - eps)
    rpen <- rep(NA_real_, m)
    if (length(active_cols) >= 2){
      for (i in active_rows){
        v <- sort(C[i, active_cols], partial = 2)[1:2]; rpen[i] <- v[2] - v[1]
      }
    }
    cpen <- rep(NA_real_, n)
    if (length(active_rows) >= 2){
      for (j in active_cols){
        v <- sort(C[active_rows, j], partial = 2)[1:2]; cpen[j] <- v[2] - v[1]
      }
    }
    list(rpen = rpen, cpen = cpen)
  }
  
  base_css <- paste0(
    .transport_css_bigM(),
    "<style>
      /* exhausted tint without overriding .alloc */
      .tbl-sim tr.exh-row td:not(.alloc), .tbl-sim tr.exh-row th { background:#fdecea; }
      .tbl-sim td.exh-col:not(.alloc),    .tbl-sim th.exh-col    { background:#fdecea; }
      .tbl-sim td.pen, .tbl-sim th.pen { background:#f8f9fa; font-weight:600; }
      .tbl-sim tfoot td { font-weight:600; }
      .tbl-mini { margin-bottom:14px }
    </style>"
  )
  
  for (t in seq_along(tr$steps)){
    if (step_count >= max_steps) {
      idx <- c(idx, "<li><em>Adımlar sınırı (max_steps) aşıldı; kesildi.</em></li>"); break
    }
    st <- tr$steps[[t]]
    cur_hash <- paste(st$alloc, collapse=",")
    if (!is.null(prev_alloc_hash) && identical(cur_hash, prev_alloc_hash)) next
    prev_alloc_hash <- cur_hash
    step_count <- step_count + 1
    
    # AFTER-move snapshot
    C <- tr$cost
    supply_total <- tr$supply
    demand_total <- tr$demand
    alloc_after <- st$alloc + 0
    m <- nrow(C); n <- ncol(C)
    eps <- 1e-12
    row_used_after <- rowSums(alloc_after)
    col_used_after <- colSums(alloc_after)
    exh_rows_after <- which(abs(row_used_after - supply_total) <= eps)
    exh_cols_after <- which(abs(col_used_after - demand_total) <= eps)
    
    # PREVIEW: reconstruct pre-move by removing the just-added shipment (if recorded)
    alloc_before <- alloc_after + 0
    if (!is.null(st$i) && !is.null(st$j) && !is.null(st$x_ij) &&
        is.finite(st$x_ij) && st$x_ij > 0){
      ii <- as.integer(st$i); jj <- as.integer(st$j); x <- as.numeric(st$x_ij)
      if (ii>=1 && ii<=m && jj>=1 && jj<=n) alloc_before[ii, jj] <- max(0, alloc_before[ii, jj] - x)
    }
    row_used_before <- rowSums(alloc_before)
    col_used_before <- colSums(alloc_before)
    exh_rows_before <- which(abs(row_used_before - supply_total) <= eps)
    exh_cols_before <- which(abs(col_used_before - demand_total) <= eps)
    
    # penalties (compute, but we will only show them if show_pen==TRUE)
    pre_pen  <- .vam_penalties(C, row_used_before, col_used_before, supply_total, demand_total)
    post_pen <- .vam_penalties(C, row_used_after,  col_used_after,  supply_total, demand_total)
    rpen_b <- pre_pen$rpen;  cpen_b <- pre_pen$cpen
    rpen_a <- post_pen$rpen; cpen_a <- post_pen$cpen
    
    lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
    row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
    col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
    
    step_title <- sprintf("Adım %d — %s",
                          st$k,
                          if (is.null(st$info$note)) st$stage else paste0(st$stage, ": ", st$info$note))
    fname_preview <- sprintf("30_%s_step_%03d_preview.html", method_label, step_count)
    fname_step    <- sprintf("31_%s_step_%03d.html",        method_label, step_count)
    
    # ---------- PREVIEW FILE (pre-move; keep RHS/Talep and existing allocations) ----------
    {
      html <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>")
      if (show_titles) html <- c(html, sprintf("<h3>%s — Önizleme (tahsis öncesi)</h3>", .html_escape(step_title)))
      html <- c(html, sprintf("<table class=\"%s %s\">", .html_escape("tbl-sim"), .html_escape("tbl-mini")))
      
      # header
      html <- c(html, "<thead><tr><th></th>")
      for (j in seq_len(n)){
        cls <- if (j %in% exh_cols_before) " class='exh-col'" else ""
        html <- c(html, sprintf("<th%s>%s</th>", cls, .html_escape(col_names[j])))
      }
      html <- c(html,
                sprintf("<th class='rhs'>%s</th>", .html_escape(lbl$supply_label)))
      if (show_pen) html <- c(html, "<th class='pen'>Row Δ</th>")
      html <- c(html, "</tr></thead><tbody>")
      
      # body (alloc_before + costs)
      for (i in seq_len(m)){
        row_cls <- if (i %in% exh_rows_before) " class='exh-row'" else ""
        html <- c(html, sprintf("<tr%s>", row_cls))
        html <- c(html, sprintf("<th>%s</th>", .html_escape(row_names[i])))
        for (j in seq_len(n)){
          td_cls <- if (j %in% exh_cols_before) "exh-col" else ""
          if (alloc_before[i,j] > 0){
            cell <- sprintf("<div class='bold'>%g</div><div class='sub'>(m=%g)</div>", alloc_before[i,j], C[i,j])
            html <- c(html, sprintf("<td class='alloc %s'>%s</td>", td_cls, cell))
          } else {
            cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(m=%g)</div>", C[i,j])
            html <- c(html, sprintf("<td class='%s'>%s</td>", td_cls, cell))
          }
        }
        html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                                row_used_before[i], supply_total[i]))
        if (show_pen){
          pen_txt <- if (is.na(rpen_b[i])) "<span class='muted'>&ndash;</span>" else sprintf("%g", rpen_b[i])
          html <- c(html, sprintf("<td class='pen'>%s</td>", pen_txt))
        }
        html <- c(html, "</tr>")
      }
      html <- c(html, "</tbody><tfoot>")
      
      # demand row
      html <- c(html, sprintf("<tr><th>%s</th>", .html_escape(lbl$demand_label)))
      for (j in seq_len(n)){
        cls <- if (j %in% exh_cols_before) " class='rhs bold exh-col'" else " class='rhs bold'"
        html <- c(html, sprintf("<td%s>%g <span class='sub'>(%g)</span></td>",
                                cls, col_used_before[j], demand_total[j]))
      }
      html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                              sum(alloc_before), sum(supply_total)))
      if (show_pen) html <- c(html, "<td class='pen'>&nbsp;</td>")
      html <- c(html, "</tr>")
      
      # column penalties (only VAM)
      if (show_pen){
        html <- c(html, "<tr><th class='pen'>Col Δ</th>")
        for (j in seq_len(n)){
          pen_txt <- if (is.na(cpen_b[j])) "<span class='muted'>&ndash;</span>" else sprintf("%g", cpen_b[j])
          cls <- if (j %in% exh_cols_before) " class='pen exh-col'" else " class='pen'"
          html <- c(html, sprintf("<td%s>%s</td>", cls, pen_txt))
        }
        html <- c(html, "<td class='pen'>&nbsp;</td><td class='pen'>&nbsp;</td></tr>")
      }
      html <- c(html, "</tfoot></table>")
      if (show_notes) html <- c(html, "<p class='sub'>Önizleme: Tahsis yapılmadan <b>önceki</b> durum.</p>")
      html <- c(html, "</body></html>")
      writeLines(html, file.path(outdir, fname_preview))
    }
    
    # ---------- FULL STEP FILE (after the move) ----------
    {
      html <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>")
      if (show_titles) html <- c(html, sprintf("<h3>%s</h3>", .html_escape(step_title)))
      html <- c(html, sprintf("<table class=\"%s\">", .html_escape("tbl-sim")))
      
      # header
      html <- c(html, "<thead><tr><th></th>")
      for (j in seq_len(n)){
        cls <- if (j %in% exh_cols_after) " class='exh-col'" else ""
        html <- c(html, sprintf("<th%s>%s</th>", cls, .html_escape(col_names[j])))
      }
      html <- c(html,
                sprintf("<th class='rhs'>%s</th>", .html_escape(lbl$supply_label)))
      if (show_pen) html <- c(html, "<th class='pen'>Row Δ</th>")
      html <- c(html, "</tr></thead><tbody>")
      
      # body
      for (i in seq_len(m)){
        row_cls <- if (i %in% exh_rows_after) " class='exh-row'" else ""
        html <- c(html, sprintf("<tr%s>", row_cls))
        html <- c(html, sprintf("<th>%s</th>", .html_escape(row_names[i])))
        for (j in seq_len(n)){
          td_cls <- if (j %in% exh_cols_after) "exh-col" else ""
          if (alloc_after[i,j] > 0){
            cell <- sprintf("<div class='bold'>%g</div><div class='sub'>(m=%g)</div>", alloc_after[i,j], C[i,j])
            html <- c(html, sprintf("<td class='alloc %s'>%s</td>", td_cls, cell))
          } else {
            cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(m=%g)</div>", C[i,j])
            html <- c(html, sprintf("<td class='%s'>%s</td>", td_cls, cell))
          }
        }
        html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                                row_used_after[i], supply_total[i]))
        if (show_pen){
          pen_txt <- if (is.na(rpen_a[i])) "<span class='muted'>&ndash;</span>" else sprintf("%g", rpen_a[i])
          html <- c(html, sprintf("<td class='pen'>%s</td>", pen_txt))
        }
        html <- c(html, "</tr>")
      }
      html <- c(html, "</tbody><tfoot>")
      
      # demand row
      html <- c(html, sprintf("<tr><th>%s</th>", .html_escape(lbl$demand_label)))
      for (j in seq_len(n)){
        cls <- if (j %in% exh_cols_after) " class='rhs bold exh-col'" else " class='rhs bold'"
        html <- c(html, sprintf("<td%s>%g <span class='sub'>(%g)</span></td>",
                                cls, col_used_after[j], demand_total[j]))
      }
      html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                              sum(alloc_after), sum(supply_total)))
      if (show_pen) html <- c(html, "<td class='pen'>&nbsp;</td>")
      html <- c(html, "</tr>")
      
      # column penalties (only VAM)
      if (show_pen){
        html <- c(html, "<tr><th class='pen'>Col Δ</th>")
        for (j in seq_len(n)){
          pen_txt <- if (is.na(cpen_a[j])) "<span class='muted'>&ndash;</span>" else sprintf("%g", cpen_a[j])
          cls <- if (j %in% exh_cols_after) " class='pen exh-col'" else " class='pen'"
          html <- c(html, sprintf("<td%s>%s</td>", cls, pen_txt))
        }
        html <- c(html, "<td class='pen'>&nbsp;</td><td class='pen'>&nbsp;</td></tr>")
      }
      html <- c(html, "</tfoot></table>")
      if (show_notes) html <- c(html, "<p class='sub'>Adım: Tahsis <b>sonrası</b> durum.</p>")
      html <- c(html, "</body></html>")
      writeLines(html, file.path(outdir, fname_step))
    }
    
    # index links
    idx <- c(idx, sprintf("<li>Adım %d: <a href='%s'>Önizleme</a> · <a href='%s'>Adım</a></li>",
                          st$k, fname_preview, fname_step))
  }
  
  idx <- c(idx, "</ol>",
           if (show_notes) "<p><em>Not:</em> Δ satır/sütunları yalnızca VAM için gösterilir.</p>" else "",
           "</body></html>")
  writeLines(idx, idx_path)
}



# =========================================================
# Presentation template (symbolic) in your HTML style
# =========================================================
transport_presentation_template_html <- function(m = 4, n = 4,
                                                 labels = list(
                                                   title = "Ulaştırma Modeli Tablosu",
                                                   row_header = "Sunum Merkezi",
                                                   col_header = "İstem Merkezi",
                                                   supply_label = "Sunum Miktarı",
                                                   demand_label = "İstem Miktarı",
                                                   row_names = NULL,  # e.g., c("Fabrika 1", ...)
                                                   col_names = NULL   # e.g., c("Depo 1", ...)
                                                 ),
                                                 include_css = TRUE,
                                                 table_class = "tbl-sim") {
  lbl <- modifyList(list(
    title = "Ulaştırma Modeli Tablosu",
    row_header = "Sunum Merkezi",
    col_header = "İstem Merkezi",
    supply_label = "Sunum Miktarı",
    demand_label = "İstem Miktarı",
    row_names = NULL,
    col_names = NULL
  ), labels, keep.null = TRUE)
  
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else paste0("SM", seq_len(m))
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else paste0("İM", seq_len(n))
  
  # extra CSS to draw the little cost box and variable inside each cell
  extra_css <- "
  <style>
    .cellwrap{position:relative; min-width:80px; height:46px}
    .costbox{
      position:absolute; top:6px; right:6px; display:inline-block;
      border:1px solid #adb5bd; border-radius:6px; padding:1px 6px; font-size:12px; color:#495057
    }
    .xvar{position:absolute; left:8px; bottom:6px; font-weight:600}
    .topband{background:#f1f3f5}
    .sideband{background:#f8f9fa; font-weight:600}
    .eqcell{font-size:13px}
  </style>"
  
  html <- c()
  if (include_css) html <- c(html, .transport_css_bigM(), extra_css)
  
  # Title block (separate small header row like the sample)
  html <- c(html, sprintf("<table class=\"%s\" style=\"margin-bottom:6px\"><thead>",
                          .html_escape(table_class)))
  html <- c(html, sprintf(
    "<tr><th class='topband' colspan='%d' style='text-align:center;font-size:16px'>%s</th></tr>",
    n + 2, .html_escape(lbl$title)))
  html <- c(html, "</thead></table>")
  
  # Main grid
  html <- c(html, sprintf("<table class=\"%s\">", .html_escape(table_class)))
  
  # Column header row: row header cell + col header band + RHS header
  html <- c(html, "<thead><tr>")
  html <- c(html, sprintf("<th class='sideband'>%s</th>", .html_escape(lbl$row_header)))
  for (j in seq_len(n)) {
    html <- c(html, sprintf("<th class='topband'>%s</th>", .html_escape(col_names[j])))
  }
  html <- c(html, sprintf("<th class='sideband'>%s</th>", .html_escape(lbl$supply_label)))
  html <- c(html, "</tr></thead><tbody>")
  
  # Body rows
  for (i in seq_len(m)) {
    html <- c(html, "<tr>")
    html <- c(html, sprintf("<th class='sideband'>%s</th>", .html_escape(row_names[i])))
    for (j in seq_len(n)) {
      cell <- sprintf(
        "<div class='cellwrap'>
           <div class='costbox'>C<sub>%d%d</sub></div>
           <div class='xvar'>x<sub>%d%d</sub></div>
         </div>", i, j, i, j)
      html <- c(html, sprintf("<td>%s</td>", cell))
    }
    html <- c(html, sprintf("<td class='sideband'>a<sub>%d</sub></td>", i))
    html <- c(html, "</tr>")
  }
  
  # Bottom demand row + equality cell
  html <- c(html, "<tr>")
  html <- c(html, sprintf("<th class='sideband'>%s</th>", .html_escape(lbl$demand_label)))
  for (j in seq_len(n)) {
    html <- c(html, sprintf("<td class='sideband'>b<sub>%d</sub></td>", j))
  }
  eq <- "&Sigma;<sub>i=1</sub><sup>m</sup>a<sub>i</sub> = &Sigma;<sub>j=1</sub><sup>n</sup>b<sub>j</sub>"
  html <- c(html, sprintf("<td class='sideband eqcell'>%s</td>", eq))
  html <- c(html, "</tr></tbody></table>")
  
  paste(html, collapse = "\n")
}


# =========================================================
# Export LaTeX objective equation to a separate *raw* HTML file
#  - cost, alloc  : matrices
#  - file         : output html path
#  - title        : small heading shown above the raw LaTeX
#  - render_math  : FALSE -> raw only (default), TRUE -> also show rendered preview via MathJax
# =========================================================
transport_export_latex_equation_html <- function(cost, alloc, file,
                                                 title = "LaTeX – Toplam Maliyet Eşitliği",
                                                 render_math = FALSE,
                                                 mathjax_src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"){
  C <- as.matrix(cost)
  A <- as.matrix(alloc)
  total_cost <- sum(C * A)
  
  # helper: integer-looking numbers as integers
  .fnum <- function(v){
    if (isTRUE(all.equal(v, round(v)))) sprintf("%d", as.integer(round(v))) else sprintf("%g", v)
  }
  # basic html escape (use your global helper if present)
  .esc <- if (exists(".html_escape")) get(".html_escape") else function(x){
    x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE); gsub(">","&gt;",x,fixed=TRUE)
  }
  
  # build LaTeX string (use only non-zero x_ij terms; fall back to generic if none)
  nz <- which(A > 0, arr.ind = TRUE)
  if (nrow(nz) == 0){
    eq <- "\\[ Z = \\sum_{i=1}^{m} \\sum_{j=1}^{n} c_{ij} x_{ij} \\]"
  } else {
    sym_terms <- character(nrow(nz))
    num_terms <- character(nrow(nz))
    for (k in seq_len(nrow(nz))){
      i <- nz[k,1]; j <- nz[k,2]
      sym_terms[k] <- sprintf("c_{%d,%d} x_{%d,%d}", i, j, i, j)
      num_terms[k] <- sprintf("%s\\cdot %s", .fnum(C[i,j]), .fnum(A[i,j]))
    }
    eq <- sprintf("\\[
\\begin{aligned}
Z &= \\sum_{i=1}^{m}\\sum_{j=1}^{n} c_{ij} x_{ij} \\\\
  &= %s \\\\
  &= %s \\\\
  &= \\mathbf{%s}
\\end{aligned}
\\]", paste(sym_terms, collapse = " + "),
                  paste(num_terms, collapse = " + "),
                  .fnum(total_cost))
  }
  
  # --- write minimal HTML with RAW LaTeX shown in a code block ---
  html <- c("<!doctype html>",
            "<html><head><meta charset='utf-8'>",
            "<title>LaTeX Equation</title>",
            "<style>",
            "body{font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:20px}",
            "h3{margin:0 0 8px 0}",
            ".raw{white-space:pre-wrap;background:#f8f9fa;border:1px solid #e9ecef;",
            "     padding:12px;border-radius:6px;font-family:ui-monospace,Consolas,Monaco,monospace}",
            ".note{color:#6c757d;font-size:13px;margin-top:8px}",
            "</style>")
  if (render_math){
    html <- c(html, sprintf("<script id='MathJax-script' async src='%s'></script>", .esc(mathjax_src)))
  }
  html <- c(html, "</head><body>",
            sprintf("<h3>%s</h3>", .esc(title)),
            sprintf("<div class='raw'>%s</div>", .esc(eq)))
  if (render_math){
    html <- c(html, "<h4>Önizleme (MathJax)</h4>", eq)
  }
  html <- c(html, "<div class='note'>Bu dosya <b>ham</b> LaTeX içerir. Quarto'da kullanmak için dosyayı include edin veya LaTeX metnini kopyalayın.</div>",
            "</body></html>")
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  writeLines(html, file)
  invisible(eq)
}


# Cost-only initial table (no supply/demand margins), in your HTML style
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
  
  # header (only column names)
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", .html_escape(col_names[j])))
  html <- c(html, "</tr></thead><tbody>")
  
  # body (cost + x_ij in every cell)
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

render_modi_uv_tables <- function(tr, outdir, method_label,
                                  labels_for_steps = .transport_labels_default(),
                                  include_css = TRUE){
  if (is.null(tr$steps) || !length(tr$steps)) return(invisible())
  
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  idx_path <- file.path(outdir, sprintf("40_%s_modi_index.html", method_label))
  
  base_css <- paste0(
    if (include_css) .transport_css_bigM() else "",
    "<style>
      .tbl-sim .neg{color:#b00020;font-weight:600}
      .tbl-sim .minneg{background:#fdecea;border:2px solid #f5c2c7}
      .tbl-sim .basic{background:#eef9f1}
      .tbl-sim .na{color:#adb5bd}
      .uvwrap{display:flex; gap:16px; align-items:flex-start; margin:8px 0}
      .uvbox{min-width:180px}
      .uvbox table{width:100%}
      .eqs{font-family:ui-monospace,Consolas,monospace; background:#f8f9fa; padding:10px; border:1px solid #e9ecef; border-radius:6px}
    </style>"
  )
  
  idx <- c("<html><head><meta charset='utf-8'><title>MODI u–v</title>", base_css, "</head><body>",
           sprintf("<h2>%s – MODI u–v Hesap Adımları</h2>", method_title(tr$method)),
           "<ol>")
  
  snap_id <- 0
  for (st in tr$steps){
    if (!(st$stage %in% c("modi-uv", "modi"))) next
    if (st$stage == "modi") next  # only show the pre-pivot snapshots with full u/v & R
    
    snap_id <- snap_id + 1
    u <- st$info$u; v <- st$info$v; R <- st$info$reduced
    basis <- st$info$basis; if (is.null(basis)) basis <- (st$alloc > 0) + 0
    
    C <- tr$cost
    m <- nrow(C); n <- ncol(C)
    
    # Find min negative reduced costs to highlight
    minR <- suppressWarnings(min(R, na.rm = TRUE))
    is_minneg <- matrix(FALSE, m, n)
    if (is.finite(minR) && minR < 0){
      where <- which(!is.na(R) & abs(R - minR) <= 1e-12, arr.ind = TRUE)
      if (nrow(where)) is_minneg[ cbind(where[,1], where[,2]) ] <- TRUE
    }
    
    # ---- Build HTML page for this snapshot ----
    html <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>")
    
    # Small heading
    cap <- if (!is.null(st$info$note)) st$info$note else "u–v hesap"
    html <- c(html, sprintf("<h3>Adım %d — %s</h3>", st$k, .html_escape(cap)))
    
    # Potentials (u and v)
    html <- c(html, "<div class='uvwrap'>")
    
    # u-table
    html <- c(html, "<div class='uvbox'>",
              sprintf("<table class='tbl-sim'><thead><tr><th>i</th><th>u<sub>i</sub></th></tr></thead><tbody>"))
    for (i in seq_len(m)){
      val <- if (is.na(u[i])) "<span class='na'>&ndash;</span>" else sprintf("%g", u[i])
      html <- c(html, sprintf("<tr><th>%d</th><td>%s</td></tr>", i, val))
    }
    html <- c(html, "</tbody></table></div>")
    
    # v-table
    html <- c(html, "<div class='uvbox'>",
              sprintf("<table class='tbl-sim'><thead><tr><th>j</th><th>v<sub>j</sub></th></tr></thead><tbody>"))
    for (j in seq_len(n)){
      val <- if (is.na(v[j])) "<span class='na'>&ndash;</span>" else sprintf("%g", v[j])
      html <- c(html, sprintf("<tr><th>%d</th><td>%s</td></tr>", j, val))
    }
    html <- c(html, "</tbody></table></div>")
    
    html <- c(html, "</div>")  # /uvwrap
    
    # Derivation log (the equations we actually used)
    eqs <- st$info$uv_log
    if (!is.null(eqs) && length(eqs)){
      html <- c(html, "<div class='eqs'><b>Türetilen eşitlikler</b><ol style='margin:6px 0'>")
      for (e in eqs) html <- c(html, sprintf("<li>%s</li>", .html_escape(e$eq)))
      html <- c(html, "</ol></div>")
    }
    
    # Reduced-cost matrix R with basic markers
    lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
    row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
    col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
    
    html <- c(html, sprintf("<table class='tbl-sim'><thead><tr><th></th>"))
    for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", .html_escape(col_names[j])))
    html <- c(html, "</tr></thead><tbody>")
    
    for (i in seq_len(m)){
      html <- c(html, sprintf("<tr><th>%s</th>", .html_escape(row_names[i])))
      for (j in seq_len(n)){
        cls <- character(0)
        if (!is.na(R[i,j]) && R[i,j] < 0) cls <- c(cls, "neg")
        if (is_minneg[i,j]) cls <- c(cls, "minneg")
        if (basis[i,j] == 1) cls <- c(cls, "basic")
        val <- if (is.na(R[i,j])) "<span class='na'>&ndash;</span>" else sprintf("%g", R[i,j])
        sub <- sprintf("<div class='sub'>(c=%g%s)</div>", C[i,j], if (basis[i,j]==1) ", basic" else "")
        html <- c(html, sprintf("<td class='%s'>%s%s</td>", paste(cls, collapse=" "), val, sub))
      }
      html <- c(html, "</tr>")
    }
    html <- c(html, "</tbody></table>")
    
    html <- c(html, "</body></html>")
    
    # file names
    fname <- sprintf("40_%s_modi_it_%03d.html", method_label, snap_id)
    writeLines(html, file.path(outdir, fname))
    
    # index item
    idx <- c(idx, sprintf("<li>Adım %d: <a href='%s'>u–v ve R</a></li>", st$k, fname))
  }
  
  idx <- c(idx, "</ol></body></html>")
  writeLines(idx, idx_path)
  invisible(TRUE)
}

# ============================================================
# MODI snapshots → THREE files per step:
#   41_*_uv.html, 42_*_eqs.html, 43_*_reduced.html  (+ index)
# Works with solve_transport(..., trace=TRUE, optimize= "modi"/"both")
# ============================================================
render_modi_split <- function(tr, outdir, method_label,
                              labels_for_steps = .transport_labels_default(),
                              include_css = TRUE,
                              show_titles = TRUE){
  if (is.null(tr$steps) || !length(tr$steps)) return(invisible())
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  .css_base <- paste0(
    if (include_css) .transport_css_bigM() else "",
    "<style>
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
  idx_path <- file.path(outdir, sprintf("40_%s_modi_index.html", method_label))
  idx <- c("<html><head><meta charset='utf-8'><title>MODI adımları</title>", .css_base, "</head><body>",
           sprintf("<h2>%s – MODI u–v adımları</h2>", method_title(tr$method)),
           "<ol>")
  
  snap_id <- 0L
  for (st in tr$steps){
    # only the MODI snapshots that contain full u/v + R
    if (!(st$stage %in% c("modi-uv"))) next
    snap_id <- snap_id + 1L
    
    C <- tr$cost
    m <- nrow(C); n <- ncol(C)
    lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
    row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
    col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
    
    u <- st$info$u; v <- st$info$v
    R <- st$info$reduced
    basis <- st$info$basis
    if (is.null(basis)) basis <- (st$alloc > 0) + 0
    
    # find minimum negative reduced cost to highlight
    minR <- suppressWarnings(min(R, na.rm = TRUE))
    is_minneg <- matrix(FALSE, m, n)
    if (is.finite(minR) && minR < 0){
      where <- which(!is.na(R) & abs(R - minR) <= 1e-12, arr.ind = TRUE)
      if (nrow(where)) is_minneg[cbind(where[,1], where[,2])] <- TRUE
    }
    
    # Friendly title per step
    cap <- if (!is.null(st$info$note)) st$info$note else "u–v hesap"
    step_title <- sprintf("Adım %d — %s", st$k, cap)
    
    # ---------- (1) u–v TABLES ----------
    {
      fname_uv <- sprintf("41_%s_it_%03d_uv.html", method_label, snap_id)
      html <- c("<html><head><meta charset='utf-8'>", .css_base, "</head><body>")
      if (show_titles) html <- c(html, sprintf("<h3>%s — u–v tabloları</h3>", .html_escape(step_title)))
      html <- c(html, "<div class='uvwrap'>")
      
      # u table
      html <- c(html, "<div class='uvbox'>",
                "<table class='tbl-sim'><thead><tr><th>i</th><th>u<sub>i</sub></th></tr></thead><tbody>")
      for (i in seq_len(m)){
        val <- if (is.na(u[i])) "<span class='na'>&ndash;</span>" else sprintf("%g", u[i])
        html <- c(html, sprintf("<tr><th>%s</th><td>%s</td></tr>", .html_escape(row_names[i]), val))
      }
      html <- c(html, "</tbody></table></div>")
      
      # v table
      html <- c(html, "<div class='uvbox'>",
                "<table class='tbl-sim'><thead><tr><th>j</th><th>v<sub>j</sub></th></tr></thead><tbody>")
      for (j in seq_len(n)){
        val <- if (is.na(v[j])) "<span class='na'>&ndash;</span>" else sprintf("%g", v[j])
        html <- c(html, sprintf("<tr><th>%s</th><td>%s</td></tr>", .html_escape(col_names[j]), val))
      }
      html <- c(html, "</tbody></table></div>")
      
      html <- c(html, "</div></body></html>")
      writeLines(html, file.path(outdir, fname_uv))
    }
    
    # ---------- (2) TÜRETİLEN EŞİTLİKLER ----------
    {
      eqs <- st$info$uv_log
      fname_eq <- sprintf("42_%s_it_%03d_eqs.html", method_label, snap_id)
      html <- c("<html><head><meta charset='utf-8'>", .css_base, "</head><body>")
      if (show_titles) html <- c(html, sprintf("<h3>%s — Türetilen eşitlikler</h3>", .html_escape(step_title)))
      
      if (!is.null(eqs) && length(eqs)){
        html <- c(html, "<div class='eqs'><ol style='margin:6px 0'>")
        for (e in eqs) html <- c(html, sprintf("<li>%s</li>", .html_escape(e$eq)))
        html <- c(html, "</ol></div>")
      } else {
        html <- c(html, "<p class='sub'>Bu adımda türetilmiş eşitlik yok / hesaplanamadı.</p>")
      }
      html <- c(html, "</body></html>")
      writeLines(html, file.path(outdir, fname_eq))
    }
    
    # ---------- (3) REDUCED-COST TABLE (R) ----------
    {
      fname_red <- sprintf("43_%s_it_%03d_reduced.html", method_label, snap_id)
      html <- c("<html><head><meta charset='utf-8'>", .css_base, "</head><body>")
      if (show_titles) html <- c(html, sprintf("<h3>%s — İndirgenmiş maliyet matrisi R</h3>", .html_escape(step_title)))
      html <- c(html, "<table class='tbl-sim'><thead><tr><th></th>")
      for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", .html_escape(col_names[j])))
      html <- c(html, "</tr></thead><tbody>")
      
      for (i in seq_len(m)){
        html <- c(html, sprintf("<tr><th>%s</th>", .html_escape(row_names[i])))
        for (j in seq_len(n)){
          cls <- character(0)
          if (!is.na(R[i,j]) && R[i,j] < 0) cls <- c(cls, "neg")
          if (is_minneg[i,j]) cls <- c(cls, "minneg")
          if (basis[i,j] == 1) cls <- c(cls, "basic")
          val <- if (is.na(R[i,j])) "<span class='na'>&ndash;</span>" else sprintf("%g", R[i,j])
          sub <- sprintf("<div class='sub'>(c=%g%s)</div>", C[i,j], if (basis[i,j]==1) ", basic" else "")
          html <- c(html, sprintf("<td class='%s'>%s%s</td>", paste(cls, collapse=" "), val, sub))
        }
        html <- c(html, "</tr>")
      }
      html <- c(html, "</tbody></table></body></html>")
      writeLines(html, file.path(outdir, fname_red))
    }
    
    # ---------- index entry ----------
    idx <- c(idx, sprintf(
      "<li>Adım %d: <a href='41_%s_it_%03d_uv.html'>u–v</a> · <a href='42_%s_it_%03d_eqs.html'>eşitlikler</a> · <a href='43_%s_it_%03d_reduced.html'>R tablosu</a></li>",
      st$k, method_label, snap_id, method_label, snap_id, method_label, snap_id))
  }
  
  idx <- c(idx, "</ol></body></html>")
  writeLines(idx, idx_path)
  invisible(TRUE)
}


# ============= NEW: allocation table overlaid with u and v (per MODI step) =============
transport_result_with_uv_step_html <- function(cost, alloc, u, v,
                                               supply_total, demand_total,
                                               title = "MODI u–v (adım)",
                                               labels = .transport_labels_default(),
                                               include_css = TRUE,
                                               table_class = "tbl-sim") {
  C <- as.matrix(cost); A <- as.matrix(alloc)
  m <- nrow(C); n <- ncol(C)
  
  # reduced costs (only if u & v known)
  R <- matrix(NA_real_, m, n)
  for (i in seq_len(m)) for (j in seq_len(n)) {
    if (!is.na(u[i]) && !is.na(v[j])) R[i,j] <- C[i,j] - u[i] - v[j]
  }
  # min negative to highlight
  minR <- suppressWarnings(min(R, na.rm = TRUE))
  is_minneg <- matrix(FALSE, m, n)
  if (is.finite(minR) && minR < 0) {
    where <- which(!is.na(R) & abs(R - minR) <= 1e-12, arr.ind = TRUE)
    if (nrow(where)) is_minneg[cbind(where[,1], where[,2])] <- TRUE
  }
  
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  
  supply_cur <- rowSums(A); demand_cur <- colSums(A)
  
  css <- paste0(
    if (include_css) .transport_css_bigM() else "",
    "<style>
      .uvchip{display:inline-block;padding:1px 6px;border:1px solid #adb5bd;border-radius:6px;
              font-size:12px;margin-left:6px;color:#495057;background:#f8f9fa}
      .tbl-sim .neg{color:#b00020;font-weight:600}
      .tbl-sim .minneg{background:#fdecea;border:2px solid #f5c2c7}
      .tbl-sim .basic{background:#eef9f1}
      .tbl-sim .na{color:#adb5bd}
      .tbl-sim .alloc .rc{opacity:.85}
      .tbl-sim .rc{font-size:12px}
    </style>"
  )
  
  H <- .html_escape
  fmt <- function(x) if (is.na(x)) "<span class='na'>&ndash;</span>" else sprintf("%g", x)
  
  html <- c("<html><head><meta charset='utf-8'>", css, "</head><body>",
            sprintf("<h3>%s</h3>", H(title)),
            sprintf("<table class=\"%s\">", H(table_class)))
  
  # header
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)) {
    html <- c(html, sprintf(
      "<th>%s <span class='uvchip'>v<sub>%d</sub> = %s</span></th>",
      H(col_names[j]), j, fmt(v[j])
    ))
  }
  html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", H(lbl$supply_label)))
  
  # body
  for (i in seq_len(m)) {
    html <- c(html, "<tr>")
    html <- c(html, sprintf("<th>%s <span class='uvchip'>u<sub>%d</sub> = %s</span></th>",
                            H(row_names[i]), i, fmt(u[i])))
    for (j in seq_len(n)) {
      is_basic <- A[i,j] > 0
      cls <- character(0)
      if (is_basic) cls <- c(cls, "alloc")
      if (!is.na(R[i,j]) && R[i,j] < 0) cls <- c(cls, "neg")
      if (is_minneg[i,j]) cls <- c(cls, "minneg")
      cell_top <- if (is_basic)
        sprintf("<div class='bold'>%g</div><div class='sub'>(m=%g)</div>", A[i,j], C[i,j])
      else
        sprintf("<div class='muted'>&ndash;</div><div class='sub'>(m=%g)</div>", C[i,j])
      rc_txt <- if (is.na(R[i,j])) "<span class='na'>R=–</span>"
      else sprintf("R = c − u − v = %g − %s − %s = <b>%g</b>",
                   C[i,j],
                   if (is.na(u[i])) "–" else sprintf("%g", u[i]),
                   if (is.na(v[j])) "–" else sprintf("%g", v[j]),
                   R[i,j])
      html <- c(html, sprintf("<td class='%s'>%s<div class='rc sub'>%s</div></td>",
                              paste(cls, collapse=" "), cell_top, rc_txt))
    }
    html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                            supply_cur[i], supply_total[i]), "</tr>")
  }
  
  # demand row
  html <- c(html, sprintf("<tr><th>%s</th>", H(lbl$demand_label)))
  for (j in seq_len(n)) {
    html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                            demand_cur[j], demand_total[j]))
  }
  html <- c(html, sprintf("<td class='rhs bold'>%g <span class='sub'>(%g)</span></td>",
                          sum(A), sum(supply_total)),
            "</tr></tbody></table></body></html>")
  paste(html, collapse="\n")
}

# ============================================================
# MODI: overlay u/v on the final table + (BEFORE & AFTER) transition tables
# + R_ij table. Writes, for each MODI pivot pair (snapshot -> pivot):
#   46_<method>_it_<k>_uv_on_final.html
#   46_<method>_it_<k>_R_on_final.html
#   46_<method>_it_<k>_transition.html          # BEFORE (kept as-is)
#   46_<method>_it_<k>_transition_after.html    # AFTER  (NEW)
# And an index:
#   46_<method>_uv_on_final_index.html
# ============================================================
render_modi_uv_on_final <- function(tr, final_res, outdir,
                                    method_label,
                                    labels_for_steps = .transport_labels_default(),
                                    final_href = NULL,
                                    render_math = FALSE) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  H <- if (exists(".html_escape")) get(".html_escape") else function(x) x
  
  C <- tr$cost
  m <- nrow(C); n <- ncol(C)
  lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else paste0("Kaynak ", seq_len(m))
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else paste0("Hedef ", seq_len(n))
  eps <- if (exists(".eps", inherits = FALSE)) get(".eps") else 1e-9
  
  base_css <- paste0(
    .transport_css_bigM(),
    "<style>
    .tbl-sim caption{font-weight:600}
    .sub{font-size:11px;color:#6c757d}
    .muted{color:#adb5bd}
    .big{font-weight:700}
    .big.zero{color:#6c757d}
    .alloc{background:#fff3cd;border:2px solid #f0ad4e}

    /* R-table helpers */
    .rneg{background:#fdecea;border:2px solid #f5c2c7}
    .rmin{box-shadow: inset 0 0 0 2px #dc3545}

    th.uhead, th.vhead{white-space:nowrap}

    .cbox{
      display:inline-block;border:1px solid #adb5bd;border-radius:6px;
      padding:1px 6px;font-size:12px;color:#495057
    }

    /* === Transition highlights (match 46_*_transition style) === */
    .pivot{
      background:#e8f0fe;                     /* light blue */
      box-shadow: inset 0 0 0 3px #1a73e8;     /* strong blue ring */
    }
    .plus{
      background:#f1faf1;                      /* soft green tint */
      box-shadow: inset 0 0 0 3px #2e7d32;     /* strong green ring */
    }
    .minus{
      background:#fff1f1;                      /* soft red tint */
      box-shadow: inset 0 0 0 3px #c62828;     /* strong red ring */
    }

    .pm{
      display:inline-block;border:1px solid #ced4da;border-radius:999px;
      padding:0 5px;font-size:12px;line-height:18px;min-width:18px;
      text-align:center;margin-right:4px;background:#fff
    }
    .pm.plus{border-color:#2e7d32;color:#2e7d32}
    .pm.minus{border-color:#c62828;color:#c62828}

    .cellwrap{position:relative;min-width:76px;height:46px}
    .corner-note{position:absolute;right:6px;top:6px}
    .xnote{position:absolute;left:6px;bottom:6px;font-size:11px;color:#6c757d}
  </style>"
  )
  
  # --- small helper to render a single allocation table with u/v annotations ---
  render_uv_alloc_table <- function(A, u, v, caption = NULL, file) {
    html <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>")
    html <- c(html, sprintf("<table class='tbl-sim'><caption>%s</caption>", H(ifelse(is.null(caption), "", caption))))
    
    # header with v
    html <- c(html, "<thead><tr><th></th>")
    for (j in seq_len(n)) {
      html <- c(html, sprintf("<th>%s<br><span class='sub'>v<sub>%d</sub> = %s</span></th>",
                              H(col_names[j]), j, ifelse(is.na(v[j]), "&ndash;", sprintf("%g", v[j]))))
    }
    html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", H(lbl$supply_label)))
    
    # body with allocations + u
    for (i in seq_len(m)) {
      html <- c(html, "<tr>")
      html <- c(html, sprintf("<th class='uhead'>%s<br><span class='sub'>u<sub>%d</sub> = %s</span></th>",
                              H(row_names[i]), i, ifelse(is.na(u[i]), "&ndash;", sprintf("%g", u[i]))))
      for (j in seq_len(n)) {
        if (A[i, j] > 0) {
          cell <- sprintf("<div class='big'>%g</div><div class='sub'>(c=%g)</div>", A[i, j], C[i, j])
          html <- c(html, sprintf("<td class='alloc'>%s</td>", cell))
        } else {
          cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(c=%g)</div>", C[i, j])
          html <- c(html, sprintf("<td>%s</td>", cell))
        }
      }
      html <- c(html, sprintf("<td class='rhs bold'>%g</td></tr>", sum(A[i, ])))
    }
    
    # demand row
    html <- c(html, sprintf("<tr><th>%s</th>", H(lbl$demand_label)))
    for (j in seq_len(n)) html <- c(html, sprintf("<td class='rhs bold'>%g</td>", sum(A[, j])))
    html <- c(html, sprintf("<td class='rhs bold'>%g</td></tr>", sum(A)), "</tbody></table>")
    
    html <- c(html, "</body></html>")
    writeLines(html, file)
  }
  
  # --- R = c - u - v table ---
  render_R_table <- function(u, v, R, file) {
    minR <- suppressWarnings(min(R, na.rm = TRUE))
    html <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>")
    html <- c(html, "<table class='tbl-sim'>")
    
    # header
    html <- c(html, "<thead><tr><th></th>")
    for (j in seq_len(n)) html <- c(html, sprintf("<th>%s<br><span class='sub'>v<sub>%d</sub> = %s</span></th>",
                                                  H(col_names[j]), j, ifelse(is.na(v[j]), "&ndash;", sprintf("%g", v[j]))))
    html <- c(html, "</tr></thead><tbody>")
    
    # body
    for (i in seq_len(m)) {
      html <- c(html, "<tr>", sprintf("<th>%s<br><span class='sub'>u<sub>%d</sub> = %s</span></th>",
                                      H(row_names[i]), i, ifelse(is.na(u[i]), "&ndash;", sprintf("%g", u[i]))))
      for (j in seq_len(n)) {
        val <- R[i, j]
        cls <- character(0)
        if (!is.na(val) && val < 0) cls <- c(cls, "rneg")
        if (is.finite(minR) && !is.na(val) && abs(val - minR) <= 1e-12) cls <- c(cls, "rmin")
        stamp <- if (is.na(val)) "<span class='muted'>&ndash;</span>" else sprintf("%g", val)
        html <- c(html, sprintf("<td class='%s'><div class='cbox'>c=%g</div><div>%s</div></td>",
                                paste(cls, collapse = " "), C[i, j], stamp))
      }
      html <- c(html, "</tr>")
    }
    html <- c(html, "</tbody></table></body></html>")
    writeLines(html, file)
  }
  
  # --- find loop (like inside solve_transport) on a basic pattern before pivot ---
  find_loop <- function(is_basic, ie, je) {
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
    loop <- matrix(c(ie, je), ncol = 2L, byrow = TRUE)
    if (length(nodes) >= 2L) {
      for (k in seq_len(length(nodes) - 1L)) {
        a <- nodes[k]; b <- nodes[k + 1L]
        if (a <= m && b > m) { i <- a; j <- b - m
        } else if (a > m && b <= m) { i <- b; j <- a - m
        } else next
        if (!(i == ie && j == je)) loop <- rbind(loop, c(i, j))
      }
    }
    loop
  }
  
  # --- Transition table (no captions/notes). Shows current values for cycle cells even if 0 ---
  render_transition <- function(A_after, A_before, ie, je, loop_path, file) {
    signs <- rep(c(1, -1), length.out = nrow(loop_path))
    plus  <- loop_path[signs ==  1, , drop = FALSE]
    minus <- loop_path[signs == -1, , drop = FALSE]
    
    is_pivot_cell <- function(i, j) i == ie && j == je
    is_plus  <- function(i, j) any(plus[, 1] == i & plus[, 2] == j)
    is_minus <- function(i, j) any(minus[, 1] == i & minus[, 2] == j)
    
    html <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>")
    html <- c(html, "<table class='tbl-sim'>")
    
    # header
    html <- c(html, "<thead><tr><th></th>")
    for (j in seq_len(n)) html <- c(html, sprintf("<th>%s</th>", H(col_names[j])))
    html <- c(html, "<th>Δ</th></tr></thead><tbody>")
    
    col_delta <- rep(0, n)
    
    for (i in seq_len(m)) {
      html <- c(html, "<tr>", sprintf("<th>%s</th>", H(row_names[i])))
      for (j in seq_len(n)) {
        cls <- character(0)
        badge <- ""
        if (is_pivot_cell(i, j)) {
          cls <- c(cls, "pivot"); badge <- "<span class='pm'>+</span>"
        } else if (is_plus(i, j)) {
          cls <- c(cls, "plus"); badge <- "<span class='pm plus'>+</span>"
          col_delta[j] <- col_delta[j] + (A_after[i, j] - A_before[i, j])
        } else if (is_minus(i, j)) {
          cls <- c(cls, "minus"); badge <- "<span class='pm minus'>&minus;</span>"
          col_delta[j] <- col_delta[j] + (A_after[i, j] - A_before[i, j])
        }
        
        # always show value for cycle cells; otherwise show – for zero
        in_cycle <- is_pivot_cell(i, j) || is_plus(i, j) || is_minus(i, j)
        big_now <- if (in_cycle) {
          sprintf("<div class='big%s'>%g</div>",
                  if (abs(A_after[i, j]) < eps) " zero" else "",
                  A_after[i, j])
        } else {
          if (A_after[i, j] > 0)
            sprintf("<div class='big'>%g</div>", A_after[i, j])
          else
            "<div class='muted'>&ndash;</div>"
        }
        
        xline <- ""
        if (in_cycle) {
          xline <- sprintf("<div class='xnote'>x: %g &rarr; %g</div>", A_before[i, j], A_after[i, j])
        }
        
        cell <- sprintf(
          "<div class='cellwrap'>
             <div class='corner-note'><span class='cbox'>c=%g</span></div>
             %s%s
           </div>",
          C[i, j], badge, big_now
        )
        
        html <- c(html, sprintf("<td class='%s'>%s%s</td>", paste(cls, collapse = " "), cell, xline))
      }
      # row delta
      rdelta <- sum(A_after[i, ] - A_before[i, ])
      html <- c(html, sprintf("<td class='rhs'>%s%g</td>",
                              if (rdelta > 0) "+" else "", rdelta))
      html <- c(html, "</tr>")
    }
    
    # bottom Δ per column
    html <- c(html, "<tr><th>Toplam</th>")
    for (j in seq_len(n)) {
      html <- c(html, sprintf("<td class='rhs'>%s%g</td>",
                              if (col_delta[j] > 0) "+" else "", col_delta[j]))
    }
    html <- c(html, sprintf("<td class='rhs'>%s%g</td>",
                            if (sum(col_delta) > 0) "+" else "", sum(col_delta)), "</tr>")
    
    html <- c(html, "</tbody></table></body></html>")
    writeLines(html, file)
  }
  
  # ---------- Pair up: (modi-uv snapshot) -> (next modi pivot) ----------
  steps <- tr$steps
  idx_items <- character(0)
  k_pair <- 0
  
  i <- 1
  while (i <= length(steps)) {
    st <- steps[[i]]
    if (!identical(st$stage, "modi-uv")) { i <- i + 1; next }
    
    # find next 'modi' after this snapshot
    j <- i + 1
    while (j <= length(steps) && !identical(steps[[j]]$stage, "modi")) j <- j + 1
    if (j > length(steps)) break
    
    st_uv <- steps[[i]]
    st_pv <- steps[[j]]
    k_pair <- k_pair + 1
    
    u <- st_uv$info$u; v <- st_uv$info$v; R <- st_uv$info$reduced
    A_before <- st_uv$alloc + 0
    A_after  <- st_pv$alloc + 0
    ie <- as.integer(st_pv$i); je <- as.integer(st_pv$j)
    theta <- as.numeric(st_pv$x_ij)
    
    # Build basis BEFORE pivot (prefer snapshot basis if present)
    if (!is.null(st_uv$info$basis)) {
      is_basic_before <- st_uv$info$basis == 1
    } else {
      is_basic_before <- A_before > 0
    }
    
    loop <- find_loop(is_basic_before, ie, je)
    if (is.null(loop) || nrow(loop) < 4L) {
      # fallback: still render uv+R tables, but transition-after won't exist
      loop <- matrix(c(ie, je), ncol = 2L)
    }
    
    # Reconstruct A_before from A_after if needed (kept for reference)
    if (nrow(loop) >= 2) {
      signs <- rep(c(1, -1), length.out = nrow(loop))
      A_before_rec <- A_after
      for (kk in seq_len(nrow(loop))) {
        i0 <- loop[kk, 1]; j0 <- loop[kk, 2]
        A_before_rec[i0, j0] <- A_before_rec[i0, j0] - signs[kk] * theta
      }
      # prefer the true snapshot if available
      if (all(dim(A_before) == dim(A_before_rec))) A_before <- A_before
    }
    
    # ------- Write files -------
    f_uv          <- file.path(outdir, sprintf("46_%s_it_%03d_uv_on_final.html",      method_label, k_pair))
    f_R           <- file.path(outdir, sprintf("46_%s_it_%03d_R_on_final.html",       method_label, k_pair))
    f_trn_before  <- file.path(outdir, sprintf("46_%s_it_%03d_transition.html",       method_label, k_pair))       # keep old name
    f_trn_after   <- file.path(outdir, sprintf("46_%s_it_%03d_transition_after.html", method_label, k_pair))       # NEW
    
    cap <- sprintf("(Toplam Maliyet = %g)%s",
                   sum(A_before * C),
                   if (!is.null(final_href)) sprintf(" – <a href='%s'>Nihai dağıtıma dön</a>", H(final_href)) else "")
    
    render_uv_alloc_table(A_before, u, v, caption = cap, file = f_uv)
    render_R_table(u, v, R, file = f_R)
    
    # BEFORE page (same as today)
    render_transition(
      A_after   = A_before,     # identical -> shows “before” state
      A_before  = A_before,
      ie = ie, je = je,
      loop_path = loop,
      file = f_trn_before
    )
    
    # AFTER page (NEW) — only if we have a valid loop/pivot
    has_after <- (!is.null(loop) && nrow(loop) >= 4L)
    if (has_after) {
      render_transition(
        A_after   = A_after,    # real post-pivot allocations
        A_before  = A_before,   # pre-pivot allocations
        ie = ie, je = je,
        loop_path = loop,
        file = f_trn_after
      )
    }
    
    # index entry
    link_after <- if (has_after) sprintf(" / <a href='%s'>sonrası</a>", basename(f_trn_after)) else ""
    idx_items <- c(idx_items,
                   sprintf("<li>Adım %d: <a href='%s'>u/v + dağıtım</a> · <a href='%s'>R<sub>ij</sub></a> · Geçiş: <a href='%s'>öncesi</a>%s</li>",
                           k_pair,
                           basename(f_uv), basename(f_R), basename(f_trn_before), link_after))
    
    i <- j + 1
  }
  
  # ---------- index ----------
  idx_file <- file.path(outdir, sprintf("46_%s_uv_on_final_index.html", method_label))
  idx <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>",
           sprintf("<h2>%s – MODI (u/v, R<sub>ij</sub>, Geçiş)</h2>", H(method_label)),
           "<ol>", idx_items, "</ol>",
           "</body></html>")
  writeLines(idx, idx_file)
  invisible(TRUE)
}





# ---------- helpers for numeric formatting & HTML escape ----------
.fnum <- function(v){
  if (isTRUE(all.equal(v, round(v)))) sprintf("%d", as.integer(round(v))) else sprintf("%g", v)
}
H <- function(x) .html_escape(x)

# ============= (NEW) Export MODI "Türetilen eşitlikler" to a *raw LaTeX* HTML =============
# - u, v: numeric vectors from a MODI step (st$info$u, st$info$v)
# - C   : cost matrix
# - uv_log: st$info$uv_log (list of derivations we used to compute u/v)
# - file: output path; title: page heading
# - render_math: TRUE to also preview via MathJax (the LaTeX is still shown raw)
transport_export_latex_uv_eqs_from_step <- function(u, v, C, uv_log, file,
                                                    title = "LaTeX – Türetilen eşitlikler (u–v)",
                                                    render_math = FALSE,
                                                    mathjax_src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"){
  if (is.null(uv_log) || !length(uv_log)) {
    uv_log <- list()
  }
  # build LaTeX derivations *in the order they were derived*
  lines <- character(0)
  for (e in uv_log){
    if (!is.list(e) || is.null(e$kind)) next
    i <- e$i; j <- e$j
    if (e$kind == "v"){
      lines <- c(lines, sprintf("v_{%d} = c_{%d,%d} - u_{%d} = %s - %s = %s",
                                j, i, j, i, .fnum(C[i,j]), .fnum(u[i]), .fnum(v[j])))
    } else if (e$kind == "u"){
      lines <- c(lines, sprintf("u_{%d} = c_{%d,%d} - v_{%d} = %s - %s = %s",
                                i, i, j, j, .fnum(C[i,j]), .fnum(v[j]), .fnum(u[i])))
    }
  }
  if (!length(lines)){
    lines <- c("% Bu adımda u–v eşitlikleri türetilemedi veya güncellenmedi.")
  }
  
  eq <- sprintf("\\[\\begin{aligned}\n%s\n\\end{aligned}\\]",
                paste(paste0(lines, " \\\\"), collapse="\n"))
  
  # write minimal HTML with RAW LaTeX block (+ optional preview)
  html <- c("<!doctype html>",
            "<html><head><meta charset='utf-8'><title>LaTeX u–v</title>",
            "<style>body{font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:20px}",
            "h3{margin:0 0 8px 0}",
            ".raw{white-space:pre-wrap;background:#f8f9fa;border:1px solid #e9ecef;padding:12px;border-radius:6px;",
            "     font-family:ui-monospace,Consolas,Monaco,monospace}",
            ".note{color:#6c757d;font-size:13px;margin-top:8px}</style>")
  if (render_math){
    html <- c(html, sprintf("<script id='MathJax-script' async src='%s'></script>", H(mathjax_src)))
  }
  html <- c(html, "</head><body>",
            sprintf("<h3>%s</h3>", H(title)),
            sprintf("<div class='raw'>%s</div>", H(eq)))
  if (render_math){
    html <- c(html, "<h4>Önizleme (MathJax)</h4>", eq)
  }
  html <- c(html, "<div class='note'>Yukarıdaki blok <b>ham LaTeX</b> içermektedir.</div>",
            "</body></html>")
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  writeLines(html, file)
  invisible(eq)
}

# ============= (NEW) Export all R_ij calculations to *raw LaTeX* (+ HTML matrix) =============
# - only_defined: if TRUE, include only cells where both u_i and v_j are known (non-NA)
transport_export_latex_rij_from_step <- function(cost, u, v, file,
                                                 title = "LaTeX – R_{ij} Hesapları",
                                                 only_defined = TRUE,
                                                 render_math = FALSE,
                                                 mathjax_src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"){
  C <- as.matrix(cost); m <- nrow(C); n <- ncol(C)
  R <- matrix(NA_real_, m, n)
  for (i in seq_len(m)) for (j in seq_len(n)) {
    if (!is.na(u[i]) && !is.na(v[j])) R[i,j] <- C[i,j] - u[i] - v[j]
  }
  
  # Build LaTeX lines
  lines <- character(0)
  for (i in seq_len(m)) for (j in seq_len(n)) {
    if (is.na(R[i,j]) && only_defined) next
    lhs <- sprintf("R_{%d,%d}", i, j)
    if (is.na(R[i,j])) {
      lines <- c(lines, sprintf("%s = c_{%d,%d} - u_{%d} - v_{%d} \\quad (\\text{tanımsız})",
                                lhs, i, j, i, j))
    } else {
      lines <- c(lines, sprintf("%s = c_{%d,%d} - u_{%d} - v_{%d} = %s - %s - %s = %s",
                                lhs, i, j, i, j,
                                .fnum(C[i,j]), .fnum(u[i]), .fnum(v[j]), .fnum(R[i,j])))
    }
  }
  if (!length(lines)){
    lines <- c("% Bu adımda hesaplanabilir R_{ij} yok (u veya v eksik).")
  }
  latex_block <- sprintf("\\[\\begin{aligned}\n%s\n\\end{aligned}\\]",
                         paste(paste0(lines, " \\\\"), collapse="\n"))
  
  # HTML table (matrix) of R values for quick reading
  css <- "<style>
    body{font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:20px}
    h3{margin:0 0 8px 0}
    table{border-collapse:collapse;margin:8px 0}
    th,td{border:1px solid #e9ecef;padding:6px 10px;text-align:center}
    thead th{background:#f1f3f5}
    .na{color:#adb5bd}
    .neg{color:#b00020;font-weight:600}
    .raw{white-space:pre-wrap;background:#f8f9fa;border:1px solid #e9ecef;padding:12px;border-radius:6px;font-family:ui-monospace,Consolas,Monaco,monospace}
  </style>"
  
  tbl <- c("<table><thead><tr><th></th>")
  for (j in seq_len(n)) tbl <- c(tbl, sprintf("<th>j=%d</th>", j))
  tbl <- c(tbl, "</tr></thead><tbody>")
  for (i in seq_len(m)){
    tbl <- c(tbl, sprintf("<tr><th>i=%d</th>", i))
    for (j in seq_len(n)){
      if (is.na(R[i,j])) {
        tbl <- c(tbl, "<td class='na'>&ndash;</td>")
      } else {
        cls <- if (R[i,j] < 0) "neg" else ""
        tbl <- c(tbl, sprintf("<td class='%s'>%s</td>", cls, .fnum(R[i,j])))
      }
    }
    tbl <- c(tbl, "</tr>")
  }
  tbl <- c(tbl, "</tbody></table>")
  
  # Write HTML with RAW LaTeX (+ optional rendered preview)
  html <- c("<!doctype html><html><head><meta charset='utf-8'><title>LaTeX R_ij</title>",
            css)
  if (render_math){
    html <- c(html, sprintf("<script id='MathJax-script' async src='%s'></script>", H(mathjax_src)))
  }
  html <- c(html, "</head><body>",
            sprintf("<h3>%s</h3>", H(title)),
            tbl,
            sprintf("<div class='raw'>%s</div>", H(latex_block)))
  if (render_math){
    html <- c(html, "<h4>Önizleme (MathJax)</h4>", latex_block)
  }
  html <- c(html, "</body></html>")
  dir.create(dirname(file), showWarnings = FALSE, recursive = TRUE)
  writeLines(html, file)
  invisible(latex_block)
}

# ============================================================
# MODI: per-step package writer
#  - For each MODI u–v snapshot (trace step with stage=="modi-uv"):
#       (A) HTML — allocation table annotated with u_i and v_j
#       (B) HTML — raw LaTeX block of the "türetilen eşitlikler" (u/v derivations)
#       (C) HTML — R_ij table (and values) + also RAW LaTeX for all R_ij lines
#  - Additionally writes .tex files with the RAW LaTeX for (B) and (C)
# ============================================================

# ----- tiny numeric formatter (int if it looks like one)
.mf <- function(x){
  if (is.na(x)) return("NA")
  if (isTRUE(all.equal(x, round(x), tol=1e-12))) return(sprintf("%d", as.integer(round(x))))
  sprintf("%g", x)
}

# ----- build RAW LaTeX for the u/v derivation log in one step
modi_uv_latex_snapshot <- function(C, st){
  # st$info$uv_log is a list of items with fields: kind ("u" or "v"), i, j
  # and we also have st$info$u, st$info$v
  u <- st$info$u; v <- st$info$v
  log <- st$info$uv_log
  if (is.null(log) || !length(log)) {
    return("\\[\\text{Bu adım için u\\,–\\,v türetimi bulunamadı.}\\]\n")
  }
  lines <- character()
  for (e in log){
    i <- e$i; j <- e$j
    if (isTRUE(e$kind=="v")) {
      lines <- c(lines, sprintf("v_{%d} &= c_{%d,%d} - u_{%d} = %s - %s = %s \\\\",
                                j, i, j, i, .mf(C[i,j]), .mf(u[i]), .mf(v[j])))
    } else {
      lines <- c(lines, sprintf("u_{%d} &= c_{%d,%d} - v_{%d} = %s - %s = %s \\\\",
                                i, i, j, j, .mf(C[i,j]), .mf(v[j]), .mf(u[i])))
    }
  }
  paste0("\\[\n\\begin{aligned}\n", paste(lines, collapse="\n"),
         "\n\\end{aligned}\n\\]\n")
}

# ----- build RAW LaTeX for all available R_ij in one step
modi_Rij_latex_snapshot <- function(C, st){
  u <- st$info$u; v <- st$info$v
  m <- nrow(C); n <- ncol(C)
  lines <- character()
  for (i in seq_len(m)){
    for (j in seq_len(n)){
      if (is.na(u[i]) || is.na(v[j])) next
      Rij <- C[i,j] - u[i] - v[j]
      lines <- c(lines,
                 sprintf("R_{%d,%d} &= c_{%d,%d} - u_{%d} - v_{%d} = %s - %s - %s = %s \\\\",
                         i, j, i, j, i, j, .mf(C[i,j]), .mf(u[i]), .mf(v[j]), .mf(Rij))
      )
    }
    if (length(lines)) lines[length(lines)] <- sub("\\\\\\\\$", "", lines[length(lines)])
  }
  if (!length(lines)) return("\\[\\text{Bu adımda R_{ij} hesaplanamadı.}\\]\n")
  paste0("\\[\n\\begin{aligned}\n", paste(lines, collapse="\n"),
         "\n\\end{aligned}\n\\]\n")
}

# ----- u/v annotated allocation table as HTML
transport_result_with_uv_html <- function(cost, alloc, u, v,
                                          title = "",
                                          labels = .transport_labels_default(),
                                          include_css = TRUE,
                                          table_class = "tbl-sim"){
  C <- as.matrix(cost); A <- as.matrix(alloc)
  m <- nrow(C); n <- ncol(C)
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  tot <- sum(C * A)
  
  css2 <- "
  <style>
    .uv{display:block; font-size:11px; color:#495057; margin-top:2px}
    .uvrow{white-space:nowrap;}
  </style>"
  
  H <- .html_escape
  html <- c()
  if (include_css) html <- c(html, .transport_css_bigM(), css2)
  
  html <- c(html, sprintf("<table class=\"%s\">", H(table_class)))
  html <- c(html, sprintf("<caption>%s (%s = <b>%s</b>)</caption>",
                          H(title), H(lbl$total_cost), H(.mf(tot))))
  # header with v_j
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)){
    html <- c(html, sprintf("<th>%s<span class='uv'>v<sub>%d</sub> = %s</span></th>",
                            H(col_names[j]), j, H(.mf(v[j]))))
  }
  html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", H(lbl$supply_label)))
  
  # body with u_i next to row name
  supply <- rowSums(A)
  for (i in seq_len(m)){
    html <- c(html, "<tr>")
    html <- c(html, sprintf("<th class='uvrow'>%s<span class='uv'>u<sub>%d</sub> = %s</span></th>",
                            H(row_names[i]), i, H(.mf(u[i]))))
    for (j in seq_len(n)){
      if (A[i,j] > 0){
        cell <- sprintf("<div class='bold'>%s</div><div class='sub'>(m=%s)</div>", .mf(A[i,j]), .mf(C[i,j]))
        html <- c(html, sprintf("<td class='alloc'>%s</td>", cell))
      } else {
        cell <- sprintf("<div class='muted'>&ndash;</div><div class='sub'>(m=%s)</div>", .mf(C[i,j]))
        html <- c(html, sprintf("<td>%s</td>", cell))
      }
    }
    html <- c(html, sprintf("<td class='rhs bold'>%s</td>", .mf(supply[i])), "</tr>")
  }
  
  # demand
  demand <- colSums(A)
  html <- c(html, sprintf("<tr><th>%s</th>", H(lbl$demand_label)))
  for (j in seq_len(n)) html <- c(html, sprintf("<td class='rhs bold'>%s</td>", .mf(demand[j])))
  html <- c(html, sprintf("<td class='rhs bold'>%s</td>", .mf(sum(supply))), "</tr></tbody></table>")
  paste(html, collapse="\n")
}

# ----- R_ij table for a step (HTML)
transport_Rij_table_html <- function(cost, u, v,
                                     labels = .transport_labels_default(),
                                     include_css = TRUE,
                                     table_class = "tbl-sim",
                                     title = "R_{ij} Hesapları"){
  C <- as.matrix(cost); m <- nrow(C); n <- ncol(C)
  H <- .html_escape
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  row_names <- if (!is.null(lbl$row_names)) lbl$row_names else .mk_names(lbl$row_prefix, m)
  col_names <- if (!is.null(lbl$col_names)) lbl$col_names else .mk_names(lbl$col_prefix, n)
  
  css <- "<style>.rij{font-size:12px;color:#495057}</style>"
  html <- c()
  if (include_css) html <- c(html, .transport_css_bigM(), css)
  html <- c(html, sprintf("<table class=\"%s\">", H(table_class)))
  html <- c(html, sprintf("<caption>%s</caption>", H(title)))
  # header
  html <- c(html, "<thead><tr><th></th>")
  for (j in seq_len(n)){
    html <- c(html, sprintf("<th>%s<span class='uv'>v<sub>%d</sub>=%s</span></th>", H(col_names[j]), j, H(.mf(v[j]))))
  }
  html <- c(html, "</tr></thead><tbody>")
  
  for (i in seq_len(m)){
    html <- c(html, sprintf("<tr><th>%s<span class='uv'>u<sub>%d</sub>=%s</span></th>", H(row_names[i]), i, H(.mf(u[i]))))
    for (j in seq_len(n)){
      if (is.na(u[i]) || is.na(v[j])) {
        html <- c(html, "<td><span class='muted'>&ndash;</span></td>")
      } else {
        Rij <- C[i,j] - u[i] - v[j]
        line <- sprintf("R<sub>%d,%d</sub>=%s-%s-%s=%s", i, j, .mf(C[i,j]), .mf(u[i]), .mf(v[j]), .mf(Rij))
        html <- c(html, sprintf("<td class='rij'>%s</td>", line))
      }
    }
    html <- c(html, "</tr>")
  }
  html <- c(html, "</tbody></table>")
  paste(html, collapse="\n")
}

# ----- Write the three HTML files + two RAW LaTeX files per step
render_modi_step_packages <- function(tr, outdir, method_label,
                                      labels_for_steps = .transport_labels_default(),
                                      math_preview = FALSE){
  if (is.null(tr$steps) || !length(tr$steps)) return(invisible())
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  # -------- base CSS (adds .neg styling for R<0) --------
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
      .pivot{
        background:#e8f0fe !important;                 /* light blue */
        box-shadow: inset 0 0 0 3px #1a73e8; /* strong blue ring */
        }

    </style>"
  )
  
  # small helper that writes a UV-only table (what you already had)
  write_uv_table <- function(step_id, st){
    C <- tr$cost; A <- st$alloc + 0
    u <- st$info$u; v <- st$info$v
    m <- nrow(C); n <- ncol(C)
    lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
    rn <- if (is.null(lbl$row_names)) .mk_names(lbl$row_prefix,m) else lbl$row_names
    cn <- if (is.null(lbl$col_names)) .mk_names(lbl$col_prefix,n) else lbl$col_names
    
    html <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>")
    html <- c(html, sprintf("<div class='hdr'>%s</div>", ""))
    html <- c(html, sprintf("<table class='tbl-sim'><thead><tr><th></th>"))
    for (j in 1:n) html <- c(html, sprintf("<th>%s<br><span class='uv'>v<sub>%d</sub> = %g</span></th>", .html_escape(cn[j]), j, v[j]))
    html <- c(html, sprintf("<th class='rhs'>%s</th></tr></thead><tbody>", .html_escape(lbl$supply_label)))
    
    for (i in 1:m){
      html <- c(html, "<tr>")
      html <- c(html, sprintf("<th>%s<br><span class='uv'>u<sub>%d</sub> = %g</span></th>", .html_escape(rn[i]), i, u[i]))
      for (j in 1:n){
        if (A[i,j] > 0){
          cell <- sprintf("<div class='big'>%g</div><div class='sub'>(m=%g)</div>", A[i,j], C[i,j])
          html <- c(html, sprintf("<td class='alloc'>%s</td>", cell))
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
  
  # NEW: UV + R table (Rij printed; negatives highlighted)
  write_uvR_table <- function(step_id, st){
    C <- tr$cost; A <- st$alloc + 0
    u <- st$info$u; v <- st$info$v; R <- st$info$reduced
    m <- nrow(C); n <- ncol(C)
    lbl <- modifyList(.transport_labels_default(), labels_for_steps, keep.null = TRUE)
    rn <- if (is.null(lbl$row_names)) .mk_names(lbl$row_prefix,m) else lbl$row_names
    cn <- if (is.null(lbl$col_names)) .mk_names(lbl$col_prefix,n) else lbl$col_names
    
    # --- pivot = most negative reduced cost (if any)
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
        if (A[i,j] > 0) cls <- c(cls, "alloc")     # keep yellow for basic cells
        if (!is.na(r) && r < 0) cls <- c(cls, "neg")
        if (is_pivot[i,j])       cls <- c(cls, "pivot") # <-- blue pivot ring (overrides)
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
    
    # --- write LaTeX to a SEPARATE file and link to it
    latex_file <- sprintf("50_%s_step_%03d_rij_latex.html", method_label, step_id)
    transport_export_latex_rij_from_step(
      cost = C, u = u, v = v,
      file = file.path(outdir, latex_file),
      title = "R_{ij} hesap (LaTeX – ham)",
      render_math = FALSE
    )
    # html <- c(html,
    #           sprintf("<p style='margin-top:8px'><a href='%s'>R_{ij} LaTeX (ayrı dosya)</a></p>",
    #                   .html_escape(latex_file)),
    #           "</body></html>")
    
    file <- sprintf("50_%s_step_%03d_uvR_table.html", method_label, step_id)
    writeLines(html, file.path(outdir, file))
    file
  }
  
  
  # write a small per-method index
  idx_path <- file.path(outdir, sprintf("50_%s_modi_pkg_index.html", method_label))
  idx <- c("<html><head><meta charset='utf-8'>", base_css, "</head><body>",
           sprintf("<h2>%s – MODI Paketleri</h2>", method_title(tr$method)),
           "<ol>")
  
  snap_id <- 0
  for (st in tr$steps){
    if (!(st$stage %in% c("modi-uv","modi"))) next
    # only log the uv-snapshot pages
    if (st$stage != "modi-uv") next
    snap_id <- snap_id + 1
    
    f1 <- write_uv_table(snap_id, st)      # existing UV-only
    f2 <- write_uvR_table(snap_id, st)     # NEW: UV+R with negatives in red
    
    idx <- c(idx,
             sprintf("<li>Adım %d: <a href='%s'>u/v tablo</a> · <a href='%s'>u/v + R<sub>ij</sub></a></li>",
                     st$k, f1, f2))
  }
  idx <- c(idx, "</ol></body></html>")
  writeLines(idx, idx_path)
  invisible(TRUE)
}


