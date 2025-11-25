
################################################################################
#### FUNCTION THAT BALANCES TABLE ##############################################
################################################################################

balance_transport <- function(cost, supply, demand) {
  ts <- sum(supply)
  td <- sum(demand)
  
  if (ts < td) {
    supply <- c(supply, td - ts)
    cost <- rbind(cost, rep(0, ncol(cost)))
    rownames(cost)[nrow(cost)] <- "DummySource"
  } else if (ts > td) {
    demand <- c(demand, ts - td)
    cost <- cbind(cost, rep(0, nrow(cost)))
    colnames(cost)[ncol(cost)] <- "DummyDest"
  }
  
  list(cost = cost, supply = supply, demand = demand)
}


################################################################################
#### FUNCTION FOR NORTHWEST ####################################################
################################################################################

northwest_corner <- function(cost, supply, demand) {
  
  m <- length(supply)
  n <- length(demand)
  
  alloc <- matrix(0, m, n)
  rownames(alloc) <- rownames(cost)
  colnames(alloc) <- colnames(cost)
  
  s_left <- supply
  d_left <- demand
  
  i <- 1; j <- 1
  steps <- list()
  
  while (i <= m && j <= n) {
    x <- min(s_left[i], d_left[j])
    alloc[i, j] <- x
    
    s_left[i] <- s_left[i] - x
    d_left[j] <- d_left[j] - x
    
    if (s_left[i] == 0 && d_left[j] == 0) {
      if (j < n) {
        j <- j + 1 
        } else {
          i <- i + 1
          }
    } else if (s_left[i] == 0) {
      i <- i + 1
    } else {
      j <- j + 1
    }
  }
  
  total_cost <- sum(alloc * cost)
  
  out <- list(
    allocation = alloc,
    total_cost = total_cost,
    balanced_cost = cost,
    balanced_supply = supply,
    balanced_demand = demand,
    steps = steps
  )
  
  return(out)
}


################################################################################
#### FUNCTION FOR LEASTCOST ####################################################
################################################################################



least_cost_method <- function(cost, supply, demand) {
  
  m <- length(supply); n <- length(demand)
  
  alloc <- matrix(0, m, n,
                  dimnames = list(rownames(cost), colnames(cost)))
  
  s_left <- supply
  d_left <- demand
  
  active_rows <- rep(TRUE, m)
  active_cols <- rep(TRUE, n)
  
  steps <- list()
  
  while (any(active_rows) && any(active_cols)) {
    
    # --- 1) find minimum-cost feasible cell ---
    min_val <- Inf
    min_i <- NA
    min_j <- NA
    
    for (i in which(active_rows)) {
      for (j in which(active_cols)) {
        if (cost[i, j] < min_val) {
          min_val <- cost[i, j]
          min_i <- i
          min_j <- j
        }
      }
    }
    
    # --- 2) allocate ---
    x <- min(s_left[min_i], d_left[min_j])
    alloc[min_i, min_j] <- x
    
    # update remaining
    s_left[min_i] <- s_left[min_i] - x
    d_left[min_j] <- d_left[min_j] - x
    
    # --- 3) deactivate satisfied row/col ---
    if (s_left[min_i] == 0) active_rows[min_i] <- FALSE
    if (d_left[min_j] == 0) active_cols[min_j] <- FALSE
    
    # Degeneracy note:
    # If both become zero at the same time, this is a degenerate corner.
    # Standard fix is to keep one active with epsilon later.
    # Here we simply deactivate both; OK for teaching initial solution.
  }
  
  total_cost <- sum(alloc * cost)
  
  list(
    allocation = alloc,
    total_cost = total_cost,
    steps = steps
  )
}

################################################################################
#### FUNCTION FOR VAM ##########################################################
################################################################################


vam_method <- function(cost, supply, demand) {
  
  m <- length(supply); n <- length(demand)
  
  alloc <- matrix(0, m, n,
                  dimnames = list(rownames(cost), colnames(cost)))
  
  s_left <- supply
  d_left <- demand
  
  active_rows <- rep(TRUE, m)
  active_cols <- rep(TRUE, n)
  
  steps <- list()
  
  # ---- helper to compute penalty of a vector of costs (active only) ----
  penalty_of <- function(x) {
    x_sorted <- sort(x)
    if (length(x_sorted) == 1) return(x_sorted[1])  # only one option left
    x_sorted[2] - x_sorted[1]
  }
  
  while (any(active_rows) && any(active_cols)) {
    
    # ---- 1) compute row penalties ----
    row_pen <- rep(NA_real_, m)
    for (i in which(active_rows)) {
      row_pen[i] <- penalty_of(cost[i, active_cols, drop = TRUE])
    }
    
    # ---- 2) compute column penalties ----
    col_pen <- rep(NA_real_, n)
    for (j in which(active_cols)) {
      col_pen[j] <- penalty_of(cost[active_rows, j, drop = TRUE])
    }
    
    # ---- 3) pick max penalty among rows/cols ----
    max_row_pen <- max(row_pen, na.rm = TRUE)
    max_col_pen <- max(col_pen, na.rm = TRUE)
    
    choose_row <- (max_row_pen >= max_col_pen)
    
    if (choose_row) {
      i_star <- which(row_pen == max_row_pen)[1]  # tie: take first
      # in that row pick min cost among active cols
      j_star <- which(active_cols)[which.min(cost[i_star, active_cols])]
    } else {
      j_star <- which(col_pen == max_col_pen)[1]
      # in that col pick min cost among active rows
      i_star <- which(active_rows)[which.min(cost[active_rows, j_star])]
    }
    
    # ---- 4) allocate ----
    x <- min(s_left[i_star], d_left[j_star])
    alloc[i_star, j_star] <- x
    
    
    # update remaining
    s_left[i_star] <- s_left[i_star] - x
    d_left[j_star] <- d_left[j_star] - x
    
    # ---- 5) deactivate satisfied row/col ----
    if (s_left[i_star] == 0) active_rows[i_star] <- FALSE
    if (d_left[j_star] == 0) active_cols[j_star] <- FALSE
    
    # degeneracy note:
    # if both become zero together, this creates degeneracy.
    # For initial solution it’s fine; for MODI later you may need ε.
  }
  
  total_cost <- sum(alloc * cost)
  
  list(
    allocation = alloc,
    total_cost = total_cost,
    steps = steps
  )
}
