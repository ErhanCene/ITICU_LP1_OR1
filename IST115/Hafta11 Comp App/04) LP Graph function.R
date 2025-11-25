solve_lp_graphical <- function(A, b, cvec,
                               sense = rep("<=", nrow(A)),
                               maximize = TRUE,
                               xlim = NULL, ylim = NULL,
                               grid_step = 0.02,
                               tol = 1e-9,
                               plot = TRUE,
                               show_labels = TRUE,
                               colors = NULL) {
  # ---------------------------
  # Basic checks
  # ---------------------------
  A <- as.matrix(A)
  stopifnot(ncol(A) == 2)
  stopifnot(length(b) == nrow(A))
  stopifnot(length(cvec) == 2)
  stopifnot(length(sense) == nrow(A))
  stopifnot(all(sense %in% c("<=", ">=", "=")))
  
  c1 <- cvec[1]; c2 <- cvec[2]
  
  # Default colors
  if (is.null(colors)) {
    colors <- c("red","darkgreen","purple","orange","brown","darkcyan")
  }
  colors <- rep(colors, length.out = nrow(A))  # recycle if needed
  
  # ---------------------------
  # Feasibility function (mixed senses)
  # ---------------------------
  is_feasible_mixed <- function(x1, x2){
    lhs <- as.vector(A %*% c(x1, x2))
    checks <- mapply(function(L, s, RHS){
      if (s == "<=") return(L <= RHS + tol)
      if (s == ">=") return(L >= RHS - tol)
      if (s == "=" ) return(abs(L - RHS) <= tol)
    }, lhs, sense, b)
    all(checks)
  }
  
  # ---------------------------
  # Auto plot limits if not given
  # Use max axis intercepts as a simple safe bound
  # ---------------------------
  if (is.null(xlim) || is.null(ylim)) {
    x_int <- c(0)
    y_int <- c(0)
    for (i in 1:nrow(A)) {
      a1 <- A[i,1]; a2 <- A[i,2]; bi <- b[i]
      if (abs(a1) > tol) x_int <- c(x_int, bi/a1)
      if (abs(a2) > tol) y_int <- c(y_int, bi/a2)
    }
    xmax <- max(x_int[x_int > 0], na.rm = TRUE)
    ymax <- max(y_int[y_int > 0], na.rm = TRUE)
    if (!is.finite(xmax)) xmax <- 5
    if (!is.finite(ymax)) ymax <- 5
    if (is.null(xlim)) xlim <- c(0, max(5, xmax*1.2))
    if (is.null(ylim)) ylim <- c(0, max(5, ymax*1.2))
  }
  
  # ---------------------------
  # Step 1: grid for feasible shading
  # ---------------------------
  x1_seq <- seq(xlim[1], xlim[2], by = grid_step)
  x2_seq <- seq(ylim[1], ylim[2], by = grid_step)
  grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)
  
  grid$feasible <- apply(grid, 1, function(row){
    row[1] >= -tol && row[2] >= -tol && is_feasible_mixed(row[1], row[2])
  })
  feasible_pts <- subset(grid, feasible)
  
  # ---------------------------
  # Step 2: corners via intersections
  # ---------------------------
  line_intersection <- function(a, b0, c, d0){
    M <- rbind(a, c)
    if (abs(det(M)) < tol) return(NULL)
    solve(M, c(b0, d0))
  }
  
  candidates <- list()
  k <- 1
  
  pairs <- combn(1:nrow(A), 2)
  for (p in 1:ncol(pairs)) {
    i <- pairs[1, p]; j <- pairs[2, p]
    pt <- line_intersection(A[i,], b[i], A[j,], b[j])
    if (!is.null(pt)) {
      candidates[[k]] <- pt; k <- k + 1
    }
  }
  
  # Axis intersections + origin
  candidates[[k]] <- c(0,0); k <- k + 1
  
  for (i in 1:nrow(A)) {
    a1 <- A[i,1]; a2 <- A[i,2]; bi <- b[i]
    if (abs(a2) > tol) { candidates[[k]] <- c(0, bi/a2); k <- k + 1 }
    if (abs(a1) > tol) { candidates[[k]] <- c(bi/a1, 0); k <- k + 1 }
  }
  
  cand_mat <- do.call(rbind, candidates)
  cand_df <- data.frame(x1 = cand_mat[,1], x2 = cand_mat[,2])
  
  cand_df$feasible <- apply(cand_df, 1, function(row){
    row[1] >= -tol && row[2] >= -tol && is_feasible_mixed(row[1], row[2])
  })
  corners <- subset(cand_df, feasible)
  corners <- unique(round(corners[,c("x1","x2")], 6))
  
  if (nrow(corners) == 0) {
    stop("No feasible corner points found. Feasible region may be empty.")
  }
  
  # ---------------------------
  # Step 3: objective values + best point
  # ---------------------------
  corners$Z <- c1*corners$x1 + c2*corners$x2
  best_idx <- if (maximize) which.max(corners$Z) else which.min(corners$Z)
  best_pt  <- corners[best_idx,]
  
  # ---------------------------
  # Plot
  # ---------------------------
  if (plot) {
    plot_title <- if (maximize) "Graphical Method (Maximization)"
    else          "Graphical Method (Minimization)"
    
    plot(NA, xlim=xlim, ylim=ylim, asp=1,
         xlab=expression(x[1]), ylab=expression(x[2]),
         main=plot_title)
    
    # nonnegativity boundaries
    abline(v=0, h=0, lwd=2, col="black")
    
    # constraint lines
    for (i in 1:nrow(A)) {
      a1 <- A[i,1]; a2 <- A[i,2]; bi <- b[i]
      
      if (abs(a2) > tol) {
        x2_line <- (bi - a1*x1_seq)/a2
        lines(x1_seq, x2_line, col=colors[i], lwd=2)
      } else if (abs(a1) > tol) {
        abline(v = bi/a1, col=colors[i], lwd=2)
      }
    }
    
    # feasible shading
    points(feasible_pts$x1, feasible_pts$x2,
           pch=".", col=rgb(0,0.6,1,0.15))
    
    # corners
    points(corners$x1, corners$x2, pch=19, cex=1.2)
    
    if (show_labels) {
      labs <- paste0("(", corners$x1, ", ", corners$x2, ")\nZ=", corners$Z)
      text(corners$x1, corners$x2, labels=labs, pos=4, cex=0.8)
    }
    
    # optimum marker
    points(best_pt$x1, best_pt$x2, pch=8, cex=2, lwd=2, col="blue")
    opt_tag <- if (maximize) "OPT (MAX)" else "OPT (MIN)"
    text(best_pt$x1, best_pt$x2,
         labels=paste0(opt_tag, "\nZ=", best_pt$Z),
         pos=3, col="blue", font=2)
    
    # legend
    legend_text <- paste0(
      apply(A,1,function(r) paste(r[1],"x1 +",r[2],"x2")),
      " ", sense, " ", b
    )
    legend("topright", legend=legend_text, col=colors, lwd=2, bty="n")
  }
  
  # ---------------------------
  # Return a structured result
  # ---------------------------
  return(list(
    corners = corners,
    optimum = best_pt,
    maximize = maximize,
    objective = cvec,
    A = A, b = b, sense = sense,
    feasible_points = feasible_pts
  ))
}

########################################################################
#### EXAMPLE 1 #########################################################
########################################################################

A <- rbind(
  c(1, 1),
  c(1, -1)
)
b <- c(4, 1)
cvec <- c(2, 1)
sense <- c(">=", "<=")

res <- solve_lp_graphical(A, b, cvec, sense, maximize = FALSE)
res$optimum

########################################################################
#### EXAMPLE 2 #########################################################
########################################################################


A <- rbind(
  c(1, 1),   # constraint 1
  c(1, 0),   # constraint 2
  c(0, 1)    # constraint 3
)
b <- c(4, 2, 3)
cvec <- c(3, 2)

sense <- c("<=", ">=", "<=")

res <- solve_lp_graphical(A, b, cvec, sense, maximize = TRUE)
res$optimum
