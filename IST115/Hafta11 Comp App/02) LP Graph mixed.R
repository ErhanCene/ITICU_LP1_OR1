# ============================================================
# GRAPHICAL METHOD for LP (2 variables) with <= and >= support
# ============================================================


# Example: Maximize Z = 3x1 + 2x2
# Subject to:
#   (1) x1 + x2 <= 4
#   (2) x1 >= 2
#   (3) x2 <= 3
#   x1, x2 >= 0

rm(list=ls())

# ---------------------------
# Step 1) Define coefficients
# ---------------------------
c1 <- 3
c2 <- 2

A <- rbind(
  c(1, 1),   # constraint 1
  c(1, 0),   # constraint 2
  c(0, 1)    # constraint 3
)
b <- c(4, 2, 3)

# >>> NEW: senses for each constraint
# Use "<=", ">=", or "=" in the same order as rows of A
# sense <- c("<=", "<=", "<=")
# Example with >= :
sense <- c("<=", ">=", "<=")

# ---------------------------
# Step 2) Make a grid to test feasibility
# ---------------------------
x1_seq <- seq(0, 5, by = 0.02)
x2_seq <- seq(0, 5, by = 0.02)
grid <- expand.grid(x1 = x1_seq, x2 = x2_seq)

# >>> UPDATED: feasibility check with mixed senses
is_feasible_mixed <- function(x1, x2, A, b, sense, tol = 1e-9){
  lhs <- as.vector(A %*% c(x1, x2))
  
  checks <- mapply(function(L, s, RHS){
    if (s == "<=") return(L <= RHS + tol)
    if (s == ">=") return(L >= RHS - tol)
    if (s == "=" ) return(abs(L - RHS) <= tol)
    stop("Unknown sense: ", s)
  }, lhs, sense, b)
  
  all(checks)
}

grid$feasible <- apply(grid, 1, function(row){
  is_feasible_mixed(row[1], row[2], A, b, sense)
})

feasible_pts <- subset(grid, feasible)

# ---------------------------
# Step 3) Plot constraints & feasible region
# ---------------------------
plot(NA, xlim=c(0,5), ylim=c(0,5),
     xlab=expression(x[1]), ylab=expression(x[2]),
     main="Graphical Method: Feasible Region",
     asp=1)

# Nonnegativity boundaries
abline(v = 0, lwd = 2, col = "black")
abline(h = 0, lwd = 2, col = "black")

# Draw constraint lines
colors <- c("red","darkgreen","purple")

for(i in 1:nrow(A)){
  a1 <- A[i,1]; a2 <- A[i,2]; bi <- b[i]
  
  if(a2 != 0){
    x2_line <- (bi - a1*x1_seq)/a2
    lines(x1_seq, x2_line, col=colors[i], lwd=2)
  } else {
    abline(v = bi/a1, col=colors[i], lwd=2)
  }
}

# Shade feasible region
points(feasible_pts$x1, feasible_pts$x2,
       pch=".", col=rgb(0,0.6,1,0.15))

# Legend with senses
legend_text <- paste0(
  apply(A,1,function(r) paste(r[1],"x1 +",r[2],"x2")),
  " ", sense, " ", b
)
legend("topright", legend=legend_text, col=colors, lwd=2, bty="n")

# ---------------------------
# Step 4) Find corner points
# ---------------------------
line_intersection <- function(a, b, c, d){
  M <- rbind(a, c)
  if(abs(det(M)) < 1e-12) return(NULL)
  solve(M, c(b, d))
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

# Axis intersections
candidates[[k]] <- c(0,0); k <- k+1

for(i in 1:nrow(A)){
  a1 <- A[i,1]; a2 <- A[i,2]; bi <- b[i]
  if(a2 != 0){
    candidates[[k]] <- c(0, bi/a2); k <- k+1
  }
}
for(i in 1:nrow(A)){
  a1 <- A[i,1]; a2 <- A[i,2]; bi <- b[i]
  if(a1 != 0){
    candidates[[k]] <- c(bi/a1, 0); k <- k+1
  }
}

cand_mat <- do.call(rbind, candidates)
cand_df <- data.frame(x1=cand_mat[,1], x2=cand_mat[,2])

# >>> UPDATED feasibility filter for corners
cand_df$feasible <- apply(cand_df, 1, function(row){
  row[1] >= -1e-9 && row[2] >= -1e-9 &&
    is_feasible_mixed(row[1], row[2], A, b, sense)
})
corners <- subset(cand_df, feasible)
corners <- unique(round(corners[,c("x1","x2")], 6))

# Plot corners
points(corners$x1, corners$x2, pch=19, cex=1.2)

# ---------------------------
# Step 5) Evaluate objective at corners
# ---------------------------
corners$Z <- c1*corners$x1 + c2*corners$x2
print(corners)

best_idx <- which.max(corners$Z)
best_pt <- corners[best_idx,]

# Labels with coordinates + Z
corner_labels <- paste0("(", corners$x1, ", ", corners$x2, ")\nZ=", corners$Z)
text(corners$x1, corners$x2, labels=corner_labels, pos=4, cex=0.8)

# Mark optimum
points(best_pt$x1, best_pt$x2, pch=8, cex=2, lwd=2, col="blue")
text(best_pt$x1, best_pt$x2,
     labels=paste0("OPT (Z=",best_pt$Z,")"),
     pos=3, col="blue", font=2)

