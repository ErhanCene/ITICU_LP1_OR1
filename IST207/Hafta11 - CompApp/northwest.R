# -----------------------------
# Transportation problem data
# -----------------------------
rm(list=ls())

# 1) Example data (supply, demand, cost matrix)

supply <- c(20, 30, 50)          # supplies of sources S1, S2, S3
demand <- c(10, 25, 20, 20)      # demands of destinations D1..D4

cost <- matrix(
  c( 8,  6, 10,  9,
     9, 12, 13,  7,
     14,  9, 16,  5),
  nrow = 3, byrow = TRUE
)

rownames(cost) <- paste0("S", 1:nrow(cost))
colnames(cost) <- paste0("D", 1:ncol(cost))

cost


# 2) Balance the problem (important!)

total_supply <- sum(supply)
total_demand <- sum(demand)

total_supply
total_demand

if (total_supply > total_demand) {
  # add dummy demand column
  demand <- c(demand, total_supply - total_demand)
  dummy_col <- matrix(0, nrow = nrow(cost), ncol = 1)
  cost <- cbind(cost, dummy_col)
  colnames(cost)[ncol(cost)] <- "DummyDest"
}




if (total_supply < total_demand) {
  # add dummy supply row
  supply <- c(supply, total_demand - total_supply)
  dummy_row <- matrix(0, nrow = 1, ncol = ncol(cost))
  cost <- rbind(cost, dummy_row)
  rownames(cost)[nrow(cost)] <- "DummySource"
}


sum(supply)
sum(demand)  # now they should match



# 3) Northwest Corner allocations (step-by-step loop)

# -----------------------------
# Northwest Corner Method
# -----------------------------
m <- length(supply)      # number of sources
n <- length(demand)      # number of destinations

alloc <- matrix(0, nrow = m, ncol = n)
rownames(alloc) <- rownames(cost)
colnames(alloc) <- colnames(cost)

# working copies
s_left <- supply
d_left <- demand

i <- 1
j <- 1


while (i <= m && j <= n) {
  x <- min(s_left[i], d_left[j])   # allocate as much as possible
  
  alloc[i, j] <- x
  
  s_left[i] <- s_left[i] - x
  d_left[j] <- d_left[j] - x
  
  # move:
  if (s_left[i] == 0 && d_left[j] == 0) {
    # Degenerate case: both satisfied simultaneously.
    # Convention: move right (or down). We'll move right if possible.
    if (j < n) {
      j <- j + 1
    } else {
      i <- i + 1
    }
  } else if (s_left[i] == 0) {
    i <- i + 1          # supply exhausted, go down
  } else {
    j <- j + 1          # demand exhausted, go right
  }
}

alloc

# 4) Compute total transportation cost

total_cost <- sum(alloc * cost)
total_cost

cost
# 5) Print a nice summary for students

cat("Allocation Matrix (NW Corner):\n")
print(alloc)

cat("\nTotal Cost:", total_cost, "\n")
