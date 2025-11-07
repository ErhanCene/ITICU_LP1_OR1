# Your LP

source('driver.R')
## BIGM EXAMPLES


################################################################################
###### EXAMPLE 1 Örnek 4.4 SLAYTLAR SAYFA 75 ###################################
################################################################################
# Zmax = -2x1 - 3x2 - 1x3
# 1x1 + 4x2 + 2x3 >= 8
# 3x1 + 2x2 + 1x3 >= 6
# x1, x2, x3  >= 0

A <- rbind(c(1,4,2), c(3,2,1))
b <- c(8,6)
c <- c(-2,-3,-1)
sense <- c(">=", ">=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res1 <- simplex_export_all_panels(
  capBM,
  out_dir = "out/ex1",
  base_name = "ex1",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)

################################################################################
###### EXAMPLE 2 Örnek 4.5 SLAYTLAR SAYFA 78 ###################################
################################################################################
# Zmax = 21x1 + 1x2 + 1x3
# 2x1 + 1x2 + 4x3  = 20
# 1x1 + 3x2 + 4x3  = 30

# x1, x2, x3  >= 0

A <- rbind(c(2,1,4), c(1,3,4))
b <- c(20,30)
c <- c(21,1,1)
sense <- c("=", "=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res2 <- simplex_export_all_panels(
  capBM,
  out_dir = "out/ex2",
  base_name = "ex2",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)


################################################################################
###### EXAMPLE 3 ORNEK 3.6.5 AHMET OZTURK SAYFA 170 ###################################
################################################################################
# Zmax = 0.2x1 + 0.5x2
# 3x1 + 2x2 >= 6
# 1x1 + 2x2 <= 4

# x1, x2 >= 0

A <- rbind(c(3,2), c(1,2))
b <- c(6,4)
c <- c(0.2,0.5)
sense <- c(">=", "<=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res3 <- simplex_export_all_panels(
  capBM,
  out_dir = "out/ex3",
  base_name = "ex3",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)

################################################################################
###### EXAMPLE 4 PROBLEM 3.4a 4a HAMDY TAHA SAYFA 91 ###########################
################################################################################
# Zmax = 2x1 + 3x2 - 5x3
# 1x1 + 1x2 + 1x3  = 7
# 2x1 - 5x2 + 1x3 >= 10

# x1, x2, x3 >= 0

A <- rbind(c(1,1,1), c(2,-5,1))
b <- c(7,10)
c <- c(2,3,5)
sense <- c("=", ">=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res4 <- simplex_export_all_panels(
  capBM,
  out_dir = "out/ex4",
  base_name = "ex4",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)

################################################################################
###### EXAMPLE 5 PROBLEM 3.4a 3B HAMDY TAHA SAYFA 91 DEĞİŞTİRİLMİŞ #############
################################################################################
# Zmax = 2x1 - 7x2
# 4x1  + 5x2 <= 10 
# 6x1 + 7x2 >= 3

# x1, x2 >= 0

A <- rbind(c(4,5),c(6,7))
b <- c(10,3)
c <- c(2,-7)
sense <- c("<=", ">=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res5 <- simplex_export_all_panels(
  capBM,
  out_dir = "out/ex5",
  base_name = "ex5",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)

################################################################################
###### EXAMPLE 6 CHECK FOR DUAL SIMPLEX METHOD ###########################
################################################################################
# Zmax = 5x1 + 4x2

# 2x1 + 3x2 >= 8 
# 2x1 + 1x2 >= 6

# x1, x2 >= 0

# Coefficient matrix (A)
A <- rbind(
  c(2, 3),
  c(2, 1)
)

# Right-hand side (b)
b <- c(8, 6)

# Objective function coefficients
c <- c(5, 4)

# Constraints are >= for both
sense <- c(">=", ">=")

capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)


res6 <- simplex_export_all_panels(
  capBM,
  out_dir = "out/ex6",
  base_name = "ex6",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)


