# Your LP

source('driver.R')
## BIGM EXAMPLES


################################################################################
###### EXAMPLE 1 Örnek 4.6 SLAYTLAR SAYFA 80 ###################################
################################################################################
# Zmin = 3x1 + 2x2 + x3
# 2x1 + 3x2 + 1x3  >= 21
# 1x1 + 1x2 + 1x3  >= 12

# x1, x2, x3  >= 0

A <- rbind(c(2,3,1), c(1,1,1))
b <- c(21,12)
c <- c(3,2,1)
sense <- c(">=", ">=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=FALSE)

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
###### EXAMPLE 2 Örnek 3.5.5 AHMET OZTURK SAYFA 149 ############################
################################################################################
# Zmin = 10x1 + 6x2 + 8x3
# 1x1 + 1x2 + 2x3  >= 2
# 5x1 + 3x2 + 2x3  >= 1

# x1, x2, x3  >= 0

A <- rbind(c(1,1,2), c(5,3,2))
b <- c(2,1)
c <- c(10,6,8)
sense <- c(">=", ">=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=FALSE)

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
###### EXAMPLE 3 Soru 3.11a AHMET OZTURK SAYFA 172 ############################
################################################################################
# Zmin = 2x1 + 3x2 - 5x3
# 1x1 + 1x2 + 1x3  = 7
# 2x1 - 5x2 + 1x3  >= 10

# x1, x2, x3  >= 0

A <- rbind(c(1,1,1), c(2,-5,1))
b <- c(7,10)
c <- c(2,3,-5)
sense <- c("=", ">=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=FALSE)

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
###### EXAMPLE 4 HAMDY TAHA Örnek 3.4.1 SAYFA 87 ###############################
################################################################################
# Zmin = 4x1 + x2
# 3x1 + x2   = 3
# 4x1 + 3x2  >= 6
#  x1 + 2x2  <= 4
# x1, x2  >= 0

A <- rbind(c(3,1), c(4,3), c(1,2))
b <- c(3,6,4)
c <- c(4,1)
sense <- c("=", '>=', "<=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=FALSE)

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
###### EXAMPLE 5 or.pdf v sayfa 87 #############################################
################################################################################
# Zmin = 2x1 + 3x2
# 0.5x1 + 0.25x2 <= 4
# 1x1   + 3x2    >= 20
# 1x1   + 1x2     = 10
# x1, x2  >= 0

A <- rbind(c(0.5,0.25), c(1,3), c(1,1))
b <- c(4,20,10)
c <- c(2,3)
sense <- c("<=", '>=', "=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=FALSE)

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