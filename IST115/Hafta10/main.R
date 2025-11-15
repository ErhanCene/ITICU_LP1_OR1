# Your LP

# ========== tiny example ==========
# A <- rbind(c(1,4,2), c(3,2,1))
# b <- c(8,6)
# c <- c(-2,-3,-1)
# sense <- c(">=", ">=")
# capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)
# html <- simplex_html_initial(capBM, add_ratio_col=TRUE, mask_rows=c("Zj","Zj-Cj"),
#                              mask_ratio=TRUE, ratio_enter="auto", choose_enter_if_masked=TRUE,
#                              math_labels=TRUE, mathjax_wrap=TRUE, symbolic_M=TRUE)
# writeLines(html, "01.html")



source('driver.R')
## DEGENERATION EXAMPLES


################################################################################
###### EXAMPLE 1 ###############################################################
################################################################################
## bi/anahtar sütun oranının 0 olması
# Zmax = 3x1+9x2
# 1x1 + 4x2  <= 8
# 1x1 + 2x2  <= 4
# x1, x2 >= 0

A <- rbind(c(1,4), c(1,2))
b <- c(8,4)
c <- c(3,9)
sense <- c("<=", "<=")
capBM1 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res1 <- simplex_export_all_panels(
  capBM1,
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
###### EXAMPLE 2 ###############################################################
################################################################################
## Zj-Cj değerlerinin aynı olması

# Zmax = 8x1+8x2+6x3
# 2x1 + 3x2 + 4x3 <= 8
# 1x1 + 2x2 + 1x3 <= 4
# 3X1 + 5X2 + 0X3 <= 10
# x1, x2, x3  >= 0

A <- rbind(c(2,3,4), c(1,2,1), c(3,5,0))
b <- c(12,4,10)
c <- c(8,8,6)
sense <- c("<=", "<=", '<=')
capBM2 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res2 <- simplex_export_all_panels(
  capBM2,
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
###### EXAMPLE 3 ###############################################################
################################################################################
## Zj-Cj değerlerinin aynı olması
# Zmax = 10x1+8x2+10x3
# 1x1 + 1x2 + 1x3 <= 20
# 5x1 + 4x2 + 3x3 <= 60
# 2X1 + 3X2 + 1X3 <= 80
# x1, x2, x3  >= 0

A <- rbind(c(1,1,1), c(5,4,3), c(2,3,1))
b <- c(20,60,80)
c <- c(10,8,10)
sense <- c("<=", "<=", '<=')
capBM3 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res3 <- simplex_export_all_panels(
  capBM3,
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
###### EXAMPLE 4 ###############################################################
################################################################################
## bi/anahtar sütun oranının aynı olması

# Zmax = 3x1+2x2
# 4x1 + 3x2  <= 12
# 4x1 + 1x2  <= 8
# 4X1 - 1X2  <= 8
# x1, x2 >= 0

A <- rbind(c(4,3), c(4,1), c(4,-1))
b <- c(12,8,8)
c <- c(3,2)
sense <- c("<=", "<=", '<=')
capBM4 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res4 <- simplex_export_all_panels(
  capBM4,
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
###### EXAMPLE 5 ###############################################################
##### DP SLAYT ÖRNEK 4.8 SAYFA 84 ##############################################
################################################################################
# BOZULMA DURUMU / ## bi/anahtar sütun oranının aynı olması

# Zmax = 8x1+6x2+5x3
# 3x1 + 4x2 + 5x3 <= 12
# 2x1 + 0x2 + 5x3 <= 8
# 1X1 + 4X2 + 2X3 <= 4
# x1, x2, x3  >= 0

A <- rbind(c(3,4,5), c(2,0,5), c(1,4,2))
b <- c(12,8,4)
c <- c(8,6,5)
sense <- c("<=", "<=", '<=')
capBM5 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res5 <- simplex_export_all_panels(
  capBM5,
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
###### EXAMPLE 6 ###############################################################
##### DP SLAYT ÖRNEK 4.7 SAYFA 82 ##############################################
################################################################################
## Zj-Cj değerlerinin aynı olması
## bi/anahtar sütun oranının aynı olması
## bi/anahtar sütun oranının 0 olması


# Zmax = 8x1+8x2+6x3
# 3x1 + 2x2 + 4x3 <= 12
# 2x1 + 1x2 + 1x3 <= 4
# 5X1 + 3X2 + 0X3 <= 10
# x1, x2, x3  >= 0

A <- rbind(c(3,2,4), c(2,1,1), c(5,3,0))
b <- c(12,4,10)
c <- c(8,8,6)
sense <- c("<=", "<=", '<=')
capBM6 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res6 <- simplex_export_all_panels(
  capBM6,
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





################################################################################
###### EXAMPLE 7 ###############################################################
################################################################################
# SINIRSIZ ÇÖZÜM
# Zmin = 5x1-6x2
# 9x1 + 2x2  >= 36
# 1x1 + 0x2  <= 15
# x1, x2 >= 0

A <- rbind(c(9,2), c(1,0))
b <- c(36,15)
c <- c(5,-6)
sense <- c(">=", "<=")
capBM7 <- capture_bigM(A,b,c,sense,M=1e6,maximize=FALSE,max_iter = 10)

res7 <- simplex_export_all_panels(
  capBM7,
  out_dir = "out/ex7",
  base_name = "ex7",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)

################################################################################
###### EXAMPLE 8 ###############################################################
##### DP SLAYT ÖRNEK 4.9 SAYFA 86 ##############################################
################################################################################
# SINIRSIZ ÇÖZÜM 
# Zmax = 2x1+5x2+9x3
# 1x1 + 1x2 + 4x3 >= 12
# 1x1 + 1x2 + 1x3 >= 4
# x1, x2, x3  >= 0

A <- rbind(c(1,1,4), c(1,1,1))
b <- c(12,4)
c <- c(2,5,9)
sense <- c(">=", ">=")
capBM8 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE,max_iter = 10)

res8 <- simplex_export_all_panels(
  capBM8,
  out_dir = "out/ex8",
  base_name = "ex8",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)



