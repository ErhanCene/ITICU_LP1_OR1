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
###### EXAMPLE 9 ###############################################################
################################################################################
# SINIRSIZ ÇÖZÜM 
# Zmax = 3x1+6x2
# 3x1  + 4x2  >= 12
# -2x1 + 1x2  <= 4

# x1, x2 >= 0

A <- rbind(c(3,4), c(-2,1))
b <- c(12,4)
c <- c(3,6)
sense <- c(">=", "<=")
capBM9 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)

res9 <- simplex_export_all_panels(
  capBM9,
  out_dir = "out/ex9",
  base_name = "ex9",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)

################################################################################
###### EXAMPLE 10 ###############################################################
##### DP SLAYT ÖRNEK 4.10 SAYFA 88 ##############################################
# ALTERNATİF EN İYİ ÇÖZÜMLER
################################################################################

# Zmax = 2x1+4x2+6x3
# 4x1 + 8x2 + 12x3 <= 36
# 1x1 + 1x2 + 3x3  <= 12
# 2x1 + 2x2 + 1x3  <= 20

# x1, x2, x3  >= 0

A <- rbind(c(4,8,12), c(1,1,3), c(2,2,1))
b <- c(36,12,20)
c <- c(2,4,6)
sense <- c("<=", "<=", "<=")
capBM10 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE,max_iter = 10)

res10 <- simplex_export_all_panels(
  capBM10,
  out_dir = "out/ex10",
  base_name = "ex10",
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE,
  continue_after_optimal = TRUE,
  continue_steps = 2,
  post_opt_manual = list(
    list(enter = "x2", leave = "s2"),    # first manual pivot
    list(enter = "x1", leave = "s3")
  )
)



################################################################################
###### EXAMPLE 11 ###############################################################
################################################################################
# ALTERNATİF EN İYİ ÇÖZÜMLER
# Zmax = 3x1+6x2
# 2x1  + 4x2  <= 1600
# 6x1 + 2x2  <= 1800
# 0x1 + 1x2 <= 350

# x1, x2 >= 0

A <- rbind(c(2,4), c(6,2),c(0,1))
b <- c(1600,1800,350)
c <- c(3,6)
sense <- c("<=", "<=",'<=')
capBM11 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE,max_iter = 11)


res11 <- simplex_export_all_panels(
  capBM11,
  out_dir = "out/ex11",
  base_name = "ex11",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE,
  continue_after_optimal = TRUE,
  continue_steps = 1,
  post_opt_manual = list(list(enter="s3", leave="s2"))
)

################################################################################
###### EXAMPLE 12 ###############################################################
################################################################################
# SINIRLANDIRILMAMIŞ DEĞİŞKENLER

# Zmax = 2x1  + x2
# 3x1  + 1x2  <= 6
# 1x1  + 1x2  <= 4
# x1 >=0, x2 unrestricted


# Zmax = 2x1  + x2^+ - x2^-
# 3x1  + x2^+ - x2^-  <= 6
# 1x1  + x2^+ - x2^-  <= 4

# x1, x2^+, x2^- >= 0

A <- rbind(c(3,1), c(1,1))
b <- c(6,4)
c <- c(2,1)
sense <- c("<=", "<=")
capBM12 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE,max_iter = 11,
                        unrestricted = c('x2'))


res12 <- simplex_export_all_panels(
  capBM12,
  out_dir = "out/ex12",
  base_name = "ex12",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)

################################################################################
###### EXAMPLE 13 ###############################################################
################################################################################
# SINIRLANDIRILMAMIŞ DEĞİŞKENLER

# Zmax = 4x1  + 3x2
# 1x1  + 1x2  <= 6
# 2x1  - 1x2  <= 4
# x1 >=0, x2 unrestricted


# Zmax = 4x1  + 3x2^+ - 3x2^-
# 1x1  + 1x2^+ - 1x2^-  <= 6
# 2x1  - x2^+ + x2^-  <= 4

# x1, x2^+, x2^- >= 0

A <- rbind(c(1,1), c(2,-1))
b <- c(6,4)
c <- c(4,3)
sense <- c("<=", "<=")
capBM13 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE,max_iter = 11,
                        unrestricted = c('x2'))


res13 <- simplex_export_all_panels(
  capBM13,
  out_dir = "out/ex13",
  base_name = "ex13",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)


################################################################################
###### EXAMPLE 14 ###############################################################
##### DP SLAYT ÖRNEK 4.11 SAYFA 89 ##############################################
################################################################################
# UYGUN OLMAYAN ÇÖZÜM
# Zmin = 2x1+2x2
# -1x1 + 1x2 <= 4
#  1x1 + 2x2 >= 5
#  3x1 + 4x2 <= 6

# x1, x2 >= 0

A <- rbind(c(-1,1), c(1,2),c(3,4))
b <- c(4,5,6)
c <- c(2,2)
sense <- c("<=", ">=",'<=')
capBM14 <- capture_bigM(A,b,c,sense,M=1e6,maximize=FALSE,
                        max_iter = 11)


res14 <- simplex_export_all_panels(
  capBM14,
  out_dir = "out/ex14",
  base_name = "ex14",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)


################################################################################
###### EXAMPLE 15 ###############################################################
################################################################################
# UYGUN OLMAYAN ÇÖZÜM

# Zmax = 3x1+2x2
#  2x1 + 1x2 <= 2
#  3x1 + 4x2 >= 12

# x1, x2 >= 0

A <- rbind(c(2,1), c(3,4))
b <- c(2,12)
c <- c(3,2)
sense <- c("<=", ">=")
capBM15 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE,
                        max_iter = 11)


res15 <- simplex_export_all_panels(
  capBM15,
  out_dir = "out/ex15",
  base_name = "ex15",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = FALSE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)
