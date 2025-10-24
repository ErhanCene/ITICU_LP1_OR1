source('driver.r')


################################################################################
###### EXAMPLE 1 Örnek 4.2 SLAYTLAR SAYFA 68 ###################################
################################################################################
# Zmax = 2x1+3x2
# 6x1+4x2 <= 20
# 3x1+1x2 <= 30
# 1x1+1x2 <= 40
# x1, x2  >= 0

A <- rbind(c(6,4),
           c(3,1),
           c(1,1))
dimnames(A) <- list(paste0("R",1:3), c("x1","x2"))
b <- c(20,30,40)
c <- c(2,3)

cap <- capture_simplexR(A, b, c, unique_slacks = TRUE, z_row = "drop")

# Export all panels for each iteration into ./out
res1 <- simplex_export_all_panels(
  cap,
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

# 'res' returns file paths and optimality info per iteration.
# str(res[[1]]$files)  # see where the HTMLs were written


################################################################################
###### EXAMPLE 2 Örnek 4.3 SLAYTLAR SAYFA 71  ##################################
################################################################################

# Zmax = 10x1+22x2+18x3
# 1x1 + 4x2 + 3x3 <= 24
# 2x1 + 2x2 + 4x3 <= 46
# 3x1 + 5x2 + 6x3 <= 60
# 4x1 + 8x2 + 3x3 <= 120
# x1, x2, x3 >= 0


A <- rbind(c(1,4,3),
           c(2,2,4),
           c(3,5,6),
           c(4,8,3))
dimnames(A) <- list(paste0("R",1:4), c("x1","x2","x3"))
b <- c(24,46,60,120)
c <- c(10,22,18)

cap <- capture_simplexR(A, b, c, unique_slacks = TRUE, z_row = "drop")

# Export all panels for each iteration into ./out
res2 <- simplex_export_all_panels(
  cap,
  out_dir = "out/ex2",
  base_name = "ex2",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = TRUE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)

# 'res' returns file paths and optimality info per iteration.
# str(res[[1]]$files)  # see where the HTMLs were written

################################################################################
###### EXAMPLE 3 AHMET ÖZTÜRK Örnek 3.5.1 SAYFA 134  ###########################
################################################################################

# Zmax = 6x1+8x2
# 30x1 + 20x2 <= 300
# 5x1  + 10x2 <= 110

# x1, x2 >= 0


A <- rbind(c(30,20),
           c(5,10))
dimnames(A) <- list(paste0("R",1:2), c("x1","x2"))
b <- c(300,110)
c <- c(6,8)

cap <- capture_simplexR(A, b, c, unique_slacks = TRUE, z_row = "drop")

# Export all panels for each iteration into ./out
res3 <- simplex_export_all_panels(
  cap,
  out_dir = "out/ex3",
  base_name = "ex3",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = TRUE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)

# 'res' returns file paths and optimality info per iteration.
# str(res[[1]]$files)  # see where the HTMLs were written

################################################################################
###### EXAMPLE 4 AHMET ÖZTÜRK Örnek 3.5.2 SAYFA 143  ###########################
################################################################################

# Zmax = 2x1+6x2+5x3
# 1x1  + 1x2 + 1x3 <= 40
# 1x1  + 2x2 + 0x3 <= 20

# x1, x2, X3 >= 0


A <- rbind(c(1,1,1),
           c(1,2,0))
dimnames(A) <- list(paste0("R",1:2), c("x1","x2", "x3"))
b <- c(40,20)
c <- c(2,6,5)

cap <- capture_simplexR(A, b, c, unique_slacks = TRUE, z_row = "drop")

# Export all panels for each iteration into ./out
res4 <- simplex_export_all_panels(
  cap,
  out_dir = "out/ex4",
  base_name = "ex4",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = TRUE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)

# 'res' returns file paths and optimality info per iteration.
# str(res[[1]]$files)  # see where the HTMLs were written


################################################################################
###### EXAMPLE 5 AHMET ÖZTÜRK Örnek 3.5.3 SAYFA 145  ###########################
################################################################################

# Zmax = 5x1+8x2
# 2x1  + 4x2 <= 40
# 3x1  + 2x2 <= 60

# x1, x2 >= 0


A <- rbind(c(2,4),
           c(3,2))
dimnames(A) <- list(paste0("R",1:2), c("x1","x2"))
b <- c(40,60)
c <- c(5,8)

cap <- capture_simplexR(A, b, c, unique_slacks = TRUE, z_row = "drop")

# Export all panels for each iteration into ./out
res5 <- simplex_export_all_panels(
  cap,
  out_dir = "out/ex5",
  base_name = "ex5",
  # tweak visuals:
  initial_mask_rows = c("Zj","Zj-Cj"),
  initial_mask_ratio = TRUE,
  select_ratio_enter = "auto",
  after_highlight_base_row = TRUE,
  mathjax_wrap = TRUE,
  highlight_pivot = TRUE
)

# 'res' returns file paths and optimality info per iteration.
# str(res[[1]]$files)  # see where the HTMLs were written

