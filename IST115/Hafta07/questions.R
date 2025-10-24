source('lp_plot.R')
source('lp_step_by_step.R')
source('driver_simplexR.R')
source('driver_bigM.R')


################################################################################
############## ÖRNEK 1 #########################################################
############## AHMET ÖZTÜRK SAYFA 111 2.5 ######################################
################################################################################



# Init with objective (max)
g <- lp_step_init(xlim = c(-1, 5), ylim = c(-1, 5),
                  title = "Uygun Bölge - Z = 3*x1 + 3.6*x2 (max)",
                  pastel = TRUE,
                  obj = c(3, 3.6), sense = "max")

g <- lp_step_add_x1_bound(g, 0,   dir = ">=", label_size=4)              # x1 ≥ 0
g <- lp_step_add_x2_bound(g, 0,   dir = ">=", label_size=4)              # x2 ≥ 0
g <- lp_step_add(g, coef = c(0, 3), rhs = 9, dir = "<=",label_size=4)
g <- lp_step_add(g, coef = c(2, 2), rhs = 8, dir = "<=",label_size=4)
g <- lp_step_add(g, coef = c(3, 1), rhs = 7.5, dir = "<=",label_size=4)
g <- lp_step_add(g, coef = c(1, 0.5), rhs = 3, dir = "<=",label_size=4)

g <- lp_step_mark_candidates(g, label = T,
                             show_z = T,
                             highlight_best = T,
                             show_obj_line = FALSE,
                             label_size = 4,
                             repel = TRUE,
                             color='red',
                             size=5)
g$p

################################################################################

# Zmax = 3x1 + 3.6x2
# 2x1 + 2x2 <= 8
# 0x1 + 3x2 <= 9
# 3x1 + 1x2 <= 7.5
# 1x1 + 0.5x2 <= 3

# x1, x2 >= 0

A <- rbind(c(2,2),c(0,3), c(3,1),c(1,0.5))
b <- c(8,9,7.5,3)
c <- c(3,3.6)
sense <- c("<=", "<=","<=","<=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=T)

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
############## ÖRNEK 2 #########################################################
############## AHMET ÖZTÜRK SAYFA 174 3.18 #####################################
################################################################################



# Init with objective (max)
g <- lp_step_init(xlim = c(-1, 5), ylim = c(-1, 5),
                  title = "Uygun Bölge - Z = 3*x1 + 3.6*x2 (max)",
                  pastel = TRUE,
                  obj = c(3, 3.6), sense = "max")

g <- lp_step_add_x1_bound(g, 0,   dir = ">=", label_size=4)              # x1 ≥ 0
g <- lp_step_add_x2_bound(g, 0,   dir = ">=", label_size=4)              # x2 ≥ 0
g <- lp_step_add(g, coef = c(0, 3), rhs = 9, dir = "<=",label_size=4)
g <- lp_step_add(g, coef = c(2, 2), rhs = 8, dir = "<=",label_size=4)
g <- lp_step_add(g, coef = c(3, 1), rhs = 7.5, dir = "<=",label_size=4)
g <- lp_step_add(g, coef = c(1, 0.5), rhs = 3, dir = "<=",label_size=4)

g <- lp_step_mark_candidates(g, label = T,
                             show_z = T,
                             highlight_best = T,
                             show_obj_line = FALSE,
                             label_size = 4,
                             repel = TRUE,
                             color='red',
                             size=5)
g$p

################################################################################

# Zmax = 5x1 + 2.5x2
# 1x1 + 1x2 <= 200
# 6x1 + 2x2 <= 500
# 1x1 + 0x2 <= 40

# x1, x2 >= 0

A <- rbind(c(1,1),c(6,2), c(1,0))
b <- c(200,500,40)
c <- c(5,2.5)
sense <- c("<=", "<=","<=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=T)

res1 <- simplex_export_all_panels(
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
############## ÖRNEK 3 #########################################################
############## AHMET ÖZTÜRK SAYFA 177 3.30 #####################################
################################################################################



# Init with objective (min)
g <- lp_step_init(xlim = c(-1, 7), ylim = c(-1, 7),
                  title = "Uygun Bölge - Z = 2x1 + x2 (min)",
                  pastel = TRUE,
                  obj = c(2, 1), sense = "min")

g <- lp_step_add_x1_bound(g, 0,   dir = ">=", label_size=4)              # x1 ≥ 0
g <- lp_step_add_x2_bound(g, 0,   dir = ">=", label_size=4)              # x2 ≥ 0
g <- lp_step_add(g, coef = c(4, 5), rhs = 20, dir = "<=",label_size=4)
g <- lp_step_add(g, coef = c(3, 1), rhs = 3, dir = ">=",label_size=4)
g <- lp_step_add(g, coef = c(1, 0), rhs = 4, dir = "<=",label_size=4)


g <- lp_step_mark_candidates(g, label = T,
                             show_z = T,
                             highlight_best = T,
                             show_obj_line = FALSE,
                             label_size = 4,
                             repel = TRUE,
                             color='red',
                             size=5)
g$p

################################################################################

# Zmin = 2x1 + 1x2
# 4x1 + 5x2 <= 20
# 3x1 + 1x2 >= 3
# 1x1 + 0x2 <= 4

# x1, x2 >= 0

A <- rbind(c(4,5),c(3,1), c(1,0))
b <- c(20,3,4)
c <- c(2,1)
sense <- c("<=", ">=","<=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=F)

res1 <- simplex_export_all_panels(
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
############## ÖRNEK 4 #########################################################
############## AHMET ÖZTÜRK SAYFA 174 3.21 #####################################
################################################################################



# Init with objective (min)
g <- lp_step_init(xlim = c(-1, 500), ylim = c(-1, 500),
                  title = "Uygun Bölge - Z = 100x1 + 50x2 (min)",
                  pastel = TRUE,
                  obj = c(100, 50), sense = "min")

g <- lp_step_add_x1_bound(g, 0,   dir = ">=", label_size=4)              # x1 ≥ 0
g <- lp_step_add_x2_bound(g, 0,   dir = ">=", label_size=4)              # x2 ≥ 0
g <- lp_step_add(g, coef = c(10, 40), rhs = 10000, dir = ">=",label_size=4)
g <- lp_step_add(g, coef = c(80, 20), rhs = 20000, dir = "<=",label_size=4)



g <- lp_step_mark_candidates(g, label = T,
                             show_z = T,
                             highlight_best = T,
                             show_obj_line = FALSE,
                             label_size = 4,
                             repel = TRUE,
                             color='red',
                             size=5)
g$p

################################################################################

# Zmin = 100x1 + 50x2
# 10x1 + 40x2 <= 10000
# 80x1 + 20x2 >= 20000


# x1, x2 >= 0

A <- rbind(c(10,40),c(80,20))
b <- c(10000,20000)
c <- c(100,50)
sense <- c(">=", "<=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=F)

res1 <- simplex_export_all_panels(
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
############## ÖRNEK 5 #########################################################
############## HAMDY TAHA SAYFA 86 12 ##########################################
################################################################################

# Zmax = 24x1 + 22x2 + 45x3
# 2x1 + 1x2   + 3x3  <= 42
# 2x1 + 1x2   + 2x3  <= 40
# 1x1 + 0.5x2 + 1x3  <= 45

# x1, x2,x3 >= 0

A <- rbind(c(2,1,3),c(2,1,2),c(1,0.5,1))
b <- c(42,40,45)
c <- c(24,22,45)
sense <- c("<=", "<=","<=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=T)

res1 <- simplex_export_all_panels(
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
############## ÖRNEK 6 #########################################################
############## HAMDY TAHA SAYFA 91 12 ##########################################
################################################################################

# Zmin = 3x1 + 2x2 + 3x3
# 1x1 + 4x2 + 1x3  >= 7
# 2x1 + 1x2 + 0x3  >= 10

# x1, x2,x3 >= 0

A <- rbind(c(1,4,1),c(2,1,0))
b <- c(7,10)
c <- c(3,2,3)
sense <- c(">=", ">=")
capBM <- capture_bigM(A,b,c,sense,M=1e6,maximize=F)

res1 <- simplex_export_all_panels(
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
