# Example problem (minimize):
# min  2x1 + 3x2 + x3
# s.t.   x1 + 4x2 + 2x3 >= 8
#       3x1 + 2x2 +  x3 >= 6
#        x1 +  x2       >= 3
# x1, x2, x3 >= 0
source("two_phase_simplex.R")

A <- rbind(c(1,4,2), c(3,2,1), c(1,1,0))
b <- c(8,6,3)
c <- c(2,3,1)
sense <- c(">=", ">=", ">=")
res <- two_phase_simplex_html(A,b,c,sense, maximize = F)
files <- two_phase_simplex_save_steps(res, dir = "ex1", prefix = "phase")


# Example problem (maximize):
# max  5x1 + x2 
# s.t.   x1 - x2  >= 3
#       -x1 + 4x2 >= 1
#        x1 + 2x2 <= 8
# x1, x2 >= 0
source("two_phase_simplex.R")

A <- rbind(c(1,-1), c(-1,4), c(1,2))
b <- c(3,1,8)
c <- c(5,1)
sense <- c(">=", ">=", "<=")
res <- two_phase_simplex_html(A,b,c,sense, maximize = T)
files <- two_phase_simplex_save_steps(res, dir = "ex2", prefix = "phase")

# Example problem (minimize):
# min  2x1 + 3x2
# s.t.  0.5x1 + 0.25x2 <= 4
#          x1 +    3x2 >= 20
#          x1 +     x2  = 10
# x1, x2 >= 0
source("two_phase_simplex.R")

A <- rbind(c(0.5,0.25), c(1,3), c(1,1))
b <- c(4,20,10)
c <- c(2,3)
sense <- c("<=", ">=", "=")
res <- two_phase_simplex_html(A,b,c,sense, maximize = F)
files <- two_phase_simplex_save_steps(res, dir = "ex3", prefix = "phase")



# Example problem (minimize):
# minimize 
# 6x1 + 3x2
# subject to 
# x1 + x2 >= 1
# 2x1 − x2 >= 1
# 3x2 <= 2
# x1, x2 >= 0
source("two_phase_simplex.R")

A <- rbind(c(1,1), c(2,-1), c(0,3))
b <- c(1,1,2)
c <- c(6,3)
sense <- c(">=", ">=", "<=")
res <- two_phase_simplex_html(A,b,c,sense, maximize = F)
files <- two_phase_simplex_save_steps(res, dir = "ex4", prefix = "phase")


# Example problem (minimize):
# minimize 
# 2x1 + 6x2 + x3 + x4
# subject to 
# x1 + 2x2 + x4 = 6
# x1 + 2x2 + x3 + x4 = 7
# x1 + 3x2 – x3 + 2x4 = 7
# x1 + x2 + x3 = 5
# x1, x2, x3, x4 >= 0

source("two_phase_simplex.R")


A <- rbind(c(1,2,0,1), c(1,2,1,1), c(1,3,-1,2),c(1,1,1,0))
b <- c(6,7,7,5)
c <- c(2,6,1,1)
sense <- c("=", "=", "=","=")
res <- two_phase_simplex_html(A,b,c,sense, maximize = F)
files <- two_phase_simplex_save_steps(res, dir = "ex5", prefix = "phase")

################################################################################
###### EXAMPLE 6 ###############################################################
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
res <- two_phase_simplex_html(A,b,c,sense, maximize = F)
files <- two_phase_simplex_save_steps(res, dir = "ex6", prefix = "phase")


################################################################################
###### EXAMPLE 07 ###############################################################
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
res <- two_phase_simplex_html(A,b,c,sense, maximize = T)
files <- two_phase_simplex_save_steps(res, dir = "ex7", prefix = "phase")