source('assignment.R')

##################################################################
############ EXAMPLE 1 DP DERS NOTLARI ÖRNEK 7.10 SAYFA 208 ######
##################################################################

cost_matrix <- matrix(c(20,11,3,6,
                 5,9,10,2,
                 18,7,4,1,
                 10,11,18,6), nrow=4, byrow=TRUE)


# Minimization (classical cost):



ans_min <- solve_assignment_min(cost_matrix,
                                file_prefix = "EX1",
                                row_labels = c("Makine 1","Makine 2",
                                               "Makine 3","Makine 4"),
                                col_labels = c("İş 1","İş 2","İş 3","İş 4"),
                                bigM = 1e9,
                                display_M = "always",   
                                show_notes = FALSE,
                                enumerate_all_optima = TRUE,
                                zero_tol = 1e-8)
# ans_min$assignment  # column j assigned to row i
# ans_min$objective   # minimal total cost
# ans_min$files       # HTML files you can open in browser or attach to Quarto


# # Maximization (profit):
# 
# ans_max <- solve_assignment_max(profit_matrix,
#                                 file_prefix = "hung_max",
#                                 row_labels = c("A","B","C","D"),
#                                 col_labels = c("W","X","Y","Z"),
#                                 bigM = 1e9,
#                                display_M = "always",   
#                                show_notes = FALSE)
# ans_max$objective   # maximal total profit (computed on original profits)


##################################################################
############ EXAMPLE 2 ÖRNEK 3.20 LP DERS NOTLARI SAYFA 210 ######
##################################################################


# Example MIN (square)
cost <- matrix(c(2,4,9,7,10,
                 4,6,8,2,3,
                 5,7,9,11,13,
                 10,9,8,7,6,
                 5,5,3,4,2), 5, byrow=TRUE)
ans_min <- solve_assignment_min(cost,
                                file_prefix="EX2",
                                row_labels=c("İş A","İş B","İş C","İş D","İş E"),
                                col_labels=c("Mak1","Mak2","Mak3","Mak4","Mak5"),
                                bigM = 1e9,
                                display_M = "always",   
                                show_notes = FALSE,
                                zero_tol = 1e-11)
# ans_min$assignment; ans_min$objective; ans_min$files

# #  Example MAX (profits, rectangular)
# profit <- matrix(c(62,75,80,95,
#                    75,80,85,55,
#                    80,75,90,80), nrow=3, byrow=TRUE)
# ans_max <- solve_assignment_max(profit,
#                                 file_prefix="hung_max",
#                                 row_labels=c("W1","W2","W3"),
#                                 col_labels=c("J1","J2","J3","J4"))
# ans_max$assignment; ans_max$objective; ans_max$files


##################################################################
############ EXAMPLE 3 ÖRNEK 3.21 LP DERS NOTLARI SAYFA 213 ######
##################################################################

# Example MIN (square)
cost <- matrix(c(2,4,5,6,8,
                 3,5,6,4,5,
                 6,1,2,3,4,
                 5,7,8,9,1,
                 2,3,5,9,10), 5, byrow=TRUE)
ans_min <- solve_assignment_min(cost,
                                file_prefix="EX3",
                                row_labels=c("A1","A2","A3","A4","A5"),
                                col_labels=c("P1","P2","P3","P4","P5"),
                                bigM = 1e9,
                                display_M = "always",   
                                show_notes = FALSE)


##################################################################
############ EXAMPLE 4 ÖRNEK 7.13 LP DERS NOTLARI SAYFA 217 ######
##################################################################

# Example MAX (square)
cost <- matrix(c(11,1,5,8,
                 9,9,8,1,
                 10,3,5,10,
                 1,13,12,11), 4, byrow=TRUE)
ans_max <- solve_assignment_max(cost,
                                file_prefix="EX4",
                                row_labels=c("E1","E2","E3","E4"),
                                col_labels=c("B1","B2","B3","B4"),
                                bigM = 1e9,
                                display_M = "always",   
                                show_notes = FALSE)



##################################################################
############ EXAMPLE 5 ÖRNEK 8.9 AHMET ÖZTÜRK SAYFA 475  #########
##################################################################


# Example MIN (square)
cost <- matrix(c("112","15","74","85",
                 "74","22","100","115",
                 "90","105","21","96",
                 "13","M","97","25"), 4, byrow=TRUE)

ans_min <- solve_assignment_min(cost, file_prefix="EX5",
                               row_labels=c("M1","M2","M3","M4"),
                               col_labels=c("A","B","C","D"),
                               bigM = 1e9,
                               display_M = "always",   
                               show_notes = FALSE)




##################################################################
############ EXAMPLE 6 ÖRNEK 8.10 AHMET ÖZTÜRK SAYFA 478  #########
##################################################################


# Example MIN (square)
cost <- matrix(c("35","15"," ","30","30",
                 "25","20","15","25","40",
                 "20"," ","30","20","50",
                 "15","40","35","15","40",
                 "10","50","40","30","35"), 5, byrow=TRUE)

ans_min <- solve_assignment_min(cost, file_prefix="EX6",
                                row_labels=c("A","B","C","D","E"),
                                col_labels=c("İş 1","İş 2","İş 3","İş 4","İş 5"),
                                bigM = 1e9,
                                display_M = "always",   
                                show_notes = FALSE)



##################################################################
############ EXAMPLE 7 ÖRNEK 8.8 AHMET ÖZTÜRK SAYFA 473  #########
##################################################################


# Example MIN (square)
cost <- matrix(c(12,14,10,9,
                 11,8,12,7,
                 8,6,9,5,
                 6,4,7,6), 4, byrow=TRUE)

ans_min <- solve_assignment_min(cost, file_prefix="EX7",
                                row_labels=c("W1","W2","W3","W4"),
                                col_labels=c("J1","J2","J3","J4"),
                                bigM = 1e9,
                                display_M = "always",   
                                show_notes = FALSE)

##################################################################
############ EXAMPLE 8 NO SOURCE RECTANGULAR EXAMPLE DUMMY ROW  ##
##################################################################


profit <- matrix(c(62,75,80,95,
                   75,80,85,55,
                   80,75,90,80), nrow=3, byrow=TRUE)
ans_max <- solve_assignment_max(profit, file_prefix="EX8",
                                row_labels=c("W1","W2","W3"),
                                col_labels=c("J1","J2","J3","J4"))

##################################################################
############ EXAMPLE 9 NO SOURCE RECTANGULAR EXAMPLE DUMMY COL  ##
##################################################################

profit <- matrix(c(13,18,10,14,
                   17,18,19,17,
                   12,15,16,14,
                   10,15,21,11,
                   22,9,12,20), nrow=5, byrow=TRUE)
ans_max <- solve_assignment_max(profit, file_prefix="EX9",
                                row_labels=c("F1","F2","F3","F4","F5"),
                                col_labels=c("P1","P2","P3","P4"))



# ---------------------------------------------------------
# Examples
# # ---------------------------------------------------------
# # 1) MIN with numbers
# cost <- matrix(c(2,4,9,7,10,
#                  4,6,8,2,3,
#                  5,7,9,11,13,
#                  10,9,8,7,6,
#                  5,5,3,4,2), 5, byrow=TRUE)
# ans_min <- solve_assignment_min(cost, file_prefix="hung_min",
#                                 row_labels=c("A","B","C","D","E"),
#                                 col_labels=c("1","2","3","4","5"))
# ans_min$objective; ans_min$files
# 
# # 2) MIN with Big-M tokens
# cm <- matrix(c("M", "4",  "9",  "7",  "10",
#                "4", "6",  "8",  "2",  "3",
#                "5", "7",  "9",  "11", "13",
#                "10","9",  "8",  "7",  "6",
#                "5", "5",  "3",  "4",  "-M"), 5, byrow=TRUE)
# ans_mM <- solve_assignment_min(cm, file_prefix="hung_M_demo", bigM=1e9,
#                                row_labels=c("A","B","C","D","E"),
#                                col_labels=c("1","2","3","4","5"))
# ans_mM$objective; ans_mM$files
# 
# # 3) MAX (profits), rectangular
# 
# # ans_max$objective; ans_max$files

