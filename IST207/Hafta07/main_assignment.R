source('assignment.R')



##################################################################
############ EXAMPLE 1 AHMET ÖZTÜRK SAYFA 483 ÖRNEK 8.13 #########
##################################################################

cost_matrix <- matrix(c(21,24,26,22,
                 28,26,30,29,
                 24,25,34,27,
                 28,26,28,25), nrow=4, byrow=TRUE)


# Minimization (classical cost):



ans_min <- solve_assignment_min(cost_matrix,
                                file_prefix = "EX1",
                                row_labels = c("İş 1","İş 2","İş 3","İş 4"),
                                col_labels = c("Tezgah 1","Tezgah 2",
                                               "Tezgah 3","Tezgah 4"),
                                bigM = 1e9,
                                display_M = "always",   
                                show_notes = FALSE,
                                enumerate_all_optima = TRUE,
                                zero_tol = 1e-8)






