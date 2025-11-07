source('helpers.R')

################################################################################
########### EXAMPLE 1 ##########################################################
########### HAMDY TAHA ÖRNEK 14.2.1 14.2.2 14.2.3 ##############################
########### SAYFA 512-514-518 ##################################################
################################################################################

crit_names <- c("S","Y")
alt_names  <- c("A","B","C")



A <- matrix(c(1,   5,
              1/5, 1), 
            2, 2, byrow = TRUE, 
            dimnames = list(crit_names, crit_names))


A_S <- matrix(c(
  1,   2,   3,
  1/2, 1,   3/2,
  1/3, 2/3, 1
), 3, 3, byrow = TRUE, dimnames = list(alt_names, alt_names))


A_Y <- matrix(c(
  1,   1/2, 1/5,
  2,   1,   1/2,
  5,   2,   1
), 3, 3, byrow = TRUE, dimnames = list(alt_names, alt_names))


# --- Build the nested model (persons supplied as list(m1, m2, ...)) ---
model <- list(
  matrix = list(A),
  children = list(
    S = list(matrix = list(A_S), children = NULL),
    Y = list(matrix = list(A_Y), children = NULL)
  )
)

# Solve (eigen path, LaTeX per node if you already defined ahp_to_latex())
res <- ahp_solve(model, method = "column", cr_threshold = 0.10,
                 plot = F)


# Expanded layout, equations printed next to A/B/C nodes:
ahp_plot_paths(res$tree, who = "group",
               file = "EX_1ahp_expanded.png",
               digits = 3,
               main_title = "AHP Paths (column)",
               layout = "expanded",
               show_cr = TRUE,
               cex_prob = 0.7,        # bigger edge probabilities
               cex_contr = 0.8,       # slightly smaller contributions
               cex_eq = 0.7,         # smaller equations
               eq_x = 0.70,           # move equations farther right
               eq_y_offset = 0.15)    # move equations further down

ahp_plot_paths(res$tree, who = "group",
               file = "EX_1_figure.png",
               digits = 3,
               main_title = "AHP Structure",
               layout = "expanded",
               names_only=T)


# After ahp_solve(...)
# res <- ahp_solve(model, method="column", cr_threshold=0.10)
ahp_export_html(
  res$tree,
  who   = "group",
  file  = "EX1_report.html",
  digits = 3,
  title = "AHP — Group Report",
  show_copy_buttons = TRUE   # shows a 'Copy LaTeX' button for every raw block
)




# 2) Write a full .tex document (standalone, compilable):
ahp_export_latex(res$tree, who = "group",
                 file = "AHP_group_report.tex",
                 standalone = TRUE, digits = 3,
                 title = "AHP — Group Report")

# 3) Or write just the LaTeX body (to embed into your own document):
ahp_export_latex(res$tree, who = "group",
                 file = "AHP_group_body.tex",
                 standalone = FALSE, digits = 3)


# Per-person local weights & CR at root (A):
res$tree$group$matrix
res$tree$per_person$person_1$normalized
res$tree$per_person$person_1$weights
res$tree$per_person$person_1$consistency

# Under S and Y (A_S and A_Y):
res$tree$children$S$group$matrix
res$tree$children$S$per_person$person_1$normalized
res$tree$children$S$per_person$person_1$weights
res$tree$children$S$per_person$person_1$consistency

res$tree$children$Y$group$matrix
res$tree$children$Y$per_person$person_1$normalized
res$tree$children$Y$per_person$person_1$weights
res$tree$children$Y$per_person$person_1$consistency


# Final global probabilities over A, B, C:
res$global$per_person$person_1
res$global$group     


################################################################################
########### EXAMPLE 2 ##########################################################
########### HAMDY TAHA PROBLEM KÜMESİ 14 2B ÖRNEK 3 ############################
########### SAYFA 520 ##########################################################
################################################################################

crit_names <- c("T","SP","A")
alt_names  <- c("H","P")

A <- matrix(c(
  1,   1,   1/4,
  1,   1,   1/5,
  4,   5,     1
), 3, 3, byrow = TRUE, dimnames = list(crit_names, crit_names))


A_T <- matrix(c(1,   2,
              1/2, 1), 
            2, 2, byrow = TRUE, 
            dimnames = list(alt_names, alt_names))


A_SP <- matrix(c(1,   1/2,
                 2, 1), 
              2, 2, byrow = TRUE, 
              dimnames = list(alt_names, alt_names))

A_A <- matrix(c(1,   1,
                1, 1), 
               2, 2, byrow = TRUE, 
               dimnames = list(alt_names, alt_names))

# --- Build the nested model (persons supplied as list(m1, m2, ...)) ---
model <- list(
  matrix = list(A),
  children = list(
    T = list(matrix = list(A_T), children = NULL),
    SP = list(matrix = list(A_SP), children = NULL),
    A = list(matrix = list(A_A), children = NULL)
  )
)

# Solve (eigen path, LaTeX per node if you already defined ahp_to_latex())
res2 <- ahp_solve(model, method = "column", cr_threshold = 0.10)

ahp_plot_paths(res2$tree, who = "group",
               file = "EX_2ahp_expanded.png",
               digits = 3,
               main_title = "AHP Paths (column)",
               layout = "expanded",
               show_cr = TRUE,
               cex_prob = 0.7,        # bigger edge probabilities
               cex_contr = 0.7,       # slightly smaller contributions
               cex_eq = 0.7,         # smaller equations
               eq_x = 0.70,           # move equations farther right
               eq_y_offset = 0.15)    # move equations further down)

ahp_plot_paths(res2$tree, who = "group",
               file = "EX_2_figure.png",
               digits = 3,
               main_title = "AHP Structure",
               layout = "expanded",
               names_only=T)

ahp_export_html(
  res2$tree,
  who   = "group",
  file  = "EX2_report.html",
  digits = 3,
  title = "AHP — Group Report",
  show_copy_buttons = TRUE   # shows a 'Copy LaTeX' button for every raw block
)



# Per-person local weights & CR at root (A):
res2$tree$group$matrix
res2$tree$per_person$person_1$normalized
res2$tree$per_person$person_1$weights
res2$tree$per_person$person_1$consistency

# Under S and Y (T, SP, A):
res2$tree$children$T$group$matrix
res2$tree$children$T$per_person$person_1$normalized
res2$tree$children$T$per_person$person_1$weights
res2$tree$children$T$per_person$person_1$consistency

res2$tree$children$SP$group$matrix
res2$tree$children$SP$per_person$person_1$normalized
res2$tree$children$SP$per_person$person_1$weights
res2$tree$children$SP$per_person$person_1$consistency

res2$tree$children$A$group$matrix
res2$tree$children$A$per_person$person_1$normalized
res2$tree$children$A$per_person$person_1$weights
res2$tree$children$A$per_person$person_1$consistency

# Final global probabilities over H and P:
res2$global$per_person$person_1
res2$global$group  



################################################################################
########### EXAMPLE 3 ##########################################################
########### HAMDY TAHA PROBLEM KÜMESİ 14 2B ÖRNEK 1 ############################
########### SAYFA 518 ##########################################################
################################################################################

crit_names <- c("I","D","R")
alt_names  <- c("S","J","M")

A <- matrix(c(
  1,   2,   1/4,
  1/2,   1,   1/5,
  4,   5,     1
), 3, 3, byrow = TRUE, dimnames = list(crit_names, crit_names))


A_I <- matrix(c(1,3,4,
                1/3, 1,1/5,
                1/4, 5, 1), 
              3, 3, byrow = TRUE, 
              dimnames = list(alt_names, alt_names))

A_D <- matrix(c(1,1/3,2,
                3, 1,1/2,
                1/2, 2, 1), 
              3, 3, byrow = TRUE, 
              dimnames = list(alt_names, alt_names))

A_R <- matrix(c(1,1/2,1,
                2, 1,1/2,
                1, 2, 1), 
              3, 3, byrow = TRUE, 
              dimnames = list(alt_names, alt_names))

# --- Build the nested model (persons supplied as list(m1, m2, ...)) ---
model <- list(
  matrix = list(A),
  children = list(
    I = list(matrix = list(A_I), children = NULL),
    D = list(matrix = list(A_D), children = NULL),
    R = list(matrix = list(A_R), children = NULL)
  )
)

# Solve (eigen path, LaTeX per node if you already defined ahp_to_latex())
res3 <- ahp_solve(model, method = "column", cr_threshold = 0.10)

ahp_plot_paths(res3$tree, who = "group",
               file = "EX_3ahp_expanded.png",
               digits = 3,
               main_title = "AHP Paths (column)",
               layout = "expanded",
               show_cr = TRUE,
               cex_prob = 0.7,        # bigger edge probabilities
               cex_contr = 0.7,       # slightly smaller contributions
               cex_eq = 0.7,         # smaller equations
               eq_x = 0.70,           # move equations farther right
               eq_y_offset = 0.15)    # move equations further down)

ahp_plot_paths(res3$tree, who = "group",
               file = "EX_3_figure.png",
               digits = 3,
               main_title = "AHP Structure",
               layout = "expanded",
               names_only=T)

ahp_export_html(
  res3$tree,
  who   = "group",
  file  = "EX3_report.html",
  digits = 3,
  title = "AHP — Group Report",
  show_copy_buttons = TRUE   # shows a 'Copy LaTeX' button for every raw block
)



# Per-person local weights & CR at root (A):
res3$tree$group$matrix
res3$tree$per_person$person_1$normalized
res3$tree$per_person$person_1$weights
res3$tree$per_person$person_1$consistency

# Under child:
res3$tree$children$I$group$matrix
res3$tree$children$I$per_person$person_1$normalized
res3$tree$children$I$per_person$person_1$weights
res3$tree$children$I$per_person$person_1$consistency

res3$tree$children$D$group$matrix
res3$tree$children$D$per_person$person_1$normalized
res3$tree$children$D$per_person$person_1$weights
res3$tree$children$D$per_person$person_1$consistency

res3$tree$children$R$group$matrix
res3$tree$children$R$per_person$person_1$normalized
res3$tree$children$R$per_person$person_1$weights
res3$tree$children$R$per_person$person_1$consistency


# Final global probabilities over H and P:
res3$global$per_person$person_1
res3$global$group  


################################################################################
########### EXAMPLE 4 ##########################################################
########### HAMDY TAHA PROBLEM KÜMESİ 14 2B ÖRNEK 2 ############################
########### SAYFA 519 ##########################################################
################################################################################

# ---- Names ----
crit_main   <- c("K","J")
sub_K       <- c("KD","KI")
sub_J       <- c("JD","JI")
alternatives<- c("A","B","C")

# ---- Fill in YOUR judgments (positive, reciprocal) ----
# Goal: K vs J
A <- matrix(c(
  1,   2,
  1/2, 1
), 2, 2, byrow=TRUE, dimnames=list(crit_main, crit_main))

# K node: KD vs KI
A_K <- matrix(c(
  1,   1/3,
  3, 1
), 2, 2, byrow=TRUE, dimnames=list(sub_K, sub_K))

# J node: JD vs JI
A_J <- matrix(c(
  1,   4,
  1/4,   1
), 2, 2, byrow=TRUE, dimnames=list(sub_J, sub_J))

# Terminal (alternatives) under KD, KI, JD, JI  — 3x3 each over A,B,C
A_KD <- matrix(c(
  1,   2,   3,
  1/2, 1,   2,
  1/3, 1/2, 1
), 3, 3, byrow=TRUE, dimnames=list(alternatives, alternatives))

A_KI <- matrix(c(
  1,   2, 1/2,
  1/2,   1, 1/3,
  2, 3, 1
), 3, 3, byrow=TRUE, dimnames=list(alternatives, alternatives))

A_JD <- matrix(c(
  1,   4,   2,
  1/4, 1,   3,
  1/2, 1/3,   1
), 3, 3, byrow=TRUE, dimnames=list(alternatives, alternatives))

A_JI <- matrix(c(
  1,   1/2, 4,
  2,   1,   3,
  1/4,  1/3,   1
), 3, 3, byrow=TRUE, dimnames=list(alternatives, alternatives))

# ---- Build the nested model (single person version) ----
model <- list(
  matrix   = list(A),                # Goal level (K,J)
  children = list(
    K = list(                        # K node (KD,KI)
      matrix   = list(A_K),
      children = list(
        KD = list(matrix = list(A_KD), children = NULL),  # KD -> A,B,C
        KI = list(matrix = list(A_KI), children = NULL)   # KI -> A,B,C
      )
    ),
    J = list(                        # J node (JD,JI)
      matrix   = list(A_J),
      children = list(
        JD = list(matrix = list(A_JD), children = NULL),  # JD -> A,B,C
        JI = list(matrix = list(A_JI), children = NULL)   # JI -> A,B,C
      )
    )
  )
)

# ---- Solve (uses your latest CI/RI/CR with λ = sum(A %*% w)) ----
res4 <- ahp_solve(model, method = "column", cr_threshold = 0.10)

ahp_plot_paths(res4$tree, who = "group",
               file = "EX_4ahp_expanded.png",
               digits = 3,
               main_title = "AHP Paths (column)",
               layout = "expanded",
               show_cr = TRUE,
               cex_prob = 0.6,        # bigger edge probabilities
               cex_contr = 0.6,       # slightly smaller contributions
               cex_eq = 0.7,         # smaller equations
               eq_x = 0.70,           # move equations farther right
               eq_y_offset = 0.15)    # move equations further down))

ahp_plot_paths(res4$tree, who = "group",
               file = "EX_4_figure.png",
               digits = 3,
               main_title = "AHP Structure",
               layout = "expanded",
               names_only=T)

ahp_export_html(
  res4$tree,
  who   = "group",
  file  = "EX4_report.html",
  digits = 3,
  title = "AHP — Group Report",
  show_copy_buttons = TRUE   # shows a 'Copy LaTeX' button for every raw block
)


# Global probabilities over alternatives (A,B,C), group path:
res4$global$group

# Per-person locals and consistency at any node, e.g.:
# Goal (A): K vs J
res4$tree$per_person$person_1$weights
res4$tree$per_person$person_1$consistency

# K node (A_K): KD vs KI
res4$tree$children$K$per_person$person_1$weights
res4$tree$children$K$per_person$person_1$consistency

# K node (A_J): JD vs JI
res4$tree$children$J$per_person$person_1$weights
res4$tree$children$J$per_person$person_1$consistency

# KD terminal (A_KD): A,B,C local weights + consistency
res4$tree$children$K$children$KD$per_person$person_1$weights
res4$tree$children$K$children$KD$per_person$person_1$consistency

# KI terminal (A_KI): A,B,C local weights + consistency
res4$tree$children$K$children$KI$per_person$person_1$weights
res4$tree$children$K$children$KI$per_person$person_1$consistency

# JD terminal (A_JD): A,B,C local weights + consistency
res4$tree$children$J$children$JD$per_person$person_1$weights
res4$tree$children$J$children$JD$per_person$person_1$consistency

# JI terminal (A_JI): A,B,C local weights + consistency
res4$tree$children$J$children$JI$per_person$person_1$weights
res4$tree$children$J$children$JI$per_person$person_1$consistency



