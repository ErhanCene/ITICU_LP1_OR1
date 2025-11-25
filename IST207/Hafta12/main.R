source("decision_tree_graph.R")
########################################################################
######### EXAMPLE 1 ####################################################
######### DERS NOTU SAYFA 99 ÖRNEK 7 ###################################
########################################################################

# ---------- Payoff table (Sipariş x İstem) ----------
P <- matrix(c(
  26, 26, 18, 22,   # Sipariş = 100
  22, 34, 30, 18,   # Sipariş = 200
  28, 24, 34, 26,
  22, 30, 28, 20 # Sipariş = 300
), nrow = 4, byrow = TRUE,
dimnames = list(c("S1","S2","S3", "S4"), c("O1","O2","O3","O4")))

# State probabilities (İstem olasılıkları)
p <- c(0.2, 0.5, 0.2, 0.1)


########################################################################
######### 01) INITIAL DECISION TABLE ###################################
########################################################################

export_payoff_html_transport(
  file = "./examples/EX1/01_EX1_Başlangıç_Karar_Tablosu.html",
  P = P, p = p,
  show_probs_row = TRUE,
  show_ev_col    = FALSE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 0,
  mark_ml = FALSE, mark_ev = FALSE
)

########################################################################
#### 02) RISK GENERAL DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Genel Gösterim",
  color_best        = "black",
  color_pruned      = "black",
  file              = "./examples/EX1/02_EX1_DT.png",
  show_summary      = FALSE,
  circle_digits     = 0,
  show_circle_metric = FALSE
)



########################################################################
#### 03) RISK ML GAIN DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - En Çok Olabilirlik - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX1/03_EX1_DT_ML_gain.png",
  show_summary      = FALSE,
  circle_digits     = 0
)



########################################################################
#### 04) RISK EV GAIN DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_value",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Değer (EV) - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX1/04_EX1_DT_EV_gain.png",
  show_summary      = FALSE,
  circle_digits     = 1
)




########################################################################
#### 05) RISK REGRET GAIN DT ###########################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_regret",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Pişmanlık (ER) - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX1/05_EX1_DT_ER_gain.png",
  show_summary      = FALSE,
  circle_digits     = 1
)


########################################################################
#### 06) RISK ML LOSS DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "loss",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - En Çok Olabilirlik - Kayıp",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX1/06_EX1_DT_ML_loss.png",
  show_summary      = FALSE,
  circle_digits     = 0
)

########################################################################
#### 07) RISK EV LOSS DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_value",
  most_likely_mode  = "any",
  type              = "loss",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Değer (EV) - Kayıp",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX1/07_EX1_DT_EV_loss.png",
  show_summary      = FALSE,
  circle_digits     = 1
)




########################################################################
#### 08) RISK REGRET LOSS DT ###########################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_regret",
  most_likely_mode  = "any",
  type              = "loss",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Pişmanlık (ER) - Kayıp",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX1/08_EX1_DT_ER_loss.png",
  show_summary      = FALSE,
  circle_digits     = 1
)


########################################################################
######### EXAMPLE 2 ####################################################
######### DERS NOTU SAYFA 107 ##########################################
########################################################################

# ---------- Payoff table (Sipariş x İstem) ----------
P <- matrix(c(
  3000, 2750, 2500, 2250,   # Sipariş = 100
  1500, 4750, 8000, 7750,   # Sipariş = 200
  2000, 5250, 8500, 11750   # Sipariş = 300
), nrow = 3, byrow = TRUE,
dimnames = list(c("100","200","300"), c("100","150","200","250")))

p <- c(0.2, 0.3, 0.3, 0.2)


########################################################################
######### 01) INITIAL DECISION TABLE ###################################
########################################################################

export_payoff_html_transport(
  file = "./examples/EX2/01_EX2_Başlangıç_Karar_Tablosu.html",
  P = P, p = p,
  show_probs_row = TRUE,
  show_ev_col    = FALSE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 0,
  mark_ml = FALSE, mark_ev = FALSE
)

########################################################################
#### 02) RISK GENERAL DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Genel Gösterim",
  color_best        = "black",
  color_pruned      = "black",
  file              = "./examples/EX2/02_EX2_DT.png",
  show_summary      = FALSE,
  circle_digits     = 0,
  show_circle_metric = F
)


########################################################################
#### 03) RISK ML GAIN DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - En Çok Olabilirlik - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX2/03_EX2_DT_ML_gain.png",
  show_summary      = FALSE,
  circle_digits     = 0
)




########################################################################
#### 04) RISK EV GAIN DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_value",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Değer (EV) - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX2/04_EX2_DT_EV_gain.png",
  show_summary      = FALSE,
  circle_digits     = 1
)



########################################################################
#### 05) RISK REGRET GAIN DT ###########################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_regret",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Pişmanlık (ER) - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX2/05_EX2_DT_ER_gain.png",
  show_summary      = FALSE,
  circle_digits     = 1
)

########################################################################
#### 06) RISK ML LOSS DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "loss",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - En Çok Olabilirlik - Kayıp",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX2/06_EX2_DT_ML_loss.png",
  show_summary      = FALSE,
  circle_digits     = 0
)

########################################################################
#### 07) RISK EV LOSS DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_value",
  most_likely_mode  = "any",
  type              = "loss",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Değer (EV) - Kayıp",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX2/07_EX2_DT_EV_loss.png",
  show_summary      = FALSE,
  circle_digits     = 1
)




########################################################################
#### 08) RISK REGRET LOSS DT ###########################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_regret",
  most_likely_mode  = "any",
  type              = "loss",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Pişmanlık (ER) - Kayıp",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX2/08_EX2_DT_ER_loss.png",
  show_summary      = FALSE,
  circle_digits     = 1
)




########################################################################
######### EXAMPLE 3 ####################################################
######### HAMDY TAHA 522-523 ###########################################
######### ÖRNEK 14.3.1 #################################################
########################################################################

# ---------- Payoff table (Hisse Senedi) ----------
P <- matrix(c(
  5000, -2000,
  1500, 500
), nrow = 2, byrow = TRUE,
dimnames = list(c("A","B"), c("Boğa","Ayı")))

p <- c(0.6, 0.4)

########################################################################
######### 01) INITIAL DECISION TABLE ###################################
########################################################################

export_payoff_html_transport(
  file = "./examples/EX3/01_EX3_Başlangıç_Karar_Tablosu.html",
  P = P, p = p,
  show_probs_row = TRUE,
  show_ev_col    = FALSE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 0,
  row_label = "Hisse Senedi",
  col_group_label = "Yıllık Getiri",
  mark_ml = FALSE, mark_ev = FALSE
)

########################################################################
#### 02) RISK GENERAL DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Genel Gösterim",
  color_best        = "black",
  color_pruned      = "black",
  file              = "./examples/EX3/02_EX3_DT.png",
  show_summary      = FALSE,
  circle_digits     = 0,
  show_circle_metric = F
)

########################################################################
#### 03) RISK ML GAIN DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - En Çok Olabilirlik - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX3/03_EX3_DT_ML_gain.png",
  show_summary      = FALSE,
  circle_digits     = 0
)







########################################################################
#### 04) RISK EV GAIN DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_value",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Değer (EV) - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX3/04_EX3_DT_EV_gain.png",
  show_summary      = FALSE,
  circle_digits     = 1
)




########################################################################
#### 05) RISK REGRET GAIN DT ###########################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_regret",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Pişmanlık (ER) - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX3/05_EX3_DT_ER_gain.png",
  show_summary      = FALSE,
  circle_digits     = 1
)

########################################################################
######### EXAMPLE 4 ####################################################
######### HAMDY TAHA 524 ###############################################
######### PROBLEM KÜMESİ 14.3a soru 3 ##################################
########################################################################

# ---------- Payoff table (Hisse Senedi) ----------
P <- matrix(c(
  5, 7,8,
  -10, 5,30,
  2,7,20
), nrow = 3, byrow = TRUE,
dimnames = list(c("Kamu","Gelişme","Global"), c("Kötü","Ortada","İyi")))

p <- c(0.1, 0.5,0.4)

########################################################################
######### 01) INITIAL DECISION TABLE ###################################
########################################################################

export_payoff_html_transport(
  file = "./examples/EX4/01_EX4_Başlangıç_Karar_Tablosu.html",
  P = P, p = p,
  show_probs_row = TRUE,
  show_ev_col    = FALSE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 1,
  row_label = "Fon",
  col_group_label = "Gelecekteki Pazar Durumu",
  mark_ml = FALSE, mark_ev = FALSE
)

########################################################################
#### 02) RISK GENERAL DT ###############################################
########################################################################


plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Genel Gösterim",
  color_best        = "black",
  color_pruned      = "black",
  file              = "./examples/EX4/02_EX4_DT.png",
  show_summary      = FALSE,
  circle_digits     = 0,
  show_circle_metric = F
)

########################################################################
#### 03) RISK ML GAIN DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - En Çok Olabilirlik - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX4/03_EX4_DT_ML_gain.png",
  show_summary      = FALSE,
  circle_digits     = 0
)







########################################################################
#### 04) RISK EV GAIN DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_value",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Değer (EV) - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX4/04_EX4_DT_EV_gain.png",
  show_summary      = FALSE,
  circle_digits     = 1
)




########################################################################
#### 05) RISK REGRET GAIN DT ###########################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_regret",
  most_likely_mode  = "any",
  type              = "gain",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Pişmanlık (ER) - Kazanç",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX4/05_EX4_DT_ER_gain.png",
  show_summary      = FALSE,
  circle_digits     = 1
)



########################################################################
######### EXAMPLE 5 ####################################################
######### HAMDY TAHA 541 ###############################################
######### ÖRNEK 14.4.1  ################################################
########################################################################

P <- matrix(c(
  5,10,18,25,
  8,7,12,23,
  21,18,12,21,
  30,22,19,15
), nrow = 4, byrow = TRUE,
dimnames = list(c("A1","A2","A3","A4"), c("S1","S2","S3","S4")))
p <- c(0.3, 0.4, 0.2, 0.1)


########################################################################
######### 01) INITIAL DECISION TABLE ###################################
########################################################################

export_payoff_html_transport(
  file = "./examples/EX5/01_EX5_Başlangıç_Karar_Tablosu.html",
  P = P, p = p,
  show_probs_row = TRUE,
  show_ev_col    = FALSE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 1,
  mark_ml = FALSE, mark_ev = FALSE
)

########################################################################
#### 02) RISK GENERAL DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "loss",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Genel Gösterim",
  color_best        = "black",
  color_pruned      = "black",
  file              = "./examples/EX5/02_EX5_DT.png",
  show_summary      = FALSE,
  circle_digits     = 0,
  show_circle_metric = F
)

########################################################################
#### 03) RISK ML LOSS DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "most_likely",
  most_likely_mode  = "any",
  type              = "loss",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - En Çok Olabilirlik - Kayıp",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX5/03_EX5_DT_ML_loss.png",
  show_summary      = FALSE,
  circle_digits     = 0
)

########################################################################
#### 04) RISK EV LOSS DT ###############################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_value",
  most_likely_mode  = "any",
  type              = "loss",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Değer (EV) - Kayıp",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX5/04_EX5_DT_EV_loss.png",
  show_summary      = FALSE,
  circle_digits     = 1
)




########################################################################
#### 05) RISK REGRET LOSS DT ###########################################
########################################################################

plot_decision_tree_classic(
  branch_costs = NULL,                 # (no labels on left branches, or put something else)
  state_labels  = colnames(P),         # O1, O2, O3, O4  → labels over the right lines
  probs         = p,
  payoffs_matrix = P,
  decision_label = "K",
  chance_labels  = rownames(P),        # S1, S2, S3, S4  → inside the circles
  prune_by          = "expected_regret",
  most_likely_mode  = "any",
  type              = "loss",
  circle_metric     = "criterion",
  main              = "Karar Ağacı - Beklenen Pişmanlık (ER) - Kayıp",
  color_best        = "green",
  color_pruned      = "red",
  file              = "./examples/EX5/05_EX5_DT_ER_loss.png",
  show_summary      = FALSE,
  circle_digits     = 1
)

########################################################################
######### EXAMPLE 6 ####################################################
######### HAMDY TAHA 524 ###############################################
######### PROBLEM KÜMESİ 14.3a soru 5 ##################################
########################################################################

# AFC, yeni Wings ’N Things fast-food ürününü ulusal çapta piyasaya sürmek üzeredir.
# Araştırma departmanı, Wings ’N Things’in büyük bir başarı olacağından emindir ve ürünün hiçbir reklam yapılmadan, tüm AFC şubelerinde hemen piyasaya sunulmasını istemektedir.
# 
# Pazarlama departmanı ise durumu farklı görmekte ve yoğun bir reklam kampanyası başlatmak istemektedir.
# Reklam kampanyasının maliyeti 100.000 $ olacak ve başarılı olması durumunda 950.000 $ gelir getirecektir.
# Kampanya başarısız olursa (başarısız olma olasılığı %30), gelirin 200.000 $ olacağı tahmin edilmektedir.
# 
# Eğer reklam yapılmazsa, müşterilerin ürünü receptif (ilgili) olma olasılığı 0.8, bu durumda gelir 400.000 $; ilgili olmama olasılığı 0.2, o durumda gelir 200.000 $ olarak tahmin edilmektedir.
# 
# (a) Karar ağacını çiziniz.
# (b) AFC bu yeni ürünü piyasaya sürerken hangi stratejiyi izlemelidir?


## ------------------------------------------------------------
## 1) Wings 'N Things kararı için yapraklar
## ------------------------------------------------------------
# Reklamlı strateji (maliyet 100 000 $ düşülmüş net getiriler):
# Başarılı reklam: 950k - 100k = 850k
# Başarısız reklam: 200k - 100k = 100k
L_ad_success <- make_terminal(850000, label = "850k")
L_ad_fail    <- make_terminal(100000, label = "100k")

# Reklamsız strateji:
# Receptive: 400k
# Not receptive: 200k
L_no_receptive    <- make_terminal(400000, label = "400k")
L_no_not_receptive <- make_terminal(200000, label = "200k")

## ------------------------------------------------------------
## 2) Şans düğümleri
## ------------------------------------------------------------
# Reklam yapılırsa:
C_ad <- make_chance(
  label = "Reklam",
  branches = list(
    list(label = "Başarılı",  prob = 0.70, node = L_ad_success),
    list(label = "Başarısız", prob = 0.30, node = L_ad_fail)
  )
)

# Reklam yapılmazsa:
C_noad <- make_chance(
  label = "Reklamsız",
  branches = list(
    list(label = "Müşteri ilgili",    prob = 0.80, node = L_no_receptive),
    list(label = "Müşteri ilgisiz",   prob = 0.20, node = L_no_not_receptive)
  )
)

## ------------------------------------------------------------
## 3) Kök karar düğümü
## ------------------------------------------------------------
Troot <- make_decision(
  label = "K",
  branches = list(
    list(label = "Reklam",       cost = 0, node = C_ad),
    list(label = "Reklam yok",   cost = 0, node = C_noad)
  )
)

## ------------------------------------------------------------
## 4) Karar ağacını çizme
## (plot_decision_tree_recursive senin fonksiyon)
## ------------------------------------------------------------

res <- plot_decision_tree_recursive(
  Troot,
  main          = "Wings 'N Things Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = F,
  circle_digits = 0,
  cex_nodes  = 1.1,
  cex_edge   = 1.0,
  cex_payoff = 1.1,
  payoff_dx  = 0.03,             # eğer fonksiyonuna eklediysen
  file       = "./examples/EX6/01_EX6_BASE.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  show_payoffs = F
)

res <- plot_decision_tree_recursive(
  Troot,
  main          = "Wings 'N Things Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = F,
  circle_digits = 0,
  cex_nodes  = 1.1,
  cex_edge   = 1.0,
  cex_payoff = 1.1,
  payoff_dx  = 0.03,             # eğer fonksiyonuna eklediysen
  file       = "./examples/EX6/02_EX6_BASE_PAYOFF.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  show_payoffs = T
)

res <- plot_decision_tree_recursive(
  Troot,
  main          = "Wings 'N Things Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = T,
  circle_digits = 0,
  cex_nodes  = 1.1,
  cex_edge   = 1.0,
  cex_payoff = 1.1,
  payoff_dx  = 0.03,             # eğer fonksiyonuna eklediysen
  file       = "./examples/EX6/03_EX6_BASE_EV.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  show_payoffs = T
)

res <- plot_decision_tree_recursive(
  Troot,
  main          = "Wings 'N Things Karar Ağacı",
  type          = "gain",
  color_best    = "limegreen",   # en iyi strateji
  color_pruned  = "red",         # diğer yollar
  show_circle_metric = TRUE,
  circle_digits = 0,
  cex_nodes  = 1.1,
  cex_edge   = 1.0,
  cex_payoff = 1.1,
  payoff_dx  = 0.03,             # eğer fonksiyonuna eklediysen
  file       = "./examples/EX6/04_EX6_DECISION.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = T,
  html_file   = "./examples/EX6/05_EX6_RAW_LATEX.html",
  html_title  = "Wings 'N Things EV Hesapları"
)


########################################################################
######### EXAMPLE 7 ####################################################
######### HAMDY TAHA 524 ###############################################
######### PROBLEM KÜMESİ 14.3a soru 6 ##################################
########################################################################

# Dürüst bir yazı–tura (yani adil madeni para) art arda üç kez atılıyor.
# Gelen her yazı (H) için 1.00 $ alıyorsun ve görünen her iki ardışık yazı (HH) için ayrıca 0.25 $ daha alıyorsun (örneğin HHH dizisi iki tane HH içerir: ilk iki atış ve son iki atış).
# Buna karşılık, gelen her tura (T) için 1.10 $ geri veriyorsun.
# Oyunu oynamak veya oynamamak seçeneklerin var.
# 
# (a) Bu oyun için karar ağacını çiziniz.
# (b) Bu oyunu oynamayı tercih eder miydiniz?


## ------ 1) Tüm sonuçların getirileri --------------------

# Dizileri ve net kazançları elle giriyoruz
outcomes <- data.frame(
  seq = c("HHH","HHT","HTH","HTT","THH","THT","TTH","TTT"),
  payoff = c(3.50, 1.15, 0.90, -1.20, 1.15, -1.20, -1.20, -3.30),
  stringsAsFactors = FALSE
)

p_each <- 1/8  # adil yazı-tura, 3 atış

## ------ 2) Oyna stratejisi için şans düğümü --------------

branches_play <- lapply(seq_len(nrow(outcomes)), function(i) {
  make_branch <- list(
    label = outcomes$seq[i],
    prob  = p_each,
    node  = make_terminal(outcomes$payoff[i],
                          label = outcomes$seq[i])
  )
})

C_play <- make_chance(
  label = "Yazi/Tura Sonuclari",
  branches = branches_play
)

## ------ 3) Kök karar düğümü (Oyna / Oynama) --------------

Troot <- make_decision(
  label = "K",
  branches = list(
    list(label = "Oyna",    cost = 0, node = C_play),
    list(label = "Oynama",  cost = 0,
         node = make_terminal(0, label = "0"))
  )
)

## ------ 4) Karar ağacını çiz -----------------------------


res <- plot_decision_tree_recursive(
  Troot,
  main          = "3 Atışlı Yazı-Tura Oyunu Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = F,
  circle_digits = 3,
  payoff_digits = 2,
  prob_digits = 3,
  cex_nodes  = 0.7,
  cex_edge   = 0.6,
  cex_payoff = 0.9,
  payoff_dx  = 0.03,
  state_perp  = -0.01,
  file       = "./examples/EX7/01_EX7_BASE.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = F
)

res <- plot_decision_tree_recursive(
  Troot,
  main          = "3 Atışlı Yazı-Tura Oyunu Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = F,
  circle_digits = 3,
  payoff_digits = 2,
  prob_digits = 3,
  cex_nodes  = 0.7,
  cex_edge   = 0.6,
  cex_payoff = 0.9,
  payoff_dx  = 0.03,
  state_perp  = -0.01,
  file       = "./examples/EX7/02_EX7_BASE_PAYOFF.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = T
)


res <- plot_decision_tree_recursive(
  Troot,
  main          = "3 Atışlı Yazı-Tura Oyunu Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = T,
  circle_digits = 3,
  payoff_digits = 2,
  prob_digits = 3,
  cex_nodes  = 0.7,
  cex_edge   = 0.6,
  cex_payoff = 0.9,
  payoff_dx  = 0.03,
  state_perp  = -0.01,
  file       = "./examples/EX7/03_EX7_BASE_EV.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = T
)

res <- plot_decision_tree_recursive(
  Troot,
  main   = "3 Atışlı Yazı-Tura Oyunu Karar Ağacı",
  type   = "gain",
  color_best   = "limegreen",
  color_pruned = "red",
  prob_digits   = 3,           # olasılık basamak sayısı (varsa)
  payoff_digits = 2,           # payoff basamak sayısı (varsa)
  circle_digits = 3,
  cex_nodes  = 0.7,
  cex_edge   = 0.6,
  cex_payoff = 0.9,
  payoff_dx  = 0.03,
  state_perp  = -0.01,
  show_circle_metric = T,
  file       = "./examples/EX7/04_EX7_DECISION.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = T,
  html_file   = "./examples/EX7/05_EX7_RAW_LATEX.html",
  html_title  = "Para Oyunu EV Hesapları",
  big_tree_mode = T
)


########################################################################
######### EXAMPLE 8 ####################################################
######### HAMDY TAHA 524 ###############################################
######### PROBLEM KÜMESİ 14.3a soru 7 ##################################
########################################################################


# Bir kumarhanede aşağıdaki oyunu oynama şansınız var. Dürüst (adil) bir zar iki kez atılıyor ve şu dört tür sonuca göre ödeme yapılıyor:
# (1) Her iki atış da çift ve aynı sayı (2–2, 4–4, 6–6).
# (2) Her iki atış da tek ve aynı sayı (1–1, 3–3, 5–5).
# (3) Atışların biri çift, diğeri tek (sırası önemli değil; çift-tek veya tek-çift).
# (4) Diğer tüm sonuçlar (çift-çift ama farklı sayılar veya tek-tek ama farklı sayılar).
# 
# Paranızla tam olarak iki sonuç üzerine, eşit miktarlarda bahis oynayabilirsiniz. Örneğin, hem 1. sonuca (çift eşleşme) hem de 2. sonuca (tek eşleşme) eşit miktarda para yatırabilirsiniz.
# 
# Her 1 dolar bahis için ödemeler şöyledir:
#   – 1. sonuç gerçekleşirse 2.00 $
#   – 2. ve 3. sonuçlar gerçekleşirse 1.95 $
#   – 4. sonuç gerçekleşirse 1.50 $
# 
#   (a) Bu oyun için karar ağacını çiziniz.
# (b) Hangi iki sonucu seçerdiniz?
#   (c) Bu oyunda beklenen olarak kâra geçme şansınız var mı?

## ----- Olasılıklar ve ödeme katsayıları -----
p_vec <- c(O1 = 1/12, O2 = 1/12, O3 = 1/2,  O4 = 1/3)
r_vec <- c(O1 = 2.00, O2 = 1.95, O3 = 1.95, O4 = 1.50)

## Seçilen iki sonuç için chance düğümü oluşturan yardımcı fonksiyon
make_game_chance <- function(chosen_idx, label) {
  branches <- lapply(seq_along(p_vec), function(k) {
    outcome_lab <- names(p_vec)[k]
    prob_k <- p_vec[k]
    
    # net kazanç (2 $ toplam bahis)
    payoff_net <- if (k %in% chosen_idx) r_vec[k] - 2 else -2
    
    leaf <- make_terminal(
      payoff = payoff_net,
      label  = sprintf("%s", outcome_lab)
    )
    
    list(
      label = outcome_lab,
      prob  = prob_k,
      node  = leaf
    )
  })
  
  make_chance(label = label, branches = branches)
}

## ----- Kök karar düğümü: 6 olası strateji -----
Troot <- make_decision(
  label = "K",
  branches = list(
    list(label = "O1 & O2", cost = 0,
         node = make_game_chance(c(1,2), "1-2")),
    list(label = "O1 & O3", cost = 0,
         node = make_game_chance(c(1,3), "1-3")),
    list(label = "O1 & O4", cost = 0,
         node = make_game_chance(c(1,4), "1-4")),
    list(label = "O2 & O3", cost = 0,
         node = make_game_chance(c(2,3), "2-3")),
    list(label = "O2 & O4", cost = 0,
         node = make_game_chance(c(2,4), "2-4")),
    list(label = "O3 & O4", cost = 0,
         node = make_game_chance(c(3,4), "3-4"))
  )
)



res <- plot_decision_tree_recursive(
  Troot,
  main          = "Casino Oyunu Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = F,
  circle_digits = 3,
  payoff_digits = 2,
  prob_digits = 3,
  cex_edge   = 0.5,
  state_perp = 0.01,
  cex_payoff = 0.6,
  file       = "./examples/EX8/01_EX8_BASE.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = F
)

res <- plot_decision_tree_recursive(
  Troot,
  main          = "Casino Oyunu Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = F,
  circle_digits = 3,
  payoff_digits = 2,
  prob_digits = 3,
  cex_edge   = 0.5,
  state_perp = 0.01,
  cex_payoff = 0.6,
  file       = "./examples/EX8/02_EX8_BASE_PAYOFF.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = T
)


res <- plot_decision_tree_recursive(
  Troot,
  main          = "Casino Oyunu Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = T,
  circle_digits = 3,
  payoff_digits = 2,
  prob_digits = 3,
  cex_edge   = 0.5,
  state_perp = 0.01,
  cex_payoff = 0.6,
  file       = "./examples/EX8/03_EX8_BASE_EV.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = T
)

## ----- Karar ağacını çiz -----
res <- plot_decision_tree_recursive(
  Troot,
  main   = "Casino Oyunu Karar Ağacı",
  type   = "gain",
  color_best   = "limegreen",
  color_pruned = "red",
  show_circle_metric = T,
  circle_digits = 3,
  payoff_digits = 2,
  prob_digits = 3,
  cex_edge   = 0.5,
  state_perp = 0.01,
  cex_payoff = 0.6,
  file = "./examples/EX8/04_EX8_DECISION.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = T,
  html_file   = "./examples/EX8/05_EX8_RAW_LATEX.html",
  html_title  = "Casino Oyunu EV Hesapları",
  big_tree_mode = T
)

# res$ev gibi bir şey dönmüyor ama her chance düğümün 'ev' alanı var;
# kökteki alternatiflerin EV'lerini görmek için küçük bir extractor yazabilirsin:
# get_strategy_EVs <- function(node) {
#   stopifnot(node$type == "decision")
#   sapply(node$branches, function(b) b$node$ev)
# }
# get_strategy_EVs(Troot)


########################################################################
######### EXAMPLE 9 ####################################################
######### NALAN CİNEMRE 397 ############################################
######### ÖRNEK 8.13 ###################################################
########################################################################

# Piyasaya sürülecek yeni bir ürünün tanıtımı için bir reklam kampanyası düzenlenecektir.
# Reklam için ya TV ya da gazete seçilecektir. TV ile kampanyanın başarılı olma olasılığı 0.6 ve
# bu durumda beklenen kar 90TL dir. Kampanya başarısız olduğunda firmanın, ürünün üretim haklarını 10TL ye
# devretmek ve ya ürünü yeniden tasarlayarak yeni bir kampanya başlatmak gibi iki stratejisi vardır.
# Yeni tasarımlı ürünün başarılı olma şansı 0.7 olup bu durumda beklenen kar 70TL iken
# başarısız olma durumunda beklenen zarar 20TL dir.
# 
# Kampanya gazete ile yapıldığında başarılı olma olasılığı 0.8 ve bu durumda beklenen kar 60TL dir.
# Başarısız olunması durumunda üretim haklarının devredilmesi ve ya reklam ajansının değiştirilmesi mümkündür.
# Üretim haklarının  devredilmesi durumunda net kar 25TL dir. Ajansın değiştirilmesi kararlaştırıldığında
# yeni kampanyanın başarılı olma olasılığı 0.7 ve bu durumda başarının getirisi 40TL,
# başarısızlığın götürüsü 15TL dir. Firma için en iyi stratejiyi belirleyiniz.


## ---------------- TV kolu ---------------------------------
# TV başarısız → yeniden tasarım kampanyası
C_TV_redesign <- make_chance(
  label = "Yeni tasarım kamp.",
  branches = list(
    list(label = "Başarılı",  prob = 0.7,
         node  = make_terminal( 70, label = "70")),
    list(label = "Başarısız", prob = 0.3,
         node  = make_terminal(-20, label = "-20"))
  )
)

# TV başarısız → karar: hak devri mi, yeniden tasarım mı?
D_TV_fail <- make_decision(
  label = "TV başarısız",
  branches = list(
    list(label = "Hakları devret", cost = 0,
         node  = make_terminal(10, label = "10")),
    list(label = "Yeniden tasarla", cost = 0,
         node  = C_TV_redesign)
  )
)

# TV kampanyası için şans düğümü
C_TV <- make_chance(
  label = "TV kampanya",
  branches = list(
    list(label = "Başarılı",  prob = 0.6,
         node  = make_terminal(90, label = "90")),
    list(label = "Başarısız", prob = 0.4,
         node  = D_TV_fail)
  )
)

## ---------------- Gazete kolu -----------------------------
# Gazete başarısız → ajans değişimi kampanyası
C_G_agency <- make_chance(
  label = "Yeni ajans kamp.",
  branches = list(
    list(label = "Başarılı",  prob = 0.7,
         node  = make_terminal( 40, label = "40")),
    list(label = "Başarısız", prob = 0.3,
         node  = make_terminal(-15, label = "-15"))
  )
)

# Gazete başarısız → karar: hak devri mi ajans değişimi mi?
D_G_fail <- make_decision(
  label = "Gazete başarısız",
  branches = list(
    list(label = "Hakları devret", cost = 0,
         node  = make_terminal(25, label = "25")),
    list(label = "Ajansı değiştir", cost = 0,
         node  = C_G_agency)
  )
)

# Gazete kampanyası için şans düğümü
C_G <- make_chance(
  label = "Gazete kampanya",
  branches = list(
    list(label = "Başarılı",  prob = 0.8,
         node  = make_terminal(60, label = "60")),
    list(label = "Başarısız", prob = 0.2,
         node  = D_G_fail)
  )
)

## ---------------- Kök karar düğümü ------------------------
Troot <- make_decision(
  label = "K",
  branches = list(
    list(label = "TV",     cost = 0, node = C_TV),
    list(label = "Gazete", cost = 0, node = C_G)
  )
)
res <- plot_decision_tree_recursive(
  Troot,
  main          = "Reklam Kampanyası Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = F,
  circle_digits = 3,
  payoff_digits = 2,
  prob_digits = 3,
  cex_edge   = 0.8,
  cex_nodes = 0.6,
  state_perp = 0.04,
  cex_payoff = 0.6,
  file       = "./examples/EX9/01_EX9_BASE.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = F
)

res <- plot_decision_tree_recursive(
  Troot,
  main          = "Reklam Kampanyası Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = F,
  circle_digits = 3,
  payoff_digits = 2,
  prob_digits = 3,
  cex_edge   = 0.8,
  cex_nodes = 0.6,
  state_perp = 0.04,
  cex_payoff = 0.8,
  file       = "./examples/EX9/02_EX9_BASE_PAYOFF.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = T
)

res <- plot_decision_tree_recursive(
  Troot,
  main          = "Reklam Kampanyası Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = T,
  circle_digits = 3,
  payoff_digits = 2,
  prob_digits = 3,
  cex_edge   = 0.8,
  cex_nodes = 0.6,
  state_perp = 0.04,
  cex_payoff = 0.8,
  file       = "./examples/EX9/03_EX9_BASE_EV.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = T
)


## ----- Karar ağacını çiz -----
res <- plot_decision_tree_recursive(
  Troot,
  main   = "Reklam Kampanyası Karar Ağacı",
  type   = "gain",
  color_best   = "limegreen",
  color_pruned = "red",
  show_circle_metric = T,
  circle_digits = 3,
  payoff_digits = 2,
  prob_digits = 3,
  cex_nodes = 0.5,
  cex_edge   = 0.6,
  state_perp = 0.04,
  cex_payoff = 0.8,
  file = "./examples/EX9/04_EX9_DECISION.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = T,
  html_file   = "./examples/EX9/05_EX9_RAW_LATEX.html",
  html_title  = "Reklam Kampanyası EV Hesapları",
  big_tree_mode = T
)


########################################################################
######### EXAMPLE 10 ####################################################
######### DERS NOTU SAYFA 118 ÖDEV #####################################
########################################################################

# YAPRAKLAR -------------------------------------------------------------

L0                <- make_terminal(payoff = 0,     label = "0")       # hiç yatırım yapmama

# A'yı önce seçtiğimiz strateji için yapraklar
L_A_fail          <- make_terminal(payoff = -2000)          # A başarısız
L_A_succ_stop     <- make_terminal(payoff =  1000)          # A başarılı, sonra bekle
L_A_succ_B_succ   <- make_terminal(payoff =  4000)          # A başarılı, B başarılı
L_A_succ_B_fail   <- make_terminal(payoff = -1000)          # A başarılı, B başarısız

# B'yi önce seçtiğimiz strateji için yapraklar
L_B_fail          <- make_terminal(payoff = -2000)          # B başarısız
L_B_succ_stop     <- make_terminal(payoff =  3000)          # B başarılı, sonra bekle
L_B_succ_A_succ   <- make_terminal(payoff =  4000)          # B başarılı, A başarılı
L_B_succ_A_fail   <- make_terminal(payoff =  1000)          # B başarılı, A başarısız


# A BAŞARILI OLDUKTAN SONRA VERİLEN KARAR -------------------------------

# A başarılı → B'ye yatırım yaparsak B için şans düğümü
C_afterA_B <- make_chance(
  label = "B",
  branches = list(
    list(label = "Başarılı",   prob = 0.40, node = L_A_succ_B_succ),
    list(label = "Başarısız",  prob = 0.60, node = L_A_succ_B_fail)
  )
)

# A başarılı olduktan sonraki karar düğümü
D_afterA <- make_decision(
  label = "D_A",
  branches = list(
    list(label = "Bekle",          cost = 0, node = L_A_succ_stop),
    list(label = "B'ye yatırım",   cost = 0, node = C_afterA_B)
  )
)


# B BAŞARILI OLDUKTAN SONRA VERİLEN KARAR -------------------------------

# B başarılı → A'ya yatırım yaparsak A için şans düğümü
C_afterB_A <- make_chance(
  label = "A",
  branches = list(
    list(label = "Başarılı",   prob = 0.70, node = L_B_succ_A_succ),
    list(label = "Başarısız",  prob = 0.30, node = L_B_succ_A_fail)
  )
)

# B başarılı olduktan sonraki karar düğümü
D_afterB <- make_decision(
  label = "D_B",
  branches = list(
    list(label = "Bekle",          cost = 0, node = L_B_succ_stop),
    list(label = "A'ya yatırım",   cost = 0, node = C_afterB_A)
  )
)


# ÖNCE A STRATEJİSİ İÇİN ŞANS DÜĞÜMÜ -----------------------------------

C_A_first <- make_chance(
  label = "A",
  branches = list(
    list(label = "Başarılı",   prob = 0.70, node = D_afterA),
    list(label = "Başarısız",  prob = 0.30, node = L_A_fail)
  )
)

# ÖNCE B STRATEJİSİ İÇİN ŞANS DÜĞÜMÜ -----------------------------------

C_B_first <- make_chance(
  label = "B",
  branches = list(
    list(label = "Başarılı",   prob = 0.40, node = D_afterB),
    list(label = "Başarısız",  prob = 0.60, node = L_B_fail)
  )
)

# KÖK KARAR DÜĞÜMÜ -----------------------------------------------------

Troot <- make_decision(
  label = "K",
  branches = list(
    list(label = "Yatırım yok", cost = 0, node = L0),
    list(label = "Önce A",      cost = 0, node = C_A_first),
    list(label = "Önce B",      cost = 0, node = C_B_first)
  )
)

# KARAR AĞACINI ÇİZ ----------------------------------------------------

res <- plot_decision_tree_recursive(
  Troot,
  main          = "A ve B Yatırım Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = F,
  circle_digits = 3,
  payoff_digits = 0,
  prob_digits = 3,
  cex_edge   = 0.8,
  cex_nodes = 0.6,
  state_perp = 0.03,
  cex_payoff = 0.6,
  file       = "./examples/EX10/01_EX10_BASE.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = F
)

res <- plot_decision_tree_recursive(
  Troot,
  main          = "A ve B Yatırım Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = F,
  circle_digits = 3,
  payoff_digits = 0,
  prob_digits = 3,
  cex_edge   = 0.8,
  cex_nodes = 0.6,
  state_perp = 0.03,
  cex_payoff = 0.8,
  file       = "./examples/EX10/02_EX10_BASE_PAYOFF.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = T
)

res <- plot_decision_tree_recursive(
  Troot,
  main          = "A ve B Yatırım Karar Ağacı",
  type          = "gain",
  color_best    = "black",   # en iyi strateji
  color_pruned  = "black",         # diğer yollar
  show_circle_metric = T,
  circle_digits = 0,
  payoff_digits = 0,
  prob_digits = 3,
  cex_edge   = 0.8,
  cex_nodes = 0.6,
  state_perp = 0.03,
  cex_payoff = 0.8,
  file       = "./examples/EX10/03_EX10_BASE_EV.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = F,
  big_tree_mode = T,
  show_payoffs = T
)


## ----- Karar ağacını çiz -----
res <- plot_decision_tree_recursive(
  Troot,
  main   = "A ve B Yatırım Karar Ağacı",
  type   = "gain",
  color_best   = "limegreen",
  color_pruned = "red",
  show_circle_metric = T,
  circle_digits = 0,
  payoff_digits = 0,
  prob_digits = 3,
  cex_nodes = 0.5,
  cex_edge   = 0.6,
  state_perp = 0.04,
  cex_payoff = 0.8,
  file = "./examples/EX10/04_EX10_DECISION.png",
  width      = 10,
  height     = 5,
  dpi        = 600,
  highlight_strategy = T,
  html_file   = "./examples/EX10/05_EX10_RAW_LATEX.html",
  html_title  = "A ve B Yatırım EV Hesapları",
  big_tree_mode = T
)

