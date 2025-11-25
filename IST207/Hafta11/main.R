source('risk_decision.R')

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
#### 02) RISK ML GAIN ##################################################
########################################################################

res_ml <- export_most_likely_html_transport(
  file = "./examples/EX1/02_EX1_ML_gain.html",
  P = P, p = p, 
  type = "gain",
  row_label = "Sipariş Miktarı",
  col_group_label = "İstem Miktarı",
  ml_label = "En Olası Durum Değeri",
  digits_cell = 0, digits_prob = 1, digits_ml = 0,
  show_ml_col = F
)




########################################################################
#### 03) RISK EV GAIN ##################################################
########################################################################


export_payoff_html_transport(
  file = "./examples/EX1/03_EX1_EV_gain.html",
  P = P, p = p,
  type='gain',
  show_probs_row = TRUE,
  show_ev_col    = TRUE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 1,
  mark_ml = F
)




########################################################################
#### 04) RISK REGRET GAIN ##############################################
########################################################################

export_regret_html_transport(
  file = "./examples/EX1/04_EX1_REGRET_gain.html",
  P = P, p = p, 
  type = "gain",
  show_probs_row = TRUE, show_er_col = TRUE,
  mark_er = TRUE, mark_mr = FALSE,
  digits_cell = 0, digits_prob = 1, digits_er = 1
)





########################################################################
#### 05) RISK ML LOSS ##################################################
########################################################################

res_ml <- export_most_likely_html_transport(
  file = "./examples/EX1/05_EX1_ML_loss.html",
  P = P, p = p, 
  type = "loss",
  row_label = "Sipariş Miktarı",
  col_group_label = "İstem Miktarı",
  ml_label = "En Olası Durum Değeri",
  digits_cell = 0, digits_prob = 1, digits_ml = 0,
  show_ml_col = F
)



########################################################################
#### 06) RISK EV LOSS ##################################################
########################################################################


export_payoff_html_transport(
  file = "./examples/EX1/06_EX1_EV_loss.html",
  type='loss',
  P = P, p = p,
  show_probs_row = TRUE,
  show_ev_col    = TRUE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 1,
  mark_ml = FALSE
)






########################################################################
#### 07) RISK REGRET GAIN ##############################################
########################################################################

export_regret_html_transport(
  file = "./examples/EX1/07_EX1_REGRET_loss.html",
  P = P, p = p, 
  type = "loss",
  show_probs_row = TRUE, show_er_col = TRUE,
  mark_er = TRUE, mark_mr = FALSE,
  digits_cell = 0, digits_prob = 1, digits_er = 1
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
dimnames = list(c("S1 (100)","S2 (200)","S3 (300)"), c("M1 (100)","M2 (150)","M3 (200)","M4 (250)")))

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
#### 02) RISK ML GAIN ##################################################
########################################################################

res_ml <- export_most_likely_html_transport(
  file = "./examples/EX2/02_EX2_ML_gain.html",
  P = P, p = p, 
  type = "gain",
  row_label = "Sipariş",
  col_group_label = "İstem Miktarı",
  ml_label = "En Olası Durum Değeri",
  digits_cell = 0, digits_prob = 1, digits_ml = 0,
  show_ml_col = F
)







########################################################################
#### 03) RISK EV GAIN ##################################################
########################################################################


export_payoff_html_transport(
  file = "./examples/EX2/03_EX2_EV_gain.html",
  P = P, p = p,
  type='gain',
  show_probs_row = TRUE,
  show_ev_col    = TRUE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 0,
  mark_ml = F
)





########################################################################
#### 04) RISK REGRET GAIN ##############################################
########################################################################

export_regret_html_transport(
  file = "./examples/EX2/04_EX2_REGRET_gain.html",
  P = P, p = p, 
  type = "gain",
  show_probs_row = TRUE, show_er_col = TRUE,
  mark_er = TRUE, mark_mr = FALSE,
  digits_cell = 0, digits_prob = 1, digits_er = 0
)

########################################################################
#### 05) RISK ML LOSS ##################################################
########################################################################

res_ml <- export_most_likely_html_transport(
  file = "./examples/EX2/05_EX2_ML_loss.html",
  P = P, p = p, 
  type = "loss",
  row_label = "Sipariş",
  col_group_label = "İstem Miktarı",
  ml_label = "En Olası Durum Değeri",
  digits_cell = 0, digits_prob = 1, digits_ml = 0,
  show_ml_col = F
)







########################################################################
#### 06) RISK EV LOSS ##################################################
########################################################################


export_payoff_html_transport(
  file = "./examples/EX2/06_EX2_EV_loss.html",
  P = P, p = p,
  type='loss',
  show_probs_row = TRUE,
  show_ev_col    = TRUE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 0,
  mark_ml = F
)





########################################################################
#### 07) RISK REGRET LOSS ##############################################
########################################################################

export_regret_html_transport(
  file = "./examples/EX2/07_EX2_REGRET_loss.html",
  P = P, p = p, 
  type = "loss",
  show_probs_row = TRUE, show_er_col = TRUE,
  mark_er = TRUE, mark_mr = FALSE,
  digits_cell = 0, digits_prob = 1, digits_er = 0
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
#### 02) RISK ML GAIN ##################################################
########################################################################

res_ml <- export_most_likely_html_transport(
  file = "./examples/EX3/02_EX3_ML_gain.html",
  P = P, p = p, 
  type = "gain",
  row_label = "Hisse Senedi",
  col_group_label = "Yıllık Getiri",
  ml_label = "En Olası Durum Değeri",
  digits_cell = 0, digits_prob = 1, digits_ml = 0,
  show_ml_col = F
)






########################################################################
#### 03) RISK EV GAIN ##################################################
########################################################################


export_payoff_html_transport(
  file = "./examples/EX3/03_EX3_EV_gain.html",
  P = P, p = p,
  type = "gain",
  show_probs_row = TRUE,
  show_ev_col    = TRUE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 0,
  row_label = "Hisse Senedi",
  col_group_label = "Yıllık Getiri",
  mark_ml = F
)






########################################################################
#### 04) RISK REGRET GAIN ##############################################
########################################################################

export_regret_html_transport(
  file = "./examples/EX3/04_EX3_REGRET_gain.html",
  P = P, p = p, 
  type = "gain",
  show_probs_row = TRUE, show_er_col = TRUE,
  mark_er = TRUE, mark_mr = FALSE,
  row_label = "Hisse Senedi",
  col_group_label = "Yıllık Getiri",
  digits_cell = 0, digits_prob = 1, digits_er = 0
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
dimnames = list(c("Kamu","Gelişme","Global"), c("Kötü","Nötr","İyi")))

p <- c(0.1, 0.5,0.4)

########################################################################
######### 01) INITIAL DECISION TABLE ###################################
########################################################################

export_payoff_html_transport(
  file = "./examples/EX4/01_EX4_Başlangıç_Karar_Tablosu.html",
  P = P, p = p,
  show_probs_row = TRUE,
  show_ev_col    = FALSE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 0,
  row_label = "Fon",
  col_group_label = "Gelecekteki Pazar Durumu (% Değişim)",
  mark_ml = FALSE, mark_ev = FALSE
)

########################################################################
#### 02) RISK ML GAIN ##################################################
########################################################################

res_ml <- export_most_likely_html_transport(
  file = "./examples/EX4/02_EX4_ML_gain.html",
  P = P, p = p, 
  type = "gain",
  row_label = "Fon",
  col_group_label = "Gelecekteki Pazar Durumu (% Değişim)",
  ml_label = "En Olası Durum Değeri",
  digits_cell = 0, digits_prob = 1, digits_ml = 0,
  show_ml_col = F
)




########################################################################
#### 03) RISK EV GAIN ##################################################
########################################################################


export_payoff_html_transport(
  file = "./examples/EX4/03_EX4_EV_gain.html",
  P = P, p = p,
  type = "gain",
  show_probs_row = TRUE,
  show_ev_col    = TRUE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 1,
  row_label = "Fon",
  col_group_label = "Gelecekteki Pazar Durumu (% Değişim)",
  mark_ml = F
)




########################################################################
#### 04) RISK REGRET GAIN ##############################################
########################################################################

export_regret_html_transport(
  file = "./examples/EX4/04_EX4_REGRET_gain.html",
  P = P, p = p, 
  type = "gain",
  show_probs_row = TRUE, show_er_col = TRUE,
  mark_er = TRUE, mark_mr = FALSE,
  row_label = "Fon",
  col_group_label = "Gelecekteki Pazar Durumu (% Değişim)",
  digits_cell = 0, digits_prob = 1, digits_er = 1
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
dimnames = list(c("A1(200)","A2(250)","A3(300)","A4(350)"), c("S1(200)","S2(250)","S3(300)","S4(350)")))
p <- c(0.3, 0.4, 0.2, 0.1)


########################################################################
######### 01) INITIAL DECISION TABLE ###################################
########################################################################

export_payoff_html_transport(
  file = "./examples/EX5/01_EX5_Başlangıç_Karar_Tablosu.html",
  P = P, p = p,
  row_label = "Kamp Yeri Kapasitesi",
  col_group_label = "Katılımcı Sayısı",
  show_probs_row = TRUE,
  show_ev_col    = FALSE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 0,
  mark_ml = FALSE, mark_ev = FALSE
)

########################################################################
#### 02) RISK ML LOSS ##################################################
########################################################################

res_ml <- export_most_likely_html_transport(
  file = "./examples/EX5/02_EX5_ML_loss.html",
  P = P, p = p, 
  type = "loss",
  row_label = "Kamp Yeri Kapasitesi",
  col_group_label = "Katılımcı Sayısı",
  ml_label = "En Olası Durum Değeri",
  digits_cell = 0, digits_prob = 1, digits_ml = 0,
  show_ml_col = F
)







########################################################################
#### 03) RISK EV LOSS ##################################################
########################################################################


export_payoff_html_transport(
  file = "./examples/EX5/03_EX5_EV_loss.html",
  P = P, p = p,
  type='loss',
  row_label = "Kamp Yeri Kapasitesi",
  col_group_label = "Katılımcı Sayısı",
  show_probs_row = TRUE,
  show_ev_col    = TRUE,     # EV görünsün
  digits_cell = 0, digits_prob = 1, digits_ev = 1,
  mark_ml = F
)





########################################################################
#### 04) RISK REGRET LOSS ##############################################
########################################################################

export_regret_html_transport(
  file = "./examples/EX5/04_EX5_REGRET_loss.html",
  P = P, p = p, 
  type = "loss",
  row_label = "Kamp Yeri Kapasitesi",
  col_group_label = "Katılımcı Sayısı",
  show_probs_row = TRUE, show_er_col = TRUE,
  mark_er = TRUE, mark_mr = FALSE,
  digits_cell = 0, digits_prob = 1, digits_er = 1
)

########################################################################
######### EXAMPLE 6 ####################################################
######### HAMDY TAHA 525 ###############################################
######### PROBLEM KÜMESİ 14.3a soru 5 ##################################
########################################################################

# ---------- Payoff table (Hisse Senedi) ----------
P <- matrix(c(
  850,100,
  400,  200
), nrow = 2, byrow = TRUE,
dimnames = list(c("Var","Yok"), c("Başarılı","Başarısız")))

p <- c(0.75,0.25)

########################################################################
######### 01) INITIAL DECISION TABLE ###################################
########################################################################

export_payoff_html_transport(
  file = "./examples/EX6/01_EX6_Başlangıç_Karar_Tablosu.html",
  P = P, p = p,
  show_probs_row = TRUE,
  show_ev_col    = FALSE,     # EV görünsün
  digits_cell = 1, digits_prob = 2, digits_ev = 2,
  row_label = "Reklam",
  col_group_label = "Ürün",
  mark_ml = FALSE, mark_ev = FALSE
)

########################################################################
#### 02) RISK ML GAIN ##################################################
########################################################################

res_ml <- export_most_likely_html_transport(
  file = "./examples/EX6/02_EX6_ML_gain.html",
  P = P, p = p, type = "gain",
  row_label = "Reklam",
  col_group_label = "Ürün",
  digits_cell = 1, digits_prob = 2, digits_ml = 2,
  show_ml_col = F
)



########################################################################
#### 03) RISK EV GAIN ##################################################
########################################################################


export_payoff_html_transport(
  file = "./examples/EX6/03_EX6_EV_gain.html",
  P = P, p = p,
  show_probs_row = TRUE,
  show_ev_col    = TRUE,     # EV görünsün
  digits_cell = 1, digits_prob = 2, digits_ev = 2,
  row_label = "Reklam",
  col_group_label = "Ürün",
  mark_ml = F
)







########################################################################
#### 04) RISK REGRET GAIN ##############################################
########################################################################

export_regret_html_transport(
  file = "./examples/EX6/06_EX4_REGRET_gain.html",
  P = P, p = p, type = "gain",
  show_probs_row = TRUE, show_er_col = TRUE,
  mark_er = TRUE, mark_mr = FALSE,
  row_label = "Reklam",
  col_group_label = "Ürün",
  digits_cell = 1, digits_prob = 2, digits_er = 2
)



