source('decision_uncertainty.R')


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



paths <- export_uncertainty_tables_all_transport(
  file_prefix = "EX01_gain",
  P = P,
  type = "gain",
  alpha = 0.6,
  row_label = "Strateji",
  col_group_label = "Olay",
  digits_prob   = 2,
  digits_metric = 2
)

paths <- export_uncertainty_tables_all_transport(
  file_prefix = "EX01_loss",
  P = P,
  type = "loss",
  alpha = 0.3,
  row_label = "Strateji",
  col_group_label = "Olay",
  digits_prob   = 2,
  digits_metric = 2
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


export_uncertainty_tables_all_transport(
  file_prefix = "Ex02_gain",
  P = P,
  type = "gain",
  alpha = 0.5,
  row_label = "Sipariş",
  col_group_label = "İstem Miktarı",
  digits_prob   = 2,
  digits_metric = 0
)

export_uncertainty_tables_all_transport(
  file_prefix = "Ex02_loss",
  P = P,
  type = "loss",
  alpha = 0.2,
  row_label = "Sipariş",
  col_group_label = "İstem Miktarı",
  digits_prob   = 2,
  digits_metric = 0
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


export_uncertainty_tables_all_transport(
  file_prefix = "Ex03_gain",
  P = P,
  type = "gain",
  alpha = 0.25,
  digits_metric = 0,
  row_label = "Hisse Senedi",
  col_group_label = "Yıllık Getiri"
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

export_uncertainty_tables_all_transport(
  file_prefix = "Ex04_gain",
  P = P,
  type = "gain",
  alpha = 0.8,
  digits_metric = 0,
  row_label = "Fon",
  col_group_label = "Gelecekteki Pazar Durumu (% Değişim)",
  digits_by_method = list(
    laplace = list(
      cell   = 0,   # Payoff hücreleri tamsayı
      metric = 3,   # Laplace kriteri 2 ondalık
      prob   = 3    # olasılıklar 3 ondalık
    ),
    hurwicz = list(
      cell   = 0,   # orijinal payoff tamsayı
      metric = 1    # en iyi/en kötü/Hurwicz 2 ondalık
    )
  )
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


export_uncertainty_tables_all_transport(
  file_prefix = "Ex05_loss",
  P = P,
  type = "loss",
  alpha = 0.5,
  row_label = "Kamp Yeri Kapasitesi",
  col_group_label = "Katılımcı Sayısı",
  digits_metric = 0,
  digits_by_method = list(
    laplace = list(
      cell   = 0,   # Payoff hücreleri tamsayı
      metric = 1,   # Laplace kriteri 2 ondalık
      prob   = 2    # olasılıklar 3 ondalık
    ),
    hurwicz = list(
      cell   = 0,   # orijinal payoff tamsayı
      metric = 1    # en iyi/en kötü/Hurwicz 2 ondalık
    )
  )
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


export_uncertainty_tables_all_transport(
  file_prefix = "Ex05_gain",
  P = P,
  type = "gain",
  alpha = 0.6,
  row_label = "Strateji",
  col_group_label = "Olay",
  digits_metric = 0,
  row_label = "Reklam",
  col_group_label = "Ürün"
)