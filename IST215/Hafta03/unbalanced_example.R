source("export_transport_demo.R")

# (1) Etiketler – Türkçe
lbl_tr <- list(
  row_names = c("İzmit","Bursa","Manisa","Adana"),
  col_names = c("Ankara","İzmir","Antalya","Gaziantep"),
  supply_label = "Arz", demand_label = "Talep",
  initial_title = "Başlangıç Tablosu (Birim Maliyetler)",
  allocation_title = "Dağıtım – %METHOD%",
  total_cost = "Toplam Maliyet"
)

# (2) Dengeli örnek
C_bal <- matrix(c(4,3,4,5, 6,8,5,8, 3,4,5,5, 1,2,3,4), nrow=4, byrow=TRUE)
sup_bal <- c(40, 60, 40, 50)
dem_bal <- c(55, 25, 50, 60)

export_transport_demo("dengeli_ornek",
                      C_bal, sup_bal, dem_bal,
                      labels = lbl_tr)

# (3) Dengesiz örnek (Talep > Arz → dummy satır)
C_unb  <- matrix(c(6,1,9, 5,7,4), nrow=2, byrow=TRUE)
sup_unb <- c(30, 20)     # 50
dem_unb <- c(10, 35, 20) # 65 → 15 açık

export_transport_demo("dengesiz_ornek",
                      C_unb, sup_unb, dem_unb,
                      labels = list(dummy_row_name="(Dummy Kaynak)"),
                      allow_unbalanced = TRUE,
                      dummy_row_cost = 0,
                      # sonuç tablolarında dummy gizle, adım sayfalarında göster
                      drop_dummy_in_initial_outputs = TRUE,
                      drop_dummy_in_optimized_outputs = TRUE,
                      drop_dummy_in_step_outputs = FALSE)
