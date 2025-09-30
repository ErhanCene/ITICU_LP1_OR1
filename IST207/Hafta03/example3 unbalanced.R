source("transportation.R")

source("export_transport_demo.R")

# ÖRNEK 3: DENGESIZ (Arz > Talep → dummy SÜTUN)
C_unb <- matrix(c(10,6,5,
                  7,8,8,
                  6,9,12), nrow=3, byrow=TRUE)
sup_unb <- c(200, 400,250)            # 50
dem_unb <- c(250, 200, 350)        # 65 → 15 açık (dummy row)

# Etiketler (TR)
lbl <- list(
  row_names = c("F1","F2","F3"),
  col_names = c("D1","D2","D3"),
  supply_label = "Arz", demand_label = "Talep",
  initial_title = "Başlangıç Tablosu (Birim Maliyetler)",
  allocation_title = "Dağıtım – %METHOD%",
  total_cost = "Toplam Maliyet"
)

export_transport_demo("dengesiz_ornek2",
                      C_unb, sup_unb, dem_unb,
                      labels = list(dummy_col_name="(Dummy Hedef)"),
                      allow_unbalanced = TRUE,
                      dummy_col_cost = 0,
                      # sonuç tablolarında dummy gizle, adım sayfalarında göster
                      drop_dummy_in_initial_outputs = FALSE,
                      drop_dummy_in_optimized_outputs = FALSE,
                      drop_dummy_in_step_outputs = FALSE)
