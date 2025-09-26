source("transportation.R")

source("export_transport_demo.R")

# ÖRNEK 4: DENGESIZ (Arz > Talep → dummy SÜTUN)
C_unb <- matrix(c(45,17,21,30,
                  14,18,19,31), nrow=2, byrow=TRUE)
sup_unb <- c(15, 13)            # 50
dem_unb <- c(9, 6, 7,9)        # 65 → 15 açık (dummy row)

# Etiketler (TR)
lbl <- list(
  row_names = c("S1","S2"),
  col_names = c("D1","D2","D3","D4"),
  supply_label = "Arz", demand_label = "Talep",
  initial_title = "Başlangıç Tablosu (Birim Maliyetler)",
  allocation_title = "Dağıtım – %METHOD%",
  total_cost = "Toplam Maliyet"
)

export_transport_demo("dengesiz_ornek3",
                      C_unb, sup_unb, dem_unb,
                      labels = list(dummy_row_name="(Dummy Arz)"),
                      allow_unbalanced = TRUE,
                      dummy_row_cost = 0,
                      # sonuç tablolarında dummy gizle, adım sayfalarında göster
                      drop_dummy_in_initial_outputs = FALSE,
                      drop_dummy_in_optimized_outputs = FALSE,
                      drop_dummy_in_step_outputs = FALSE)
