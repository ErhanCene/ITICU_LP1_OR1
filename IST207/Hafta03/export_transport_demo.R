# ============================================================
# export_transport_demo()
# Tüm yöntemleri çalıştırır, HTML çıktıları üretir (balanced/unbalanced).
# ============================================================
export_transport_demo <- function(dataset_name,
                                  C, sup, dem,
                                  methods    = c("northwest","rowmin","colmin","leastcost","vam"),
                                  optimizers = c("none","modi","stepping","both"),
                                  labels     = list(),
                                  outdir_root = "transport_demo_unified",
                                  # Unbalanced kontrol ve dummy maliyetleri
                                  allow_unbalanced = TRUE,
                                  dummy_row_cost = 0,
                                  dummy_col_cost = 0,
                                  # Girdiler: unbalanced ise augment edilmiş başlangıç tablosunu da göster
                                  show_augmented_input = TRUE,
                                  # Çıktılarda dummy görünürlüğü
                                  drop_dummy_in_initial_outputs = TRUE,   # yöntem sonuç tablolarında gizle
                                  drop_dummy_in_optimized_outputs = TRUE, # optimize sonuç tablolarında gizle
                                  drop_dummy_in_step_outputs = FALSE      # adım sayfalarında GÖSTER (boyut/izleme için önerilir)
){
  # --- Klasör ---
  lbl <- modifyList(.transport_labels_default(), labels, keep.null = TRUE)
  outdir <- file.path(outdir_root, dataset_name)
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # --- 0) Orijinal başlangıç tablosu ---
  cat(
    transport_initial_html(C, sup, dem, labels = lbl),
    file = file.path(outdir, "00_baslangic_orijinal.html")
  )
  
  # --- 1) Unbalanced ise augment edilmiş başlangıç tablosu (opsiyonel) ---
  augmented_info <- NULL
  if (show_augmented_input) {
    tmp <- solve_transport(C, sup, dem,
                           method="northwest", optimize="none",
                           allow_unbalanced = allow_unbalanced,
                           dummy_row_cost = dummy_row_cost,
                           dummy_col_cost = dummy_col_cost,
                           drop_dummy_in_output = FALSE)
    augmented_info <- tmp$augmented
    # Dummy isimlerini otomatik ekleyelim
    lbl_aug <- labels_with_dummy(lbl, tmp$augmented,
                                 drop_dummy_in_output = FALSE,
                                 m_after = nrow(tmp$cost), n_after = ncol(tmp$cost),
                                 dummy_row_name="(Dummy Kaynak)", dummy_col_name="(Dummy Hedef)")
    cat(
      transport_initial_html(tmp$cost, tmp$supply, tmp$demand,
                             labels = modifyList(lbl_aug, list(initial_title="Başlangıç (Augment - Dummy dahil)"))),
      file = file.path(outdir, "01_baslangic_augmented.html")
    )
  }
  
  # --- 2) Yöntem bazlı: başlangıç ve optimize sonuç tabloları ---
  save_alloc_table <- function(cost, alloc, title, fname, labels_obj){
    cat(
      transport_result_from_alloc_html(cost, alloc, title = title, labels = labels_obj),
      file = file.path(outdir, fname)
    )
  }
  
  for (m in methods){
    # Başlangıç (dummy görünürlüğü parametresiyle)
    r0 <- solve_transport(C, sup, dem, method = m, optimize = "none",
                          allow_unbalanced = allow_unbalanced,
                          dummy_row_cost = dummy_row_cost,
                          dummy_col_cost = dummy_col_cost,
                          drop_dummy_in_output = drop_dummy_in_initial_outputs)
    lbl_init <- if (!drop_dummy_in_initial_outputs) {
      labels_with_dummy(lbl, r0$augmented, FALSE, nrow(r0$cost), ncol(r0$cost))
    } else lbl
    save_alloc_table(r0$cost, r0$allocation,
                     sprintf("Dağıtım – %s (%s)", method_title(m), opt_title("none")),
                     sprintf("10_%s_initial.html", m),
                     labels_obj = lbl_init)
    
    # Optimizasyonlar
    for (opt in optimizers[optimizers != "none"]){
      r1 <- solve_transport(C, sup, dem, method = m, optimize = opt,
                            allow_unbalanced = allow_unbalanced,
                            dummy_row_cost = dummy_row_cost,
                            dummy_col_cost = dummy_col_cost,
                            drop_dummy_in_output = drop_dummy_in_optimized_outputs)
      lbl_opt <- if (!drop_dummy_in_optimized_outputs) {
        labels_with_dummy(lbl, r1$augmented, FALSE, nrow(r1$cost), ncol(r1$cost))
      } else lbl
      save_alloc_table(r1$cost, r1$allocation,
                       sprintf("Dağıtım – %s (%s)", method_title(m), opt_title(opt)),
                       sprintf("20_%s_%s.html", m, opt),
                       labels_obj = lbl_opt)
    }
  }
  
  # --- 3) Adım adım (trace) sayfaları ---
  # Adımlarda dummy'yi görünür tutmak iyi olur (boyut sabitliği ve eğitim için)
  for (m in methods){
    tr <- solve_transport(C, sup, dem, method = m, optimize = "both",
                          trace = TRUE,
                          allow_unbalanced = allow_unbalanced,
                          dummy_row_cost = dummy_row_cost,
                          dummy_col_cost = dummy_col_cost,
                          drop_dummy_in_output = drop_dummy_in_step_outputs)
    
    sup_total <- tr$supply
    dem_total <- tr$demand
    
    lbl_steps <- if (!drop_dummy_in_step_outputs) {
      labels_with_dummy(lbl, tr$augmented, FALSE, nrow(tr$cost), ncol(tr$cost))
    } else lbl
    
    render_trace_steps(tr,
                       dataset_title = method_title(m),
                       outdir = outdir,
                       method_label = m,
                       labels_for_steps = lbl_steps,
                       max_steps = 2000)   # güvenli üst sınır
  }
  
  
  # --- 4) Özet Index ---
  summary_html <- file.path(outdir, "01_ozet_index.html")
  lines <- c("<html><head><meta charset='utf-8'><title>Transport Çıktıları</title></head><body>",
             sprintf("<h1>Transport Problemi – %s</h1>", .html_escape(dataset_name)),
             "<ul>",
             "<li><a href='00_baslangic_orijinal.html'>Başlangıç Tablosu (Orijinal)</a></li>")
  if (show_augmented_input) {
    lines <- c(lines, "<li><a href='01_baslangic_augmented.html'>Başlangıç (Augment - Dummy dahil)</a></li>")
  }
  lines <- c(lines, "</ul>", "<h2>Yöntemlere Göre Dağıtımlar</h2>")
  for (m in methods){
    lines <- c(lines, sprintf("<h3>%s</h3>", method_title(m)), "<ul>",
               sprintf("<li><a href='%s'>Başlangıç</a></li>", sprintf("10_%s_initial.html", m)))
    for (opt in optimizers[optimizers != "none"]){
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         sprintf("20_%s_%s.html", m, opt), opt_title(opt)))
    }
    lines <- c(lines,
               sprintf("<li><a href='%s'>Adım Adım (mevcut / toplam)</a></li>",
                       sprintf("30_%s_adimlar_index.html", m)),
               "</ul>")
  }
  lines <- c(lines, "</body></html>")
  writeLines(lines, summary_html)
  
  message("Tamamlandı. Özet dizin: ", summary_html)
  invisible(summary_html)
}
