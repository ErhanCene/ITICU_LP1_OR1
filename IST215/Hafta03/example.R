source("transportation.R")

# ============================================================
# DEMO (unbalanced destekli, önceki demoya benzer)
# ============================================================

# ÖRNEK 1: DENGELI
C_bal <- matrix(c(4,3,4,5,
                  6,8,5,8,
                  3,4,5,5,
                  1,2,3,4), nrow=4, byrow=TRUE)
sup_bal <- c(40, 60, 40, 50)
dem_bal <- c(55, 25, 50, 60)

# ÖRNEK 2: DENGESIZ (Talep > Arz → dummy SATIR)
C_unb <- matrix(c(6,1,9,
                  5,7,4), nrow=2, byrow=TRUE)
sup_unb <- c(30, 20)            # 50
dem_unb <- c(10, 35, 20)        # 65 → 15 açık (dummy row)

# Etiketler (TR)
lbl <- list(
  row_names = c("İzmit","Bursa","Manisa","Adana"),
  col_names = c("Ankara","İzmir","Antalya","Gaziantep"),
  supply_label = "Arz", demand_label = "Talep",
  initial_title = "Başlangıç Tablosu (Birim Maliyetler)",
  allocation_title = "Dağıtım – %METHOD%",
  total_cost = "Toplam Maliyet"
)

outdir <- "transport_demo_unbalanced"
if (!dir.exists(outdir)) dir.create(outdir)

# ---------- 1) Balanced: giriş tablosu ----------
cat( transport_initial_html(C_bal, sup_bal, dem_bal, labels = lbl),
     file = file.path(outdir, "00_bal_baslangic.html") )

# ---------- 2) Unbalanced: orijinal giriş + augment edilmiş giriş ----------
cat( transport_initial_html(C_unb, sup_unb, dem_unb,
                            labels = list(initial_title="Başlangıç (Dengesiz - Orijinal)")),
     file = file.path(outdir, "00_unb_orijinal.html") )

# Augment edilmiş (dummy eklenmiş) girişi üretmek için bir kez çalıştırıp augmented bilgisi alalım:
tmp <- solve_transport(C_unb, sup_unb, dem_unb, method="northwest", optimize="none",
                       allow_unbalanced=TRUE, dummy_row_cost=0, drop_dummy_in_output=FALSE)
# Dummy görünür başlangıç tablosu:
lbl_aug <- labels_with_dummy(.transport_labels_default(), tmp$augmented,
                             drop_dummy_in_output = FALSE,
                             m_after = nrow(tmp$cost), n_after = ncol(tmp$cost),
                             dummy_row_name="(Dummy Kaynak)", dummy_col_name="(Dummy Hedef)")
cat( transport_initial_html(tmp$cost, tmp$supply, tmp$demand,
                            labels = modifyList(lbl_aug, list(initial_title="Başlangıç (Augment - Dummy dahil)")) ),
     file = file.path(outdir, "01_unb_augmented.html") )

# ---------- 3) Tüm yöntemler & optimizer’lar (balanced örnek) ----------
methods    <- c("northwest","rowmin","colmin","leastcost","vam")
optimizers <- c("none","modi","stepping","both")


for (m in methods){
  # initial
  r0 <- solve_transport(C_bal, sup_bal, dem_bal, method=m, optimize="none")
  save_alloc_table(r0$cost, r0$allocation,
                   sprintf("Dağıtım – %s (%s)", method_title(m), opt_title("none")),
                   sprintf("10_bal_%s_initial.html", m), lbl)
  # optimized
  for (opt in optimizers[optimizers!="none"]){
    r1 <- solve_transport(C_bal, sup_bal, dem_bal, method=m, optimize=opt)
    save_alloc_table(r1$cost, r1$allocation,
                     sprintf("Dağıtım – %s (%s)", method_title(m), opt_title(opt)),
                     sprintf("20_bal_%s_%s.html", m, opt), lbl)
  }
}

# ---------- 4) Dengesiz örnek: VAM + MODI/Stepping (dummy gizli & görünür) ----------
# Gizli (drop_dummy_in_output=TRUE) → sonuçta dummy satır yok
ru_hide <- solve_transport(C_unb, sup_unb, dem_unb, method="vam", optimize="both",
                           allow_unbalanced=TRUE, dummy_row_cost=0, drop_dummy_in_output=TRUE)
save_alloc_table(ru_hide$cost, ru_hide$allocation,
                 "Dağıtım – VAM (Dengesiz, Dummy gizli) – MODI+Stepping",
                 "40_unb_vam_both_hidden.html",
                 labels = list(row_prefix="Kaynak", col_prefix="Hedef",
                               supply_label="Arz", demand_label="Talep",
                               total_cost="Toplam Maliyet"))

# Görünür (drop_dummy_in_output=FALSE) → dummy satır/kol ekrana yansır ve isimlendirilir
ru_show <- solve_transport(C_unb, sup_unb, dem_unb, method="vam", optimize="both",
                           allow_unbalanced=TRUE, dummy_row_cost=0, drop_dummy_in_output=FALSE)
lbl_show <- labels_with_dummy(.transport_labels_default(), ru_show$augmented,
                              drop_dummy_in_output=FALSE,
                              m_after=nrow(ru_show$cost), n_after=ncol(ru_show$cost),
                              dummy_row_name="(Dummy Kaynak)", dummy_col_name="(Dummy Hedef)")
save_alloc_table(ru_show$cost, ru_show$allocation,
                 "Dağıtım – VAM (Dengesiz, Dummy görünür) – MODI+Stepping",
                 "41_unb_vam_both_visible.html", lbl_show)

# ---------- 5) Adım adım sayfalar (mevcut/Toplam) – dengesiz örnek ----------
# İz sürme: hem başlangıç hem iyileştirme adımlarını al
rt <- solve_transport(C_unb, sup_unb, dem_unb, method="northwest", optimize="both",
                      trace=TRUE, allow_unbalanced=TRUE, drop_dummy_in_output=FALSE)
# Adımlar için toplamlar (augment edilmiş toplamları göstermek daha doğru)
sup_total_aug <- rt$supply
dem_total_aug <- rt$demand
# Etiketler: dummy isimleri eklensin
lbl_steps <- labels_with_dummy(.transport_labels_default(), rt$augmented,
                               drop_dummy_in_output=FALSE,
                               m_after=nrow(rt$cost), n_after=ncol(rt$cost),
                               dummy_row_name="(Dummy Kaynak)", dummy_col_name="(Dummy Hedef)")

idxp <- file.path(outdir, "50_unb_steps_index.html")
idx <- c("<html><head><meta charset='utf-8'><title>Adımlar</title></head><body>",
         "<h2>Dengesiz Örnek – Adımlar (Başlangıç + İyileştirme)</h2>","<ol>")
for (t in seq_along(rt$steps)){
  st <- rt$steps[[t]]
  step_title <- sprintf("Adım %d — %s", st$k,
                        if (is.null(st$info$note)) st$stage else paste0(st$stage, ": ", st$info$note))
  fname <- sprintf("51_unb_step_%03d.html", t)
  cat(
    transport_result_from_alloc_step_html(
      cost = rt$cost, alloc = st$alloc,
      supply_total = sup_total_aug, demand_total = dem_total_aug,
      title = step_title, labels = lbl_steps, include_css = TRUE
    ),
    file = file.path(outdir, fname)
  )
  idx <- c(idx, sprintf("<li><a href='%s'>%s</a></li>", fname, step_title))
}
idx <- c(idx, "</ol>",
         "<p><em>Not:</em> Arz/Talep hücrelerinde <b>mevcut</b> miktar ve parantez içinde <b>toplam</b> gösterilir.</p>",
         "</body></html>")
writeLines(idx, idxp)

# ---------- 6) Özet Index ----------
summary_html <- file.path(outdir, "01_ozet_index.html")
lines <- c("<html><head><meta charset='utf-8'><title>Transport Çıktıları</title></head><body>",
           "<h1>Transport Problemi – Balanced & Unbalanced</h1>",
           "<ul>",
           "<li><a href='00_bal_baslangic.html'>Balanced – Başlangıç</a></li>",
           "<li><a href='00_unb_orijinal.html'>Unbalanced – Başlangıç (Orijinal)</a></li>",
           "<li><a href='01_unb_augmented.html'>Unbalanced – Başlangıç (Augment, Dummy dahil)</a></li>",
           "</ul>",
           "<h2>Balanced: Yöntemlere Göre Dağıtımlar</h2>")
for (m in methods){
  lines <- c(lines, sprintf("<h3>%s</h3>", method_title(m)), "<ul>",
             sprintf("<li><a href='%s'>Başlangıç</a></li>", sprintf("10_bal_%s_initial.html", m)))
  for (opt in optimizers[optimizers!="none"]){
    lines <- c(lines,
               sprintf("<li><a href='%s'>%s</a></li>",
                       sprintf("20_bal_%s_%s.html", m, opt), opt_title(opt)))
  }
  lines <- c(lines, "</ul>")
}
lines <- c(lines,
           "<h2>Unbalanced: Örnek Çıktılar</h2>",
           "<ul>",
           "<li><a href='40_unb_vam_both_hidden.html'>VAM – MODI+Stepping (Dummy gizli)</a></li>",
           "<li><a href='41_unb_vam_both_visible.html'>VAM – MODI+Stepping (Dummy görünür)</a></li>",
           "<li><a href='50_unb_steps_index.html'>Adım Adım (mevcut / toplam)</a></li>",
           "</ul>",
           "</body></html>")
writeLines(lines, summary_html)

message("Bitti. Çıktılar: ", summary_html)
# browseURL(summary_html)  # isterseniz açın
