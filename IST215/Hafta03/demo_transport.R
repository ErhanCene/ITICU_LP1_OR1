

# ============================================================
# 1) Example data
# ============================================================
C <- matrix(c(4,3,4,5,
              6,8,5,8,
              3,4,5,5,
              1,2,3,4), nrow=4, byrow=TRUE)
sup <- c(40, 60, 40, 50)
dem <- c(55, 25, 50, 60)

# ============================================================
# 2) Turkish labels (BigM-style)
# ============================================================
lbl <- list(
  row_names = c("Fabrika 1","Fabrika 2","Fabrika 3","Fabrika 4"),
  col_names = c("Depo 1","Depo 2","Depo 3","Depo 4"),
  supply_label = "Arz",
  demand_label = "Talep",
  initial_title = "Başlangıç Tablosu (Birim Maliyetler)",
  allocation_title = "Dağıtım – %METHOD%",
  total_cost = "Toplam Maliyet"
)

# ============================================================
# 3) Output dir
# ============================================================
outdir <- "./examples/example1"
if (!dir.exists(outdir)) dir.create(outdir)

# ============================================================
# 4) Save initial (input) table
# ============================================================
cat(
  transport_initial_html(C, sup, dem, labels = lbl),
  file = file.path(outdir, "00_baslangic_tablosu.html")
)

# ============================================================
# 5) Run all methods + optimizers; save results
# ============================================================
methods    <- c("northwest","rowmin","colmin","leastcost","vam")
optimizers <- c("none","modi","stepping","both")

save_alloc_table <- function(alloc, title, fname){
  cat(
    transport_result_from_alloc_html(C, alloc, title = title, labels = lbl),
    file = file.path(outdir, fname)
  )
}

for (m in methods){
  # Initial (no optimization)
  res_init <- solve_transport(C, sup, dem, method = m, optimize = "none")
  title_init <- sprintf("Dağıtım – %s (%s)", method_title(m), opt_title("none"))
  save_alloc_table(res_init$allocation, title_init, sprintf("10_%s_initial.html", m))
  
  # Optimized variants
  for (opt in optimizers[optimizers != "none"]){
    res_opt <- solve_transport(C, sup, dem, method = m, optimize = opt)
    title_opt <- sprintf("Dağıtım – %s (%s)", method_title(m), opt_title(opt))
    save_alloc_table(res_opt$allocation, title_opt, sprintf("20_%s_%s.html", m, opt))
  }
}

# ============================================================
# 6) Step-by-step pages with "mevcut (toplam)" for Arz/Talep
# ============================================================
for (m in methods){
  res_trace <- solve_transport(C, sup, dem, method = m, optimize = "both", trace = TRUE)
  
  # Index page for steps
  idx_path <- file.path(outdir, sprintf("30_%s_adimlar_index.html", m))
  idx <- c("<html><head><meta charset='utf-8'><title>Adımlar</title></head><body>",
           sprintf("<h2>%s – Adımlar (Başlangıç + İyileştirme)</h2>", method_title(m)),
           "<ol>")
  
  for (t in seq_along(res_trace$steps)){
    st <- res_trace$steps[[t]]
    step_title <- sprintf("Adım %d — %s",
                          st$k,
                          if (is.null(st$info$note)) st$stage else paste0(st$stage, ": ", st$info$note))
    fname <- sprintf("30_%s_step_%03d.html", m, t)
    
    # NEW: use the step helper with current vs total in parens
    cat(
      transport_result_from_alloc_step_html(
        cost = C,
        alloc = st$alloc,
        supply_total = sup,
        demand_total = dem,
        title = paste0(method_title(m), " – ", step_title),
        labels = lbl,
        include_css = TRUE
      ),
      file = file.path(outdir, fname)
    )
    
    idx <- c(idx, sprintf("<li><a href='%s'>%s</a></li>", fname, step_title))
  }
  idx <- c(idx, "</ol>",
           "<p><em>Not:</em> Arz/Talep hücrelerinde <b>mevcut</b> miktar ve parantez içinde <b>toplam</b> gösterilir.</p>",
           "</body></html>")
  writeLines(idx, idx_path)
}

# ============================================================
# 7) Summary index linking all outputs
# ============================================================
summary_html <- file.path(outdir, "01_ozet_index.html")
lines <- c("<html><head><meta charset='utf-8'><title>Transport Çıktıları</title></head><body>",
           "<h1>Transport Problemi – Tüm Yöntemler</h1>",
           "<ul>",
           sprintf("<li><a href='%s'>Başlangıç Tablosu (Birim Maliyetler)</a></li>", "00_baslangic_tablosu.html"),
           "</ul>",
           "<h2>Yöntemlere Göre Dağıtımlar</h2>")
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

message("Bitti. Dizin dosyası: ", summary_html)
# browseURL(summary_html)  # isterseniz otomatik açın
