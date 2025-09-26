
source('transportation.R')


################# EXAMPLE 1 #####################################################
# ============================================================
# 1) Example data
# ============================================================
C <- matrix(c(3,5,7,10,
              6,8,13,5,
              4,2,6,7,
              12,9,4,10), nrow=4, byrow=TRUE)
sup <- c(50, 200, 150, 300)
dem <- c(150, 75, 175, 300)

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

for (m in methods){
  # Initial (no optimization)
  res_init <- solve_transport(
    C, sup, dem,
    method = m,
    optimize = "none",
    drop_dummy_in_output = FALSE   # <- safer if a case is unbalanced later
  )
  title_init <- sprintf("Dağıtım – %s (%s)", method_title(m), opt_title("none"))
  save_alloc_table(
    res_init,                                 # pass the WHOLE result
    title_init,
    sprintf("10_%s_initial.html", m),
    lbl
  )
  
  # Optimized variants
  for (opt in optimizers[optimizers != "none"]){
    res_opt <- solve_transport(
      C, sup, dem,
      method = m,
      optimize = opt,
      drop_dummy_in_output = FALSE           # keep shapes aligned with any dummy
    )
    title_opt <- sprintf("Dağıtım – %s (%s)", method_title(m), opt_title(opt))
    save_alloc_table(
      res_opt,                               # pass the WHOLE result
      title_opt,
      sprintf("20_%s_%s.html", m, opt),
      lbl
    )
  }
}


# ============================================================
# 6) Step-by-step pages with "mevcut (toplam)" for Arz/Talep
# ============================================================
for (m in methods){
  res_trace <- solve_transport(C, sup, dem, method = m, optimize = "both", trace = TRUE)
  render_trace_steps(res_trace, method_title(m), outdir, m, lbl)
}

# ============================================================
# 6b) MODI u–v / R tabloları (her yöntem için)
#    Not: trace=TRUE ve optimize='modi' olmalı ki u–v snapshot'ları üretelim
# ============================================================
for (m in methods){
  res_modi_trace <- solve_transport(C, sup, dem, method = m, optimize = "modi", trace = TRUE)
  render_modi_uv_tables(res_modi_trace, outdir = outdir, method_label = m, labels_for_steps = lbl)
}

# ============================================================
# 6c) MODI (split): u–v / eşitlikler / R ayrı dosyalar
# ============================================================
for (m in methods){
  res_modi_trace <- solve_transport(C, sup, dem,
                                    method = m,
                                    optimize = "modi",
                                    trace = TRUE,
                                    allow_unbalanced = TRUE,
                                    drop_dummy_in_output = FALSE)
  render_modi_split(res_modi_trace, outdir = outdir,
                    method_label = m, labels_for_steps = lbl)
}

# ============================================================
# 6d) MODI u–v on FINAL table (overlay uᵢ, vⱼ and show R in cells)
#     Links back to the final MODI allocation page for each method.
# ============================================================
for (m in methods){
  # final result you want to link to (here: MODI)
  final_page <- sprintf("20_%s_%s.html", m, "modi")
  final_res  <- solve_transport(C, sup, dem, method = m, optimize = "modi",
                                allow_unbalanced = TRUE, drop_dummy_in_output = FALSE)
  # produce raw LaTeX of the FINAL MODI cost calculation (HTML + .tex)
  transport_export_latex_equation_html(
    cost  = C,
    alloc = final_res$allocation,
    file  = file.path(outdir, sprintf("21_%s_modi_final_cost_eq.html", m)),
    title = sprintf("%s – MODI Nihai Toplam Maliyet (LaTeX – ham)", method_title(m)),
    render_math = FALSE
  )
  # a MODI trace to get per-step u,v snapshots
  tr <- solve_transport(C, sup, dem, method = m, optimize = "modi", trace = TRUE,
                        allow_unbalanced = TRUE, drop_dummy_in_output = FALSE)
  
  render_modi_uv_on_final(tr, final_res, outdir = outdir,
                          method_label = m, labels_for_steps = lbl,
                          final_href = final_page,
                          render_math = TRUE)
}


# ============================================================
# 6c) MODI step packages: (u/v + final table) + (u–v LaTeX) + (Rij table + LaTeX)
# ============================================================
for (m in methods){
  res_modi_trace <- solve_transport(
    C, sup, dem,
    method = m,
    optimize = "modi",
    trace = TRUE,
    drop_dummy_in_output = FALSE
  )
  render_modi_step_packages(
    tr = res_modi_trace,
    outdir = outdir,
    method_label = m,
    labels_for_steps = lbl,
    math_preview = FALSE   # set TRUE if you also want rendered MathJax previews
  )
}

# ============================================================
# 7) Summary index linking all outputs
# ============================================================
# ============================================================
# Build summary index (adds cost-only table + LaTeX eq links)
# ============================================================
build_transport_summary_index <- function(C, sup, dem, outdir,
                                          methods    = c("northwest","rowmin","colmin","leastcost","vam"),
                                          optimizers = c("none","modi","stepping","both"),
                                          labels     = .transport_labels_default(),
                                          generate_cost_only = TRUE,
                                          generate_initial_eq = TRUE,
                                          eq_render_math = FALSE) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  # 1) Optional: generate cost-only initial table
  cost_only_file <- file.path(outdir, "05_maliyetler_sadece.html")
  if (generate_cost_only) {
    cat(
      transport_initial_costs_only_html(
        cost = C,
        labels = labels,
        title = "Birim Maliyet Tablosu (Sadece Maliyetler)"
      ),
      file = cost_only_file
    )
  }
  
  # 2) Optional: generate LaTeX RAW equation per method (for initial allocations)
  if (generate_initial_eq) {
    for (m in methods) {
      res_init <- solve_transport(C, sup, dem, method = m, optimize = "none")
      transport_export_latex_equation_html(
        cost = C,
        alloc = res_init$allocation,
        file  = file.path(outdir, sprintf("11_%s_initial_eq.html", m)),
        title = sprintf("%s – Başlangıç Toplam Maliyet (LaTeX)", method_title(m)),
        render_math = eq_render_math
      )
    }
  }
  
  # 3) Build the summary index
  summary_html <- file.path(outdir, "01_ozet_index.html")
  H <- function(x) .html_escape(x)
  
  lines <- c(
    "<html><head><meta charset='utf-8'><title>Transport Çıktıları</title>",
    "<style>body{font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:20px} h1,h2,h3{margin:.2em 0} ul{margin:.3em 0 1em 1.2em}</style>",
    "</head><body>",
    "<h1>Transport Problemi – Tüm Yöntemler</h1>",
    "<ul>",
    sprintf("<li><a href='%s'>%s</a></li>", "00_baslangic_tablosu.html", H("Başlangıç Tablosu (Birim Maliyetler)")),
    if (file.exists(cost_only_file))
      sprintf("<li><a href='%s'>%s</a></li>", "05_maliyetler_sadece.html", H("Birim Maliyet Tablosu (Sadece Maliyetler)"))
    else NULL,
    "</ul>",
    "<h2>Yöntemlere Göre Dağıtımlar</h2>"
  )
  
  for (m in methods) {
    lines <- c(lines, sprintf("<h3>%s</h3>", H(method_title(m))), "<ul>",
               sprintf("<li><a href='%s'>%s</a></li>", sprintf("10_%s_initial.html", m), H("Başlangıç")))
    for (opt in optimizers[optimizers != "none"]) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         sprintf("20_%s_%s.html", m, opt), H(opt_title(opt))))
    }
    # Step-by-step index
    lines <- c(lines,
               sprintf("<li><a href='%s'>%s</a></li>",
                       sprintf("30_%s_adimlar_index.html", m),
                       H("Adım Adım (mevcut / toplam)")))
    # NEW: MODI u–v tables index (if exists)
    modi_uv_file <- sprintf("40_%s_modi_index.html", m)
    if (file.exists(file.path(outdir, modi_uv_file))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         modi_uv_file, H("MODI u–v Hesapları (tablo)")))
    }
    
    
    # MODI step packages (u/v + final table, u–v LaTeX, R_ij table + LaTeX)
    pkg_idx <- sprintf("50_%s_modi_pkg_index.html", m)
    if (file.exists(file.path(outdir, pkg_idx))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         pkg_idx, H("MODI: u/v + R_ij + LaTeX (adım adım)")))
    }
    
    
    # NEW: MODI u–v overlay on final table
    modi_uv_final_idx <- sprintf("46_%s_uv_on_final_index.html", m)
    if (file.exists(file.path(outdir, modi_uv_final_idx))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         modi_uv_final_idx, H("MODI u–v (nihai tablo üzerinde)")))
    }
    
    
    
    # NEW: MODI split index (u–v / eşitlikler / R ayrı)
    modi_split_file <- sprintf("45_%s_modi_split_index.html", m)
    if (file.exists(file.path(outdir, modi_split_file))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         modi_split_file, H("MODI (u–v / eşitlikler / R ayrı)")))
    }
    
    
    
    # LaTeX (raw) link if exists
    latex_file <- sprintf("11_%s_initial_eq.html", m)
    if (file.exists(file.path(outdir, latex_file))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         latex_file, H("Başlangıç Maliyet Eşitliği (LaTeX – raw)")))
    }
    lines <- c(lines, "</ul>")
  }
  
  lines <- c(lines, "</body></html>")
  writeLines(lines, summary_html)
  message("Özet dizin yazıldı: ", summary_html)
}

build_transport_summary_index(
  C = C, sup = sup, dem = dem,
  outdir = outdir,
  methods = methods,
  optimizers = optimizers,
  labels = lbl,
  generate_cost_only = TRUE,     # writes 05_maliyetler_sadece.html
  generate_initial_eq = TRUE,    # writes 11_<method>_initial_eq.html
  eq_render_math = FALSE         # keep LaTeX RAW in those files
)


################# EXAMPLE 2 #####################################################
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
outdir <- "./examples/example2"
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

for (m in methods){
  # Initial (no optimization)
  res_init <- solve_transport(
    C, sup, dem,
    method = m,
    optimize = "none",
    drop_dummy_in_output = FALSE   # <- safer if a case is unbalanced later
  )
  title_init <- sprintf("Dağıtım – %s (%s)", method_title(m), opt_title("none"))
  save_alloc_table(
    res_init,                                 # pass the WHOLE result
    title_init,
    sprintf("10_%s_initial.html", m),
    lbl
  )
  
  # Optimized variants
  for (opt in optimizers[optimizers != "none"]){
    res_opt <- solve_transport(
      C, sup, dem,
      method = m,
      optimize = opt,
      drop_dummy_in_output = FALSE           # keep shapes aligned with any dummy
    )
    title_opt <- sprintf("Dağıtım – %s (%s)", method_title(m), opt_title(opt))
    save_alloc_table(
      res_opt,                               # pass the WHOLE result
      title_opt,
      sprintf("20_%s_%s.html", m, opt),
      lbl
    )
  }
}

# ============================================================
# 6) Step-by-step pages with "mevcut (toplam)" for Arz/Talep
# ============================================================
for (m in methods){
  res_trace <- solve_transport(C, sup, dem, method = m, optimize = "both", trace = TRUE)
  render_trace_steps(res_trace, method_title(m), outdir, m, lbl)
}

# ============================================================
# 6b) MODI u–v / R tabloları (her yöntem için)
#    Not: trace=TRUE ve optimize='modi' olmalı ki u–v snapshot'ları üretelim
# ============================================================
for (m in methods){
  res_modi_trace <- solve_transport(C, sup, dem, method = m, optimize = "modi", trace = TRUE)
  render_modi_uv_tables(res_modi_trace, outdir = outdir, method_label = m, labels_for_steps = lbl)
}

# ============================================================
# 6c) MODI (split): u–v / eşitlikler / R ayrı dosyalar
# ============================================================
for (m in methods){
  res_modi_trace <- solve_transport(C, sup, dem,
                                    method = m,
                                    optimize = "modi",
                                    trace = TRUE,
                                    allow_unbalanced = TRUE,
                                    drop_dummy_in_output = FALSE)
  render_modi_split(res_modi_trace, outdir = outdir,
                    method_label = m, labels_for_steps = lbl)
}

# ============================================================
# 6d) MODI u–v on FINAL table (overlay uᵢ, vⱼ and show R in cells)
#     Links back to the final MODI allocation page for each method.
# ============================================================
for (m in methods){
  # final result you want to link to (here: MODI)
  final_page <- sprintf("20_%s_%s.html", m, "modi")
  final_res  <- solve_transport(C, sup, dem, method = m, optimize = "modi",
                                allow_unbalanced = TRUE, drop_dummy_in_output = FALSE)
  # produce raw LaTeX of the FINAL MODI cost calculation (HTML + .tex)
  transport_export_latex_equation_html(
    cost  = C,
    alloc = final_res$allocation,
    file  = file.path(outdir, sprintf("21_%s_modi_final_cost_eq.html", m)),
    title = sprintf("%s – MODI Nihai Toplam Maliyet (LaTeX – ham)", method_title(m)),
    render_math = FALSE
  )
  # a MODI trace to get per-step u,v snapshots
  tr <- solve_transport(C, sup, dem, method = m, optimize = "modi", trace = TRUE,
                        allow_unbalanced = TRUE, drop_dummy_in_output = FALSE)
  
  render_modi_uv_on_final(tr, final_res, outdir = outdir,
                          method_label = m, labels_for_steps = lbl,
                          final_href = final_page,
                          render_math = TRUE)
}

# ============================================================
# 6c) MODI step packages: (u/v + final table) + (u–v LaTeX) + (Rij table + LaTeX)
# ============================================================
for (m in methods){
  res_modi_trace <- solve_transport(
    C, sup, dem,
    method = m,
    optimize = "modi",
    trace = TRUE,
    drop_dummy_in_output = FALSE
  )
  render_modi_step_packages(
    tr = res_modi_trace,
    outdir = outdir,
    method_label = m,
    labels_for_steps = lbl,
    math_preview = FALSE   # set TRUE if you also want rendered MathJax previews
  )
}


# ============================================================
# 7) Summary index linking all outputs
# ============================================================
# ============================================================
# Build summary index (adds cost-only table + LaTeX eq links)
# ============================================================
build_transport_summary_index <- function(C, sup, dem, outdir,
                                          methods    = c("northwest","rowmin","colmin","leastcost","vam"),
                                          optimizers = c("none","modi","stepping","both"),
                                          labels     = .transport_labels_default(),
                                          generate_cost_only = TRUE,
                                          generate_initial_eq = TRUE,
                                          eq_render_math = FALSE) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  # 1) Optional: generate cost-only initial table
  cost_only_file <- file.path(outdir, "05_maliyetler_sadece.html")
  if (generate_cost_only) {
    cat(
      transport_initial_costs_only_html(
        cost = C,
        labels = labels,
        title = "Birim Maliyet Tablosu (Sadece Maliyetler)"
      ),
      file = cost_only_file
    )
  }
  
  # 2) Optional: generate LaTeX RAW equation per method (for initial allocations)
  if (generate_initial_eq) {
    for (m in methods) {
      res_init <- solve_transport(C, sup, dem, method = m, optimize = "none")
      transport_export_latex_equation_html(
        cost = C,
        alloc = res_init$allocation,
        file  = file.path(outdir, sprintf("11_%s_initial_eq.html", m)),
        title = sprintf("%s – Başlangıç Toplam Maliyet (LaTeX)", method_title(m)),
        render_math = eq_render_math
      )
    }
  }
  
  # 3) Build the summary index
  summary_html <- file.path(outdir, "01_ozet_index.html")
  H <- function(x) .html_escape(x)
  
  lines <- c(
    "<html><head><meta charset='utf-8'><title>Transport Çıktıları</title>",
    "<style>body{font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:20px} h1,h2,h3{margin:.2em 0} ul{margin:.3em 0 1em 1.2em}</style>",
    "</head><body>",
    "<h1>Transport Problemi – Tüm Yöntemler</h1>",
    "<ul>",
    sprintf("<li><a href='%s'>%s</a></li>", "00_baslangic_tablosu.html", H("Başlangıç Tablosu (Birim Maliyetler)")),
    if (file.exists(cost_only_file))
      sprintf("<li><a href='%s'>%s</a></li>", "05_maliyetler_sadece.html", H("Birim Maliyet Tablosu (Sadece Maliyetler)"))
    else NULL,
    "</ul>",
    "<h2>Yöntemlere Göre Dağıtımlar</h2>"
  )
  
  for (m in methods) {
    lines <- c(lines, sprintf("<h3>%s</h3>", H(method_title(m))), "<ul>",
               sprintf("<li><a href='%s'>%s</a></li>", sprintf("10_%s_initial.html", m), H("Başlangıç")))
    for (opt in optimizers[optimizers != "none"]) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         sprintf("20_%s_%s.html", m, opt), H(opt_title(opt))))
    }
    # Step-by-step index
    lines <- c(lines,
               sprintf("<li><a href='%s'>%s</a></li>",
                       sprintf("30_%s_adimlar_index.html", m),
                       H("Adım Adım (mevcut / toplam)")))
    # NEW: MODI u–v tables index (if exists)
    modi_uv_file <- sprintf("40_%s_modi_index.html", m)
    if (file.exists(file.path(outdir, modi_uv_file))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         modi_uv_file, H("MODI u–v Hesapları (tablo)")))
    }
    
    # MODI step packages (u/v + final table, u–v LaTeX, R_ij table + LaTeX)
    pkg_idx <- sprintf("50_%s_modi_pkg_index.html", m)
    if (file.exists(file.path(outdir, pkg_idx))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         pkg_idx, H("MODI: u/v + R_ij + LaTeX (adım adım)")))
    }
    
    
    # NEW: MODI u–v overlay on final table
    modi_uv_final_idx <- sprintf("46_%s_uv_on_final_index.html", m)
    if (file.exists(file.path(outdir, modi_uv_final_idx))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         modi_uv_final_idx, H("MODI u–v (nihai tablo üzerinde)")))
    }
    
    
    # NEW: MODI split index (u–v / eşitlikler / R ayrı)
    modi_split_file <- sprintf("45_%s_modi_split_index.html", m)
    if (file.exists(file.path(outdir, modi_split_file))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         modi_split_file, H("MODI (u–v / eşitlikler / R ayrı)")))
    }
    
    # LaTeX (raw) link if exists
    latex_file <- sprintf("11_%s_initial_eq.html", m)
    if (file.exists(file.path(outdir, latex_file))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         latex_file, H("Başlangıç Maliyet Eşitliği (LaTeX – raw)")))
    }
    lines <- c(lines, "</ul>")
  }
  
  lines <- c(lines, "</body></html>")
  writeLines(lines, summary_html)
  message("Özet dizin yazıldı: ", summary_html)
}

build_transport_summary_index(
  C = C, sup = sup, dem = dem,
  outdir = outdir,
  methods = methods,
  optimizers = optimizers,
  labels = lbl,
  generate_cost_only = TRUE,     # writes 05_maliyetler_sadece.html
  generate_initial_eq = TRUE,    # writes 11_<method>_initial_eq.html
  eq_render_math = FALSE         # keep LaTeX RAW in those files
)

################# EXAMPLE 3 #####################################################
# ============================================================
# 1) Example data
# ============================================================
C <- matrix(c(15,18,12,13,
              10,10,11,9,
              8,5,7,8), nrow=3, byrow=TRUE)
sup <- c(200, 300, 450)
dem <- c(250, 100, 225, 325)

# ============================================================
# 2) Turkish labels (BigM-style)
# ============================================================
lbl <- list(
  row_names = c("Fabrika 1","Fabrika 2","Fabrika 3"),
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
outdir <- "./examples/example3"
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

for (m in methods){
  # Initial (no optimization)
  res_init <- solve_transport(
    C, sup, dem,
    method = m,
    optimize = "none",
    drop_dummy_in_output = FALSE   # <- safer if a case is unbalanced later
  )
  title_init <- sprintf("Dağıtım – %s (%s)", method_title(m), opt_title("none"))
  save_alloc_table(
    res_init,                                 # pass the WHOLE result
    title_init,
    sprintf("10_%s_initial.html", m),
    lbl
  )
  
  # Optimized variants
  for (opt in optimizers[optimizers != "none"]){
    res_opt <- solve_transport(
      C, sup, dem,
      method = m,
      optimize = opt,
      drop_dummy_in_output = FALSE           # keep shapes aligned with any dummy
    )
    title_opt <- sprintf("Dağıtım – %s (%s)", method_title(m), opt_title(opt))
    save_alloc_table(
      res_opt,                               # pass the WHOLE result
      title_opt,
      sprintf("20_%s_%s.html", m, opt),
      lbl
    )
  }
}
# ============================================================
# 6) Step-by-step pages with "mevcut (toplam)" for Arz/Talep
# ============================================================
for (m in methods){
  res_trace <- solve_transport(C, sup, dem, method = m, optimize = "both", trace = TRUE,
                               allow_unbalanced = TRUE, drop_dummy_in_output = FALSE)
  render_trace_steps(res_trace, method_title(m), outdir, m, lbl)
}

# ============================================================
# 6b) MODI u–v / R tabloları (her yöntem için)
#    Not: trace=TRUE ve optimize='modi' olmalı ki u–v snapshot'ları üretelim
# ============================================================
for (m in methods){
  res_modi_trace <- solve_transport(C, sup, dem, method = m, optimize = "modi", trace = TRUE,
                                    allow_unbalanced = TRUE, drop_dummy_in_output = FALSE)
  render_modi_uv_tables(res_modi_trace, outdir = outdir, method_label = m, labels_for_steps = lbl)
}

# ============================================================
# 6c) MODI (split): u–v / eşitlikler / R ayrı dosyalar
# ============================================================
for (m in methods){
  res_modi_trace <- solve_transport(C, sup, dem,
                                    method = m,
                                    optimize = "modi",
                                    trace = TRUE,
                                    allow_unbalanced = TRUE,
                                    drop_dummy_in_output = FALSE)
  render_modi_split(res_modi_trace, outdir = outdir,
                    method_label = m, labels_for_steps = lbl)
}


# ============================================================
# 6d) MODI u–v on FINAL table (overlay uᵢ, vⱼ and show R in cells)
#     Links back to the final MODI allocation page for each method.
# ============================================================
for (m in methods){
  # final result you want to link to (here: MODI)
  final_page <- sprintf("20_%s_%s.html", m, "modi")
  final_res  <- solve_transport(C, sup, dem, method = m, optimize = "modi",
                                allow_unbalanced = TRUE, drop_dummy_in_output = FALSE)
  # produce raw LaTeX of the FINAL MODI cost calculation (HTML + .tex)
  transport_export_latex_equation_html(
    cost  = C,
    alloc = final_res$allocation,
    file  = file.path(outdir, sprintf("21_%s_modi_final_cost_eq.html", m)),
    title = sprintf("%s – MODI Nihai Toplam Maliyet (LaTeX – ham)", method_title(m)),
    render_math = FALSE
  )
  # a MODI trace to get per-step u,v snapshots
  tr <- solve_transport(C, sup, dem, method = m, optimize = "modi", trace = TRUE,
                        allow_unbalanced = TRUE, drop_dummy_in_output = FALSE)
  
  render_modi_uv_on_final(tr, final_res, outdir = outdir,
                          method_label = m, labels_for_steps = lbl,
                          final_href = final_page,
                          render_math = TRUE)
}

# ============================================================
# 6c) MODI step packages: (u/v + final table) + (u–v LaTeX) + (Rij table + LaTeX)
# ============================================================
for (m in methods){
  res_modi_trace <- solve_transport(
    C, sup, dem,
    method = m,
    optimize = "modi",
    trace = TRUE,
    drop_dummy_in_output = FALSE
  )
  render_modi_step_packages(
    tr = res_modi_trace,
    outdir = outdir,
    method_label = m,
    labels_for_steps = lbl,
    math_preview = FALSE   # set TRUE if you also want rendered MathJax previews
  )
}

# ============================================================
# 7) Summary index linking all outputs
# ============================================================
# ============================================================
# Build summary index (adds cost-only table + LaTeX eq links)
# ============================================================
build_transport_summary_index <- function(C, sup, dem, outdir,
                                          methods    = c("northwest","rowmin","colmin","leastcost","vam"),
                                          optimizers = c("none","modi","stepping","both"),
                                          labels     = .transport_labels_default(),
                                          generate_cost_only = TRUE,
                                          generate_initial_eq = TRUE,
                                          eq_render_math = FALSE) {
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  
  # 1) Optional: generate cost-only initial table
  cost_only_file <- file.path(outdir, "05_maliyetler_sadece.html")
  if (generate_cost_only) {
    cat(
      transport_initial_costs_only_html(
        cost = C,
        labels = labels,
        title = "Birim Maliyet Tablosu (Sadece Maliyetler)"
      ),
      file = cost_only_file
    )
  }
  
  # 2) Optional: generate LaTeX RAW equation per method (for initial allocations)
  if (generate_initial_eq) {
    for (m in methods) {
      res_init <- solve_transport(C, sup, dem, method = m, optimize = "none")
      transport_export_latex_equation_html(
        cost = C,
        alloc = res_init$allocation,
        file  = file.path(outdir, sprintf("11_%s_initial_eq.html", m)),
        title = sprintf("%s – Başlangıç Toplam Maliyet (LaTeX)", method_title(m)),
        render_math = eq_render_math
      )
    }
  }
  
  # 3) Build the summary index
  summary_html <- file.path(outdir, "01_ozet_index.html")
  H <- function(x) .html_escape(x)
  
  lines <- c(
    "<html><head><meta charset='utf-8'><title>Transport Çıktıları</title>",
    "<style>body{font-family:'Segoe UI',Roboto,Helvetica,Arial,sans-serif;margin:20px} h1,h2,h3{margin:.2em 0} ul{margin:.3em 0 1em 1.2em}</style>",
    "</head><body>",
    "<h1>Transport Problemi – Tüm Yöntemler</h1>",
    "<ul>",
    sprintf("<li><a href='%s'>%s</a></li>", "00_baslangic_tablosu.html", H("Başlangıç Tablosu (Birim Maliyetler)")),
    if (file.exists(cost_only_file))
      sprintf("<li><a href='%s'>%s</a></li>", "05_maliyetler_sadece.html", H("Birim Maliyet Tablosu (Sadece Maliyetler)"))
    else NULL,
    "</ul>",
    "<h2>Yöntemlere Göre Dağıtımlar</h2>"
  )
  
  for (m in methods) {
    lines <- c(lines, sprintf("<h3>%s</h3>", H(method_title(m))), "<ul>",
               sprintf("<li><a href='%s'>%s</a></li>", sprintf("10_%s_initial.html", m), H("Başlangıç")))
    for (opt in optimizers[optimizers != "none"]) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         sprintf("20_%s_%s.html", m, opt), H(opt_title(opt))))
    }
    # Step-by-step index
    lines <- c(lines,
               sprintf("<li><a href='%s'>%s</a></li>",
                       sprintf("30_%s_adimlar_index.html", m),
                       H("Adım Adım (mevcut / toplam)")))
    # NEW: MODI u–v tables index (if exists)
    modi_uv_file <- sprintf("40_%s_modi_index.html", m)
    if (file.exists(file.path(outdir, modi_uv_file))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         modi_uv_file, H("MODI u–v Hesapları (tablo)")))
    }
    
    
    # MODI step packages (u/v + final table, u–v LaTeX, R_ij table + LaTeX)
    pkg_idx <- sprintf("50_%s_modi_pkg_index.html", m)
    if (file.exists(file.path(outdir, pkg_idx))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         pkg_idx, H("MODI: u/v + R_ij + LaTeX (adım adım)")))
    }
    
    
    # NEW: MODI u–v overlay on final table
    modi_uv_final_idx <- sprintf("46_%s_uv_on_final_index.html", m)
    if (file.exists(file.path(outdir, modi_uv_final_idx))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         modi_uv_final_idx, H("MODI u–v (nihai tablo üzerinde)")))
    }
    
    
    
    # NEW: MODI split index (u–v / eşitlikler / R ayrı)
    modi_split_file <- sprintf("45_%s_modi_split_index.html", m)
    if (file.exists(file.path(outdir, modi_split_file))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         modi_split_file, H("MODI (u–v / eşitlikler / R ayrı)")))
    }
    
    # LaTeX (raw) link if exists
    latex_file <- sprintf("11_%s_initial_eq.html", m)
    if (file.exists(file.path(outdir, latex_file))) {
      lines <- c(lines,
                 sprintf("<li><a href='%s'>%s</a></li>",
                         latex_file, H("Başlangıç Maliyet Eşitliği (LaTeX – raw)")))
    }
    lines <- c(lines, "</ul>")
  }
  
  
  lines <- c(lines, "</body></html>")
  writeLines(lines, summary_html)
  message("Özet dizin yazıldı: ", summary_html)
}

build_transport_summary_index(
  C = C, sup = sup, dem = dem,
  outdir = outdir,
  methods = methods,
  optimizers = optimizers,
  labels = lbl,
  generate_cost_only = TRUE,     # writes 05_maliyetler_sadece.html
  generate_initial_eq = TRUE,    # writes 11_<method>_initial_eq.html
  eq_render_math = FALSE         # keep LaTeX RAW in those files
)

