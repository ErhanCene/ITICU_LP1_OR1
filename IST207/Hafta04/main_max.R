source("transport_full.R")
# The example at the bottom runs automatically and writes HTML files to:
# ./examples/example1


# ============================================================
# EXAMPLE
# ============================================================
if (sys.nframe() == 0) {
  # Example data
  C <- matrix(c(2,3,5,
                3,5,7,
                3,6,8), nrow=3, byrow=TRUE)
  sup <- c(30, 60, 100)
  dem <- c(30, 80, 80)
  
  lbl <- list(
    row_names = c("Fabrika 1","Fabrika 2","Fabrika 3"),
    col_names = c("Depo 1","Depo 2","Depo 3"),
    supply_label = "Arz",
    demand_label = "Talep",
    initial_title = "Başlangıç Tablosu (Birim Maliyetler)",
    allocation_title = "Dağıtım – %METHOD%",
    total_cost = "Toplam Maliyet"
  )
  
  outdir <- "./examples/example_max"
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  
  # initial table
  cat(transport_initial_costs_only_html(C, labels = lbl),
      file = file.path(outdir, "05_maliyetler_sadece.html"))
  
  methods    <- c("northwest","rowmin","colmin","leastcost","vam")
  optimizers <- c("none","modi","stepping","both")
  
  save_alloc_table <- function(res, title, fname, labels){
    # try to extract a basis from the last MODI step if present (for degenerate highlighting)
    basis <- NULL
    if (!is.null(res$steps) && length(res$steps)){
      last_modiuv <- NULL
      for (st in rev(res$steps)){
        if (!is.null(st$info$basis)) { last_modiuv <- st; break }
      }
      if (!is.null(last_modiuv)) basis <- (last_modiuv$info$basis == 1) * 1
    }
    cat(
      transport_result_from_alloc_html(
        cost  = res$cost,
        alloc = res$allocation,
        title = title,
        labels = labels,
        basis  = basis
      ),
      file = file.path(outdir, fname)
    )
  }
  
  for (m in methods){
    res_init <- solve_transport(C, sup, dem, method = m, optimize = "none", drop_dummy_in_output = FALSE)
    title_init <- sprintf("Dağıtım – %s (%s)", method_title(m), opt_title("none"))
    save_alloc_table(res_init, title_init, sprintf("10_%s_initial.html", m), lbl)
    
    for (opt in optimizers[optimizers != "none"]){
      res_opt <- solve_transport(C, sup, dem, method = m, optimize = opt, drop_dummy_in_output = FALSE)
      title_opt <- sprintf("Dağıtım – %s (%s)", method_title(m), opt_title(opt))
      save_alloc_table(res_opt, title_opt, sprintf("20_%s_%s.html", m, opt), lbl)
      
      # when opt is "modi": also write RAW LaTeX for the final cost equation
      if (opt == "modi") {
        transport_export_latex_equation_html(
          cost = C,
          alloc = res_opt$allocation,
          file  = file.path(outdir, sprintf("21_%s_modi_final_eq.html", m)),
          title = sprintf("%s – MODI Nihai Toplam Maliyet (LaTeX – ham)", method_title(m)),
          render_math = FALSE
        )
      }
    }
  }
  
  # MODI per-step packages (with degenerate marking)
  for (m in methods){
    res_modi_trace <- solve_transport(C, sup, dem, method = m, optimize = "modi", trace = TRUE, drop_dummy_in_output = FALSE)
    render_modi_step_packages(res_modi_trace, outdir = outdir, method_label = m, labels_for_steps = lbl, math_preview = FALSE)
  }
  
  message("Example written under: ", normalizePath(outdir, winslash = "/"))
}
