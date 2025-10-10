# example_max_full.R
# Runs a MAX transportation model and writes all HTML outputs (like minimization),
# including step-by-step pages and alternate-optimal “R=0” transitions.

source("transport_max.R")  # <-- your big file with objective="max" support

# =============== Example data (profits) ===============
# A 4x4 example that ends with an alternate optimum (some R_ij = 0 non-basic)
P <- matrix(c(
  3,1,2,5,
  5,2,3,6,
  6,4,4,7,
  3,2,3,4
), nrow=4, byrow=TRUE)
sup <- c(20,15,10,45)
dem <- c(30,16,24,20)

# Labels
lbl <- list(
  row_names = paste("Kaynak", 1:4),
  col_names = paste("Hedef",  1:4),
  supply_label = "Arz",
  demand_label = "Talep",
  initial_title   = "Başlangıç Tablosu (Birim Kârlar)",
  allocation_title= "Dağıtım – %METHOD%",
  total_cost      = "Toplam Kâr"
)

outdir <- "./examples/example_max1"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# =============== Initial “profits-only” table ===============
cat(
  transport_initial_costs_only_html(P, labels = lbl,
                                    title = "Birim Kâr Tablosu (Sadece Kâr + xᵢⱼ)"),
  file = file.path(outdir, "05_karlar_sadece.html")
)

methods    <- c("northwest","rowmin","colmin","leastcost","vam")
optimizers <- c("none","modi","stepping","both")

# Helper to save allocation table (keeps any degenerate basic cells, if your renderer supports it)
save_alloc_table <- function(res, title, fname, labels){
  cat(
    transport_result_from_alloc_html(
      cost  = res$cost,          # shown as profits
      alloc = res$allocation,
      title = title,
      labels = labels
    ),
    file = file.path(outdir, fname)
  )
}

# =============== Run all methods/optimizers under MAX objective ===============
for (m in methods){
  # Initial (no optimization)
  res_init <- solve_transport(
    cost = P, supply = sup, demand = dem,
    method = m, optimize = "none",
    objective = "max",                    # <-- key
    drop_dummy_in_output = FALSE
  )
  title_init <- sprintf("Dağıtım – %s (%s)", method_title(m, res_init$objective), opt_title("none"))
  save_alloc_table(res_init, title_init, sprintf("10_%s_initial.html", m), lbl)
  
  # Optimized variants
  for (opt in optimizers[optimizers != "none"]){
    res_opt <- solve_transport(
      cost = P, supply = sup, demand = dem,
      method = m, optimize = opt,
      objective = "max",                  # <-- key
      trace = (opt != "none"),
      drop_dummy_in_output = FALSE
    )
    title_opt <- sprintf("Dağıtım – %s (%s)", method_title(m, res_opt$objective), opt_title(opt))
    save_alloc_table(res_opt, title_opt, sprintf("20_%s_%s.html", m, opt), lbl)
    
    # For MODI final: also dump RAW LaTeX for the final profit equation
    if (opt == "modi") {
      transport_export_latex_equation_html(
        cost  = res_opt$cost,             # profits shown on output
        alloc = res_opt$allocation,
        file  = file.path(outdir, sprintf("21_%s_modi_final_eq.html", m)),
        title = sprintf("%s – MODI Nihai Toplam Kâr (LaTeX – ham)", method_title(m)),
        render_math = FALSE
      )
    }
  }
}

# =============== Step-by-step pages (mevcut/toplam) ===============
for (m in methods){
  res_trace <- solve_transport(P, sup, dem,
                               method = m, optimize = "both",
                               objective = "max",
                               trace = TRUE, drop_dummy_in_output = FALSE)
  # 1) Başlangıç tablosu (kâr + x_ij + Arz/Talep) kaydet
  cat(
    transport_initial_costs_xvars_with_margins_html(
      cost   = P,
      supply = sup,
      demand = dem,
      title  = sprintf("Birim Kâr Tablosu (xᵢⱼ + kâr + Arz/Talep) — %s", method_title(m, res_trace$objective)),
      file   = file.path(outdir, sprintf("09_%s_profit_xvars_with_margins.html", m))
    ),
    file = file.path(outdir, sprintf("09_%s_profit_xvars_with_margins.html", m))
  )
  
  # 2) Adım adım tabloları oluştur
  render_trace_steps(res_trace,  method_title(m, res_trace$objective), outdir, m, lbl,
                     show_titles = TRUE, show_notes = TRUE)
  
  # NEW: raw LaTeX for the initial solution of this algorithm
  export_initial_solution_latex(res_trace, outdir, m, render_math = FALSE)
  
}

# =============== MODI u–v / R tables per step ===============
for (m in methods){
  tr_modi <- solve_transport(P, sup, dem,
                             method = m, optimize = "modi",
                             objective = "max",
                             trace = TRUE, drop_dummy_in_output = FALSE)
  render_modi_uv_tables(tr_modi, outdir = outdir, method_label = m, labels_for_steps = lbl)
}

# =============== MODI (split): u–v / Eşitlikler / R (3 files per step) ===============
for (m in methods){
  res_modi_trace <- solve_transport(P, sup, dem,
                             method = m, optimize = "modi",
                             objective = "max",
                             trace = TRUE, drop_dummy_in_output = FALSE)
  render_modi_split(res_modi_trace, outdir = outdir, method_label = m, labels_for_steps = lbl)
}

# =============== MODI u–v overlay on FINAL + R + transition (before/after) ===============
for (m in methods){
  final_res <- solve_transport(P, sup, dem,
                               method = m, optimize = "modi",
                               objective = "max",
                               drop_dummy_in_output = FALSE)
  tr_modi <- solve_transport(P, sup, dem,
                             method = m, optimize = "modi",
                             objective = "max",
                             trace = TRUE, drop_dummy_in_output = FALSE)
  final_page <- sprintf("20_%s_modi.html", m)
  render_modi_uv_on_final(tr_modi, final_res, outdir = outdir,
                          method_label = m, labels_for_steps = lbl,
                          final_href = final_page,
                          render_math = TRUE)
  
  # NEW: list and materialize *alternative* optimal solutions (if any)
  render_modi_alternative_solutions(tr = res_modi_trace, outdir = outdir,
                                    method_label = "vam",
                                    labels_for_steps = lbl)
}

# =============== MODI step packages (50_*) ===============
for (m in methods){
  tr_modi <- solve_transport(P, sup, dem,
                             method = m, optimize = "modi",
                             objective = "max",
                             trace = TRUE, drop_dummy_in_output = FALSE)
  render_modi_step_packages(
    tr = tr_modi,
    outdir = outdir,
    method_label = m,
    labels_for_steps = lbl,
    math_preview = FALSE
  )
}

# =============== Alternate Optima (R_ij = 0 non-basic) — extra pages ===============
# This creates "virtual" transitions starting from the *final* MODI snapshot and
# pivoting any non-basic cell with R=0 along its cycle (theta>0), producing an
# alternative optimal allocation without violating optimality.
make_alternate_optima_pages <- function(tr, outdir, method_label, labels = .transport_labels_default()){
  if (is.null(tr$steps) || !length(tr$steps)) return(invisible())
  # get the LAST modi-uv snapshot (optimal)
  st <- NULL
  for (k in seq_along(tr$steps)) {
    if (identical(tr$steps[[k]]$stage, "modi-uv")) st <- tr$steps[[k]]
  }
  if (is.null(st)) return(invisible())
  
  C  <- tr$cost        # NOTE: for MAX this is the displayed “profit” matrix returned by your solver
  A0 <- st$alloc + 0
  u  <- st$info$u; v <- st$info$v
  R  <- st$info$reduced
  basis <- if (!is.null(st$info$basis)) st$info$basis==1 else (A0>0)
  
  m <- nrow(C); n <- ncol(C)
  eps <- 1e-12
  
  # BFS-loop builder on the basis
  find_loop <- function(is_basic, ie, je){
    rows_to_cols <- lapply(seq_len(m), function(i) which(is_basic[i, ]))
    cols_to_rows <- lapply(seq_len(n), function(j) which(is_basic[, j]))
    start <- ie; target <- m + je; N <- m + n
    prev <- rep(NA_integer_, N); vis <- rep(FALSE, N)
    q <- integer(0); head <- 1L
    q <- c(q, start); vis[start] <- TRUE; prev[start] <- 0
    while (head <= length(q)){
      node <- q[head]; head <- head + 1L
      if (node <= m){
        for (jj in rows_to_cols[[node]]) {
          vtx <- m + jj
          if (!vis[vtx]) { vis[vtx] <- TRUE; prev[vtx] <- node; q <- c(q, vtx) }
        }
      } else {
        j <- node - m
        for (ii in cols_to_rows[[j]]) {
          if (!vis[ii]) { vis[ii] <- TRUE; prev[ii] <- node; q <- c(q, ii) }
        }
      }
      if (vis[target]) break
    }
    if (!vis[target]) return(NULL)
    nodes <- integer(0); cur <- target
    while (cur != 0 && !is.na(cur)) { nodes <- c(cur, nodes); cur <- prev[cur] }
    loop <- matrix(c(ie, je), ncol=2, byrow=TRUE)
    if (length(nodes) >= 2){
      for (k in seq_len(length(nodes)-1L)){
        a <- nodes[k]; b <- nodes[k+1L]
        if (a <= m && b >  m){ i <- a; j <- b - m
        } else if (a > m && b <= m){ i <- b; j <- a - m
        } else next
        if (!(i == ie && j == je)) loop <- rbind(loop, c(i, j))
      }
    }
    loop
  }
  
  # find all non-basic R==0 cells
  zeros <- which(!basis & !is.na(R) & abs(R) <= 1e-12, arr.ind = TRUE)
  if (!nrow(zeros)) return(invisible())
  
  H <- function(x) { x <- gsub("&","&amp;",x, fixed=TRUE); x <- gsub("<","&lt;",x, fixed=TRUE); gsub(">","&gt;",x, fixed=TRUE) }
  alt_idx <- c("<html><head><meta charset='utf-8'><title>Alternatif Optimumlar (R=0)</title></head><body>",
               sprintf("<h2>%s — Alternatif Optimumlar (R=0 non-basic)</h2>", H(method_label)),
               "<ol>")
  
  case_id <- 0
  for (k in seq_len(nrow(zeros))){
    ie <- zeros[k,1]; je <- zeros[k,2]
    loop <- find_loop(basis, ie, je)
    if (is.null(loop) || nrow(loop) < 4L) next
    signs <- rep(c(1,-1), length.out = nrow(loop))
    minus <- loop[signs==-1, , drop=FALSE]
    theta <- suppressWarnings(min(A0[cbind(minus[,1], minus[,2])], na.rm=TRUE))
    if (!is.finite(theta) || theta <= eps) next  # nothing to move → skip
    
    # apply a *virtual* pivot of size theta
    A1 <- A0
    for (t in seq_len(nrow(loop))){
      i <- loop[t,1]; j <- loop[t,2]
      A1[i,j] <- A1[i,j] + signs[t]*theta
      if (abs(A1[i,j]) < 1e-12) A1[i,j] <- 0
    }
    
    case_id <- case_id + 1
    # reuse the transition renderer from 46_* (before/after pages)
    f_before <- sprintf("60_%s_alt_%02d_before.html", method_label, case_id)
    f_after  <- sprintf("60_%s_alt_%02d_after.html",  method_label, case_id)
    
    # Minimal, self-contained transition page (BEFORE)
    writeLines(transport_result_from_alloc_html(cost=C, alloc=A0,
                                                title=sprintf("Alternatif #%d — Öncesi (R[%d,%d]=0)", case_id, ie, je),
                                                labels=labels),
               file.path(outdir, f_before))
    
    # AFTER page (we want the *changed* loop cells visible)
    # Quick inline transition highlighter (compact)
    render_alt_after <- function(C, A_before, A_after, loop, file){
      m <- nrow(C); n <- ncol(C)
      eps <- 1e-12
      css <- "<style>
        .tbl{border-collapse:collapse;font-family:Segoe UI,Roboto,Arial;font-size:14px}
        th,td{border:1px solid #dee2e6;padding:6px 10px;text-align:center}
        .alloc{background:#fff3cd;border:2px solid #f0ad4e}
        .pivot{background:#e8f0fe;box-shadow:inset 0 0 0 3px #1a73e8}
        .plus{background:#f1faf1;box-shadow:inset 0 0 0 3px #2e7d32}
        .minus{background:#fff1f1;box-shadow:inset 0 0 0 3px #c62828}
      </style>"
      is_pivot <- function(i,j) i==loop[1,1] && j==loop[1,2]
      signs <- rep(c(1,-1), length.out=nrow(loop))
      plus  <- loop[signs== 1,,drop=FALSE]
      minus <- loop[signs==-1,,drop=FALSE]
      is_plus  <- function(i,j) any(plus[,1]==i & plus[,2]==j)
      is_minus <- function(i,j) any(minus[,1]==i & minus[,2]==j)
      
      H <- function(x) { x <- gsub("&","&amp;",x, fixed=TRUE); x <- gsub("<","&lt;",x, fixed=TRUE); gsub(">","&gt;",x, fixed=TRUE) }
      lbl <- labels
      rn <- if (!is.null(lbl$row_names)) lbl$row_names else paste0("Kaynak ", seq_len(m))
      cn <- if (!is.null(lbl$col_names)) lbl$col_names else paste0("Hedef ",  seq_len(n))
      
      html <- c("<html><head><meta charset='utf-8'>", css, "</head><body>")
      html <- c(html, sprintf("<h3>Alternatif — Sonrası (R[%d,%d]=0, θ=%g)</h3>", loop[1,1], loop[1,2], suppressWarnings(min(A_before[cbind(minus[,1], minus[,2])], na.rm=TRUE))))
      html <- c(html, "<table class='tbl'><thead><tr><th></th>")
      for (j in 1:n) html <- c(html, sprintf("<th>%s</th>", H(cn[j])))
      html <- c(html, sprintf("<th>%s</th></tr></thead><tbody>", H(lbl$supply_label)))
      for (i in 1:m){
        html <- c(html, sprintf("<tr><th>%s</th>", H(rn[i])))
        for (j in 1:n){
          cls <- character(0)
          if (A_after[i,j] > 0) cls <- c(cls, "alloc")
          if (is_pivot(i,j)) cls <- c(cls, "pivot")
          else if (is_plus(i,j)) cls <- c(cls, "plus")
          else if (is_minus(i,j)) cls <- c(cls, "minus")
          val <- if (A_after[i,j] > 0) sprintf("<div><b>%g</b></div>", A_after[i,j]) else "<span style='color:#adb5bd'>&ndash;</span>"
          html <- c(html, sprintf("<td class='%s'>%s<div class='sub'>(c=%g)</div></td>", paste(cls, collapse=" "), val, C[i,j]))
        }
        html <- c(html, sprintf("<td><b>%g</b></td></tr>", sum(A_after[i,])))
      }
      html <- c(html, sprintf("<tr><th>%s</th>", H(lbl$demand_label)))
      for (j in 1:n) html <- c(html, sprintf("<td><b>%g</b></td>", sum(A_after[,j])))
      html <- c(html, sprintf("<td><b>%g</b></td></tr>", sum(A_after)))
      html <- c(html, "</tbody></table></body></html>")
      writeLines(html, file)
    }
    
    writeLines(transport_result_from_alloc_html(cost=C, alloc=A1,
                                                title=sprintf("Alternatif #%d — Sonrası (R[%d,%d]=0)", case_id, ie, je),
                                                labels=labels),
               file.path(outdir, f_after))
    # also write a compact “after with cycle highlights” page:
    render_alt_after(C, A0, A1, loop, file.path(outdir, sub("\\.html$","_highlight.html", f_after)))
    
    alt_idx <- c(alt_idx,
                 sprintf("<li>R[%d,%d]=0 → θ=%g : <a href='%s'>öncesi</a> · <a href='%s'>sonrası</a> · <a href='%s'>sonrası (vurgulu)</a></li>",
                         ie, je, theta, f_before, f_after, sub("\\.html$","_highlight.html", f_after)))
  }
  
  alt_idx <- c(alt_idx, "</ol></body></html>")
  writeLines(alt_idx, file.path(outdir, sprintf("60_%s_alternates_index.html", method_label)))
}

# run alternate-optimal page builder for each method (based on MODI final snapshot)
for (m in methods){
  tr_modi <- solve_transport(P, sup, dem,
                             method = m, optimize = "modi",
                             objective = "max",
                             trace = TRUE, drop_dummy_in_output = FALSE)
  make_alternate_optima_pages(tr_modi, outdir, m, labels = lbl)
}

message("MAX example written to: ", normalizePath(outdir, winslash = "/"))


####################### EXAMPLE 2 ################################################
# ===================== Unbalanced MAX transportation example =====================

# Profit matrix (to maximize)
# Rows = sources (Kaynaklar), Cols = destinations (Hedefler)
P <- matrix(
  c( 6, 1, 7, 5,
     5, 4, 3, 6,
     7, 6, 5, 9 ),
  nrow = 3, byrow = TRUE
)

# Supplies (sum = 90)
sup <- c(35, 25, 30)

# Demands (sum = 57)  -> UNBALANCED (supply > demand)
dem <- c(15, 10, 20, 12)

# Labels are optional, just for nicer headings
lbl <- list(row_prefix = "Kaynak", col_prefix = "Hedef",
            supply_label = "Arz", demand_label = "Talep")

# Directory to write HTML artifacts
outdir <- "./examples/example_max2"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# =============== Initial “profits-only” table ===============
cat(
  transport_initial_costs_only_html(P, labels = lbl,
                                    title = "Birim Kâr Tablosu (Sadece Kâr + xᵢⱼ)"),
  file = file.path(outdir, "05_karlar_sadece.html")
)

methods    <- c("northwest","rowmin","colmin","leastcost","vam")
optimizers <- c("none","modi","stepping","both")

# Helper to save allocation table (keeps any degenerate basic cells, if your renderer supports it)
save_alloc_table <- function(res, title, fname, labels){
  cat(
    transport_result_from_alloc_html(
      cost  = res$cost,          # shown as profits
      alloc = res$allocation,
      title = title,
      labels = labels
    ),
    file = file.path(outdir, fname)
  )
}

# =============== Run all methods/optimizers under MAX objective ===============
for (m in methods){
  # Initial (no optimization)
  res_init <- solve_transport(
    cost = P, supply = sup, demand = dem,
    method = m, optimize = "none",
    objective = "max",                    # <-- key
    drop_dummy_in_output = FALSE
  )
  title_init <- sprintf("Dağıtım – %s (%s)", method_title(m, res_init$objective), opt_title("none"))
  save_alloc_table(res_init, title_init, sprintf("10_%s_initial.html", m), lbl)
  
  # Optimized variants
  for (opt in optimizers[optimizers != "none"]){
    res_opt <- solve_transport(
      cost = P, supply = sup, demand = dem,
      method = m, optimize = opt,
      objective = "max",                  # <-- key
      trace = (opt != "none"),
      drop_dummy_in_output = FALSE
    )
    title_opt <- sprintf("Dağıtım – %s (%s)", method_title(m, res_opt$objective), opt_title(opt))
    save_alloc_table(res_opt, title_opt, sprintf("20_%s_%s.html", m, opt), lbl)
    
    # For MODI final: also dump RAW LaTeX for the final profit equation
    if (opt == "modi") {
      transport_export_latex_equation_html(
        cost  = res_opt$cost,             # profits shown on output
        alloc = res_opt$allocation,
        file  = file.path(outdir, sprintf("21_%s_modi_final_eq.html", m)),
        title = sprintf("%s – MODI Nihai Toplam Kâr (LaTeX – ham)", method_title(m)),
        render_math = FALSE
      )
    }
  }
}

# =============== Step-by-step pages (mevcut/toplam) ===============
for (m in methods){
  res_trace <- solve_transport(P, sup, dem,
                               method = m, optimize = "both",
                               objective = "max",
                               trace = TRUE, drop_dummy_in_output = FALSE)
  # 1) Başlangıç tablosu (kâr + x_ij + Arz/Talep) kaydet
  cat(
    transport_initial_costs_xvars_with_margins_html(
      cost   = P,
      supply = sup,
      demand = dem,
      title  = sprintf("Birim Kâr Tablosu (xᵢⱼ + kâr + Arz/Talep) — %s", method_title(m, res_trace$objective)),
      file   = file.path(outdir, sprintf("09_%s_profit_xvars_with_margins.html", m))
    ),
    file = file.path(outdir, sprintf("09_%s_profit_xvars_with_margins.html", m))
  )
  
  # 2) Adım adım tabloları oluştur
  render_trace_steps(res_trace,  method_title(m, res_trace$objective), outdir, m, lbl,
                     show_titles = TRUE, show_notes = TRUE)
  
  # NEW: raw LaTeX for the initial solution of this algorithm
  export_initial_solution_latex(res_trace, outdir, m, render_math = FALSE)
  
}

# =============== MODI u–v / R tables per step ===============
for (m in methods){
  tr_modi <- solve_transport(P, sup, dem,
                             method = m, optimize = "modi",
                             objective = "max",
                             trace = TRUE, drop_dummy_in_output = FALSE)
  render_modi_uv_tables(tr_modi, outdir = outdir, method_label = m, labels_for_steps = lbl)
}

# =============== MODI (split): u–v / Eşitlikler / R (3 files per step) ===============
for (m in methods){
  res_modi_trace <- solve_transport(P, sup, dem,
                                    method = m, optimize = "modi",
                                    objective = "max",
                                    trace = TRUE, drop_dummy_in_output = FALSE)
  render_modi_split(res_modi_trace, outdir = outdir, method_label = m, labels_for_steps = lbl)
}

# =============== MODI u–v overlay on FINAL + R + transition (before/after) ===============
for (m in methods){
  final_res <- solve_transport(P, sup, dem,
                               method = m, optimize = "modi",
                               objective = "max",
                               drop_dummy_in_output = FALSE)
  tr_modi <- solve_transport(P, sup, dem,
                             method = m, optimize = "modi",
                             objective = "max",
                             trace = TRUE, drop_dummy_in_output = FALSE)
  final_page <- sprintf("20_%s_modi.html", m)
  render_modi_uv_on_final(tr_modi, final_res, outdir = outdir,
                          method_label = m, labels_for_steps = lbl,
                          final_href = final_page,
                          render_math = TRUE)
  
  # NEW: list and materialize *alternative* optimal solutions (if any)
  render_modi_alternative_solutions(tr = res_modi_trace, outdir = outdir,
                                    method_label = "vam",
                                    labels_for_steps = lbl)
}

# =============== MODI step packages (50_*) ===============
for (m in methods){
  tr_modi <- solve_transport(P, sup, dem,
                             method = m, optimize = "modi",
                             objective = "max",
                             trace = TRUE, drop_dummy_in_output = FALSE)
  render_modi_step_packages(
    tr = tr_modi,
    outdir = outdir,
    method_label = m,
    labels_for_steps = lbl,
    math_preview = FALSE
  )
}

# =============== Alternate Optima (R_ij = 0 non-basic) — extra pages ===============
# This creates "virtual" transitions starting from the *final* MODI snapshot and
# pivoting any non-basic cell with R=0 along its cycle (theta>0), producing an
# alternative optimal allocation without violating optimality.
make_alternate_optima_pages <- function(tr, outdir, method_label, labels = .transport_labels_default()){
  if (is.null(tr$steps) || !length(tr$steps)) return(invisible())
  # get the LAST modi-uv snapshot (optimal)
  st <- NULL
  for (k in seq_along(tr$steps)) {
    if (identical(tr$steps[[k]]$stage, "modi-uv")) st <- tr$steps[[k]]
  }
  if (is.null(st)) return(invisible())
  
  C  <- tr$cost        # NOTE: for MAX this is the displayed “profit” matrix returned by your solver
  A0 <- st$alloc + 0
  u  <- st$info$u; v <- st$info$v
  R  <- st$info$reduced
  basis <- if (!is.null(st$info$basis)) st$info$basis==1 else (A0>0)
  
  m <- nrow(C); n <- ncol(C)
  eps <- 1e-12
  
  # BFS-loop builder on the basis
  find_loop <- function(is_basic, ie, je){
    rows_to_cols <- lapply(seq_len(m), function(i) which(is_basic[i, ]))
    cols_to_rows <- lapply(seq_len(n), function(j) which(is_basic[, j]))
    start <- ie; target <- m + je; N <- m + n
    prev <- rep(NA_integer_, N); vis <- rep(FALSE, N)
    q <- integer(0); head <- 1L
    q <- c(q, start); vis[start] <- TRUE; prev[start] <- 0
    while (head <= length(q)){
      node <- q[head]; head <- head + 1L
      if (node <= m){
        for (jj in rows_to_cols[[node]]) {
          vtx <- m + jj
          if (!vis[vtx]) { vis[vtx] <- TRUE; prev[vtx] <- node; q <- c(q, vtx) }
        }
      } else {
        j <- node - m
        for (ii in cols_to_rows[[j]]) {
          if (!vis[ii]) { vis[ii] <- TRUE; prev[ii] <- node; q <- c(q, ii) }
        }
      }
      if (vis[target]) break
    }
    if (!vis[target]) return(NULL)
    nodes <- integer(0); cur <- target
    while (cur != 0 && !is.na(cur)) { nodes <- c(cur, nodes); cur <- prev[cur] }
    loop <- matrix(c(ie, je), ncol=2, byrow=TRUE)
    if (length(nodes) >= 2){
      for (k in seq_len(length(nodes)-1L)){
        a <- nodes[k]; b <- nodes[k+1L]
        if (a <= m && b >  m){ i <- a; j <- b - m
        } else if (a > m && b <= m){ i <- b; j <- a - m
        } else next
        if (!(i == ie && j == je)) loop <- rbind(loop, c(i, j))
      }
    }
    loop
  }
  
  # find all non-basic R==0 cells
  zeros <- which(!basis & !is.na(R) & abs(R) <= 1e-12, arr.ind = TRUE)
  if (!nrow(zeros)) return(invisible())
  
  H <- function(x) { x <- gsub("&","&amp;",x, fixed=TRUE); x <- gsub("<","&lt;",x, fixed=TRUE); gsub(">","&gt;",x, fixed=TRUE) }
  alt_idx <- c("<html><head><meta charset='utf-8'><title>Alternatif Optimumlar (R=0)</title></head><body>",
               sprintf("<h2>%s — Alternatif Optimumlar (R=0 non-basic)</h2>", H(method_label)),
               "<ol>")
  
  case_id <- 0
  for (k in seq_len(nrow(zeros))){
    ie <- zeros[k,1]; je <- zeros[k,2]
    loop <- find_loop(basis, ie, je)
    if (is.null(loop) || nrow(loop) < 4L) next
    signs <- rep(c(1,-1), length.out = nrow(loop))
    minus <- loop[signs==-1, , drop=FALSE]
    theta <- suppressWarnings(min(A0[cbind(minus[,1], minus[,2])], na.rm=TRUE))
    if (!is.finite(theta) || theta <= eps) next  # nothing to move → skip
    
    # apply a *virtual* pivot of size theta
    A1 <- A0
    for (t in seq_len(nrow(loop))){
      i <- loop[t,1]; j <- loop[t,2]
      A1[i,j] <- A1[i,j] + signs[t]*theta
      if (abs(A1[i,j]) < 1e-12) A1[i,j] <- 0
    }
    
    case_id <- case_id + 1
    # reuse the transition renderer from 46_* (before/after pages)
    f_before <- sprintf("60_%s_alt_%02d_before.html", method_label, case_id)
    f_after  <- sprintf("60_%s_alt_%02d_after.html",  method_label, case_id)
    
    # Minimal, self-contained transition page (BEFORE)
    writeLines(transport_result_from_alloc_html(cost=C, alloc=A0,
                                                title=sprintf("Alternatif #%d — Öncesi (R[%d,%d]=0)", case_id, ie, je),
                                                labels=labels),
               file.path(outdir, f_before))
    
    # AFTER page (we want the *changed* loop cells visible)
    # Quick inline transition highlighter (compact)
    render_alt_after <- function(C, A_before, A_after, loop, file){
      m <- nrow(C); n <- ncol(C)
      eps <- 1e-12
      css <- "<style>
        .tbl{border-collapse:collapse;font-family:Segoe UI,Roboto,Arial;font-size:14px}
        th,td{border:1px solid #dee2e6;padding:6px 10px;text-align:center}
        .alloc{background:#fff3cd;border:2px solid #f0ad4e}
        .pivot{background:#e8f0fe;box-shadow:inset 0 0 0 3px #1a73e8}
        .plus{background:#f1faf1;box-shadow:inset 0 0 0 3px #2e7d32}
        .minus{background:#fff1f1;box-shadow:inset 0 0 0 3px #c62828}
      </style>"
      is_pivot <- function(i,j) i==loop[1,1] && j==loop[1,2]
      signs <- rep(c(1,-1), length.out=nrow(loop))
      plus  <- loop[signs== 1,,drop=FALSE]
      minus <- loop[signs==-1,,drop=FALSE]
      is_plus  <- function(i,j) any(plus[,1]==i & plus[,2]==j)
      is_minus <- function(i,j) any(minus[,1]==i & minus[,2]==j)
      
      H <- function(x) { x <- gsub("&","&amp;",x, fixed=TRUE); x <- gsub("<","&lt;",x, fixed=TRUE); gsub(">","&gt;",x, fixed=TRUE) }
      lbl <- labels
      rn <- if (!is.null(lbl$row_names)) lbl$row_names else paste0("Kaynak ", seq_len(m))
      cn <- if (!is.null(lbl$col_names)) lbl$col_names else paste0("Hedef ",  seq_len(n))
      
      html <- c("<html><head><meta charset='utf-8'>", css, "</head><body>")
      html <- c(html, sprintf("<h3>Alternatif — Sonrası (R[%d,%d]=0, θ=%g)</h3>", loop[1,1], loop[1,2], suppressWarnings(min(A_before[cbind(minus[,1], minus[,2])], na.rm=TRUE))))
      html <- c(html, "<table class='tbl'><thead><tr><th></th>")
      for (j in 1:n) html <- c(html, sprintf("<th>%s</th>", H(cn[j])))
      html <- c(html, sprintf("<th>%s</th></tr></thead><tbody>", H(lbl$supply_label)))
      for (i in 1:m){
        html <- c(html, sprintf("<tr><th>%s</th>", H(rn[i])))
        for (j in 1:n){
          cls <- character(0)
          if (A_after[i,j] > 0) cls <- c(cls, "alloc")
          if (is_pivot(i,j)) cls <- c(cls, "pivot")
          else if (is_plus(i,j)) cls <- c(cls, "plus")
          else if (is_minus(i,j)) cls <- c(cls, "minus")
          val <- if (A_after[i,j] > 0) sprintf("<div><b>%g</b></div>", A_after[i,j]) else "<span style='color:#adb5bd'>&ndash;</span>"
          html <- c(html, sprintf("<td class='%s'>%s<div class='sub'>(c=%g)</div></td>", paste(cls, collapse=" "), val, C[i,j]))
        }
        html <- c(html, sprintf("<td><b>%g</b></td></tr>", sum(A_after[i,])))
      }
      html <- c(html, sprintf("<tr><th>%s</th>", H(lbl$demand_label)))
      for (j in 1:n) html <- c(html, sprintf("<td><b>%g</b></td>", sum(A_after[,j])))
      html <- c(html, sprintf("<td><b>%g</b></td></tr>", sum(A_after)))
      html <- c(html, "</tbody></table></body></html>")
      writeLines(html, file)
    }
    
    writeLines(transport_result_from_alloc_html(cost=C, alloc=A1,
                                                title=sprintf("Alternatif #%d — Sonrası (R[%d,%d]=0)", case_id, ie, je),
                                                labels=labels),
               file.path(outdir, f_after))
    # also write a compact “after with cycle highlights” page:
    render_alt_after(C, A0, A1, loop, file.path(outdir, sub("\\.html$","_highlight.html", f_after)))
    
    alt_idx <- c(alt_idx,
                 sprintf("<li>R[%d,%d]=0 → θ=%g : <a href='%s'>öncesi</a> · <a href='%s'>sonrası</a> · <a href='%s'>sonrası (vurgulu)</a></li>",
                         ie, je, theta, f_before, f_after, sub("\\.html$","_highlight.html", f_after)))
  }
  
  alt_idx <- c(alt_idx, "</ol></body></html>")
  writeLines(alt_idx, file.path(outdir, sprintf("60_%s_alternates_index.html", method_label)))
}

# run alternate-optimal page builder for each method (based on MODI final snapshot)
for (m in methods){
  tr_modi <- solve_transport(P, sup, dem,
                             method = m, optimize = "modi",
                             objective = "max",
                             trace = TRUE, drop_dummy_in_output = FALSE)
  make_alternate_optima_pages(tr_modi, outdir, m, labels = lbl)
}

message("MAX example written to: ", normalizePath(outdir, winslash = "/"))