# ============================================================
`%||%` <- function(a,b) if (is.null(a)) b else a

decision_criteria <- function(payoff, p = NULL, type = c("gain","loss"),
                              do = c("most_likely","expected_value","expected_regret","minimax_regret"),
                              tol = 1e-12, latex = TRUE, digits = 3) {
  
  type <- match.arg(type); do <- unique(do)
  A <- as.matrix(payoff)
  alt_names   <- rownames(A) %||% paste0("A_", seq_len(nrow(A)))
  state_names <- colnames(A) %||% paste0("S_", seq_len(ncol(A)))
  rownames(A) <- alt_names; colnames(A) <- state_names
  
  if (!is.null(p)) { s <- sum(p); if (abs(s-1) > 1e-8) p <- p/s; names(p) <- state_names }
  
  # Regret matrix
  state_best <- apply(A, 2, if (type=="gain") max else min)
  best_mat   <- matrix(state_best, nrow=nrow(A), ncol=ncol(A), byrow=TRUE, dimnames=dimnames(A))
  Reg <- if (type=="gain") best_mat - A else A - best_mat
  
  out <- list(); chosen <- list(); latex_out <- list()
  fmt <- function(x, k=digits) formatC(x, digits=k, format="f", drop0trailing = TRUE,
                                       decimal.mark = ".")
  
  # ------------------------------------------------------------
  # MOST-LIKELY (add raw LaTeX)
  # ------------------------------------------------------------
  if ("most_likely" %in% do) {
    if (is.null(p)) {
      out$most_likely <- list(error = "p required"); chosen$most_likely <- NA
      if (latex) latex_out$most_likely <- list(header = "% Most-likely: probabilities not provided.")
    } else {
      j <- which.max(p)
      col_vals <- A[, j]
      iopt <- if (type=="gain") which(col_vals == max(col_vals)) else which(col_vals == min(col_vals))
      out$most_likely <- list(state = state_names[j], p_state = p[j],
                              payoff_column = col_vals, chosen = alt_names[iopt])
      chosen$most_likely <- alt_names[iopt]
      
      if (latex) {
        header <- sprintf("%% --- Most Likely ---\n\\[ j^* = \\arg\\max_j\\, p_{S_j} = %s,\\quad p_{%s} = %s \\]",
                          state_names[j], state_names[j], fmt(p[j]))
        col_pairs <- paste(sprintf("%s: %s", alt_names, fmt(col_vals)), collapse = "\\,,\\; ")
        rule <- if (type=="gain") "\\max" else "\\min"
        choice <- paste(alt_names[iopt], collapse = ", ")
        detail <- paste0("\\[ \\text{", state_names[j], " sütunu: } ", col_pairs,
                         ".\\quad \\text{Seçim: } i^* = \\arg", rule,
                         "_i\\, a_{i,", state_names[j], "} \\Rightarrow ", choice, " \\]")
        latex_out$most_likely <- list(header = header, detail = detail)
      }
    }
  }
  
  # ------------------------------------------------------------
  # EXPECTED VALUE (Beklenen Değer) + LaTeX
  # ------------------------------------------------------------
  if ("expected_value" %in% do) {
    p_use <- if (is.null(p)) rep(1/ncol(A), ncol(A)) else p
    EV <- as.numeric(A %*% p_use); names(EV) <- alt_names
    iopt <- if (type=="gain") which(EV==max(EV)) else which(EV==min(EV))
    out$expected_value <- list(expected_value=EV, p=p_use, chosen=alt_names[iopt])
    chosen$expected_value <- alt_names[iopt]
    
    if (latex) {
      latex_out$ev_header_general <- paste0(
        "% --- Expected Value (EV) ---\n",
        "\\[ \\mathrm{EV}(i) = \\sum_{j=1}^{", ncol(A), "} p_{S_j}\\, a_{i,S_j} \\]"
      )
      ev_lines <- vapply(seq_len(nrow(A)), function(i){
        terms_sym <- paste(sprintf("p_{%s}\\cdot a_{%s,%s}", state_names, alt_names[i], state_names),
                           collapse = " + ")
        terms_num <- paste(sprintf("%s\\cdot %s", fmt(p_use), fmt(A[i, ])), collapse = " + ")
        paste0("\\[\\mathrm{EV}(", alt_names[i],") = ",
               terms_sym, " = ", terms_num, " = ", fmt(sum(p_use * A[i, ])), "\\]")
      }, character(1))
      names(ev_lines) <- alt_names
      latex_out$ev_by_alt <- ev_lines
    }
  }
  
  # ------------------------------------------------------------
  # EXPECTED REGRET (Beklenen Pişmanlık) + LaTeX
  # ------------------------------------------------------------
  if ("expected_regret" %in% do) {
    p_use <- if (is.null(p)) rep(1/ncol(A), ncol(A)) else p
    ER <- as.numeric(Reg %*% p_use); names(ER) <- alt_names
    iopt <- which(ER == min(ER))
    out$expected_regret <- list(expected_regret = ER, p = p_use, regret = Reg,
                                chosen = alt_names[iopt])
    chosen$expected_regret <- alt_names[iopt]
    
    if (latex) {
      if (type == "gain") {
        latex_out$er_header_general <- sprintf(
          "%% --- Expected Regret (ER) ---\n\\[ R_{ij} = \\max_i a_{i,S_j} - a_{i,S_j},\\quad \\mathrm{ER}(i) = \\sum_{j=1}^{%d} p_{S_j}\\, R_{ij} \\]",
          ncol(A)
        )
      } else {
        latex_out$er_header_general <- sprintf(
          "%% --- Expected Regret (ER) ---\n\\[ R_{ij} = a_{i,S_j} - \\min_i a_{i,S_j},\\quad \\mathrm{ER}(i) = \\sum_{j=1}^{%d} p_{S_j}\\, R_{ij} \\]",
          ncol(A)
        )
      }
      
      er_lines <- vapply(seq_len(nrow(Reg)), function(i) {
        pR_sym <- paste(sprintf("p_{%s}\\\\cdot R_{i,%s}", state_names, state_names), collapse = " + ")
        pR_num <- paste(sprintf("p_{%s}\\\\cdot %s", state_names, fmt(Reg[i, ])), collapse = " + ")
        weighted_num <- paste(sprintf("%s\\\\cdot %s", fmt(p_use), fmt(Reg[i, ])), collapse = " + ")
        sprintf("\\[\\mathrm{ER}(%s) = \\sum_j p_{S_j}\\, R_{ij} = %s = %s = %s = %s\\]",
                alt_names[i], pR_sym, pR_num, weighted_num, fmt(sum(p_use * Reg[i, ])))
      }, character(1))
      names(er_lines) <- alt_names
      latex_out$er_by_alt <- er_lines
    }
  }
  
  # ------------------------------------------------------------
  # MINIMAX REGRET + LaTeX
  # ------------------------------------------------------------
  if ("minimax_regret" %in% do) {
    MR <- apply(Reg, 1, max); names(MR) <- alt_names
    iopt <- which(MR == min(MR))
    out$minimax_regret <- list(max_regret = MR, regret = Reg, chosen = alt_names[iopt])
    chosen$minimax_regret <- alt_names[iopt]
    
    if (latex) {
      header <- "% --- Minimax Regret ---\n\\[ \\mathrm{MR}(i) = \\max_j R_{ij},\\quad i^* = \\arg\\min_i \\mathrm{MR}(i) \\]"
      lines <- vapply(seq_len(nrow(Reg)), function(i){
        sprintf("\\[\\mathrm{MR}(%s) = \\max\\{ %s \\} = %s\\]",
                alt_names[i],
                paste(fmt(Reg[i, ]), collapse = ",\\,"),
                fmt(max(Reg[i, ])))
      }, character(1))
      choice <- sprintf("\\[ i^* = \\arg\\min_i\\, \\mathrm{MR}(i) \\Rightarrow %s \\]",
                        paste(alt_names[iopt], collapse = ", "))
      latex_out$minimax_regret <- list(header = header, by_alt = lines, choice = choice)
    }
  }
  
  list(
    type = type,
    payoff = A,
    probabilities = p,
    regret = Reg,
    results = out,
    chosen = chosen,
    summary = data.frame(
      Alternative = alt_names,
      EV = out$expected_value$expected_value[alt_names],
      ER = out$expected_regret$expected_regret[alt_names]
    ),
    latex = latex_out
  )
}

# ------------------------------------------------------------
# Export ALL LaTeX blocks to a simple HTML (copy-friendly)
# ------------------------------------------------------------
export_decision_latex_html <- function(
    file, res, title = "Decision Criteria — Raw LaTeX") {
  
  esc <- function(s){
    s <- gsub("&","&amp;",s,fixed=TRUE)
    s <- gsub("<","&lt;", s, fixed=TRUE)
    gsub(">","&gt;", s, fixed=TRUE)
  }
  css <- "
  <style>
    body{font-family:Consolas,Menlo,Monaco,monospace;margin:24px;color:#111}
    h1{font-size:20px;margin:0 0 10px}
    h2{font-size:16px;margin:18px 0 6px}
    pre{border:1px solid #ccc;background:#fafafa;padding:12px;border-radius:8px;white-space:pre-wrap}
  </style>"
  
  add_block <- function(h, heading, content){
    if (is.null(content)) return(h)
    c(h, sprintf("<h2>%s</h2>", heading),
      "<pre><code class='latex'>", esc(paste(content, collapse = "\n\n")), "</code></pre>")
  }
  
  html <- c(
    "<!DOCTYPE html><html><head><meta charset='utf-8'/>",
    sprintf("<title>%s</title>", title), css, "</head><body>",
    sprintf("<h1>%s</h1>", title)
  )
  
  # ML
  html <- add_block(html, "Most-Likely — Header", res$latex$most_likely$header)
  html <- add_block(html, "Most-Likely — Detail", res$latex$most_likely$detail)
  
  # EV
  html <- add_block(html, "Expected Value — General", res$latex$ev_header_general)
  html <- add_block(html, "Expected Value — Per Alternative", res$latex$ev_by_alt)
  
  # ER
  html <- add_block(html, "Expected Regret — General", res$latex$er_header_general)
  html <- add_block(html, "Expected Regret — Per Alternative", res$latex$er_by_alt)
  
  # MR
  html <- add_block(html, "Minimax Regret — Header", res$latex$minimax_regret$header)
  html <- add_block(html, "Minimax Regret — Per Alternative", res$latex$minimax_regret$by_alt)
  html <- add_block(html, "Minimax Regret — Choice", res$latex$minimax_regret$choice)
  
  html <- c(html, "</body></html>")
  writeLines(html, file)
  invisible(normalizePath(file, winslash="/"))
}





export_payoff_html_transport <- function(
    file, P, p = NULL,
    type = c("gain","loss"),
    # labels
    row_label       = "Sipariş Miktarı",
    col_group_label = "İstem Miktarı",
    prob_label      = "Olasılık",
    ev_label        = "Beklenen Değer (EV)",
    # display toggles
    show_probs_row  = TRUE,
    show_ev_col     = TRUE,
    # marking toggles
    mark_ml = TRUE,     # mark most-likely column (+ best cell)
    mark_ev = TRUE,     # mark EV-best row (+ EV cell)
    # digits
    digits_cell = 0,
    digits_prob = 3,
    digits_ev   = 0
){
  type <- match.arg(type)
  
  A <- as.matrix(P)
  alt_names   <- rownames(A); if (is.null(alt_names))   alt_names   <- paste0("O", seq_len(nrow(A)))
  state_names <- colnames(A); if (is.null(state_names)) state_names <- paste0("S", seq_len(ncol(A)))
  rownames(A) <- alt_names; colnames(A) <- state_names
  
  # probabilities (uniform if missing)
  if (is.null(p)) {
    p_use <- rep(1/ncol(A), ncol(A))
  } else {
    p_use <- as.numeric(p); s <- sum(p_use)
    if (is.na(s) || s <= 0) stop("`p` must sum to a positive value.")
    if (abs(s - 1) > 1e-8) p_use <- p_use / s
  }
  names(p_use) <- state_names
  
  EV <- if (show_ev_col) as.numeric(A %*% p_use) else NULL
  if (!is.null(EV)) names(EV) <- alt_names
  
  # ---- which to mark ----
  j_ml <- if (mark_ml) which.max(p_use) else NA_integer_
  i_ml <- if (mark_ml) {
    col_vals <- A[, j_ml]
    if (type == "gain") which.max(col_vals) else which.min(col_vals)
  } else NA_integer_
  
  i_ev <- if (mark_ev && !is.null(EV)) {
    if (type == "gain") which.max(EV) else which.min(EV)
  } else NA_integer_
  
  # ---- helpers ----
  fmt_num <- function(x, d) {
    vapply(x, function(xx) if (is.na(xx)) "" else
      formatC(xx, digits = d, format = "f", big.mark = " ", decimal.mark = "."),
      character(1))
  }
  
  css <- "
  <style>
    :root{
      --grid:#333; --head-bg:#f0f0f0; --lbl-bg:#fafafa;
      --lilac-bg:#F5F3FF;  --lilac-br:#7C3AED;   /* EV row */
      --green-bg:#ECFDF5;  --green-br:#16A34A;  /* ML col */
      --amber-bg:#FFF7ED;  --amber-br:#F59E0B;  /* best cell */
      --prob-bg:#FCFCFC;   --evcol-bg:#F8F9FF;  /* EV col */
    }
    body { font-family:'Segoe UI', Roboto, Arial, sans-serif; margin:18px; color:#111; }
    table.tt { border-collapse:collapse; border:1.5px solid var(--grid); margin-bottom:14px; }
    table.tt th, table.tt td { border:1px solid var(--grid); padding:6px 10px; }
    thead th { background:var(--head-bg); font-weight:700; text-align:center; }
    td.lbl { font-weight:700; background:var(--lbl-bg); white-space:nowrap; }
    td.num { text-align:right; }
    .corner { background:var(--head-bg); font-weight:700; text-align:center; }
    .prob   { background:var(--prob-bg); font-size:12px; }
    .evh    { background:var(--head-bg); }
    .evc    { background:var(--evcol-bg); font-weight:700; }

    /* ----- MARKS ----- */

    /* full green band for ML column */
    .mark-col-hdr { background:var(--green-bg) !important; border-bottom:3px solid var(--green-br) !important; }
    .ml-col       { background:var(--green-bg) !important; border-left:3px solid var(--green-br) !important; border-right:3px solid var(--green-br) !important; }

    /* purple framed lilac row for EV best */
    .mark-row td  { background:var(--lilac-bg) !important; }
    .mark-row td.lbl { border-left:2px solid var(--lilac-br) !important; }
    .mark-row td   { border-top:2px solid var(--lilac-br) !important; border-bottom:2px solid var(--lilac-br) !important; }
    .mark-row td:last-child { border-right:2px solid var(--lilac-br) !important; }

    /* amber best cell *inside* ML column, keep outer green borders via inset box-shadow */
    .best-ml { background:var(--amber-bg) !important; font-weight:700; box-shadow: inset 0 0 0 3px var(--amber-br); }

    /* EV winning cell */
    .mark-ev-cell { background:var(--amber-bg) !important; font-weight:700; box-shadow: inset 0 0 0 3px var(--amber-br); }

    caption { caption-side: top; text-align:left; font-weight:700; margin:0 0 8px 0; }
  </style>"
  
  # header
  thead_top <- paste0(
    "<tr>",
    sprintf("<th class='corner' rowspan='2'>%s</th>", row_label),
    sprintf("<th colspan='%d'>%s</th>", ncol(A), col_group_label),
    if (!is.null(EV)) sprintf("<th class='evh' rowspan='2'>%s</th>", ev_label) else "",
    "</tr>"
  )
  th_states <- paste(vapply(seq_along(state_names), function(j){
    cls <- if (!is.na(j_ml) && j==j_ml && mark_ml) " class='mark-col-hdr'" else ""
    sprintf("<th%s>%s</th>", cls, state_names[j])
  }, character(1)), collapse = "")
  thead_mid <- paste0("<tr>", th_states, "</tr>")
  
  prob_row <- if (show_probs_row) {
    c("<tr class='prob'>",
      sprintf("<td class='lbl'>%s</td>", prob_label),
      paste(sprintf("<td class='num'>%s</td>", fmt_num(p_use, digits_prob)), collapse = ""),
      if (!is.null(EV)) "<td class='evc'></td>" else "",
      "</tr>")
  } else character(0)
  
  # body
  body_rows <- unlist(lapply(seq_len(nrow(A)), function(i){
    row_cls <- if (!is.na(i_ev) && i==i_ev && mark_ev) " class='mark-row'" else ""
    open <- paste0("<tr", row_cls, ">")
    
    cells <- paste(vapply(seq_len(ncol(A)), function(j){
      cls <- "num"
      if (!is.na(j_ml) && j==j_ml && mark_ml) cls <- paste(cls, "ml-col")
      if (!is.na(i_ml) && !is.na(j_ml) && i==i_ml && j==j_ml && mark_ml) cls <- paste(cls, "best-ml")
      sprintf("<td class='%s'>%s</td>", cls, fmt_num(A[i, j], digits_cell))
    }, character(1)), collapse = "")
    
    ev_cell <- if (!is.null(EV)) {
      cls <- "num evc"
      if (!is.na(i_ev) && i==i_ev && mark_ev) cls <- paste(cls, "mark-ev-cell")
      sprintf("<td class='%s'>%s</td>", cls, fmt_num(EV[i], digits_ev))
    } else ""
    
    c(open, sprintf("<td class='lbl'>%s</td>", alt_names[i]), cells, ev_cell, "</tr>")
  }))
  
  html <- c(
    "<!DOCTYPE html>",
    "<html lang='tr'><head><meta charset='utf-8'/>",
    "<title>Payoff</title>", css, "</head><body>",
    "<table class='tt'>",
    "<caption>Payoff Tablosu</caption>",
    "<thead>", thead_top, thead_mid, "</thead>",
    "<tbody>", prob_row, body_rows, "</tbody>",
    "</table>",
    "</body></html>"
  )
  
  con <- file(file, open = "wb"); on.exit(close(con), add = TRUE)
  writeLines(html, con, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/"))
}




export_regret_html_transport <- function(
    file, P, p = NULL,
    type = c("gain","loss"),
    # labels
    row_label       = "Sipariş Miktarı",
    col_group_label = "İstem Miktarı",
    prob_label      = "Olasılık",
    er_label        = "Beklenen Pişmanlık (ER)",
    # display toggles
    show_probs_row  = TRUE,
    show_er_col     = TRUE,
    # marking toggles
    mark_er = TRUE,   # ER-min row + ER cell
    mark_mr = TRUE,   # MR-min row + its row-maximum cell(s)
    # digits
    digits_cell = 0,
    digits_prob = 3,
    digits_er   = 3
){
  type <- match.arg(type)
  
  A <- as.matrix(P)
  alt_names   <- rownames(A); if (is.null(alt_names))   alt_names   <- paste0("O", seq_len(nrow(A)))
  state_names <- colnames(A); if (is.null(state_names)) state_names <- paste0("S", seq_len(ncol(A)))
  rownames(A) <- alt_names; colnames(A) <- state_names
  
  # probabilities (uniform if missing)
  if (is.null(p)) {
    p_use <- rep(1/ncol(A), ncol(A))
  } else {
    p_use <- as.numeric(p); s <- sum(p_use)
    if (is.na(s) || s <= 0) stop("`p` must sum to a positive value.")
    if (abs(s - 1) > 1e-8) p_use <- p_use / s
  }
  names(p_use) <- state_names
  
  # regret, ER, MR
  state_best <- apply(A, 2, if (type=="gain") max else min)
  best_mat   <- matrix(state_best, nrow=nrow(A), ncol=ncol(A), byrow=TRUE, dimnames=dimnames(A))
  R  <- if (type=="gain") best_mat - A else A - best_mat
  ER <- as.numeric(R %*% p_use); names(ER) <- alt_names
  MR_rowmax <- apply(R, 1, max)
  
  i_er <- if (mark_er) which.min(ER) else NA_integer_
  i_mr <- if (mark_mr) which.min(MR_rowmax) else NA_integer_
  mr_cols_on_best <- if (!is.na(i_mr)) which(R[i_mr, ] == MR_rowmax[i_mr]) else integer(0)
  
  fmt_num <- function(x, d) {
    vapply(x, function(xx) if (is.na(xx)) "" else
      formatC(xx, digits = d, format = "f", big.mark = " ", decimal.mark = "."),
      character(1))
  }
  
  css <- "
  <style>
    :root{
      --grid:#333; --head-bg:#f0f0f0; --lbl-bg:#fafafa;
      --lilac-bg:#F5F3FF;  --lilac-br:#7C3AED;   /* winning row */
      --amber-bg:#FFF7ED;  --amber-br:#F59E0B;  /* ER cell */
      --rose-bg:#FFE4E6;   --rose-br:#EF4444;   /* MR cells */
      --prob-bg:#FCFCFC;   --evcol-bg:#F8F9FF;  /* ER col */
    }
    body { font-family:'Segoe UI', Roboto, Arial, sans-serif; margin:18px; color:#111; }
    table.tt { border-collapse:collapse; border:1.5px solid var(--grid); margin-bottom:14px; }
    table.tt th, table.tt td { border:1px solid var(--grid); padding:6px 10px; }
    thead th { background:var(--head-bg); font-weight:700; text-align:center; }
    td.lbl { font-weight:700; background:var(--lbl-bg); white-space:nowrap; }
    td.num { text-align:right; }
    .corner { background:var(--head-bg); font-weight:700; text-align:center; }
    .prob   { background:var(--prob-bg); font-size:12px; }
    .evh    { background:var(--head-bg); }
    .evc    { background:var(--evcol-bg); font-weight:700; }

    /* purple framed lilac row (ER or MR winner) */
    .mark-row td  { background:var(--lilac-bg) !important; }
    .mark-row td.lbl { border-left:2px solid var(--lilac-br) !important; }
    .mark-row td   { border-top:2px solid var(--lilac-br) !important; border-bottom:2px solid var(--lilac-br) !important; }
    .mark-row td:last-child { border-right:2px solid var(--lilac-br) !important; }

    /* amber ER cell (inset so row frame stays visible) */
    .er-best { background:var(--amber-bg) !important; font-weight:700; box-shadow: inset 0 0 0 3px var(--amber-br); }

    /* dashed red MR max cells */
    .mr-cell { background:var(--rose-bg) !important; font-weight:700; border:3px dashed var(--rose-br) !important; }

    caption { caption-side: top; text-align:left; font-weight:700; margin:0 0 8px 0; }
  </style>"
  
  thead <- paste0(
    "<tr>",
    sprintf("<th class='corner' rowspan='2'>%s</th>", row_label),
    sprintf("<th colspan='%d'>%s</th>", ncol(R), col_group_label),
    if (show_er_col) sprintf("<th class='evh' rowspan='2'>%s</th>", er_label) else "",
    "</tr>",
    "<tr>", paste(sprintf("<th>%s</th>", state_names), collapse = ""), "</tr>"
  )
  
  prob_row <- if (show_probs_row) {
    c("<tr class='prob'>",
      sprintf("<td class='lbl'>%s</td>", prob_label),
      paste(sprintf("<td class='num'>%s</td>", fmt_num(p_use, digits_prob)), collapse = ""),
      if (show_er_col) "<td class='evc'></td>" else "",
      "</tr>")
  } else character(0)
  
  body_rows <- unlist(lapply(seq_len(nrow(R)), function(i){
    row_cls <- if ((!is.na(i_er) && i==i_er && mark_er) || (!is.na(i_mr) && i==i_mr && mark_mr)) " class='mark-row'" else ""
    open <- paste0("<tr", row_cls, ">")
    
    cells <- paste(vapply(seq_len(ncol(R)), function(j){
      if (mark_mr && !is.na(i_mr) && i==i_mr && (j %in% mr_cols_on_best)) {
        sprintf("<td class='num mr-cell'>%s</td>", fmt_num(R[i, j], digits_cell))
      } else {
        sprintf("<td class='num'>%s</td>", fmt_num(R[i, j], digits_cell))
      }
    }, character(1)), collapse = "")
    
    er_cell <- if (show_er_col) {
      cls <- "num evc"
      if (!is.na(i_er) && i==i_er && mark_er) cls <- paste(cls, "er-best")
      sprintf("<td class='%s'>%s</td>", cls, fmt_num(ER[i], digits_er))
    } else ""
    
    c(open, sprintf("<td class='lbl'>%s</td>", alt_names[i]), cells, er_cell, "</tr>")
  }))
  
  html <- c(
    "<!DOCTYPE html>",
    "<html lang='tr'><head><meta charset='utf-8'/>",
    "<title>Regret</title>", css, "</head><body>",
    "<table class='tt'>",
    "<caption>Pişmanlık (Regret) Tablosu</caption>",
    "<thead>", thead, "</thead>",
    "<tbody>", prob_row, body_rows, "</tbody>",
    "</table>",
    "</body></html>"
  )
  
  con <- file(file, open = "wb"); on.exit(close(con), add = TRUE)
  writeLines(html, con, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/"))
}


export_most_likely_html_transport <- function(
    file,
    P,
    p,
    type = c("gain","loss"),
    row_label       = "Sipariş Miktarı",
    col_group_label = "İstem Miktarı",
    prob_label      = "Olasılık",
    ml_label        = "En Olası Durum Değeri",
    # digits
    digits_cell   = 0,
    digits_prob   = 3,
    digits_ml     = 3,
    # show/hide ML column
    show_ml_col   = TRUE
){
  `%||%` <- function(a,b) if (is.null(a)) b else a
  
  type <- match.arg(type)
  A <- as.matrix(P)
  alt_names   <- rownames(A) %||% paste0("O", seq_len(nrow(A)))
  state_names <- colnames(A) %||% paste0("S", seq_len(ncol(A)))
  rownames(A) <- alt_names; colnames(A) <- state_names
  n <- nrow(A); m <- ncol(A)
  
  # --- normalise probabilities ---
  p <- as.numeric(p)
  if (length(p) != m) stop("length(p) must equal ncol(P).")
  if (any(is.na(p))) stop("`p` contains NA.")
  s <- sum(p); if (abs(s - 1) > 1e-8) p <- p / s
  names(p) <- state_names
  
  # --- helper for numeric formatting (dot decimal) ---
  fmtn <- function(x, d) vapply(
    x,
    function(xx) if (is.na(xx)) "" else
      formatC(xx, digits = d, format = "f",
              big.mark = " ", decimal.mark = "."),
    character(1)
  )
  
  # --- CSS (compatible with other *transport* tables) ---
  css <- "
  <style>
    :root{
      --grid:#333; --head-bg:#f0f0f0; --lbl-bg:#fafafa;
      --lilac-bg:#F5F3FF; --lilac-br:#7C3AED;
      --amber-bg:#FFF7ED; --amber-br:#F59E0B;
      --prob-bg:#FCFCFC;
      --prob-best-bg:#DBEAFE;
      --metcol-bg:#F8F9FF;
    }
    body { font-family:'Segoe UI', Roboto, Arial, sans-serif; margin:18px; color:#111; }
    table.tt { border-collapse:collapse; border:1.5px solid var(--grid); margin-bottom:14px; width:auto; }
    table.tt th, table.tt td { border:1px solid var(--grid); padding:6px 10px; }
    thead th { background:var(--head-bg); font-weight:700; text-align:center; }
    td.lbl { font-weight:700; background:var(--lbl-bg); white-space:nowrap; }
    td.num { text-align:right; }
    .corner { background:var(--head-bg); font-weight:700; text-align:center; }
    .prob   { background:var(--prob-bg); font-size:12px; }
    .prob-best { background:var(--prob-best-bg); font-weight:700; }
    .mark-row td  { background:var(--lilac-bg) !important; }
    .mark-row td.lbl { border-left:2px solid var(--lilac-br) !important; }
    .mark-row td   { border-top:2px solid var(--lilac-br) !important;
                     border-bottom:2px solid var(--lilac-br) !important; }
    .mark-row td:last-child { border-right:2px solid var(--lilac-br) !important; }
    .best-cell { background:var(--amber-bg) !important; font-weight:700;
                 box-shadow: inset 0 0 0 3px var(--amber-br); }
    .met-h  { background:var(--head-bg); }
    .met-c  { background:var(--metcol-bg); font-weight:700; }
    caption { caption-side: top; text-align:left; font-weight:700; margin:0 0 8px 0; }
  </style>"
  
  # --- MOST LIKELY logic with ties ---
  max_p <- max(p)
  idx_ml <- which(p == max_p)  # all most-likely states
  ml_states <- state_names[idx_ml]
  
  # row scores: best (gain) or worst (loss) among tied columns
  if (type == "gain") {
    row_score <- apply(A[, idx_ml, drop = FALSE], 1, max)
    best_rows <- which(row_score == max(row_score))
  } else {
    row_score <- apply(A[, idx_ml, drop = FALSE], 1, min)
    best_rows <- which(row_score == min(row_score))
  }
  
  # --- build caption / description ---
  caption <- "En Olası Durum (Most Likely) Tablosu"
  ml_info <- if (length(ml_states) == 1L) {
    sprintf(" (Durum: %s, p = %.3f)", ml_states, max_p)
  } else {
    sprintf(" (Durumlar: %s, p = %.3f)",
            paste(ml_states, collapse = ", "), max_p)
  }
  caption_full <- paste0(caption, ml_info)
  
  # --- THEAD: second header row + optional ML column ---
  metric_th <- if (show_ml_col)
    sprintf("<th class='met-h' rowspan='2'>%s</th>", ml_label) else ""
  
  thead <- paste0(
    "<tr>",
    sprintf("<th class='corner' rowspan='2'>%s</th>", row_label),
    sprintf("<th colspan='%d'>%s</th>", m, col_group_label),
    metric_th,
    "</tr>",
    "<tr>",
    paste(sprintf("<th>%s</th>", state_names), collapse = ""),
    "</tr>"
  )
  
  # --- probability row, highlight all tied most-likely columns ---
  prob_cells <- vapply(seq_len(m), function(j){
    cls <- if (j %in% idx_ml) "prob-best" else ""
    sprintf("<td class='num prob %s'>%s</td>",
            cls, fmtn(p[j], digits_prob))
  }, character(1))
  
  prob_row <- paste0(
    "<tr>",
    sprintf("<td class='lbl'>%s</td>", prob_label),
    paste(prob_cells, collapse = ""),
    if (show_ml_col) "<td class='met-c'></td>" else "",
    "</tr>"
  )
  
  # --- BODY rows: highlight best row(s) & decisive cells ---
  body <- paste(
    unlist(lapply(seq_len(n), function(i){
      row_cls <- if (i %in% best_rows) " class='mark-row'" else ""
      row_vec <- A[i, ]
      
      cell_html <- vapply(seq_len(m), function(j){
        base <- sprintf("<td class='num'>%s</td>", fmtn(row_vec[j], digits_cell))
        
        if (j %in% idx_ml) {
          decisive_value <- row_score[i]
          if (!is.na(decisive_value) && row_vec[j] == decisive_value &&
              i %in% best_rows) {
            return(sprintf("<td class='num best-cell'>%s</td>",
                           fmtn(row_vec[j], digits_cell)))
          }
        }
        base
      }, character(1))
      
      ml_td <- if (show_ml_col) {
        sprintf("<td class='num met-c %s'>%s</td>",
                if (i %in% best_rows) "best-cell" else "",
                fmtn(row_score[i], digits_ml))
      } else ""
      
      c(
        "<tr", row_cls, ">",
        sprintf("<td class='lbl'>%s</td>", alt_names[i]),
        paste(cell_html, collapse = ""),
        ml_td,
        "</tr>"
      )
    })),
    collapse = ""
  )
  
  # --- write HTML ---
  html <- c(
    "<!DOCTYPE html>",
    "<html lang='tr'><head><meta charset='utf-8'/>",
    "<title>", caption_full, "</title>",
    css,
    "</head><body>",
    "<table class='tt'>",
    "<caption>", caption_full, "</caption>",
    "<thead>", thead, "</thead>",
    "<tbody>",
    prob_row,
    body,
    "</tbody></table>",
    "</body></html>"
  )
  
  con <- file(file, open = "wb"); on.exit(close(con), add = TRUE)
  writeLines(html, con, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/"))
}