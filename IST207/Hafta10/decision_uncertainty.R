`%||%` <- function(a,b) if (is.null(a)) b else a

######################################################################

export_uncertainty_tables_all_transport <- function(
    file_prefix,
    P,
    type = c("gain","loss"),
    alpha = 0.5,
    # labels (TR defaults)
    row_label       = "Sipariş Miktarı",
    col_group_label = "İstem Miktarı",
    prob_label      = "Olasılık",
    laplace_label   = "Laplace",
    maximin_label   = "Maximin",
    minimax_label   = "Minimax",
    maximax_label   = "Maximax",
    minimin_label   = "Minimin",
    savage_label    = "MR (Satır Maks. Pişmanlık)",
    best_label      = "En İyi",
    worst_label     = "En Kötü",
    hurwicz_label   = NULL,   # auto: Hurwicz (α=...)
    # digits (global defaults)
    digits_cell   = 0,
    digits_prob   = 3,
    digits_metric = 3,
    # per-method digit overrides
    # list(
    #   base = list(cell=0),
    #   laplace = list(cell=0, metric=3, prob=3),
    #   ...
    # )
    digits_by_method = NULL
){
  type <- match.arg(type)
  caption_term <- if (type == "gain") "Getiri" else "Kayıp"
  
  A <- as.matrix(P)
  alt_names   <- rownames(A)    %||% paste0("O", seq_len(nrow(A)))
  state_names <- colnames(A)    %||% paste0("S", seq_len(ncol(A)))
  rownames(A) <- alt_names; colnames(A) <- state_names
  n <- nrow(A); m <- ncol(A)
  
  # allow vector alpha
  alpha_vec <- as.numeric(alpha)
  if (any(is.na(alpha_vec))) stop("`alpha` must be numeric (no NA).")
  if (any(alpha_vec < 0 | alpha_vec > 1)) stop("`alpha` must be in [0,1].")
  
  # ---------- helpers ----------
  fmtn <- function(x, d) vapply(
    x,
    function(xx) if (is.na(xx)) "" else
      formatC(xx, digits = d, format = "f", big.mark = " ", decimal.mark = "."),
    character(1)
  )
  
  # per-method digit getter
  get_digits <- function(method, kind = c("cell","metric","prob")) {
    kind <- match.arg(kind)
    # global defaults
    default <- switch(kind,
                      cell   = digits_cell,
                      metric = digits_metric,
                      prob   = digits_prob
    )
    if (is.null(digits_by_method)) return(default)
    mth <- digits_by_method[[method]]
    if (is.null(mth)) return(default)
    val <- mth[[kind]]
    if (is.null(val)) default else as.integer(val)
  }
  
  css <- "
  <style>
    :root{
      --grid:#333; --head-bg:#f0f0f0; --lbl-bg:#fafafa;
      --lilac-bg:#F5F3FF; --lilac-br:#7C3AED;   /* winner row */
      --amber-bg:#FFF7ED; --amber-br:#F59E0B;  /* criterion cell */
      --prob-bg:#FCFCFC;  --metcol-bg:#F8F9FF; /* right metric cols */
      --rose-bg:#FFE4E6;  --rose-br:#EF4444;   /* dashed regret cells */
    }
    body { font-family:'Segoe UI', Roboto, Arial, sans-serif; margin:18px; color:#111; }
    table.tt { border-collapse:collapse; border:1.5px solid var(--grid); margin-bottom:14px; width:auto; }
    table.tt th, table.tt td { border:1px solid var(--grid); padding:6px 10px; }
    thead th { background:var(--head-bg); font-weight:700; text-align:center; }
    td.lbl { font-weight:700; background:var(--lbl-bg); white-space:nowrap; }
    td.num { text-align:right; }
    .corner { background:var(--head-bg); font-weight:700; text-align:center; }
    .prob   { background:var(--prob-bg); font-size:12px; }
    .met-h  { background:var(--head-bg); }
    .met-c  { background:var(--metcol-bg); font-weight:700; }
    .mark-row td  { background:var(--lilac-bg) !important; }
    .mark-row td.lbl { border-left:2px solid var(--lilac-br) !important; }
    .mark-row td   { border-top:2px solid var(--lilac-br) !important; border-bottom:2px solid var(--lilac-br) !important; }
    .mark-row td:last-child { border-right:2px solid var(--lilac-br) !important; }
    .best-cell { background:var(--amber-bg) !important; font-weight:700; box-shadow: inset 0 0 0 3px var(--amber-br); }
    .mr-cell   { background:var(--rose-bg) !important;  font-weight:700; border:3px dashed var(--rose-br) !important; }
    caption { caption-side: top; text-align:left; font-weight:700; margin:0 0 8px 0; }
  </style>"
  
  write_html <- function(path, caption, thead_rows, prob_row = NULL, body_rows) {
    html <- c(
      "<!DOCTYPE html>",
      "<html lang='tr'><head><meta charset='utf-8'/>",
      "<title>", caption, "</title>", css, "</head><body>",
      "<table class='tt'>",
      "<caption>", caption, "</caption>",
      "<thead>", thead_rows, "</thead>",
      "<tbody>",
      if (!is.null(prob_row)) prob_row else "",
      body_rows,
      "</tbody>",
      "</table>", "</body></html>"
    )
    con <- file(path, open="wb"); on.exit(close(con), add=TRUE)
    writeLines(html, con, useBytes=TRUE)
    normalizePath(path, winslash="/")
  }
  
  # ========= 0) Base payoff table =========
  thead_base <- paste0(
    "<tr>",
    sprintf("<th class='corner' rowspan='2'>%s</th>", row_label),
    sprintf("<th colspan='%d'>%s</th>", m, col_group_label),
    "</tr>",
    "<tr>", paste(sprintf("<th>%s</th>", state_names), collapse=""), "</tr>"
  )
  d_cell_base <- get_digits("base","cell")
  
  body_base <- paste(
    unlist(lapply(seq_len(n), function(i){
      c(
        "<tr>",
        sprintf("<td class='lbl'>%s</td>", alt_names[i]),
        paste(sprintf("<td class='num'>%s</td>", fmtn(A[i,], d_cell_base)), collapse=""),
        "</tr>"
      )
    })),
    collapse=""
  )
  
  base_caption <- sprintf("Temel %s Tablosu", caption_term)
  
  path_base <- write_html(
    paste0(file_prefix, "_base.html"),
    base_caption,
    thead_base,
    NULL,
    body_base
  )
  
  # ========= 1) Laplace =========
  p_eq <- rep(1/m, m); names(p_eq) <- state_names
  lap_vals <- rowMeans(A)
  i_lap <- if (type=="gain") which(lap_vals==max(lap_vals)) else which(lap_vals==min(lap_vals))
  
  d_cell_lap <- get_digits("laplace","cell")
  d_prob_lap <- get_digits("laplace","prob")
  d_met_lap  <- get_digits("laplace","metric")
  
  thead_lap <- paste0(
    "<tr>",
    sprintf("<th class='corner' rowspan='2'>%s</th>", row_label),
    sprintf("<th colspan='%d'>%s</th>", m, col_group_label),
    sprintf("<th class='met-h' rowspan='2'>%s</th>", laplace_label),
    "</tr>",
    "<tr>", paste(sprintf("<th>%s</th>", state_names), collapse=""), "</tr>"
  )
  prob_lap <- c(
    "<tr class='prob'>",
    sprintf("<td class='lbl'>%s</td>", prob_label),
    paste(sprintf("<td class='num'>%s</td>", fmtn(p_eq, d_prob_lap)), collapse=""),
    "<td class='met-c'></td>",
    "</tr>"
  )
  body_lap <- paste(
    unlist(lapply(seq_len(n), function(i){
      row_cls <- if (i %in% i_lap) " class='mark-row'" else ""
      c(
        "<tr", row_cls, ">",
        sprintf("<td class='lbl'>%s</td>", alt_names[i]),
        paste(sprintf("<td class='num'>%s</td>", fmtn(A[i,], d_cell_lap)), collapse=""),
        sprintf(
          "<td class='num met-c %s'>%s</td>",
          if (i %in% i_lap) "best-cell" else "",
          fmtn(lap_vals[i], d_met_lap)
        ),
        "</tr>"
      )
    })),
    collapse=""
  )
  
  laplace_caption <- sprintf("Laplace — %s Tablosu", caption_term)
  
  path_lap <- write_html(
    paste0(file_prefix, "_laplace.html"),
    laplace_caption,
    thead_lap,
    paste(prob_lap, collapse=""),
    body_lap
  )
  
  # ========= 2) Maximin / Minimax =========
  row_min <- apply(A,1,min); row_max <- apply(A,1,max)
  crit_mmn <- if (type=="gain") row_min else row_max      # Maximin for gain, Minimax for loss
  i_mmn <- if (type=="gain") which(crit_mmn==max(crit_mmn)) else which(crit_mmn==min(crit_mmn))
  lab_mmn <- if (type=="gain") maximin_label else minimax_label
  
  d_cell_mmn <- get_digits("maximin_minimax","cell")
  d_met_mmn  <- get_digits("maximin_minimax","metric")
  
  thead_mmn <- paste0(
    "<tr>",
    sprintf("<th class='corner' rowspan='2'>%s</th>", row_label),
    sprintf("<th colspan='%d'>%s</th>", m, col_group_label),
    sprintf("<th class='met-h' rowspan='2'>%s</th>", lab_mmn),
    "</tr>",
    "<tr>", paste(sprintf("<th>%s</th>", state_names), collapse=""), "</tr>"
  )
  body_mmn <- paste(
    unlist(lapply(seq_len(n), function(i){
      row_cls <- if (i %in% i_mmn) " class='mark-row'" else ""
      c(
        "<tr", row_cls, ">",
        sprintf("<td class='lbl'>%s</td>", alt_names[i]),
        paste(sprintf("<td class='num'>%s</td>", fmtn(A[i,], d_cell_mmn)), collapse=""),
        sprintf(
          "<td class='num met-c %s'>%s</td>",
          if (i %in% i_mmn) "best-cell" else "",
          fmtn(crit_mmn[i], d_met_mmn)
        ),
        "</tr>"
      )
    })),
    collapse=""
  )
  
  mmn_caption <- sprintf("%s — %s Tablosu", lab_mmn, caption_term)
  
  path_mmn <- write_html(
    paste0(file_prefix, "_maximin_minimax.html"),
    mmn_caption,
    thead_mmn,
    NULL,
    body_mmn
  )
  
  # ========= 3) Maximax / Minimin =========
  crit_mmx <- if (type=="gain") row_max else row_min
  i_mmx <- if (type=="gain") which(crit_mmx==max(crit_mmx)) else which(crit_mmx==min(crit_mmx))
  lab_mmx <- if (type=="gain") maximax_label else minimin_label
  
  d_cell_mmx <- get_digits("maximax_minimin","cell")
  d_met_mmx  <- get_digits("maximax_minimin","metric")
  
  thead_mmx <- paste0(
    "<tr>",
    sprintf("<th class='corner' rowspan='2'>%s</th>", row_label),
    sprintf("<th colspan='%d'>%s</th>", m, col_group_label),
    sprintf("<th class='met-h' rowspan='2'>%s</th>", lab_mmx),
    "</tr>",
    "<tr>", paste(sprintf("<th>%s</th>", state_names), collapse=""), "</tr>"
  )
  body_mmx <- paste(
    unlist(lapply(seq_len(n), function(i){
      row_cls <- if (i %in% i_mmx) " class='mark-row'" else ""
      c(
        "<tr", row_cls, ">",
        sprintf("<td class='lbl'>%s</td>", alt_names[i]),
        paste(sprintf("<td class='num'>%s</td>", fmtn(A[i,], d_cell_mmx)), collapse=""),
        sprintf(
          "<td class='num met-c %s'>%s</td>",
          if (i %in% i_mmx) "best-cell" else "",
          fmtn(crit_mmx[i], d_met_mmx)
        ),
        "</tr>"
      )
    })),
    collapse=""
  )
  
  mmx_caption <- sprintf("%s — %s Tablosu", lab_mmx, caption_term)
  
  path_mmx <- write_html(
    paste0(file_prefix, "_maximax_minimin.html"),
    mmx_caption,
    thead_mmx,
    NULL,
    body_mmx
  )
  
  # ========= 4) Savage (minimax regret) =========
  col_best <- apply(A, 2, if (type=="gain") max else min)
  Reg <- if (type=="gain") (matrix(col_best, n, m, byrow=TRUE) - A) else (A - matrix(col_best, n, m, byrow=TRUE))
  MR <- apply(Reg, 1, max)
  i_svg <- which(MR == min(MR))
  mr_cols_on_best <- if (length(i_svg)) which(Reg[i_svg[1], ] == MR[i_svg[1]]) else integer(0)
  
  d_cell_svg <- get_digits("savage","cell")
  d_met_svg  <- get_digits("savage","metric")
  
  thead_svg <- paste0(
    "<tr>",
    sprintf("<th class='corner' rowspan='2'>%s</th>", row_label),
    sprintf("<th colspan='%d'>Pişmanlık (Regret) Matrisi</th>", m),
    sprintf("<th class='met-h' rowspan='2'>%s</th>", savage_label),
    "</tr>",
    "<tr>", paste(sprintf("<th>%s</th>", state_names), collapse=""), "</tr>"
  )
  body_svg <- paste(
    unlist(lapply(seq_len(n), function(i){
      row_cls <- if (i %in% i_svg) " class='mark-row'" else ""
      c(
        "<tr", row_cls, ">",
        sprintf("<td class='lbl'>%s</td>", alt_names[i]),
        paste(
          vapply(seq_len(m), function(j){
            if (length(i_svg) && i == i_svg[1] && j %in% mr_cols_on_best)
              sprintf("<td class='num mr-cell'>%s</td>", fmtn(Reg[i, j], d_cell_svg))
            else
              sprintf("<td class='num'>%s</td>", fmtn(Reg[i, j], d_cell_svg))
          }, character(1)),
          collapse = ""
        ),
        sprintf(
          "<td class='num met-c %s'>%s</td>",
          if (i %in% i_svg) "best-cell" else "",
          fmtn(MR[i], d_met_svg)
        ),
        "</tr>"
      )
    })),
    collapse=""
  )
  
  svg_caption <- sprintf("Savage (Minimax Pişmanlık) — %s Tablosu", caption_term)
  
  path_svg <- write_html(
    paste0(file_prefix, "_savage.html"),
    svg_caption,
    thead_svg,
    NULL,
    body_svg
  )
  
  # ========= 5) Hurwicz =========
  best  <- if (type=="gain") row_max else row_min
  worst <- if (type=="gain") row_min else row_max
  
  d_cell_hrw <- get_digits("hurwicz","cell")
  d_met_hrw  <- get_digits("hurwicz","metric")
  
  if (length(alpha_vec) == 1L) {
    a <- alpha_vec[1L]
    H  <- a * best + (1 - a) * worst
    i_hrw <- if (type=="gain") which(H == max(H)) else which(H == min(H))
    hur_label_eff <- hurwicz_label %||% sprintf("Hurwicz (α=%.2f)", a)
    
    thead_hrw <- paste0(
      "<tr>",
      sprintf("<th class='corner' rowspan='2'>%s</th>", row_label),
      sprintf("<th colspan='%d'>%s</th>", m, col_group_label),
      sprintf("<th class='met-h' rowspan='2'>%s</th>", best_label),
      sprintf("<th class='met-h' rowspan='2'>%s</th>", worst_label),
      sprintf("<th class='met-h' rowspan='2'>%s</th>", hur_label_eff),
      "</tr>",
      "<tr>", paste(sprintf("<th>%s</th>", state_names), collapse=""), "</tr>"
    )
    body_hrw <- paste(
      unlist(lapply(seq_len(n), function(i){
        row_cls <- if (i %in% i_hrw) " class='mark-row'" else ""
        c(
          "<tr", row_cls, ">",
          sprintf("<td class='lbl'>%s</td>", alt_names[i]),
          paste(sprintf("<td class='num'>%s</td>", fmtn(A[i,], d_cell_hrw)), collapse=""),
          sprintf("<td class='num met-c'>%s</td>", fmtn(best[i],  d_met_hrw)),
          sprintf("<td class='num met-c'>%s</td>", fmtn(worst[i], d_met_hrw)),
          sprintf(
            "<td class='num met-c %s'>%s</td>",
            if (i %in% i_hrw) "best-cell" else "",
            fmtn(H[i], d_met_hrw)
          ),
          "</tr>"
        )
      })),
      collapse=""
    )
    
    hur_caption <- sprintf("Hurwicz (α=%.2f) — %s Tablosu", a, caption_term)
    
    path_hrw <- write_html(
      paste0(file_prefix, "_hurwicz.html"),
      hur_caption,
      thead_hrw,
      NULL,
      body_hrw
    )
    
    hurwicz_paths <- path_hrw
    
  } else {
    # multiple alphas: one file per alpha
    hurwicz_paths <- vector("list", length(alpha_vec))
    names(hurwicz_paths) <- paste0("alpha_", alpha_vec)
    
    for (idx in seq_along(alpha_vec)) {
      a <- alpha_vec[idx]
      H  <- a * best + (1 - a) * worst
      i_hrw <- if (type=="gain") which(H == max(H)) else which(H == min(H))
      hur_label_eff <- hurwicz_label %||% sprintf("Hurwicz (α=%.2f)", a)
      
      thead_hrw <- paste0(
        "<tr>",
        sprintf("<th class='corner' rowspan='2'>%s</th>", row_label),
        sprintf("<th colspan='%d'>%s</th>", m, col_group_label),
        sprintf("<th class='met-h' rowspan='2'>%s</th>", best_label),
        sprintf("<th class='met-h' rowspan='2'>%s</th>", worst_label),
        sprintf("<th class='met-h' rowspan='2'>%s</th>", hur_label_eff),
        "</tr>",
        "<tr>", paste(sprintf("<th>%s</th>", state_names), collapse=""), "</tr>"
      )
      body_hrw <- paste(
        unlist(lapply(seq_len(n), function(i){
          row_cls <- if (i %in% i_hrw) " class='mark-row'" else ""
          c(
            "<tr", row_cls, ">",
            sprintf("<td class='lbl'>%s</td>", alt_names[i]),
            paste(sprintf("<td class='num'>%s</td>", fmtn(A[i,], d_cell_hrw)), collapse=""),
            sprintf("<td class='num met-c'>%s</td>", fmtn(best[i],  d_met_hrw)),
            sprintf("<td class='num met-c'>%s</td>", fmtn(worst[i], d_met_hrw)),
            sprintf(
              "<td class='num met-c %s'>%s</td>",
              if (i %in% i_hrw) "best-cell" else "",
              fmtn(H[i], d_met_hrw)
            ),
            "</tr>"
          )
        })),
        collapse=""
      )
      
      file_alpha    <- sprintf("%s_hurwicz_alpha_%g.html", file_prefix, a)
      hur_caption_a <- sprintf("Hurwicz (α=%.2f) — %s Tablosu", a, caption_term)
      
      path_hrw_a <- write_html(
        file_alpha,
        hur_caption_a,
        thead_hrw,
        NULL,
        body_hrw
      )
      hurwicz_paths[[idx]] <- path_hrw_a
    }
  }
  
  invisible(list(
    base             = path_base,
    laplace          = path_lap,
    maximin_minimax  = path_mmn,
    maximax_minimin  = path_mmx,
    savage           = path_svg,
    hurwicz          = hurwicz_paths
  ))
}
