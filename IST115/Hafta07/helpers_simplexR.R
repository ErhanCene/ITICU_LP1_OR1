# ============================================================
# Simplex capture + HTML rendering toolkit
# ============================================================

wrap_with_mathjax <- function(html, title = "Simplex Tableau") {
  paste0(
    "<!doctype html><html><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>", title, "</title>",
    
    # MathJax v3 config: enable $...$ and $$...$$
    "<script>",
    "window.MathJax = {",
    "  tex: {",
    "    inlineMath: [['$', '$'], ['\\\\(', '\\\\)']],",
    "    displayMath: [['$$','$$'], ['\\\\[','\\\\]']],",
    "    processEscapes: true",
    "  },",
    "  options: {",
    "    skipHtmlTags: ['script','noscript','style','textarea','pre','code']",
    "  }",
    "};",
    "</script>",
    
    # MathJax v3 loader
    "<script id='MathJax-script' async ",
    "src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>",
    "</head><body>",
    html,
    "</body></html>"
  )
}

# keep as is
.simplex_label_tex <- function(x) {
  if (x == "b")            return("$b$")
  if (x == "Cj")           return("$C_{j}$")
  if (x == "Zj")           return("$Z_{j}$")
  if (x == "Zj-Cj")        return("$Z_{j}-C_{j}$")
  if (grepl("^x\\d+$", x)) return(sprintf("$x_{%s}$", sub("^x","",x)))
  if (grepl("^s\\d+$", x)) return(sprintf("$s_{%s}$", sub("^s","",x)))
  if (grepl("^ratio_", x)) return("$\\displaystyle\\frac{b_{j}}{a_{j^*}}$")
  x
}

# NEW: always return one math expression for the whole row label
.simplex_rowlabel_tex <- function(rn) {
  # match "coef var" like "0 s1", "22 x2", etc.
  m  <- regexec("^\\s*([+-]?[0-9]*\\.?[0-9]+)\\s+([A-Za-z]+\\d+)\\s*$", rn)
  mt <- regmatches(rn, m)[[1]]
  
  if (length(mt) == 3) {
    coef <- mt[2]
    var  <- mt[3]
    # strip $...$ from var token so we can embed it inside one $...$
    var_tex <- sub("^\\$(.*)\\$$", "\\1", .simplex_label_tex(var))
    return(sprintf("$%s\\, %s$", coef, var_tex))  # whole label in math
  }
  
  # otherwise, map known tokens & make them math too if they aren’t already
  tok <- .simplex_label_tex(rn)
  if (grepl("^\\$.*\\$$", tok)) tok else paste0("$", tok, "$")
}


# ---------- internal helpers ----------
.simplex_escape <- function(x){
  x <- gsub("&","&amp;",x,fixed=TRUE); x <- gsub("<","&lt;",x,fixed=TRUE)
  x <- gsub(">","&gt;",x,fixed=TRUE);  x <- gsub('"',"&quot;",x,fixed=TRUE); x
}
# .simplex_fmt <- function(v,digits=2){
#   if (is.na(v)) return("&nbsp;")
#   vv <- suppressWarnings(as.numeric(v))
#   if (is.finite(vv)) return(.simplex_escape(formatC(vv, format="f", digits=digits)))
#   .simplex_escape(as.character(v))
# }

.simplex_fmt <- function(v, digits = 2, int_tol = 1e-9){
  if (is.na(v)) return("&nbsp;")
  vv <- suppressWarnings(as.numeric(v))
  if (is.finite(vv)) {
    if (abs(vv) < int_tol) vv <- 0               # -0.00 gibi durumları önle
    if (abs(vv - round(vv)) <= int_tol) {
      # tamsayı: ondalık yok
      return(.simplex_escape(as.character(as.integer(round(vv)))))
    } else {
      # değilse digits kadar ondalık
      return(.simplex_escape(formatC(vv, format = "f", digits = digits)))
    }
  }
  .simplex_escape(as.character(v))
}



# ---------- HTML renderer ----------
.simplex_df_to_html <- function(df, head_left="TDV", digits=2,
                                highlight_col=NULL, highlight_row=NULL, highlight_pivot=FALSE,
                                color_sign=FALSE, color_sign_rows=c("Zj","Zj-Cj"),
                                highlight_cell_enter=NULL, highlight_cell_leave=NULL,
                                highlight_cell_optimum=NULL,                 # <-- NEW
                                highlight_ratio_col=NULL,
                                enter_col_style = c("tint","frame","none"),
                                highlight_rows_extra = character(0),
                                math_labels = FALSE,
                                leave_fallback = TRUE) {
  enter_col_style <- match.arg(enter_col_style)
  stopifnot(is.data.frame(df))
  rn <- rownames(df); cn <- colnames(df)
  
  # ---- build display labels (keep rn/cn unchanged for logic) ----
  disp_cols <- setNames(cn, cn)
  if (isTRUE(math_labels)) {
    disp_cols <- setNames(vapply(cn, .simplex_label_tex, character(1)), cn)
    head_left_disp <- if (identical(head_left, "TDV")) "$\\textbf{TDV}$" else head_left
  } else {
    head_left_disp <- head_left
    if ("b" %in% cn) disp_cols["b"] <- "ÇV"
    rcands <- grep("^ratio_", cn, value = TRUE)
    if (length(rcands)) {
      disp_cols[rcands] <- "Oran<br><small><i>b<sub>j</sub>/a<sub>j*</sub></i></small>"
    }
  }
  
  # ---- column classes (enter / ratio highlight) ----
  ratio_candidates <- grep("^ratio_", cn, value = TRUE)
  if (identical(highlight_ratio_col, "auto")) {
    highlight_ratio_col <- if (length(ratio_candidates)) ratio_candidates[1] else NULL
  }
  ratio_col_hi <- highlight_ratio_col
  
  # ---- row classes (Cj / Zj / Zj-Cj / leaving / extra) ----
  row_class <- rep("", length(rn))
  row_class[rn == "Cj"]    <- "cj"
  row_class[rn == "Zj"]    <- "zj"
  row_class[rn == "Zj-Cj"] <- "zjm"
  if (!is.null(highlight_row) && highlight_row %in% rn) {
    row_class[rn == highlight_row] <- paste(row_class[rn == highlight_row], "leave")
  }
  if (length(highlight_rows_extra)) {
    hits <- intersect(highlight_rows_extra, rn)
    row_class[rn %in% hits] <- paste(row_class[rn %in% hits], "otherhi")
  }
  
  # ---- CSS (same look & feel as before) ----
  css <- "
  <style>
    table.simplex{border-collapse:collapse;font-family:system-ui,-apple-system,Segoe UI,Roboto,Arial;font-size:13px;
    margin: 0px;
    }
    table.simplex th,table.simplex td{border:1px solid #d0d7de;
    padding:4px 4px;
    text-align:right}
    table.simplex thead th{background:#f1f1f1;font-weight:600}
    table.simplex th.rowname{background:#f8f9fb;text-align:left;white-space:nowrap}
    tr.cj  td, tr.cj  th.rowname{background:#eef6ff}
    tr.zj  td, tr.zj  th.rowname{background:#fff3e0}
    tr.zjm td, tr.zjm th.rowname{background:#ffeaea}
    
    /* Optional: tighter left “TDV” header cell */
    table.simplex th.rowname{
    padding:0px 4px;
  }

    .simplex th.colB, .simplex td.colB { 
  background-color: #FFF3CD;   
  padding-left:4px;
  padding-right:4px;  /* pastel amber — istediğin renkle değiştir */
}

    /* entering column */
    td.enterTint, th.enterTint{border-left:2px solid #1f8b24 !important; border-right:2px solid #1f8b24 !important; background:#eaf9ec !important;}
    td.enterFrame, th.enterFrame{border-left:2px solid #1f8b24;border-right:2px solid #1f8b24}

    /* leaving (base) row */
    tr.leave td, tr.leave th.rowname{background:#f4e8ff !important; border-top:2px solid #7e33cc !important; border-bottom:2px solid #7e33cc !important}

    /* extra highlighted rows (orange band) */
    tr.otherhi td, tr.otherhi th.rowname{
      background:#fff7eb;border-top:2px solid #ff9800 !important; border-bottom:2px solid #ff9800 !important;color:#e67e00 !important;
    }

    td.pivot{background:#fff3cd !important;font-weight:700 !important;}

    /* ratio column frame */
    td.ratiohi, th.ratiohi{border-left:2px solid #0b5ed7;border-right:2px solid #0b5ed7;background:#e7f1ff;
    padding-left:4px;
    padding-right:4px;}

    /* sign coloring */
    td.signpos{color:#1f8b24;font-weight:600}
    td.signneg{color:#c62828;font-weight:600}

  /* special value boxes (strong specificity so they win over row/column tints) */
  td.enterVal,
  tr.leave td.enterVal,
  td.ratiohi.enterVal,
  tr.leave td.ratiohi.enterVal {
    background:#c8e6c9 !important;
    font-weight:700;
    border:2px solid #2e7d32 !important;
  }
  
  td.leaveVal,
  tr.leave td.leaveVal,
  td.ratiohi.leaveVal,
  tr.leave td.ratiohi.leaveVal {
    background:#ffcdd2 !important;
    font-weight:700;
    border:2px solid #c62828 !important;
  }
  td.na{color:#9aa0a6}
  
  /* optimal Z value box */
  td.optVal,
  tr.leave td.optVal,
  td.ratiohi.optVal,
  tr.leave td.ratiohi.optVal,
  td.enterTint.optVal,
  td.enterFrame.optVal,
  tr.leave td.enterTint.optVal,
  tr.leave td.enterFrame.optVal {
    background:#fff8e1 !important;      /* light gold */
    font-weight:700 !important;
    border:2px solid blue !important; /* amber border */
    box-shadow: inset 0 0 0 1px rgba(0,0,0,.04);
    color: blue !important;
  }

  
  /* pivot highlight — must beat row/column tints/frames */
  td.pivot,
  tr.leave td.pivot,
  td.enterTint.pivot,
  td.enterFrame.pivot,
  td.ratiohi.pivot,
  tr.leave td.enterTint.pivot,
  tr.leave td.enterFrame.pivot,
  tr.leave td.ratiohi.pivot {
    background: #fff3cd !important;          /* amber */
    font-weight: 700 !important;
    box-shadow: inset 0 0 0 2px #d39e00;      /* subtle frame so it pops */
  }

  </style>"
  
  # ---- helper to identify a specific cell (for value boxing) ----
  is_target_cell <- function(i,j, cell){
    if (is.null(cell)) return(FALSE)
    r_ok <- (!is.null(cell$row)) && identical(rn[i], cell$row)
    c_ok <- (!is.null(cell$col)) && identical(cn[j], cell$col)
    if (!(r_ok && c_ok)) return(FALSE)
    if (is.null(cell$value)) return(TRUE)
    tol <- if (is.null(cell$tol)) 1e-9 else cell$tol
    val_num <- suppressWarnings(as.numeric(df[i,j,drop=TRUE]))
    is.finite(val_num) && abs(val_num - cell$value) <= tol
  }
  
  # ---- header ----
  thead <- paste0(
    "<thead><tr><th class='rowname'>", if (isTRUE(math_labels)) head_left_disp else .simplex_escape(head_left_disp), "</th>",
    paste(vapply(seq_along(cn), function(j){
      classes <- character(0)
      if (!is.null(highlight_col) && cn[j]==highlight_col) {
        classes <- c(classes, if (enter_col_style=="frame") "enterFrame" else if (enter_col_style=="tint") "enterTint")
      }
      if (!is.null(highlight_ratio_col) && cn[j]==highlight_ratio_col) classes <- c(classes, "ratiohi")
      # b sütununa özel sınıf
      if (cn[j] == "b") classes <- c(classes, "colB")
      cls <- if (length(classes)) paste0(" class='", paste(classes, collapse=" "), "'") else ""
      lab <- if (isTRUE(math_labels)) disp_cols[cn[j]] else .simplex_escape(disp_cols[cn[j]])
      sprintf("<th%s>%s</th>", cls, lab)
    }, ""), collapse = ""),
    "</tr></thead>"
  )
  
  # ---- body rows ----
  want_color <- isTRUE(color_sign)
  color_rows <- intersect(color_sign_rows, rn)
  row_label_disp <- if (isTRUE(math_labels)) vapply(rn, .simplex_rowlabel_tex, character(1)) else .simplex_escape(rn)
  
  body_rows <- character(length(rn))
  for (i in seq_along(rn)) {
    cls_tr <- trimws(row_class[i]); if (nzchar(cls_tr)) cls_tr <- paste0(" class='", cls_tr, "'") else cls_tr <- ""
    tds <- vapply(seq_along(cn), function(j){
      classes <- character(0)
      if (cn[j] == "b") classes <- c(classes, "colB")
      
      if (!is.null(highlight_col) && cn[j]==highlight_col) {
        classes <- c(classes, if (enter_col_style=="frame") "enterFrame" else if (enter_col_style=="tint") "enterTint")
      }
      if (!is.null(highlight_ratio_col) && cn[j]==highlight_ratio_col) classes <- c(classes,"ratiohi")
      if (isTRUE(highlight_pivot) && !is.null(highlight_col) && !is.null(highlight_row) &&
          cn[j]==highlight_col && rn[i]==highlight_row) classes <- c(classes,"pivot")
      
      val <- df[i,j,drop=TRUE]; val_num <- suppressWarnings(as.numeric(val))
      if (want_color && rn[i] %in% color_rows && is.finite(val_num)) {
        if (val_num > 0) classes <- c(classes,"signpos")
        else if (val_num < 0) classes <- c(classes,"signneg")
      }
      if (is_target_cell(i,j, highlight_cell_enter)) classes <- c(classes,"enterVal")
      if (is_target_cell(i,j, highlight_cell_leave)) classes <- c(classes,"leaveVal")
      if (is_target_cell(i,j, highlight_cell_optimum)) classes <- c(classes,"optVal")   # <-- NEW
      
      if (isTRUE(leave_fallback) &&
          !is.null(highlight_row) && rn[i] == highlight_row &&
          !is.null(ratio_col_hi)  && cn[j] == ratio_col_hi) {
        classes <- c(classes, "leaveVal")
      }
      
      
      cls <- if (length(classes)) paste0(" class='", paste(classes, collapse=" "), "'") else ""

      cell_txt <- if (isTRUE(math_labels)) {
        if (is.na(val)) {
          "&nbsp;"
        } else {
          vv <- suppressWarnings(as.numeric(val))
          if (is.finite(vv)) {
            int_tol <- 1e-9
            if (abs(vv) < int_tol) vv <- 0
            if (abs(vv - round(vv)) <= int_tol) {
              sprintf("\\(%s\\)", as.integer(round(vv)))         # tamsayı: ondalık yok
            } else {
              sprintf("\\(%s\\)", formatC(vv, format = "f", digits = digits))
            }
          } else {
            .simplex_escape(as.character(val))
          }
        }
      } else {
        .simplex_fmt(val, digits)  # yukarıda zaten tamsayıyı trimsiz yazıyor
      }
      
      sprintf("<td%s>%s</td>", cls, cell_txt)
      
    }, "")
    body_rows[i] <- paste0("<tr", cls_tr, "><th class='rowname'>", row_label_disp[i], "</th>", paste(tds, collapse=""), "</tr>")
  }
  tbody <- paste0("<tbody>", paste(body_rows, collapse=""), "</tbody>")
  
  paste0(css, "<table class='simplex'>", thead, tbody, "</table>")
}

`%||%` <- function(x, y) if (!is.null(x)) x else y

.aux_rows <- c("z","Cj","Zj","Zj-Cj")
.constr_rows <- function(tbl) setdiff(rownames(tbl), .aux_rows)

.Cj_for_cols <- function(cols) {
  Cj <- setNames(numeric(length(cols)), cols)
  x_idx <- grep("^x\\d+$", cols)
  if (length(x_idx)) {
    xnums <- as.integer(sub("^x(\\d+)$", "\\1", cols[x_idx]))
    Cj[x_idx] <- c[xnums]
  }
  Cj[grepl("^s\\d+$", names(Cj))] <- 0
  Cj[["b"]] <- 0
  Cj
}

# --- replace .basis_from_tableau with this version ---
.basis_from_tableau <- function(tbl) {
  if (is.null(tbl) || !nrow(tbl)) return(character(0))
  rows_constr <- setdiff(rownames(tbl), c("z","Cj","Zj","Zj-Cj"))
  # exclude b and any ratio_* helpers
  ratio_cols <- grep("^ratio_", colnames(tbl), value = TRUE)
  cols_vars  <- setdiff(colnames(tbl), c("b", ratio_cols))
  tol <- 1e-8
  basis <- setNames(rep(NA_character_, length(rows_constr)), rows_constr)
  if (!length(rows_constr) || !length(cols_vars)) return(basis)
  
  M <- suppressWarnings(as.matrix(tbl[rows_constr, cols_vars, drop = FALSE]))
  for (i in seq_along(rows_constr)) {
    is_I <- vapply(seq_along(cols_vars), function(j) {
      colvec <- suppressWarnings(as.numeric(M[, j]))
      if (any(!is.finite(colvec))) return(FALSE)
      abs(colvec[i] - 1) < tol && sum(abs(colvec[-i]) < tol) == (length(colvec) - 1)
    }, logical(1))
    if (any(is_I)) {
      idx <- which(is_I)
      cand_names <- cols_vars[idx]
      # tie-break preference: slacks > artificials > original x’s
      pref <- ifelse(grepl("^s\\d+$", cand_names), 1,
                     ifelse(grepl("^a\\d+$", cand_names), 2, 3))
      basis[i] <- cand_names[order(pref, idx)][1]
    }
  }
  basis
}



# return the set of nonbasic (eligible entering) columns
.nonbasic_columns <- function(tbl){
  if (is.null(tbl) || !nrow(tbl)) return(character(0))
  # exclude b and any ratio_* columns
  ratio_cols <- grep("^ratio_", colnames(tbl), value = TRUE)
  var_cols   <- setdiff(colnames(tbl), c("b", ratio_cols))
  # detect current basic variables
  basic_vars <- unique(.basis_from_tableau(tbl))
  basic_vars <- basic_vars[!is.na(basic_vars)]
  # candidates = all var_cols not in basis
  setdiff(var_cols, basic_vars)
}



# ---------- capture simplexR output & augment ----------
capture_simplexR <- function(A, b, c, unique_slacks = TRUE, z_row = c("drop","rename","keep"), ...) {
  z_row <- match.arg(z_row)
  out_lines <- utils::capture.output({ res <- simplexR(A, b, c, ...) })
  
  # helpers to parse console output
  strip_msg <- function(x) sub('^\\[\\d+\\]\\s+"(.*)"\\s*$', '\\1', x)
  lines_raw   <- out_lines
  lines_logic <- vapply(out_lines, strip_msg, character(1))
  
  .read_block <- function(lines) {
    lines <- lines[nzchar(lines)]
    con <- textConnection(paste0(lines, collapse = "\n"))
    on.exit(close(con), add = TRUE)
    df <- utils::read.table(con, header = TRUE, check.names = FALSE)
    if (all(grepl("^(R\\d+|z)$", df[[1]]))) { rn <- df[[1]]; df <- df[-1]; rownames(df) <- rn }
    if (unique_slacks) {
      cn <- colnames(df)
      if (any(cn == "s")) {
        idx <- which(cn == "s"); cn[idx] <- paste0("s", seq_along(idx)); colnames(df) <- cn
      }
    }
    df
  }
  

  
  # Put c_B into row names: "cB var"
  .apply_cost_row_labels <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(tbl)
    rows_constr <- .constr_rows(tbl); if (!length(rows_constr)) return(tbl)
    basis_names <- .basis_from_tableau(tbl); Cj <- .Cj_for_cols(colnames(tbl))
    rn <- rownames(tbl)
    for (r in rows_constr) {
      var <- basis_names[[r]]
      if (!is.na(var) && var %in% names(Cj)) rn[rn == r] <- paste0(Cj[[var]], " ", var)
    }
    rownames(tbl) <- rn
    tbl
  }
  
  # Add Cj/Zj/Zj-Cj; handle z-row as requested
  .augment_prices <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(tbl)
    cols <- colnames(tbl); has_z <- "z" %in% rownames(tbl)
    tbl_work <- tbl
    if (has_z && z_row == "drop") {
      tbl_work <- tbl_work[setdiff(rownames(tbl_work), "z"), , drop = FALSE]
    } else if (has_z && z_row == "rename") {
      rn <- rownames(tbl_work); rn[rn == "z"] <- "Zj-Cj"; rownames(tbl_work) <- rn
    }
    
    Cj <- .Cj_for_cols(cols)
    rows_constr <- .constr_rows(tbl_work)
    basis_names <- .basis_from_tableau(tbl_work)
    
    cB <- rep(0, length(rows_constr)); names(cB) <- rows_constr
    for (i in seq_along(rows_constr)) {
      bi <- basis_names[[ rows_constr[i] ]]
      cB[i] <- if (!is.na(bi) && bi %in% names(Cj)) Cj[[bi]] else 0
    }
    
    A_can <- as.matrix(tbl_work[rows_constr, , drop = FALSE])
    Zj <- as.numeric(cB %*% A_can); names(Zj) <- colnames(A_can)
    ZmC <- Zj - Cj[names(Zj)]
    
    add_list <- list(Cj = Cj[cols], Zj = Zj[cols], `Zj-Cj` = ZmC[cols])
    if ("Zj-Cj" %in% rownames(tbl_work)) add_list$`Zj-Cj` <- NULL
    add <- do.call(rbind, add_list)
    
    out <- rbind(add["Cj", , drop = FALSE], tbl_work)
    if ("Zj" %in% rownames(add)) out <- rbind(out, add["Zj", , drop = FALSE])
    if ("Zj-Cj" %in% rownames(add)) out <- rbind(out, add["Zj-Cj", , drop = FALSE])
    
    out <- .apply_cost_row_labels(out)
    
    final_rows <- rownames(out)
    rows_basic <- setdiff(final_rows, c("Cj","Zj","Zj-Cj","z"))
    ord <- c("Cj", rows_basic, intersect("z", final_rows), intersect("Zj", final_rows), intersect("Zj-Cj", final_rows))
    out[ord, , drop = FALSE]
  }
  
  .extract_state <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl) || !("b" %in% colnames(tbl))) return(list(z = NA_real_, x = numeric(0)))
    zval <- NA_real_
    if ("Zj" %in% rownames(tbl)) zval <- suppressWarnings(as.numeric(tbl["Zj","b"]))
    if (is.na(zval) && "z" %in% rownames(tbl)) zval <- suppressWarnings(as.numeric(tbl["z","b"]))
    x_cols <- which(grepl("^x\\d+$", colnames(tbl)))
    x_vals <- rep(0, length(x_cols)); names(x_vals) <- colnames(tbl)[x_cols]
    if (length(x_cols)) {
      tol <- 1e-8; rows_constr <- .constr_rows(tbl)
      M <- as.matrix(tbl[rows_constr, x_cols, drop = FALSE]); rhs <- as.numeric(tbl[rows_constr, "b"])
      for (j in seq_along(x_cols)) {
        colvec <- M[, j]; one_at <- which(abs(colvec - 1) < tol); zeros <- which(abs(colvec) < tol)
        if (length(one_at) == 1 && length(zeros) == (length(colvec) - 1)) x_vals[j] <- rhs[one_at]
      }
    }
    list(z = zval, x = x_vals)
  }
  
  .to_long <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(data.frame())
    rn <- rownames(tbl); cn <- colnames(tbl)
    data.frame(row = rep(rn, each = length(cn)),
               col = rep(cn, times = length(rn)),
               value = as.numeric(t(tbl)), stringsAsFactors = FALSE)
  }
  
  # ----- locate sections in printed output -----
  dash_idx      <- which(grepl("^-{3,}$", lines_logic))
  init_start    <- which(grepl("^Initial Tableau \\(Tableau 0\\)$", lines_logic))
  pivot_col_idx <- which(grepl("^Pivot column:\\s*\\d+$", lines_logic))
  pivot_row_idx <- which(grepl("^Pivot row:\\s*\\d+$", lines_logic))
  new_tbl_idx   <- which(grepl("^New tableau at the end of iteration \\d+$", lines_logic))
  status_idx    <- which(grepl("^Status:", lines_logic))
  
  # ----- initial (BEFORE) tableau -----
  init_block <- NULL
  if (length(init_start) == 1) {
    next_dash <- dash_idx[dash_idx > init_start]
    next_hdr  <- new_tbl_idx[new_tbl_idx > init_start]
    end_idx <- if (length(next_dash)) next_dash[1] - 1 else if (length(next_hdr)) next_hdr[1] - 1 else length(lines_raw)
    start_idx <- init_start + 1
    init_tbl_lines <- lines_raw[start_idx:end_idx]
    init_tbl_lines <- init_tbl_lines[!grepl('^\\[\\d+\\]\\s*"', init_tbl_lines)]
    init_block <- .augment_prices(.read_block(init_tbl_lines))
  }
  
  # ----- AFTER tableaux per iteration -----
  iter_tbls <- list()
  if (length(new_tbl_idx)) {
    for (i in seq_along(new_tbl_idx)) {
      start_logic <- new_tbl_idx[i] + 1
      next_end <- sort(c(dash_idx[dash_idx > new_tbl_idx[i]], new_tbl_idx[new_tbl_idx > new_tbl_idx[i]]))[1]
      end_logic <- if (length(next_end)) next_end - 1 else length(lines_logic)
      block <- lines_raw[start_logic:end_logic]
      block <- block[!grepl('^\\[\\d+\\]\\s*"', block)]
      iter_tbls[[i]] <- .augment_prices(.read_block(block))
    }
    names(iter_tbls) <- paste0("iter_", seq_along(iter_tbls))
  }
  
  # ----- pivot meta -----
  get_num <- function(x, pat) as.integer(sub(pat, "\\1", x))
  piv_cols <- if (length(pivot_col_idx)) get_num(lines_logic[pivot_col_idx], '^Pivot column:\\s*(\\d+)$') else integer(0)
  piv_rows <- if (length(pivot_row_idx)) get_num(lines_logic[pivot_row_idx], '^Pivot row:\\s*(\\d+)$') else integer(0)
  L <- length(iter_tbls)
  if (length(piv_cols) < L) piv_cols <- c(piv_cols, rep(NA_integer_, L - length(piv_cols)))
  if (length(piv_rows) < L) piv_rows <- c(piv_rows, rep(NA_integer_, L - length(piv_rows)))
  
  # ----- steps: BEFORE/AFTER, ratios, etc. -----
  steps <- vector("list", L)
  for (k in seq_len(L)) {
    before_tbl <- if (k == 1) init_block else iter_tbls[[k - 1]]
    after_tbl  <- iter_tbls[[k]]
    
    enter_var <- if (!is.null(before_tbl) && !is.na(piv_cols[k]) &&
                     piv_cols[k] >= 1 && piv_cols[k] <= ncol(before_tbl)) {
      colnames(before_tbl)[piv_cols[k]]
    } else NA_character_
    
    rows_before <- .constr_rows(before_tbl)
    pivot_row_name <- if (length(rows_before) && !is.na(piv_rows[k]) &&
                          piv_rows[k] >= 1 && piv_rows[k] <= length(rows_before)) {
      rows_before[piv_rows[k]]
    } else NA_character_
    
    ratios <- numeric(0); ratio_colname <- if (!is.na(enter_var)) paste0("ratio_", enter_var) else "ratio"
    if (!is.null(before_tbl) && !is.na(enter_var) && enter_var %in% colnames(before_tbl) && "b" %in% colnames(before_tbl)) {
      a <- as.numeric(before_tbl[rows_before, enter_var])
      bRHS <- as.numeric(before_tbl[rows_before, "b"])
      valid <- a > 1e-12
      ratios <- rep(NA_real_, length(a)); names(ratios) <- rows_before
      ratios[valid] <- bRHS[valid] / a[valid]
      
      before_tbl[[ratio_colname]] <- NA_real_
      before_tbl[rows_before, ratio_colname] <- ratios
      
      cols <- colnames(before_tbl); pos_b <- which(cols == "b")[1]
      cols <- append(cols, ratio_colname, after = pos_b); cols <- cols[!duplicated(cols)]
      before_tbl <- before_tbl[, cols, drop = FALSE]
    }
    
    before_long <- .to_long(before_tbl)
    if (nrow(before_long)) before_long$is_pivot_cell <- with(before_long, row == pivot_row_name & col == enter_var)
    
    st <- .extract_state(after_tbl)
    
    steps[[k]] <- list(
      iter = k,
      before = before_tbl,
      after  = after_tbl,
      pivot_col_index = piv_cols[k],
      pivot_row_index = piv_rows[k],
      pivot_col_name  = enter_var,
      pivot_row_name  = pivot_row_name,
      enter = enter_var,
      leave = pivot_row_name,
      ratios = ratios,
      ratio_colname = ratio_colname,
      z_after = st$z,
      x_after = st$x
    )
  }
  
  summ_list <- lapply(steps, function(stp){
    row <- data.frame(iter = stp$iter, pivot_col = stp$pivot_col_index, pivot_row = stp$pivot_row_index,
                      enter = stp$enter, leave = stp$leave, z = stp$z_after, check.names = FALSE)
    if (length(stp$x_after)) {
      xdf <- as.data.frame(as.list(stp$x_after))
      row <- cbind(row, xdf[, order(names(xdf)), drop = FALSE])
    }
    row
  })
  summary <- if (length(summ_list)) do.call(rbind, summ_list) else
    data.frame(iter = integer(0), pivot_col = integer(0), pivot_row = integer(0),
               enter = character(0), leave = character(0), z = numeric(0))
  
  status <- if (length(status_idx)) sub('^Status:\\s*(.*)$', "\\1", lines_logic[status_idx[length(status_idx)]]) else NA_character_
  attr(summary, "status") <- status
  
  list(
    initial     = init_block,   # augmented initial
    iterations  = iter_tbls,    # AFTER tableaux
    steps       = steps,        # BEFORE/AFTER metadata
    summary     = summary,
    result      = res,
    raw_output  = out_lines
  )
}

# ---------- Zj / Zj-Cj recomputation utilities ----------
.simplex_basis_costs <- function(T){
  rn <- rownames(T); cn <- colnames(T)
  aux <- intersect(c("Cj","Zj","Zj-Cj","z"), rn)
  rows_cons <- setdiff(rn, aux)
  
  cb <- rep(NA_real_, length(rows_cons)); names(cb) <- rows_cons
  m <- regexec("^\\s*([+-]?[0-9]*\\.?[0-9]+)\\s+([A-Za-z]+\\d+)\\s*$", rows_cons)
  mt <- regmatches(rows_cons, m)
  if (length(mt)) {
    for (k in seq_along(rows_cons)) if (length(mt[[k]]) == 3)
      cb[k] <- suppressWarnings(as.numeric(mt[[k]][2]))
  }
  need <- which(!is.finite(cb))
  if (length(need)) {
    if (!"Cj" %in% rn) stop("Tableau missing 'Cj' row to retrieve costs.")
    ratio_cols <- grep("^ratio_", cn, value = TRUE)
    var_cols <- setdiff(cn, c("b", ratio_cols))
    M <- suppressWarnings(as.matrix(T[rows_cons, var_cols, drop = FALSE]))
    for (idx in need) {
      i <- which(rows_cons == rows_cons[idx]); if (length(i) != 1) next
      # --- in .simplex_basis_costs(), replace the block that sets `col_is_basis` ---
      col_is_basis <- sapply(seq_along(var_cols), function(j){
        col <- suppressWarnings(as.numeric(M[, j]))
        if (any(!is.finite(col))) return(FALSE)
        abs(col[i] - 1) < 1e-9 && sum(abs(col[-i]) < 1e-9) == (length(col) - 1)
      })
      if (any(col_is_basis)) {
        ids <- which(col_is_basis)
        cand <- var_cols[ids]
        pref <- ifelse(grepl("^s\\d+$", cand), 1,
                       ifelse(grepl("^a\\d+$", cand), 2, 3))
        j <- ids[order(pref, ids)][1]
        cj <- suppressWarnings(as.numeric(T["Cj", var_cols[j]]))
        cb[idx] <- cj
      }
      
    }
  }
  cb
}

.simplex_recompute_Z_rows <- function(T){
  if (!"Cj" %in% rownames(T)) stop("Tableau must contain a 'Cj' row.")
  cn <- colnames(T); ratio_cols <- grep("^ratio_", cn, value = TRUE)
  cols_num <- setdiff(cn, ratio_cols)
  
  rn <- rownames(T); rows_cons <- setdiff(rn, c("Cj","Zj","Zj-Cj","z"))
  cb <- .simplex_basis_costs(T)
  if (any(!is.finite(cb))) warning("Some basis costs could not be inferred; Zj may be NA.")
  
  Z <- setNames(rep(NA_real_, length(cols_num)), cols_num)
  for (j in cols_num) {
    aij <- suppressWarnings(as.numeric(T[rows_cons, j, drop = TRUE]))
    if (all(!is.na(aij))) Z[j] <- sum(cb * aij, na.rm = TRUE)
  }
  
  if (!"Zj" %in% rn) { T <- rbind(T, Zj = rep(NA, ncol(T))); rownames(T)[nrow(T)] <- "Zj" }
  for (j in cols_num) T["Zj", j] <- Z[j]
  
  ZmC <- setNames(rep(NA_real_, length(cols_num)), cols_num)
  for (j in cols_num) {
    cj <- suppressWarnings(as.numeric(T["Cj", j])); zj <- suppressWarnings(as.numeric(T["Zj", j]))
    if (j == "b") ZmC[j] <- zj else if (is.finite(cj) && is.finite(zj)) ZmC[j] <- zj - cj
  }
  
  if (!"Zj-Cj" %in% rn) { T <- rbind(T, `Zj-Cj` = rep(NA, ncol(T))); rownames(T)[nrow(T)] <- "Zj-Cj" }
  for (j in cols_num) T["Zj-Cj", j] <- ZmC[j]
  
  list(tableau = T, Z = Z, ZmC = ZmC)
}











# ---------- HTML helpers for stages ----------
simplex_html_initial <- function(cap, digits = 2,
                                 add_ratio_col   = TRUE,
                                 mask_rows       = character(0),
                                 mask_ratio      = FALSE,
                                 mask_initial    = NULL,
                                 ratio_enter     = NULL,              # NULL | "auto" | "x2"
                                 auto_compute_ratios = FALSE,
                                 choose_enter_if_masked = FALSE,
                                 highlight_col   = NULL,              # NULL | "auto" | "xk"
                                 highlight_row   = NULL,              # NULL | "auto" | row name
                                 highlight_pivot = FALSE,
                                 highlight_ratio_col = FALSE,         # FALSE | TRUE | "auto"
                                 color_sign      = TRUE,
                                 color_sign_rows = c("Zj","Zj-Cj"),
                                 mark_enter_leave_values = TRUE,      # back‑compat
                                 mark_enter_value = TRUE,
                                 mark_leave_value = TRUE,
                                 math_labels = FALSE,
                                 mathjax_wrap = FALSE,
                                 title = "Simplex Tableau") {
  
  stopifnot(is.list(cap), !is.null(cap$initial))
  T0 <- cap$initial; if (!"b" %in% colnames(T0)) stop("Initial tableau missing 'b' column.")
  T0_orig <- T0
  
  if (!is.null(mask_initial) && isTRUE(mask_initial)) {
    if (!length(mask_rows)) mask_rows <- c("Zj","Zj-Cj")
    if (!isTRUE(mask_ratio)) mask_ratio <- TRUE
  }
  
  # ensure exactly one ratio column placed after 'b'
  rcols <- grep("^ratio_", colnames(T0), value = TRUE)
  if (isTRUE(add_ratio_col) && !length(rcols)) { T0[["ratio_oran"]] <- NA_real_; rcols <- "ratio_oran" }
  if (length(rcols) > 1L) { keep <- rcols[1]; T0 <- T0[, c(setdiff(colnames(T0), rcols), keep), drop = FALSE]; rcols <- keep }
  if (length(rcols)) { cols <- colnames(T0); pos_b <- which(cols == "b")[1]; cols <- append(setdiff(cols, rcols), rcols, after = pos_b); T0 <- T0[, cols, drop = FALSE] }
  ratio_colname <- if (length(rcols)) rcols[1] else NULL
  
  # masking
  mask_rows <- intersect(mask_rows, c("Zj","Zj-Cj"))
  if ("Zj" %in% mask_rows && "Zj" %in% rownames(T0)) T0["Zj", ] <- NA
  if ("Zj-Cj" %in% mask_rows && "Zj-Cj" %in% rownames(T0)) { T0["Zj-Cj", ] <- NA; if ("b" %in% colnames(T0)) T0["Zj-Cj","b"] <- "—" }
  if (isTRUE(mask_ratio) && !is.null(ratio_colname)) T0[, ratio_colname] <- NA
  
  # choose entering column
  enter_col <- NULL
  if (!is.null(ratio_enter) && !identical(ratio_enter, "auto")) {
    if (ratio_enter %in% colnames(T0)) enter_col <- ratio_enter
  } else {
    zj_row_visible <- !("Zj-Cj" %in% mask_rows) && "Zj-Cj" %in% rownames(T0)
    src  <- if (zj_row_visible) T0 else if (isTRUE(choose_enter_if_masked)) T0_orig else NULL
    cand <- .nonbasic_columns(src %||% T0)
    if (!is.null(src) && "Zj-Cj" %in% rownames(src) && length(cand)) {
      rc <- suppressWarnings(as.numeric(src["Zj-Cj", cand, drop = TRUE]))
      if (any(is.finite(rc))) enter_col <- cand[which.min(rc)]
    }
  }
  
  # compute ratios only if we’ll show them
  need_ratios <- isTRUE(add_ratio_col) && !isTRUE(mask_ratio) && !is.null(enter_col) &&
    (isTRUE(auto_compute_ratios) || (!is.null(ratio_enter) && !identical(ratio_enter,"auto")))
  
  leave_row_auto <- NULL; entered_value_auto <- NA_real_; leaved_value_auto <- NA_real_
  if (!is.null(enter_col) && !is.null(ratio_colname) && need_ratios) {
    T0[, ratio_colname] <- NA_real_
    aux_rows <- c("Cj","Zj","Zj-Cj","z"); rows_constr <- setdiff(rownames(T0), aux_rows)
    a <- suppressWarnings(as.numeric(T0[rows_constr, enter_col])); bRHS <- suppressWarnings(as.numeric(T0[rows_constr, "b"]))
    valid <- is.finite(a) & (a > 1e-12) & is.finite(bRHS)
    ratios <- rep(NA_real_, length(rows_constr)); names(ratios) <- rows_constr
    ratios[valid] <- bRHS[valid] / a[valid]
    T0[rows_constr, ratio_colname] <- ratios
    pos <- which(valid & ratios >= 0)
    if (length(pos)) { idx <- pos[which.min(ratios[pos])]; leave_row_auto <- rows_constr[idx]; leaved_value_auto <- ratios[idx] }
    if ("Zj-Cj" %in% rownames(T0_orig)) entered_value_auto <- suppressWarnings(as.numeric(T0_orig["Zj-Cj", enter_col]))
  } else if (!is.null(enter_col) && "Zj-Cj" %in% rownames(T0_orig)) {
    entered_value_auto <- suppressWarnings(as.numeric(T0_orig["Zj-Cj", enter_col]))
  }
  
  col_hi   <- if (identical(highlight_col, "auto")) enter_col else highlight_col
  row_hi   <- if (identical(highlight_row, "auto")) leave_row_auto else highlight_row
  ratio_hi <- if (identical(highlight_ratio_col, TRUE)) ratio_colname else if (identical(highlight_ratio_col, "auto")) "auto" else NULL
  
  do_enter_box <- isTRUE(mark_enter_leave_values) && isTRUE(mark_enter_value)
  do_leave_box <- isTRUE(mark_enter_leave_values) && isTRUE(mark_leave_value)
  
  enter_cell <- if (do_enter_box && !is.null(enter_col)) list(row="Zj-Cj", col=enter_col, value=entered_value_auto, tol=1e-9) else NULL
  leave_cell <- if (do_leave_box && !is.null(leave_row_auto) && !is.null(ratio_colname)) {
    # Always mark by row/col only — no numeric equality check
    list(row = leave_row_auto, col = ratio_colname)
  } else NULL
  
  
  
  tbl <- .simplex_df_to_html(
    T0, head_left = "TDV", digits = digits,
    highlight_col   = col_hi,
    highlight_row   = row_hi,
    highlight_pivot = isTRUE(highlight_pivot),
    color_sign      = color_sign,
    color_sign_rows = color_sign_rows,
    highlight_cell_enter = enter_cell,
    highlight_cell_leave = leave_cell,
    highlight_ratio_col  = ratio_hi,
    math_labels = math_labels,
    leave_fallback = isTRUE(mark_leave_value)
  )
  # Optionally wrap
  if (mathjax_wrap) {
    tbl <- wrap_with_mathjax(tbl, title = title)
  }
  
  tbl
}

# ---------- Step 1: update only the entering row ----------
simplex_html_step_update_enter_row <- function(
    cap, digits = 2,
    ratio_enter = "auto",
    choose_enter_if_masked = TRUE,
    highlight_pivot = TRUE,
    rename_leaving_row = TRUE,
    mask_rows = character(0),
    mask_ratio = FALSE,
    highlight_ratio_col = TRUE,
    enter_col_style = c("tint","frame","none"),
    color_sign = TRUE,
    color_sign_rows = c("Zj","Zj-Cj"),
    mark_enter_value = TRUE,
    mark_leave_value = TRUE,
    math_labels = FALSE,                                 
    mathjax_wrap = FALSE,
    title = "Simplex Tableau"){
  
  enter_col_style <- match.arg(enter_col_style)
  stopifnot(is.list(cap), !is.null(cap$initial))
  T0 <- cap$initial; if (!"b" %in% colnames(T0)) stop("Initial tableau missing 'b' column.")
  T0_orig <- T0
  
  rcols <- grep("^ratio_", colnames(T0), value = TRUE)
  if (!length(rcols)) { T0[["ratio_oran"]] <- NA_real_; rcols <- "ratio_oran" }
  if (length(rcols) > 1L) { keep <- rcols[1]; T0 <- T0[, c(setdiff(colnames(T0), rcols), keep), drop = FALSE]; rcols <- keep }
  if (length(rcols)) { cols <- colnames(T0); pos_b <- which(cols == "b")[1]; cols <- append(setdiff(cols, rcols), rcols, after = pos_b); T0 <- T0[, cols, drop = FALSE] }
  ratio_colname <- rcols[1]
  
  mask_rows <- intersect(mask_rows, c("Zj","Zj-Cj"))
  if ("Zj" %in% mask_rows && "Zj" %in% rownames(T0)) T0["Zj", ] <- NA
  if ("Zj-Cj" %in% mask_rows && "Zj-Cj" %in% rownames(T0)) { T0["Zj-Cj", ] <- NA; if ("b" %in% colnames(T0)) T0["Zj-Cj","b"] <- "—" }
  
  enter_col <- NULL
  if (!is.null(ratio_enter) && !identical(ratio_enter, "auto")) {
    if (ratio_enter %in% colnames(T0)) enter_col <- ratio_enter
  } else {
    zj_visible <- !("Zj-Cj" %in% mask_rows) && "Zj-Cj" %in% rownames(T0)
    src <- if (zj_visible) T0 else if (isTRUE(choose_enter_if_masked)) T0_orig else NULL
    cand <- .nonbasic_columns(src %||% T0)
    if (!is.null(src) && "Zj-Cj" %in% rownames(src) && length(cand)) {
      rc <- suppressWarnings(as.numeric(src["Zj-Cj", cand, drop = TRUE]))
      if (any(is.finite(rc))) enter_col <- cand[which.min(rc)]
    }
  }
  if (is.null(enter_col)) stop("Could not determine entering column. Provide ratio_enter='xk' or unmask Zj-Cj.")
  
  aux_rows <- c("Cj","Zj","Zj-Cj","z")
  row_constr <- setdiff(rownames(T0), aux_rows)
  a <- suppressWarnings(as.numeric(T0[row_constr, enter_col]))
  bRHS <- suppressWarnings(as.numeric(T0[row_constr, "b"]))
  valid <- is.finite(a) & (a > 1e-12) & is.finite(bRHS)
  ratios <- rep(NA_real_, length(row_constr)); names(ratios) <- row_constr
  ratios[valid] <- bRHS[valid] / a[valid]
  pos <- which(valid & ratios > 0)
  if (!length(pos)) stop("No positive ratio -> unbounded or cannot pivot.")
  idx_leave <- pos[which.min(ratios[pos])]
  leave_row <- row_constr[idx_leave]
  pivot <- suppressWarnings(as.numeric(T0[leave_row, enter_col])); if (!is.finite(pivot) || abs(pivot) < 1e-12) stop("Invalid pivot value.")
  
  # normalize only pivot row
  row_vals <- T0[leave_row, , drop = FALSE]
  for (cn in colnames(row_vals)) { v <- suppressWarnings(as.numeric(row_vals[1, cn])); if (is.finite(v)) row_vals[1, cn] <- v / pivot }
  T0[leave_row, ] <- row_vals[1, ]
  
  if (!isTRUE(mask_ratio)) T0[row_constr, ratio_colname] <- ratios else T0[, ratio_colname] <- NA
  
  if (isTRUE(rename_leaving_row)) {
    cj_val <- suppressWarnings(as.numeric(T0_orig["Cj", enter_col]))
    new_name <- if (is.finite(cj_val)) sprintf("%g %s", cj_val, enter_col) else enter_col
    rn <- rownames(T0); rn[rn == leave_row] <- new_name; rownames(T0) <- rn; leave_row <- new_name
  }
  
  entered_value_auto <- suppressWarnings(as.numeric(T0_orig["Zj-Cj", enter_col]))
  leaved_value_auto  <- ratios[names(ratios) == row_constr[idx_leave]]
  
  enter_cell <- if (isTRUE(mark_enter_value)) list(row="Zj-Cj", col=enter_col, value=entered_value_auto, tol=1e-9) else NULL
  leave_cell <- if (isTRUE(mark_leave_value) && !isTRUE(mask_ratio)) list(row=leave_row, col=ratio_colname, value=leaved_value_auto, tol=1e-9) else NULL
  
  tbl<- .simplex_df_to_html(
    T0, head_left = "TDV", digits = digits,
    highlight_col   = enter_col,
    enter_col_style = enter_col_style,
    highlight_row   = leave_row,
    highlight_pivot = isTRUE(highlight_pivot),
    color_sign      = color_sign,
    color_sign_rows = color_sign_rows,
    highlight_cell_enter = enter_cell,
    highlight_cell_leave = leave_cell,
    highlight_ratio_col  = if (isTRUE(highlight_ratio_col)) ratio_colname else NULL,
    math_labels = math_labels,
    leave_fallback = isTRUE(mark_leave_value)
  )
  # Optionally wrap
  if (mathjax_wrap) {
    tbl <- wrap_with_mathjax(tbl, title = title)
  }
  
  tbl
}

# ---------- Step 2: eliminate other rows ----------
# Start from the right tableau and eliminate non-pivot rows for iteration k.
simplex_html_step_eliminate_other_rows <- function(
    cap,
    iter,                               # 1-based iteration index
    digits = 2,
    # visuals
    enter_col_style = c("frame","tint","none"),
    highlight_ratio_col = TRUE,
    highlight_pivot = TRUE,
    color_sign = TRUE,
    color_sign_rows = c("Zj","Zj-Cj"),
    highlight_base_row = TRUE,
    highlight_rows_other = character(0),
    # masking
    mask_rows = character(0),
    mask_ratio = TRUE,
    # boxing
    mark_enter_value = TRUE,
    mark_leave_value = TRUE,
    ratio_enter = "auto", 
    choose_enter_if_masked = TRUE, 
    rename_leaving_row = TRUE,
    # LaTeX labels / wrapper
    math_labels = FALSE,
    mathjax_wrap = FALSE,
    title = sprintf("Simplex – iteration %d (eliminate)", iter)
){
  enter_col_style <- match.arg(enter_col_style)
  stopifnot(is.list(cap), length(cap$steps) >= iter)
  
  stp <- cap$steps[[iter]]
  
  # --- choose starting tableau:
  # for iter=1 start from initial BEFORE; else AFTER tableau of previous iter
  T0 <- if (iter == 1) stp$before else cap$iterations[[iter - 1]]
  T0_orig <- T0
  if (is.null(T0) || !nrow(T0)) stop("No starting tableau for this iteration.")
  
  enter_col <- stp$pivot_col_name
  leave_row <- stp$pivot_row_name
  if (is.na(enter_col) || is.na(leave_row)) stop("Missing pivot information for this iteration.")
  
  # ensure ratio column exists and sits after 'b'
  rcols <- grep("^ratio_", colnames(T0), value = TRUE)
  if (!length(rcols)) { T0[["ratio_oran"]] <- NA_real_; rcols <- "ratio_oran" }
  if (length(rcols) > 1L) {
    keep <- rcols[1]
    T0 <- T0[, c(setdiff(colnames(T0), rcols), keep), drop = FALSE]
    rcols <- keep
  }
  if (length(rcols)) {
    cols <- colnames(T0)
    pos_b <- match("b", cols)
    cols  <- append(setdiff(cols, rcols), rcols, after = pos_b)
    T0    <- T0[, cols, drop = FALSE]
  }
  ratio_colname <- rcols[1]
  
  # (patch) if ratios are missing (e.g., iter > 1 starts from AFTER tableau),
  # compute b/a for current entering column so the ratio column is visible.
  rows_constr <- setdiff(rownames(T0), intersect(c("Cj","Zj","Zj-Cj","z"), rownames(T0)))
  if (length(rows_constr) && enter_col %in% colnames(T0) && "b" %in% colnames(T0)) {
    cur_vals <- suppressWarnings(as.numeric(T0[rows_constr, ratio_colname]))
    if (all(is.na(cur_vals))) {
      a    <- suppressWarnings(as.numeric(T0[rows_constr, enter_col]))
      bRHS <- suppressWarnings(as.numeric(T0[rows_constr, "b"]))
      valid  <- is.finite(a) & (a > 1e-12) & is.finite(bRHS)
      ratios <- rep(NA_real_, length(rows_constr)); names(ratios) <- rows_constr
      ratios[valid] <- bRHS[valid] / a[valid]
      
      T0[, ratio_colname] <- NA_real_
      T0[rows_constr, ratio_colname] <- ratios
      
      # keep ratio column right after 'b'
      cols  <- colnames(T0); pos_b <- match("b", cols)
      cols  <- append(setdiff(cols, ratio_colname), ratio_colname, after = pos_b)
      T0    <- T0[, cols, drop = FALSE]
    }
  }
  
  
  # rows to treat as constraints (not Cj/Zj/Zj-Cj/z)
  aux_rows  <- intersect(c("Cj","Zj","Zj-Cj","z"), rownames(T0))
  rows_cons <- setdiff(rownames(T0), aux_rows)
  
  # --- normalize pivot row
  pivot_val <- suppressWarnings(as.numeric(T0[leave_row, enter_col]))
  if (!is.finite(pivot_val) || abs(pivot_val) < 1e-12) stop("Invalid pivot value.")
  row_vals <- T0[leave_row, , drop = FALSE]
  for (cn in colnames(row_vals)) {
    v <- suppressWarnings(as.numeric(row_vals[1, cn]))
    if (is.finite(v)) row_vals[1, cn] <- v / pivot_val
  }
  T0[leave_row, ] <- row_vals[1, ]
  
  # --- eliminate entering column from other rows
  for (r in setdiff(rows_cons, leave_row)) {
    coeff <- suppressWarnings(as.numeric(T0[r, enter_col]))
    if (!is.finite(coeff) || abs(coeff) < 1e-12) next
    for (cn in colnames(T0)) {
      v_r <- suppressWarnings(as.numeric(T0[r, cn]))
      v_p <- suppressWarnings(as.numeric(T0[leave_row, cn]))
      if (is.finite(v_r) && is.finite(v_p)) T0[r, cn] <- v_r - coeff * v_p
    }
  }
  
  # --- mask visuals
  mask_rows <- intersect(mask_rows, c("Zj","Zj-Cj"))
  if ("Zj" %in% mask_rows && "Zj" %in% rownames(T0))     T0["Zj", ] <- NA
  if ("Zj-Cj" %in% mask_rows && "Zj-Cj" %in% rownames(T0)) {
    T0["Zj-Cj", ] <- NA
    if ("b" %in% colnames(T0)) T0["Zj-Cj","b"] <- "—"
  }
  if (isTRUE(mask_ratio)) T0[, ratio_colname] <- NA
  
  # rename pivot row like "cB var" if your pipeline expects it (optional; comment out if you don’t want renaming here)
  # cj_val <- suppressWarnings(as.numeric(stp$before["Cj", enter_col]))
  # if (is.finite(cj_val)) {
  #   rn <- rownames(T0); rn[rn == leave_row] <- sprintf("%g %s", cj_val, enter_col); rownames(T0) <- rn
  #   leave_row <- sprintf("%g %s", cj_val, enter_col)
  # }
  
  # --- optionally rename leaving row as "cB var" (e.g. "22 x2")
  if (isTRUE(rename_leaving_row)) {
    cj_val <- suppressWarnings(as.numeric(stp$before["Cj", enter_col]))
    if (is.finite(cj_val)) {
      # 22.0 yerine 22 yaz; tam sayı değilse digits ile biçimle
      lab_cost <- if (abs(cj_val - round(cj_val)) < 1e-9) {
        as.integer(round(cj_val))
      } else {
        formatC(cj_val, format = "f", digits = digits)
      }
      new_name <- sprintf("%s %s", lab_cost, enter_col)
      rn <- rownames(T0)
      rn[rn == leave_row] <- new_name
      rownames(T0) <- rn
      leave_row <- new_name  # highlight vb. için güncel ismi kullan
    }
  }
  
  
  # choose extra rows to highlight (optional)
  rows_extra <- character(0)
  if (identical(highlight_rows_other, "others")) {
    rows_extra <- setdiff(rownames(T0), c("Cj","Zj","Zj-Cj", leave_row))
  } else if (length(highlight_rows_other)) {
    rows_extra <- intersect(highlight_rows_other, rownames(T0))
  }
  
  # value boxes (enter from Zj-Cj before; leave from min positive ratio if available)
  enter_val <- suppressWarnings(as.numeric(stp$before["Zj-Cj", enter_col]))
  leave_val <- if (length(stp$ratios)) stp$ratios[match(leave_row, names(stp$ratios))] else NA_real_
  
  html <- .simplex_df_to_html(
    T0, head_left = "TDV", digits = digits,
    highlight_col   = enter_col,
    enter_col_style = enter_col_style,
    highlight_row   = if (isTRUE(highlight_base_row)) leave_row else NULL,
    highlight_pivot = isTRUE(highlight_pivot),
    color_sign      = color_sign,
    color_sign_rows = color_sign_rows,
    highlight_cell_enter = if (isTRUE(mark_enter_value)) list(row="Zj-Cj", col=enter_col, value=enter_val, tol=1e-9) else NULL,
    highlight_cell_leave = if (isTRUE(mark_leave_value) && !isTRUE(mask_ratio)) list(row=leave_row, col=ratio_colname, value=leave_val, tol=1e-9) else NULL,
    highlight_ratio_col  = if (isTRUE(highlight_ratio_col)) ratio_colname else NULL,
    highlight_rows_extra = rows_extra,
    math_labels = math_labels,
    leave_fallback = isTRUE(mark_leave_value)
  )
  
  if (isTRUE(mathjax_wrap)) {
    html <- wrap_with_mathjax(html, title = title)
  }
  
  html
}

# ---------- Step 3: after pivot & recompute objective ----------
simplex_html_step_after_pivot_and_objective <- function(
    cap, digits = 2,
    ratio_enter = "auto",
    choose_enter_if_masked = TRUE,
    enter_col_style = c("frame","tint","none"),
    highlight_ratio_col = TRUE,
    highlight_pivot = TRUE,
    color_sign = TRUE,
    highlight_base_row = TRUE,
    mask_rows = character(0),
    mask_ratio = TRUE,
    mark_enter_value = TRUE,
    mark_leave_value = TRUE,
    highlight_optimum_value = TRUE,     # <-- NEW
    math_labels = FALSE,
    mathjax_wrap = FALSE,
    title = "Simplex Tableau"){
  
  enter_col_style <- match.arg(enter_col_style)
  T0 <- cap$initial; T0_orig <- T0
  if (!"b" %in% colnames(T0)) stop("Initial tableau missing 'b' column.")
  
  rcols <- grep("^ratio_", colnames(T0), value = TRUE)
  if (!length(rcols)) { T0[["ratio_oran"]] <- NA_real_; rcols <- "ratio_oran" }
  if (length(rcols) > 1L) { keep <- rcols[1]; T0 <- T0[, c(setdiff(colnames(T0), rcols), keep), drop = FALSE]; rcols <- keep }
  if (length(rcols)) { cols <- colnames(T0); pos_b <- which(cols == "b")[1]; cols <- append(setdiff(cols, rcols), rcols, after = pos_b); T0 <- T0[, cols, drop = FALSE] }
  ratio_colname <- rcols[1]
  
  enter_col <- NULL
  zj_visible <- "Zj-Cj" %in% rownames(T0) && !("Zj-Cj" %in% mask_rows)
  if (!is.null(ratio_enter) && !identical(ratio_enter, "auto")) {
    if (ratio_enter %in% colnames(T0)) enter_col <- ratio_enter
  } else {
    src <- if (zj_visible) T0 else if (isTRUE(choose_enter_if_masked)) T0_orig else NULL
    cand <- .nonbasic_columns(src %||% T0)
    if (!is.null(src) && "Zj-Cj" %in% rownames(src) && length(cand)) {
      rc <- suppressWarnings(as.numeric(src["Zj-Cj", cand, drop = TRUE]))
      if (any(is.finite(rc))) enter_col <- cand[which.min(rc)]
    }
  }
  if (is.null(enter_col)) stop("Could not determine entering column.")
  
  aux <- c("Cj","Zj","Zj-Cj","z")
  rows_cons <- setdiff(rownames(T0), aux)
  a    <- suppressWarnings(as.numeric(T0[rows_cons, enter_col]))
  bRHS <- suppressWarnings(as.numeric(T0[rows_cons, "b"]))
  valid <- is.finite(a) & (a > 1e-12) & is.finite(bRHS)
  ratios <- rep(NA_real_, length(rows_cons)); names(ratios) <- rows_cons
  ratios[valid] <- bRHS[valid] / a[valid]
  pos <- which(valid & ratios > 0); if (!length(pos)) stop("No positive ratio -> unbounded or cannot pivot.")
  idx_leave <- pos[which.min(ratios[pos])]; leave_row <- rows_cons[idx_leave]
  pivot_val <- suppressWarnings(as.numeric(T0[leave_row, enter_col])); if (!is.finite(pivot_val) || abs(pivot_val) < 1e-12) stop("Invalid pivot value.")
  
  # normalize pivot row and eliminate others
  row_vals <- T0[leave_row, , drop = FALSE]
  for (cn in colnames(row_vals)) { v <- suppressWarnings(as.numeric(row_vals[1, cn])); if (is.finite(v)) row_vals[1, cn] <- v / pivot_val }
  T0[leave_row, ] <- row_vals[1, ]
  for (r in setdiff(rows_cons, leave_row)) {
    coeff <- suppressWarnings(as.numeric(T0[r, enter_col])); if (!is.finite(coeff) || abs(coeff) < 1e-12) next
    for (cn in colnames(T0)) {
      v_r <- suppressWarnings(as.numeric(T0[r, cn])); v_p <- suppressWarnings(as.numeric(T0[leave_row, cn]))
      if (is.finite(v_r) && is.finite(v_p)) T0[r, cn] <- v_r - coeff * v_p
    }
  }
  if (isTRUE(mask_ratio)) T0[, ratio_colname] <- NA
  
  cj_enter <- suppressWarnings(as.numeric(T0_orig["Cj", enter_col]))
  new_name <- if (is.finite(cj_enter)) sprintf("%g %s", cj_enter, enter_col) else enter_col
  rn <- rownames(T0); rn[rn == leave_row] <- new_name; rownames(T0) <- rn; leave_row <- new_name
  
  comp <- .simplex_recompute_Z_rows(T0)
  T1   <- comp$tableau; zj <- comp$Z; zmc <- comp$ZmC
  
  if (length(mask_rows)) {
    if ("Zj" %in% mask_rows && "Zj" %in% rownames(T1))     T1["Zj", ] <- NA
    if ("Zj-Cj" %in% mask_rows && "Zj-Cj" %in% rownames(T1)) { T1["Zj-Cj", ] <- NA; if ("b" %in% colnames(T1)) T1["Zj-Cj","b"] <- "—" }
  }
  
  var_cols <- setdiff(names(zmc), c("b"))
  optimal <- all(is.na(zmc[var_cols]) | (zmc[var_cols] >= -1e-9))
  
  enter_val <- suppressWarnings(as.numeric(T0_orig["Zj-Cj", enter_col]))
  
  opt_cell <- NULL
  if (isTRUE(highlight_optimum_value) && "Zj" %in% rownames(T1) && "b" %in% colnames(T1)) {
    z_val <- suppressWarnings(as.numeric(T1["Zj","b"]))
    if (is.finite(z_val)) {
      opt_cell <- list(row = "Zj", col = "b", value = z_val, tol = 1e-9)
    }
  }
  
  
  html <- .simplex_df_to_html(
    T1, head_left = "TDV", digits = digits,
    highlight_col   = enter_col,
    enter_col_style = enter_col_style,
    highlight_row   = if (isTRUE(highlight_base_row)) leave_row else NULL,
    highlight_pivot = isTRUE(highlight_pivot),
    color_sign      = color_sign,
    color_sign_rows = c("Zj","Zj-Cj"),
    highlight_cell_enter = if (isTRUE(mark_enter_value)) list(row="Zj-Cj", col=enter_col, value=enter_val, tol=1e-9) else NULL,
    highlight_cell_leave = NULL,
    highlight_cell_optimum = opt_cell,                     # <-- NEW
    highlight_ratio_col  = if (isTRUE(highlight_ratio_col)) ratio_colname else NULL,
    math_labels = math_labels,
    leave_fallback = isTRUE(mark_leave_value)
  )
  # Optionally wrap
  if (mathjax_wrap) {
    html <- wrap_with_mathjax(html, title = title)
  }
  
  list(html = html, optimal = optimal, zj = zj, zjcj = zmc)
}

# # --- 1) The two functions you pasted (renderer + html builder) ---
# # (Use exactly the versions from your last message — I won’t repeat to save space.)
# # .simplex_df_to_html <- function(...) { ... }
# # simplex_html_initial <- function(...) { ... }
# 
# # --- 2) Your LP ---
# A <- rbind(c(1,4,3),
#            c(2,2,4),
#            c(3,5,6),
#            c(4,8,3))
# dimnames(A) <- list(paste0("R",1:4), c("x1","x2","x3"))
# b <- c(24,46,60,120)
# c <- c(10,22,18)
# 
# # --- 3) Capture the simplexR printout (NO extra args here) ---
# cap <- capture_simplexR(A, b, c)
# 
# # choose entering column from the original Zj-Cj, but render with Zj/Zj-Cj hidden and ratios blank
# html_choose_col_hide_ratios <- simplex_html_initial(
#   cap,
#   add_ratio_col = TRUE,
#   mask_rows  = c('Zj', 'Zj-Cj'),     # hide both rows
#   mask_ratio = TRUE,                # keep ratio column blank
#   ratio_enter = "auto",             # pick by most-negative Zj-Cj
#   choose_enter_if_masked = TRUE,    # <-- key switch
#   highlight_ratio_col = TRUE,        # also frame the Oran column
#   math_labels = TRUE,
#   mathjax_wrap = TRUE
# )
# writeLines(html_choose_col_hide_ratios, "01.html")
# 
# 
# html_choose_col_hide_ratios <- simplex_html_initial(
#   cap,
#   add_ratio_col = TRUE,
#   mask_rows  = character(0),     # hide both rows
#   mask_ratio = TRUE,                # keep ratio column blank
#   highlight_ratio_col = TRUE,        # also frame the Oran column
#   mark_enter_value = FALSE,        # <- don't box the min Zj-Cj value
#   math_labels = TRUE,
#   mathjax_wrap = TRUE
# )
# writeLines(html_choose_col_hide_ratios, "02.html")
# 
# 
# 
# html_choose_col_hide_ratios <- simplex_html_initial(
#   cap,
#   add_ratio_col = TRUE,
#   mask_rows  = character(0),     # hide both rows
#   mask_ratio = TRUE,                # keep ratio column blank
#   highlight_ratio_col = TRUE,        # also frame the Oran column
#   highlight_col = 'auto',
#   mark_enter_value = TRUE,
#   auto_compute_ratios = TRUE,
#   math_labels = TRUE,
#   mathjax_wrap = TRUE
# )
# writeLines(html_choose_col_hide_ratios, "03.html")
# 
# 
# html_choose_col_hide_ratios <- simplex_html_initial(
#   cap,
#   add_ratio_col = TRUE,
#   mask_rows  = character(0),     # hide both rows
#   mask_ratio = FALSE,                # keep ratio column blank
#   highlight_ratio_col = TRUE,        # also frame the Oran column
#   highlight_col = 'auto',
#   mark_enter_value = TRUE,
#   highlight_row = 'auto',
#   mark_leave_value = TRUE,
#   auto_compute_ratios = TRUE,
#   highlight_pivot = TRUE,
#   choose_enter_if_masked = TRUE,    # <-- key switch
#   math_labels = TRUE,
#   mathjax_wrap = TRUE
# )
# writeLines(html_choose_col_hide_ratios, "04.html")
# 
# 
# # Update only the entering row, outline (frame) the base column without green fill
# html_step1_no_tint <- simplex_html_step_update_enter_row(
#   cap,
#   ratio_enter = "auto",
#   mask_rows  = c('Zj', 'Zj-Cj'),     # hide both rows
#   mask_ratio = TRUE,                # keep ratio column blank
#   choose_enter_if_masked = TRUE,
#   rename_leaving_row = TRUE,
#   highlight_pivot = TRUE,
#   highlight_ratio_col = TRUE,
#   mark_enter_value = FALSE,
#   enter_col_style = "frame",   # << no green tint; just a border
#   math_labels = TRUE,
#   mathjax_wrap = TRUE,
#   mark_leave_value = FALSE
# )
# writeLines(html_step1_no_tint, "05.html")
# 
# 
# 
# html_step2_no_base_hi <- simplex_html_step_eliminate_other_rows(
#   cap, iter = 1,
#   highlight_rows_other = "others",   # optional orange band on non-pivot rows
#   math_labels = TRUE,
#   mathjax_wrap = TRUE,
#   rename_leaving_row = TRUE,
#   mark_leave_value = FALSE,
#   mark_enter_value = FALSE
#   
# )
# writeLines(html_step2_no_base_hi, "06.html")
# 
# 
# step3 <- simplex_html_step_after_pivot_and_objective(
#   cap,
#   highlight_base_row = FALSE,   # <- turns off purple band
#   math_labels = TRUE,
#   mathjax_wrap = TRUE
# )
# writeLines(step3$html, "07.html")
# 
# 
# # Iteration 1 (starts from initial)
# html_it1 <- simplex_html_step_eliminate_other_rows(
#   cap, iter = 1,
#   highlight_rows_other = "others",   # optional orange band on non-pivot rows
#   math_labels = TRUE,
#   mathjax_wrap = TRUE,
#   rename_leaving_row = TRUE
# )
# 
# # Iteration 2 (starts from AFTER tableau of iter 1)
# html_it2 <- simplex_html_step_eliminate_other_rows(
#   cap, iter = 2,
#   highlight_rows_other = "others",
#   math_labels = TRUE,
#   mathjax_wrap = TRUE,
#   rename_leaving_row = TRUE
# )
# 
# # write to files (optional)
# writeLines(html_it1, "iter01_eliminate.html")
# writeLines(html_it2, "iter02_eliminate.html")
