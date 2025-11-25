# ============================================================
# Simplex + Big-M: capture + symbolic rendering (single file)
# ============================================================

# ---------- Utilities ----------
.simplex_escape <- function(x){
  x <- gsub("&","&amp;",x,fixed=TRUE)
  x <- gsub("<","&lt;",x,fixed=TRUE)
  x <- gsub(">","&gt;",x,fixed=TRUE)
  x <- gsub('"',"&quot;",x,fixed=TRUE)
  x
}

.simplex_fmt <- function(v, digits = 2, int_tol = 1e-9){
  if (is.na(v)) return("&nbsp;")
  vv <- suppressWarnings(as.numeric(v))
  if (is.finite(vv)) {
    if (abs(vv) < int_tol) vv <- 0
    if (abs(vv - round(vv)) <= int_tol) {
      return(.simplex_escape(as.character(as.integer(round(vv)))))
    } else {
      return(.simplex_escape(formatC(vv, format = "f", digits = digits)))
    }
  }
  .simplex_escape(as.character(v))
}

# Pretty label helpers (MathJax)
.simplex_label_tex <- function(x) {
  if (x == "b")     return("$b$")
  if (x == "Cj")    return("$C_{j}$")
  if (x == "Zj")    return("$Z_{j}$")
  if (x == "Zj-Cj") return("$Z_{j}-C_{j}$")
  if (grepl("^ratio_", x)) return("$\\displaystyle\\frac{b_{j}}{a_{j^*}}$")
  
  # xk, sk, ak (optionally ^+ / ^-)
  m <- regexec("^([xsa])(\\d+)(?:\\^(\\+|\\-))?$", x, perl = TRUE)
  mt <- regmatches(x, m)[[1]]
  if (length(mt)) {
    sym <- mt[2]; idx <- mt[3]; sign <- mt[4]
    exp <- if (!is.na(sign) && nzchar(sign)) paste0("^{", sign, "}") else ""
    return(sprintf("$%s_{%s}%s$", sym, idx, exp))
  }
  x
}

# --- REPLACE your .simplex_rowlabel_tex with this ---
.simplex_rowlabel_tex <- function(rn) {
  # Case A: coefficient is k*M (e.g., -M a2, 2M x1^+)
  mM  <- regexec("^\\s*([+-]?(?:\\d+(?:\\.\\d+)?)?M)\\s+([A-Za-z]+\\d+(?:\\^\\+|\\^-)?)\\s*$", rn, perl = TRUE)
  mtM <- regmatches(rn, mM)[[1]]
  if (length(mtM) == 3) {
    coef <- mtM[2]; var <- mtM[3]
    var_tex <- sub("^\\$(.*)\\$$", "\\1", .simplex_label_tex(var))
    return(sprintf("$%s\\, %s$", coef, var_tex))
  }
  # Case B: numeric coefficient (e.g., -3 x2, 5 x1^-)
  m  <- regexec("^\\s*([+-]?[0-9]*\\.?[0-9]+)\\s+([A-Za-z]+\\d+(?:\\^\\+|\\^-)?)\\s*$", rn, perl = TRUE)
  mt <- regmatches(rn, m)[[1]]
  if (length(mt) == 3) {
    coef <- mt[2]; var <- mt[3]
    var_tex <- sub("^\\$(.*)\\$$", "\\1", .simplex_label_tex(var))
    return(sprintf("$%s\\, %s$", coef, var_tex))
  }
  tok <- .simplex_label_tex(rn)
  if (grepl("^\\$.*\\$$", tok)) tok else paste0("$", tok, "$")
}


# ---------- Tie-breakers (Bland for entering) ----------
.bland_enter_from_vals <- function(cands, vals, maximize = TRUE, eps = 1e-9, order = cands){
  if (!length(cands)) return(NULL)
  v <- suppressWarnings(as.numeric(vals))
  finite <- which(is.finite(v))
  if (!length(finite)) return(NULL)
  cands_f <- cands[finite]; vf <- v[finite]
  if (isTRUE(maximize)) {
    best <- min(vf); keep <- which(abs(vf - best) <= eps)
  } else {
    best <- max(vf); keep <- which(abs(vf - best) <= eps)
  }
  ties <- cands_f[keep]
  ord_idx <- match(ties, order)
  ties[order(ord_idx, na.last = NA)][1]
}



.choose_leave_index <- function(T, ratios, rows_cons, tol = 1e-12){
  # candidates with nonnegative ratio (allow tiny negatives)
  pos <- which(is.finite(ratios) & ratios >= -tol)
  if (!length(pos)) return(integer(0))
  rmin <- min(ratios[pos], na.rm = TRUE)
  ties <- pos[ abs(ratios[pos] - rmin) <= tol ]
  if (length(ties) == 1) return(ties)
  
  # basic variable per constraint row
  bvars  <- .simplex_basis_vars(T)[rows_cons]
  # column position of each tied row's basic variable
  colpos <- match(bvars[ rows_cons[ties] ], colnames(T))
  
  # Fallback: if any NA (no clean basic var found), keep first in physical order
  if (any(is.na(colpos))) return(ties[1])
  
  ties[ which.min(colpos) ]
}

.resolve_leave_row <- function(T0, leave_row_override, rows_cons){
  if (is.null(leave_row_override)) return(NULL)
  # Direct row name?
  if (leave_row_override %in% rows_cons) return(leave_row_override)
  # Accept "coef var" or just a var token; extract trailing token like x2/s1/a1
  token <- sub(".*?([A-Za-z]+\\d+(?:\\^\\+|\\^-)?)\\s*$", "\\1", leave_row_override)
  bvars <- .simplex_basis_vars(T0)[rows_cons]
  hit <- which(bvars == token)
  if (length(hit)) return(rows_cons[hit[1]])
  stop(sprintf("leave_row_override '%s' didn't match any constraint row.", leave_row_override))
}

.resolve_enter_leave <- function(T0, cap, enter_hint = "auto", leave_hint = NULL, eps = 1e-9){
  cn <- colnames(T0)
  ratio_cols <- grep("^ratio_", cn, value = TRUE)
  var_cols <- setdiff(cn, c("b", ratio_cols, cap$bigM$a_cols))
  comp <- .simplex_recompute_Z_rows(T0)
  zjcj <- comp$ZmC
  
  # ENTER: user hint or near-zero reduced costs (alternate-opt face), Bland tie-break
  if (!identical(enter_hint, "auto")) {
    enter <- enter_hint
  } else {
    near0 <- var_cols[ is.finite(zjcj[var_cols]) & abs(zjcj[var_cols]) <= eps ]
    pool  <- if (length(near0)) near0 else var_cols
    enter <- .bland_enter_from_vals(pool, zjcj[pool],
                                    maximize = isTRUE(cap$bigM$maximize),
                                    eps = eps, order = cn)
  }
  
  # Ratios for that entering column
  rows_cons <- setdiff(rownames(T0), c("Cj","Zj","Zj-Cj","z"))
  a <- suppressWarnings(as.numeric(T0[rows_cons, enter]))
  b <- suppressWarnings(as.numeric(T0[rows_cons, "b"]))
  valid <- is.finite(a) & (a > 1e-12) & is.finite(b)
  ratios <- setNames(rep(NA_real_, length(rows_cons)), rows_cons)
  ratios[valid] <- b[valid] / a[valid]
  
  # LEAVE: user hint or our tie-breaker
  if (!is.null(leave_hint)) {
    leave <- .resolve_leave_row(T0, leave_hint, rows_cons)
  } else {
    idx <- .choose_leave_index(T0, ratios, rows_cons, tol = 1e-12)
    if (!length(idx)) stop("No nonnegative ratio -> cannot pivot (post-opt).")
    leave <- rows_cons[idx]
  }
  
  list(enter = enter, leave = leave, ratios = ratios)
}


.expand_unrestricted <- function(A, c, unrestricted = integer(0), base = "x") {
  stopifnot(is.matrix(A), length(c) == ncol(A))
  n <- ncol(A)
  
  # Names we'll start from: use existing colnames(A) if present, else x1..xn
  orig_names <- if (!is.null(colnames(A)) && length(colnames(A)) == n) {
    colnames(A)
  } else {
    paste0(base, seq_len(n))
  }
  
  # Resolve unrestricted -> column indices
  if (is.character(unrestricted)) {
    idx_urv <- match(unrestricted, orig_names)
  } else {
    idx_urv <- as.integer(unrestricted)
  }
  idx_urv <- sort(unique(idx_urv[!is.na(idx_urv) & idx_urv >= 1 & idx_urv <= n]))
  
  if (!length(idx_urv)) {
    return(list(A = A, c = c, x_names = orig_names))
  }
  
  # Build expanded A, c, and names (keep order; split j into j^+, j^-)
  A_new <- NULL
  c_new <- numeric(0)
  names_new <- character(0)
  
  for (j in seq_len(n)) {
    col_j <- A[, j, drop = FALSE]
    if (j %in% idx_urv) {
      A_new <- cbind(A_new, col_j, -col_j)
      c_new <- c(c_new, c[j], -c[j])
      base_name <- orig_names[j]
      names_new <- c(names_new, paste0(base_name, "^+"), paste0(base_name, "^-"))
    } else {
      A_new <- cbind(A_new, col_j)
      c_new <- c(c_new, c[j])
      names_new <- c(names_new, orig_names[j])
    }
  }
  
  colnames(A_new) <- names_new
  list(A = A_new, c = c_new, x_names = names_new)
}


wrap_with_mathjax <- function(html, title = "Simplex Tableau") {
  paste0(
    "<!doctype html><html><head><meta charset='utf-8'>",
    "<meta name='viewport' content='width=device-width, initial-scale=1'>",
    "<title>", title, "</title>",
    "<script>",
    "window.MathJax = {",
    "  tex: { inlineMath: [['$', '$'], ['\\\\(', '\\\\)']],",
    "         displayMath: [['$$','$$'], ['\\\\[','\\\\]']],",
    "         processEscapes: true },",
    "  options: { skipHtmlTags: ['script','noscript','style','textarea','pre','code'] }",
    "};</script>",
    "<script id='MathJax-script' async ",
    "src='https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js'></script>",
    "</head><body>", html, "</body></html>"
  )
}

# ---------- M-formatting (keeps fractional multipliers) ----------
.fmtMcoef <- function(a, digits = 2, tol = 1e-9, M_label = "M"){
  if (is.na(a) || abs(a) <= tol) return("")              # 0·M -> empty
  if (abs(a - 1) <= tol)  return(M_label)
  if (abs(a + 1) <= tol)  return(paste0("-", M_label))
  s <- sub("\\.?0+$","", formatC(a, format="f", digits=digits))
  paste0(s, M_label)
}

.bigM_format_term <- function(m, c, digits = 2, math = FALSE, M_label = "M"){
  tol <- 1e-9
  m0 <- is.na(m) || abs(m) < tol
  c0 <- is.na(c) || abs(c) < tol
  s  <- ""
  if (!m0) s <- .fmtMcoef(m, digits, M_label = M_label)
  if (!c0){
    c_str <- .simplex_fmt(c, digits = digits)
    if (nchar(s) == 0) s <- c_str
    else if (c >= 0)   s <- paste0(s, " + ", c_str)
    else               s <- paste0(s, " - ", .simplex_fmt(abs(c), digits = digits))
  }
  if (nchar(s) == 0) s <- "0"
  if (isTRUE(math)) s <- paste0("\\(", s, "\\)")
  s
}

.bigM_fmt_expr <- function(alpha, beta, digits = 2, M_label = "M"){
  tol <- 1e-9
  a0 <- is.na(alpha) || abs(alpha) <= tol
  b0 <- is.na(beta)  || abs(beta)  <= tol
  if (a0 && !b0) {
    if (abs(beta - round(beta)) <= tol) return(as.character(as.integer(round(beta))))
    return(formatC(beta, format="f", digits=digits))
  }
  if (!a0 && b0) return(.fmtMcoef(alpha, digits, M_label = M_label))
  mpart <- .fmtMcoef(alpha, digits, M_label = M_label)
  signb <- if (beta >= 0) " + " else " - "
  babs  <- abs(beta)
  btxt  <- if (abs(babs - round(babs)) <= tol) as.character(as.integer(round(babs)))
  else formatC(babs, format="f", digits=digits)
  paste0(mpart, signb, btxt)
}

# ---------- Basis helpers ----------
.simplex_basis_vars <- function(T){
  rn <- rownames(T); cn <- colnames(T)
  aux <- intersect(c("Cj","Zj","Zj-Cj","z"), rn)
  rows_cons <- setdiff(rn, aux)
  ratio_cols <- grep("^ratio_", cn, value = TRUE)
  var_cols <- setdiff(cn, c("b", ratio_cols))
  if (!length(rows_cons) || !length(var_cols))
    return(setNames(rep(NA_character_, length(rows_cons)), rows_cons))
  M <- suppressWarnings(as.matrix(T[rows_cons, var_cols, drop = FALSE]))
  out <- setNames(rep(NA_character_, length(rows_cons)), rows_cons)
  for (i in seq_along(rows_cons)) {
    col_is_I <- sapply(seq_along(var_cols), function(j){
      col <- suppressWarnings(as.numeric(M[, j])); if (any(!is.finite(col))) return(FALSE)
      abs(col[i] - 1) < 1e-9 && sum(abs(col[-i]) < 1e-9) == (length(col) - 1)
    })
    if (any(col_is_I)) out[i] <- var_cols[which(col_is_I)[1]]
  }
  out
}

.simplex_basis_costs <- function(T){
  rn <- rownames(T); cn <- colnames(T)
  aux <- intersect(c("Cj","Zj","Zj-Cj","z"), rn)
  rows_cons <- setdiff(rn, aux)
  cb <- rep(NA_real_, length(rows_cons)); names(cb) <- rows_cons
  m  <- regexec("^\\s*([+-]?[0-9]*\\.?[0-9]+)\\s+([A-Za-z]+\\d+(?:\\^\\+|\\^-)?)\\s*$", rows_cons)
  mt <- regmatches(rows_cons, m)
  if (length(mt)) for (k in seq_along(rows_cons)) if (length(mt[[k]]) == 3)
    cb[k] <- suppressWarnings(as.numeric(mt[[k]][2]))
  need <- which(!is.finite(cb))
  if (length(need)) {
    if (!"Cj" %in% rn) stop("Tableau missing 'Cj' row to retrieve costs.")
    ratio_cols <- grep("^ratio_", cn, value = TRUE)
    var_cols <- setdiff(cn, c("b", ratio_cols))
    M <- suppressWarnings(as.matrix(T[rows_cons, var_cols, drop = FALSE]))
    for (idx in need) {
      i <- which(rows_cons == rows_cons[idx])
      col_is_basis <- sapply(seq_along(var_cols), function(j){
        col <- suppressWarnings(as.numeric(M[, j])); if (any(!is.finite(col))) return(FALSE)
        abs(col[i] - 1) < 1e-9 && sum(abs(col[-i]) < 1e-9) == (length(col) - 1)
      })
      if (any(col_is_basis)) {
        j <- which(col_is_basis)[1]
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

# --- REPLACE your bigM_symbolic_annotate with this ---
bigM_symbolic_annotate <- function(Tnum, cap, digits = 2, M_label = "M"){
  if (is.null(cap$bigM)) return(Tnum)
  meta <- cap$bigM
  a_cols   <- meta$a_cols
  maximize <- isTRUE(meta$maximize)
  signM <- if (maximize) -1 else +1
  
  Tdisp <- Tnum
  
  rn <- rownames(Tnum); cn <- colnames(Tnum)
  rows_cons  <- setdiff(rn, intersect(c("Cj","Zj","Zj-Cj","z"), rn))
  ratio_cols <- grep("^ratio_", cn, value = TRUE)
  var_cols   <- setdiff(cn, c("b", ratio_cols))
  
  # basis info
  basis_vars <- .simplex_basis_vars(Tnum)
  is_art_row <- (basis_vars[rows_cons] %in% a_cols)
  
  # numeric c_B for non-art rows (finite only)
  cb <- .simplex_basis_costs(Tnum)
  
  # ---------- α (M part) & β (numeric part) ----------
  alpha <- setNames(rep(0, length(cn)), cn)
  beta  <- setNames(rep(NA_real_, length(cn)), cn)
  
  # variable columns (x,s,a)
  for (j in var_cols) {
    aij <- suppressWarnings(as.numeric(Tnum[rows_cons, j]))
    alpha[j] <- signM * sum(aij[is_art_row], na.rm = TRUE)
    keep <- (!is_art_row) & is.finite(cb)
    beta[j]  <- sum(cb[keep] * aij[keep], na.rm = TRUE)
  }
  
  # RHS (b) column — symbolic too
  if ("b" %in% cn) {
    bi <- suppressWarnings(as.numeric(Tnum[rows_cons, "b"]))
    alpha["b"] <- signM * sum(bi[is_art_row], na.rm = TRUE)
    keep <- (!is_art_row) & is.finite(cb)
    beta["b"]  <- sum(cb[keep] * bi[keep], na.rm = TRUE)
  }
  
  # Cj split for Zj - Cj
  Cj <- cap$bigM$Cj
  CjM <- setNames(rep(0, length(var_cols)), var_cols)
  CjC <- setNames(rep(0, length(var_cols)), var_cols)
  for (j in var_cols) {
    if (j %in% a_cols) { CjM[j] <- signM; CjC[j] <- 0 }
    else {
      CjM[j] <- 0
      cc <- suppressWarnings(as.numeric(Cj[j])); if (!is.finite(cc)) cc <- 0
      CjC[j] <- cc
    }
  }
  
  # Cosmetic: show ±M in Cj over artificial columns
  if ("Cj" %in% rn && length(a_cols)) {
    for (ac in intersect(a_cols, cn)) {
      Tdisp["Cj", ac] <- if (signM < 0) paste0("-", M_label) else M_label
    }
  }
  
  # ---------- Zj (symbolic, incl. b) ----------
  if ("Zj" %in% rn) {
    for (j in setdiff(cn, ratio_cols)) {
      Tdisp["Zj", j] <- .bigM_fmt_expr(alpha[j], beta[j], digits, M_label)
    }
  }
  
  # ---------- Zj - Cj (symbolic; for b it's the same as Zj[b]) ----------
  if ("Zj-Cj" %in% rn) {
    for (j in var_cols) {
      mcoef <- alpha[j] - CjM[j]
      ccoef <- beta[j]  - CjC[j]
      Tdisp["Zj-Cj", j] <- .bigM_format_term(mcoef, ccoef, digits = digits, math = FALSE, M_label = M_label)
    }
    if ("b" %in% cn) {
      Tdisp["Zj-Cj","b"] <- .bigM_fmt_expr(alpha["b"], beta["b"], digits, M_label)
    }
  }
  
  # ---------- NEW: rename constraint rows to "cB var" (show ±M for artificials) ----------
  # ---------- rename constraint rows to "cB var" (but don't overwrite if already labeled) ----------
  # ---------- rename constraint rows to "cB var" (but don't overwrite if already labeled) ----------
  rn2 <- rownames(Tdisp)
  
  # looks like "<coef> <var>" e.g. "5 x2", "-M a1", "2M a2"
  coef_var_re <- "^\\s*(?:[+-]?(?:\\d+(?:\\.\\d+)?)?M|[+-]?[0-9]*\\.?[0-9]+)\\s+[A-Za-z]+\\d+\\s*$"
  
  for (r in rows_cons) {
    # r IS the current row label string; check it directly
    if (grepl(coef_var_re, r, perl = TRUE)) next
    
    bvar <- .simplex_basis_vars(Tnum)[[r]]
    if (is.na(bvar)) next
    
    # cost label (±M for artificials, numeric for others)
    if (bvar %in% cap$bigM$a_cols) {
      coef_lab <- if (isTRUE(cap$bigM$maximize)) "-M" else "M"
    } else {
      cj <- suppressWarnings(as.numeric(cap$bigM$Cj[bvar])); if (!is.finite(cj)) cj <- 0
      coef_lab <- if (abs(cj - round(cj)) < 1e-9) as.character(as.integer(round(cj)))
      else formatC(cj, format = "f", digits = digits)
    }
    
    # rename the matching position (defensive even if duplicates)
    idx <- which(rn2 == r)
    if (length(idx)) rn2[idx[1]] <- paste(coef_lab, bvar)
  }
  rownames(Tdisp) <- rn2
  
  
  
  Tdisp
}

# Decide sign of a value that may be an "a M + b" expression.
.bigM_sign <- function(x){
  if (is.na(x)) return(NA_integer_)
  # fast path: plain numeric
  nx <- suppressWarnings(as.numeric(x))
  if (is.finite(nx)) return(sign(nx))
  # robust parse of strings like " -2.5M + 0.75 ", "\(-M\)", "M - 3", "0M + 1.2", etc.
  s <- as.character(x)
  s <- gsub("\\s+", "", s)                 # remove spaces
  s <- gsub("\\\\|[()$]", "", s)           # strip MathJax wrappers if any
  
  # find first M-term coefficient (empty -> 1; "+" -> 1; "-" -> -1)
  m <- regexpr("([+-]?\\d*\\.?\\d*)M", s, perl = TRUE)
  if (m[1] > 0) {
    coef <- sub(".*?([+-]?\\d*\\.?\\d*)M.*", "\\1", s, perl = TRUE)
    a <- if (coef == "" || coef == "+") 1 else if (coef == "-") -1 else suppressWarnings(as.numeric(coef))
    if (is.finite(a) && abs(a) > 1e-12) return(ifelse(a > 0, 1L, -1L))
    # α == 0 → use β (constant part)
    s_const <- gsub("([+-]?\\d*\\.?\\d*)M", "", s, perl = TRUE)
    # if nothing left, it's 0
    if (s_const == "" || s_const == "+" || s_const == "-") return(0L)
    b <- suppressWarnings(as.numeric(eval(parse(text = s_const))))
    return(if (is.finite(b)) sign(b) else NA_integer_)
  } else {
    # no M in string: try evaluating +/− expression (e.g., "1.5-0.75")
    v <- suppressWarnings(as.numeric(eval(parse(text = s))))
    return(if (is.finite(v)) sign(v) else NA_integer_)
  }
}

simplex_html_step_eliminate_other_rows_manual <- function(
    cap, digits = 2,
    ratio_enter = "auto",
    choose_enter_if_masked = TRUE,
    leave_row_override = NULL,
    enter_col_style = c("frame","tint","none"),
    highlight_ratio_col = TRUE,
    highlight_pivot = TRUE,
    color_sign = TRUE,
    color_sign_rows = c("Zj","Zj-Cj"),
    highlight_base_row = TRUE,
    highlight_rows_other = "others",
    mask_rows = c("Zj","Zj-Cj"),
    mask_ratio = TRUE,
    mark_enter_value = TRUE,
    mark_leave_value = FALSE,
    math_labels = TRUE,
    mathjax_wrap = FALSE,
    title = "Simplex – eliminate other rows",
    symbolic_M = TRUE, M_label = "M"
){
  enter_col_style <- match.arg(enter_col_style)
  stopifnot(is.list(cap), !is.null(cap$initial))
  
  T0 <- cap$initial
  T0_orig <- T0
  
  # --- ensure a ratio column exists & placed next to b
  rcols <- grep("^ratio_", colnames(T0), value = TRUE)
  if (!length(rcols)) { T0[["ratio_oran"]] <- NA_real_; rcols <- "ratio_oran" }
  if (length(rcols) > 1L) {
    keep <- rcols[1]
    T0 <- T0[, c(setdiff(colnames(T0), rcols), keep), drop = FALSE]
    rcols <- keep
  }
  if (length(rcols)) {
    cols <- colnames(T0); pos_b <- match("b", cols)
    cols <- append(setdiff(cols, rcols), rcols, after = pos_b)
    T0 <- T0[, cols, drop = FALSE]
  }
  ratio_colname <- rcols[1]
  
  # --- choose entering column
  enter_col <- NULL
  zj_visible <- "Zj-Cj" %in% rownames(T0) && !("Zj-Cj" %in% mask_rows)
  if (!is.null(ratio_enter) && !identical(ratio_enter, "auto")) {
    if (ratio_enter %in% colnames(T0)) enter_col <- ratio_enter
  } else {
    src <- if (zj_visible) T0 else if (isTRUE(choose_enter_if_masked)) T0_orig else NULL
    cand <- setdiff(colnames(T0), c("b", ratio_colname, cap$bigM$a_cols))
    if (!is.null(src) && "Zj-Cj" %in% rownames(src) && length(cand)) {
      rc <- suppressWarnings(as.numeric(src["Zj-Cj", cand, drop = TRUE]))
      enter_col <- .bland_enter_from_vals(
        cand, rc,
        maximize = isTRUE(cap$bigM$maximize),
        eps      = 1e-9,
        order    = colnames(T0)
      )
    }
  }
  if (is.null(enter_col)) stop("Could not determine entering column for elimination panel.")
  
  # --- ratios & leaving row (honor override)
  aux_rows <- c("Cj","Zj","Zj-Cj","z")
  rows_cons <- setdiff(rownames(T0), aux_rows)
  a    <- suppressWarnings(as.numeric(T0[rows_cons, enter_col]))
  bRHS <- suppressWarnings(as.numeric(T0[rows_cons, "b"]))
  valid  <- is.finite(a) & (a > 1e-12) & is.finite(bRHS)
  ratios <- rep(NA_real_, length(rows_cons)); names(ratios) <- rows_cons
  ratios[valid] <- bRHS[valid] / a[valid]
  
  if (!is.null(leave_row_override)) {
    leave_row <- .resolve_leave_row(T0, leave_row_override, rows_cons)
  } else {
    pos <- which(valid & (ratios >= -1e-12))
    if (!length(pos)) stop("No nonnegative ratio -> cannot pivot.")
    idx_leave <- .choose_leave_index(T0, ratios, rows_cons, tol = 1e-12)
    leave_row <- rows_cons[idx_leave]
  }
  
  # --- normalize pivot row, then eliminate other rows
  pv <- suppressWarnings(as.numeric(T0[leave_row, enter_col]))
  if (!is.finite(pv) || abs(pv) < 1e-12) stop("Invalid pivot value.")
  # divide pivot row
  row_vals <- T0[leave_row, , drop = FALSE]
  for (cn in colnames(row_vals)) {
    v <- suppressWarnings(as.numeric(row_vals[1, cn]))
    if (is.finite(v)) row_vals[1, cn] <- v / pv
  }
  T0[leave_row, ] <- row_vals[1, ]
  # eliminate others
  for (r in setdiff(rows_cons, leave_row)) {
    coeff <- suppressWarnings(as.numeric(T0[r, enter_col]))
    if (!is.finite(coeff) || abs(coeff) < 1e-12) next
    for (cn in colnames(T0)) {
      v_r <- suppressWarnings(as.numeric(T0[r, cn]))
      v_p <- suppressWarnings(as.numeric(T0[leave_row, cn]))
      if (is.finite(v_r) && is.finite(v_p)) T0[r, cn] <- v_r - coeff * v_p
    }
  }
  
  # ratio column visibility
  if (isTRUE(mask_ratio)) T0[, ratio_colname] <- NA
  
  # rename leaving row as "cB var"
  cj_val <- suppressWarnings(as.numeric(T0_orig["Cj", enter_col]))
  if (is.finite(cj_val)) {
    lab_cost <- if (abs(cj_val - round(cj_val)) < 1e-9) as.integer(round(cj_val)) else formatC(cj_val, format="f", digits=digits)
    new_name <- sprintf("%s %s", lab_cost, enter_col)
    rn <- rownames(T0); rn[rn == leave_row] <- new_name; rownames(T0) <- rn
    leave_row <- new_name
  }
  
  # which “other rows” to tint orange
  rows_extra <- if (identical(highlight_rows_other, "others"))
    setdiff(rownames(T0), c("Cj","Zj","Zj-Cj", leave_row))
  else intersect(highlight_rows_other, rownames(T0))
  
  # keep indices before symbolic rename
  idx_leave_before <- match(leave_row, rownames(T0))
  idx_extra_before <- match(rows_extra, rownames(T0))
  
  # values to box (optional)
  enter_val <- suppressWarnings(as.numeric(T0_orig["Zj-Cj", enter_col]))
  leave_cell <- if (isTRUE(mark_leave_value) && !isTRUE(mask_ratio))
    list(row = leave_row, col = ratio_colname) else NULL
  
  # symbolic rendering
  T_display <- if (isTRUE(symbolic_M)) bigM_symbolic_annotate(T0, cap, digits=digits, M_label=M_label) else T0
  
  # re-map names after symbolic rename
  leave_row_disp <- leave_row
  if (!is.na(idx_leave_before) && idx_leave_before >= 1 && idx_leave_before <= nrow(T_display))
    leave_row_disp <- rownames(T_display)[idx_leave_before]
  
  rows_extra_disp <- character(0)
  if (length(idx_extra_before)) {
    keep <- which(!is.na(idx_extra_before) & idx_extra_before >= 1 & idx_extra_before <= nrow(T_display))
    if (length(keep)) rows_extra_disp <- rownames(T_display)[ idx_extra_before[keep] ]
  }
  
  # mask Z rows if requested
  if (length(mask_rows)) {
    mr <- intersect(mask_rows, c("Zj","Zj-Cj"))
    if (length(mr)) for (rr in mr) if (rr %in% rownames(T_display)) T_display[rr, ] <- NA
  }
  
  html <- .simplex_df_to_html(
    T_display, head_left = "TDV", digits = digits,
    highlight_col   = enter_col,
    enter_col_style = enter_col_style,
    highlight_row   = if (isTRUE(highlight_base_row)) leave_row_disp else NULL,
    highlight_pivot = isTRUE(highlight_pivot),
    color_sign      = color_sign,
    color_sign_rows = color_sign_rows,
    highlight_cell_enter = if (isTRUE(mark_enter_value)) list(row="Zj-Cj", col=enter_col, value=enter_val, tol=1e-9) else NULL,
    highlight_cell_leave = leave_cell,
    highlight_ratio_col  = if (isTRUE(highlight_ratio_col)) ratio_colname else NULL,
    highlight_rows_extra = rows_extra_disp,
    math_labels = math_labels,
    leave_fallback = isTRUE(mark_leave_value)
  )
  if (isTRUE(mathjax_wrap)) html <- wrap_with_mathjax(html, title = title)
  html
}


# ---------- HTML renderer ----------
.simplex_df_to_html <- function(df, head_left="TDV", digits=2,
                                highlight_col=NULL, highlight_row=NULL, highlight_pivot=FALSE,
                                color_sign=FALSE, color_sign_rows=c("Zj","Zj-Cj"),
                                highlight_cell_enter=NULL, highlight_cell_leave=NULL,
                                highlight_cell_optimum=NULL,
                                highlight_ratio_col=NULL,
                                enter_col_style = c("tint","frame","none"),
                                highlight_rows_extra = character(0),
                                math_labels = FALSE,
                                leave_fallback = TRUE) {
  enter_col_style <- match.arg(enter_col_style)
  stopifnot(is.data.frame(df))
  rn <- rownames(df); cn <- colnames(df)
  
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
  
  ratio_candidates <- grep("^ratio_", cn, value = TRUE)
  if (identical(highlight_ratio_col, "auto")) {
    highlight_ratio_col <- if (length(ratio_candidates)) ratio_candidates[1] else NULL
  }
  ratio_col_hi <- highlight_ratio_col
  
  row_class <- rep("", length(rn))
  row_class[rn == "Cj"]    <- "cj"
  row_class[rn == "Zj"]    <- "zj"
  row_class[rn == "Zj-Cj"] <- "zjm"
  if (!is.null(highlight_row) && highlight_row %in% rn)
    row_class[rn == highlight_row] <- paste(row_class[rn == highlight_row], "leave")
  if (length(highlight_rows_extra)) {
    hits <- intersect(highlight_rows_extra, rn)
    row_class[rn %in% hits] <- paste(row_class[rn %in% hits], "otherhi")
  }
  
  css <- "
  <style>
    table.simplex{border-collapse:collapse;font-family:system-ui,-apple-system,Segoe UI,Roboto,Arial;font-size:13px}
    table.simplex th,table.simplex td{border:1px solid #d0d7de;padding:6px 8px;text-align:right}
    table.simplex thead th{background:#f1f1f1;font-weight:600}
    table.simplex th.rowname{background:#f8f9fb;text-align:left;white-space:nowrap}
    tr.cj  td, tr.cj  th.rowname{background:#eef6ff}
    tr.zj  td, tr.zj  th.rowname{background:#fff3e0}
    tr.zjm td, tr.zjm th.rowname{background:#ffeaea}

    .simplex th.colB, .simplex td.colB { 
  background-color: #FFF3CD;   /* pastel amber — istediğin renkle değiştir */
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
    td.ratiohi, th.ratiohi{border-left:2px solid #0b5ed7;border-right:2px solid #0b5ed7;background:#e7f1ff}

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


# header
thead <- paste0(
  "<thead><tr><th class='rowname'>", if (isTRUE(math_labels)) head_left_disp else .simplex_escape(head_left_disp), "</th>",
  paste(vapply(seq_along(cn), function(j){
    classes <- character(0)
    if (!is.null(highlight_col) && cn[j]==highlight_col)
      classes <- c(classes, if (enter_col_style=="frame") "enterFrame" else if (enter_col_style=="tint") "enterTint")
    if (!is.null(highlight_ratio_col) && cn[j]==highlight_ratio_col) classes <- c(classes, "ratiohi")
    if (cn[j] == "b") classes <- c(classes, "colB")
    cls <- if (length(classes)) paste0(" class='", paste(classes, collapse=" "), "'") else ""
    lab <- if (isTRUE(math_labels)) disp_cols[cn[j]] else .simplex_escape(disp_cols[cn[j]])
    sprintf("<th%s>%s</th>", cls, lab)
  }, ""), collapse = ""),
  "</tr></thead>"
)

# body
want_color <- isTRUE(color_sign)
color_rows <- intersect(color_sign_rows, rn)
row_label_disp <- if (isTRUE(math_labels)) vapply(rn, .simplex_rowlabel_tex, character(1)) else .simplex_escape(rn)

body_rows <- character(length(rn))
for (i in seq_along(rn)) {
  cls_tr <- trimws(row_class[i]); if (nzchar(cls_tr)) cls_tr <- paste0(" class='", cls_tr, "'") else cls_tr <- ""
  tds <- vapply(seq_along(cn), function(j){
    classes <- character(0)
    if (cn[j] == "b") classes <- c(classes, "colB")
    if (!is.null(highlight_col) && cn[j]==highlight_col)
      classes <- c(classes, if (enter_col_style=="frame") "enterFrame" else if (enter_col_style=="tint") "enterTint")
    if (!is.null(highlight_ratio_col) && cn[j]==highlight_ratio_col) classes <- c(classes,"ratiohi")
    if (isTRUE(highlight_pivot) && !is.null(highlight_col) && !is.null(highlight_row) &&
        cn[j]==highlight_col && rn[i]==highlight_row) classes <- c(classes,"pivot")
    
    val <- df[i,j,drop=TRUE]
    if (want_color && rn[i] %in% color_rows) {
      sgn <- .bigM_sign(val)
      if (!is.na(sgn) && sgn != 0) {
        classes <- c(classes, if (sgn > 0) "signpos" else "signneg")
      }
    }
    # inside .simplex_df_to_html(), in the vapply(...) where each <td> is built,
    # replace your current local helper with this version:
    
    is_target <- function(cell){
      if (is.null(cell)) return(FALSE)
      r_ok <- (!is.null(cell$row)) && identical(rn[i], cell$row)
      c_ok <- (!is.null(cell$col)) && identical(cn[j], cell$col)
      if (!(r_ok && c_ok)) return(FALSE)
      
      raw_val <- df[i, j, drop = TRUE]
      if (is.na(raw_val)) return(FALSE)           # NEW: don't box hidden cells
      
      if (is.null(cell$value)) return(TRUE)
      
      tol     <- if (is.null(cell$tol)) 1e-9 else cell$tol
      val_num <- suppressWarnings(as.numeric(raw_val))
      if (is.finite(val_num)) {
        abs(val_num - cell$value) <= tol         # numeric -> check value
      } else {
        TRUE                                     # symbolic (e.g. contains M) -> position only
      }
    }
    
    
    if (is_target(highlight_cell_enter))  classes <- c(classes,"enterVal")
    if (is_target(highlight_cell_leave))  classes <- c(classes,"leaveVal")
    if (is_target(highlight_cell_optimum)) classes <- c(classes,"optVal")
    
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
          if (abs(vv - round(vv)) <= int_tol) sprintf("\\(%s\\)", as.integer(round(vv)))
          else sprintf("\\(%s\\)", formatC(vv, format = "f", digits = digits))
        } else {
          .simplex_escape(as.character(val))
        }
      }
    } else {
      .simplex_fmt(val, digits)
    }
    sprintf("<td%s>%s</td>", cls, cell_txt)
  }, "")
  body_rows[i] <- paste0("<tr", cls_tr, "><th class='rowname'>", row_label_disp[i], "</th>", paste(tds, collapse=""), "</tr>")
}
tbody <- paste0("<tbody>", paste(body_rows, collapse=""), "</tbody>")
paste0(css, "<table class='simplex'>", thead, tbody, "</table>")
}

# ---------- Big-M build + capture ----------
bigM_build_initial <- function(A, b, c, sense, M = 1e6, maximize = TRUE,
                               unrestricted = NULL, var_names = NULL) {
  stopifnot(is.matrix(A), length(b) == nrow(A), length(c) == ncol(A))
  m <- nrow(A); n <- ncol(A)
  sense <- trimws(sense)
  if (length(sense) != m) stop("sense must have length = nrow(A)")
  
  # --- NEW: expand unrestricted vars (if any)
  expd <- .expand_unrestricted(A, c, unrestricted = unrestricted %||% integer(0))
  A2 <- expd$A
  c2 <- expd$c
  x_names_expanded <- expd$x_names
  n <- ncol(A2)
  
  # optional override via var_names (must match expanded width)
  if (!is.null(var_names)) {
    if (length(var_names) != n) stop("var_names must have length ncol(A) after URV expansion.")
    x_names_expanded <- var_names
  }
  
  # normalize RHS >= 0
  A3 <- A2; b3 <- b; s3 <- sense
  for (i in seq_len(m)) {
    if (b3[i] < 0) {
      A3[i, ] <- -A3[i, ]; b3[i] <- -b3[i]
      s3[i] <- if (sense[i] == "<=") ">=" else if (sense[i] == ">=") "<=" else "="
    }
  }
  
  # --- names
  x_cols <- x_names_expanded
  
  # Build S and A columns
  S_names <- character(0); A_names <- character(0)
  S_mat <- matrix(0, m, 0); A_mat <- matrix(0, m, 0)
  
  s_count <- 0; a_count <- 0
  for (i in seq_len(m)) {
    if (s3[i] == "<=") {
      s_count <- s_count + 1; S_names <- c(S_names, paste0("s", s_count))
      col <- rep(0, m); col[i] <- 1; S_mat <- cbind(S_mat, col)
    } else if (s3[i] == ">=") {
      s_count <- s_count + 1; S_names <- c(S_names, paste0("s", s_count))
      colS <- rep(0, m); colS[i] <- -1; S_mat <- cbind(S_mat, colS)
      a_count <- a_count + 1; A_names <- c(A_names, paste0("a", a_count))
      colA <- rep(0, m); colA[i] <- 1; A_mat <- cbind(A_mat, colA)
    } else if (s3[i] == "=") {
      a_count <- a_count + 1; A_names <- c(A_names, paste0("a", a_count))
      colA <- rep(0, m); colA[i] <- 1; A_mat <- cbind(A_mat, colA)
    } else stop("sense must be from {'<=','>=','='}")
  }
  
  T_can <- cbind(A3, if (ncol(S_mat)) S_mat else NULL, if (ncol(A_mat)) A_mat else NULL)
  colnames(T_can) <- c(x_cols, S_names, A_names)
  rownames(T_can) <- paste0("R", seq_len(m))
  T_can <- as.data.frame(T_can, check.names = FALSE, stringsAsFactors = FALSE)
  T_can[["b"]] <- b3
  
  Cj <- setNames(numeric(ncol(T_can)), colnames(T_can))
  Cj[x_cols] <- c2
  if (length(S_names)) Cj[S_names] <- 0
  if (length(A_names)) Cj[A_names] <- if (maximize) -M else +M
  Cj["b"] <- 0
  
  list(tableau = T_can, Cj = Cj, x_cols = x_cols, s_cols = S_names, a_cols = A_names)
}


capture_bigM <- function(A, b, c, sense, M = 1e6, maximize = TRUE, max_iter = 200,
                         unrestricted = NULL, var_names = NULL) {
  
  bm <- bigM_build_initial(A, b, c, sense, M = M, maximize = maximize,
                           unrestricted = unrestricted, var_names = var_names)
  T_cons <- bm$tableau; Cj <- bm$Cj
  x_cols <- bm$x_cols; a_cols <- bm$a_cols
  
  recompute_all <- function(Tc){
    T0 <- rbind(Cj = Cj[colnames(Tc)], Tc)
    .simplex_recompute_Z_rows(T0)$tableau
  }
  
  basis_of_rows <- function(Tc){
    rn <- rownames(Tc); cn <- colnames(Tc)
    ratio_cols <- grep("^ratio_", cn, value = TRUE)
    var_cols <- setdiff(cn, c("b", ratio_cols))
    M <- suppressWarnings(as.matrix(Tc[, var_cols, drop = FALSE]))
    out <- setNames(rep(NA_character_, nrow(Tc)), rn)
    for (i in seq_len(nrow(Tc))) {
      col_is_I <- sapply(seq_along(var_cols), function(j){
        col <- suppressWarnings(as.numeric(M[, j]))
        if (any(!is.finite(col))) return(FALSE)
        abs(col[i] - 1) < 1e-9 && sum(abs(col[-i]) < 1e-9) == (nrow(Tc) - 1)
      })
      if (any(col_is_I)) out[i] <- var_cols[which(col_is_I)[1]]
    }
    out
  }
  
  T_before <- recompute_all(T_cons)
  initial  <- T_before
  
  steps <- list(); iterations <- list(); status <- NA_character_
  
  for (k in seq_len(max_iter)) {
    cn <- colnames(T_before)
    ratio_cols <- grep("^ratio_", cn, value = TRUE)
    cand <- setdiff(cn, c("b", ratio_cols, a_cols))
    zmcr <- suppressWarnings(as.numeric(T_before["Zj-Cj", cand, drop = TRUE]))
    
    if (isTRUE(maximize)) {
      if (all(!is.finite(zmcr) | zmcr >= -1e-9)) { status <- "Optimal (Big-M)"; break }
      enter_col <- .bland_enter_from_vals(cand, zmcr, maximize = TRUE,  eps = 1e-9, order = cn)
    } else {
      if (all(!is.finite(zmcr) | zmcr <= +1e-9)) { status <- "Optimal (Big-M)"; break }
      enter_col <- .bland_enter_from_vals(cand, zmcr, maximize = FALSE, eps = 1e-9, order = cn)
    }
    
    
    
    
    
    rows_cons <- setdiff(rownames(T_before), c("Cj","Zj","Zj-Cj","z"))
    a <- suppressWarnings(as.numeric(T_before[rows_cons, enter_col]))
    bRHS <- suppressWarnings(as.numeric(T_before[rows_cons, "b"]))
    valid <- is.finite(a) & (a > 1e-12) & is.finite(bRHS)
    ratios <- rep(NA_real_, length(rows_cons)); names(ratios) <- rows_cons
    ratios[valid] <- bRHS[valid] / a[valid]
    # new (tolerant + tie-breaker):
    pos <- which(valid & (ratios >= -1e-12))
    if (!length(pos)) { status <- "Unbounded (no nonnegative ratio)"; iterations[[k]] <- T_before; break }
    idx_leave <- .choose_leave_index(T_before, ratios, rows_cons, tol = 1e-12)
    leave_row <- rows_cons[idx_leave]
    
    ratio_colname <- paste0("ratio_", enter_col)
    T_before_with_ratio <- T_before
    T_before_with_ratio[[ratio_colname]] <- NA_real_
    T_before_with_ratio[rows_cons, ratio_colname] <- ratios
    colsBR <- colnames(T_before_with_ratio); pB <- match("b", colsBR)
    colsBR <- append(setdiff(colsBR, ratio_colname), ratio_colname, after = pB)
    T_before_with_ratio <- T_before_with_ratio[, colsBR, drop = FALSE]
    
    T_work <- T_before[rows_cons, setdiff(colnames(T_before), c("Cj","Zj","Zj-Cj","z")), drop = FALSE]
    pv <- suppressWarnings(as.numeric(T_work[leave_row, enter_col]))
    if (!is.finite(pv) || abs(pv) < 1e-12) { status <- "Invalid pivot value"; iterations[[k]] <- T_before; break }
    for (cnm in colnames(T_work)) {
      v <- suppressWarnings(as.numeric(T_work[leave_row, cnm])); if (is.finite(v)) T_work[leave_row, cnm] <- v / pv
    }
    for (r in setdiff(rownames(T_work), leave_row)) {
      coeff <- suppressWarnings(as.numeric(T_work[r, enter_col])); 
      if (!is.finite(coeff) || abs(coeff) < 1e-12) next
      for (cnm in colnames(T_work)) {
        vr <- suppressWarnings(as.numeric(T_work[r, cnm])); 
        vp <- suppressWarnings(as.numeric(T_work[leave_row, cnm]))
        if (is.finite(vr) && is.finite(vp)) T_work[r, cnm] <- vr - coeff * vp
      }
    }
    
    # --- snap pivot column to an exact identity (robust to FP noise) ---
    T_work[leave_row, enter_col] <- 1
    others <- setdiff(rownames(T_work), leave_row)
    if (length(others)) T_work[others, enter_col] <- 0
    
    # entering variable becomes basic by definition — use it for the label
    cj_val <- suppressWarnings(as.numeric(Cj[enter_col]))
    lab_cost <- if (is.finite(cj_val) && abs(cj_val - round(cj_val)) < 1e-9) as.integer(round(cj_val)) else cj_val
    rn <- rownames(T_work); rn[rn == leave_row] <- sprintf("%s %s", lab_cost, enter_col); rownames(T_work) <- rn
    leave_row_disp <- sprintf("%s %s", lab_cost, enter_col)
    
    T_after <- recompute_all(T_work)
    
    steps[[k]] <- list(
      iter = k, before = T_before_with_ratio, after  = T_after,
      pivot_col_index = match(enter_col, colnames(T_before_with_ratio)),
      pivot_row_index = idx_leave,
      pivot_col_name  = enter_col,
      pivot_row_name  = leave_row,
      enter = enter_col, leave = leave_row_disp,
      ratios = ratios, ratio_colname = ratio_colname,
      z_after = suppressWarnings(as.numeric(T_after["Zj","b"])),
      x_after = {
        xvals <- setNames(rep(0, length(x_cols)), x_cols)
        for (xj in x_cols) {
          col <- suppressWarnings(as.numeric(T_after[setdiff(rownames(T_after), c("Cj","Zj","Zj-Cj","z")), xj]))
          rhs <- suppressWarnings(as.numeric(T_after[setdiff(rownames(T_after), c("Cj","Zj","Zj-Cj","z")), "b"]))
          if (sum(abs(col - 1) < 1e-9) == 1 && sum(abs(col) < 1e-9) == (length(col) - 1))
            xvals[xj] <- rhs[which(abs(col - 1) < 1e-9)]
        }
        xvals
      }
    )
    
    iterations[[k]] <- T_after
    T_before <- T_after
  }
  
  if (is.na(status)) status <- "Stopped (max_iter reached)"
  
  last_tab <- if (length(iterations)) iterations[[length(iterations)]] else initial
  if (!is.null(last_tab)) {
    rows_cons_last <- setdiff(rownames(last_tab), c("Cj","Zj","Zj-Cj","z"))
    var_cols <- setdiff(colnames(last_tab), c("b", grep("^ratio_", colnames(last_tab), value = TRUE)))
    Mmat <- suppressWarnings(as.matrix(last_tab[rows_cons_last, var_cols, drop = FALSE]))
    rhs <- suppressWarnings(as.numeric(last_tab[rows_cons_last, "b"]))
    basic_idx <- rep(NA_integer_, length(rows_cons_last))
    for (i in seq_along(rows_cons_last)) {
      col_is_I <- sapply(seq_along(var_cols), function(j){
        col <- suppressWarnings(as.numeric(Mmat[, j])); if (any(!is.finite(col))) return(FALSE)
        abs(col[i] - 1) < 1e-9 && sum(abs(col[-i]) < 1e-9) == (length(col) - 1)
      })
      if (any(col_is_I)) basic_idx[i] <- which(col_is_I)[1]
    }
    basic_vars <- ifelse(is.na(basic_idx), NA_character_, var_cols[basic_idx])
    if (length(a_cols) && any(basic_vars %in% a_cols & rhs > 1e-8))
      status <- "Infeasible (artificial variable positive in basis)"
  }
  
  summ_list <- lapply(steps, function(stp){
    row <- data.frame(iter = stp$iter,
                      pivot_col = stp$pivot_col_index,
                      pivot_row = stp$pivot_row_index,
                      enter = stp$enter, leave = stp$leave,
                      z = stp$z_after, check.names = FALSE)
    if (length(stp$x_after)) {
      xdf <- as.data.frame(as.list(stp$x_after))
      row <- cbind(row, xdf[, order(names(xdf)), drop = FALSE])
    }
    row
  })
  summary <- if (length(summ_list)) do.call(rbind, summ_list) else
    data.frame(iter = integer(0), pivot_col = integer(0), pivot_row = integer(0),
               enter = character(0), leave = character(0), z = numeric(0))
  attr(summary, "status") <- status
  
  bigM_meta <- list(Cj = Cj, a_cols = a_cols, maximize = maximize)
  
  list(
    initial     = initial,
    iterations  = iterations,
    steps       = steps,
    summary     = summary,
    result      = NULL,
    raw_output  = NULL,
    bigM        = bigM_meta
  )
}

# ---------- HTML builders (single symbolic path) ----------
simplex_html_initial <- function(cap, digits = 2,
                                 add_ratio_col   = TRUE,
                                 mask_rows       = character(0),
                                 mask_ratio      = FALSE,
                                 mask_initial    = NULL,
                                 ratio_enter     = NULL,
                                 auto_compute_ratios = FALSE,
                                 choose_enter_if_masked = FALSE,
                                 highlight_col   = NULL,
                                 highlight_row   = NULL,
                                 highlight_pivot = FALSE,
                                 highlight_ratio_col = FALSE,
                                 color_sign      = TRUE,
                                 color_sign_rows = c("Zj","Zj-Cj"),
                                 mark_enter_leave_values = TRUE,
                                 mark_enter_value = TRUE,
                                 mark_leave_value = TRUE,
                                 math_labels = FALSE,
                                 mathjax_wrap = FALSE,
                                 title = "Simplex Tableau",
                                 symbolic_M = FALSE, M_label = "M",
                                 leave_row_override = NULL) {
  stopifnot(is.list(cap), !is.null(cap$initial))
  T0 <- cap$initial; if (!"b" %in% colnames(T0)) stop("Initial tableau missing 'b' column.")
  T0_orig <- T0
  
  if (!is.null(mask_initial) && isTRUE(mask_initial)) {
    if (!length(mask_rows)) mask_rows <- c("Zj","Zj-Cj")
    if (!isTRUE(mask_ratio)) mask_ratio <- TRUE
  }
  
  rcols <- grep("^ratio_", colnames(T0), value = TRUE)
  if (isTRUE(add_ratio_col) && !length(rcols)) { T0[["ratio_oran"]] <- NA_real_; rcols <- "ratio_oran" }
  if (length(rcols) > 1L) { keep <- rcols[1]; T0 <- T0[, c(setdiff(colnames(T0), rcols), keep), drop = FALSE]; rcols <- keep }
  if (length(rcols)) { cols <- colnames(T0); pos_b <- which(cols == "b")[1]; cols <- append(setdiff(cols, rcols), rcols, after = pos_b); T0 <- T0[, cols, drop = FALSE] }
  ratio_colname <- if (length(rcols)) rcols[1] else NULL
  
  mask_rows <- intersect(mask_rows, c("Zj","Zj-Cj"))
  if ("Zj" %in% mask_rows && "Zj" %in% rownames(T0)) T0["Zj", ] <- NA
  if ("Zj-Cj" %in% mask_rows && "Zj-Cj" %in% rownames(T0)) T0["Zj-Cj", ] <- NA
  if (isTRUE(mask_ratio) && !is.null(ratio_colname)) T0[, ratio_colname] <- NA
  
  enter_col <- NULL
  if (!is.null(ratio_enter) && !identical(ratio_enter, "auto")) {
    if (ratio_enter %in% colnames(T0)) enter_col <- ratio_enter
  } else {
    zj_row_visible <- !("Zj-Cj" %in% mask_rows) && "Zj-Cj" %in% rownames(T0)
    cand <- setdiff(colnames(T0), c("b", ratio_colname, cap$bigM$a_cols))
    src  <- if (zj_row_visible) T0 else if (isTRUE(choose_enter_if_masked)) T0_orig else NULL
    if (!is.null(src) && "Zj-Cj" %in% rownames(src) && length(cand)) {
      rc <- suppressWarnings(as.numeric(src["Zj-Cj", cand, drop = TRUE]))
      enter_col <- .bland_enter_from_vals(
        cand, rc,
        maximize = isTRUE(cap$bigM$maximize),
        eps      = 1e-9,
        order    = colnames(T0)
      )
    }
  }
  
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
    pos <- which(valid & (ratios >= -1e-12))
    if (length(pos)) {
      idx <- .choose_leave_index(T0, ratios, rows_constr, tol = 1e-12)
      leave_row_auto   <- rows_constr[idx]
      leaved_value_auto <- ratios[idx]
    }
    if ("Zj-Cj" %in% rownames(T0_orig)) entered_value_auto <- suppressWarnings(as.numeric(T0_orig["Zj-Cj", enter_col]))
  } else if (!is.null(enter_col) && "Zj-Cj" %in% rownames(T0_orig)) {
    entered_value_auto <- suppressWarnings(as.numeric(T0_orig["Zj-Cj", enter_col]))
  }
  
  idx_leave_orig <- if (!is.null(leave_row_auto)) match(leave_row_auto, rownames(T0)) else NA_integer_
  
  # If a manual leave row is provided, resolve and use it (and its ratio value)
  if (!is.null(leave_row_override)) {
    aux_rows <- c("Cj","Zj","Zj-Cj","z")
    rows_constr <- setdiff(rownames(T0), aux_rows)
    
    forced_leave <- .resolve_leave_row(T0, leave_row_override, rows_constr)
    leave_row_auto <- forced_leave                      # override auto choice
    idx_leave_orig <- match(leave_row_auto, rownames(T0))
    
    if (!is.null(ratio_colname) && !isTRUE(mask_ratio)) {
      # ratios should already be computed above when add_ratio_col & auto_compute_ratios are TRUE
      if (exists("ratios")) {
        leaved_value_auto <- ratios[leave_row_auto]
      }
    }
  }
  
  
  
  col_hi <- if (identical(highlight_col, FALSE)) NULL
  else if (identical(highlight_col, "auto")) enter_col
  else highlight_col
  
  row_hi <- if (identical(highlight_row, "auto")) leave_row_auto else highlight_row
  
  ratio_hi <- if (identical(highlight_ratio_col, TRUE)) ratio_colname else if (identical(highlight_ratio_col, "auto")) "auto" else NULL
  
  disable_enter_hilite <- isFALSE(highlight_col)    # NEW
  enter_cell <- if (!disable_enter_hilite &&
                    isTRUE(mark_enter_leave_values) &&
                    isTRUE(mark_enter_value) &&
                    !is.null(enter_col)) {
    list(row="Zj-Cj", col=enter_col, value=entered_value_auto, tol=1e-9)
  } else NULL
  leave_cell <- if (isTRUE(mark_enter_leave_values) && isTRUE(mark_leave_value) && !is.null(leave_row_auto) && !is.null(ratio_colname))
    list(row = leave_row_auto, col = ratio_colname) else NULL
  
  T_display <- if (isTRUE(symbolic_M)) bigM_symbolic_annotate(T0, cap, digits = digits, M_label = M_label) else T0
  
  if (length(mask_rows)) {
    mr <- intersect(mask_rows, c("Zj","Zj-Cj"))
    if (length(mr)) for (rr in mr) if (rr %in% rownames(T_display)) T_display[rr, ] <- NA
  }
  if (isTRUE(mask_ratio) && !is.null(ratio_colname) && ratio_colname %in% colnames(T_display)) {
    T_display[, ratio_colname] <- NA
  }
  
  # ADD THIS REMAP JUST BEFORE RENDERING:
  if (!is.na(idx_leave_orig) && idx_leave_orig >= 1 && idx_leave_orig <= nrow(T_display)) {
    new_leave_name <- rownames(T_display)[idx_leave_orig]
    if (identical(highlight_row, "auto") || !is.null(leave_row_override)) {
      row_hi <- new_leave_name
    }    
    if (!is.null(leave_cell)) leave_cell$row <- new_leave_name
  }
  
  # NEW: keep the purple row band in lockstep with the leave cell
  if (!is.null(leave_cell)) {
    row_hi <- leave_cell$row
  }
  
  tbl <- .simplex_df_to_html(
    T_display, head_left = "TDV", digits = digits,
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
  if (mathjax_wrap) tbl <- wrap_with_mathjax(tbl, title = title)
  tbl
}

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
    title = "Simplex Tableau",
    symbolic_M = FALSE, M_label = "M",
    leave_row_override = NULL){
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
  if ("Zj-Cj" %in% mask_rows && "Zj-Cj" %in% rownames(T0)) T0["Zj-Cj", ] <- NA
  
  enter_col <- NULL
  if (!is.null(ratio_enter) && !identical(ratio_enter, "auto")) {
    if (ratio_enter %in% colnames(T0)) enter_col <- ratio_enter
  } else {
    zj_visible <- !("Zj-Cj" %in% mask_rows) && "Zj-Cj" %in% rownames(T0)
    src <- if (zj_visible) T0 else if (isTRUE(choose_enter_if_masked)) T0_orig else NULL
    cand <- setdiff(colnames(T0), c("b", ratio_colname, cap$bigM$a_cols))
    if (!is.null(src) && "Zj-Cj" %in% rownames(src) && length(cand)) {
      rc <- suppressWarnings(as.numeric(src["Zj-Cj", cand, drop = TRUE]))
      enter_col <- .bland_enter_from_vals(
        cand, rc,
        maximize = isTRUE(cap$bigM$maximize),
        eps      = 1e-9,
        order    = colnames(T0)
      )
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
  ## NEW (consistent with capture_bigM; allow degeneracy)
  ## choose leave-row (manual override if provided)
  if (!is.null(leave_row_override)) {
    leave_row <- .resolve_leave_row(T0, leave_row_override, row_constr)
    idx_leave <- match(leave_row, row_constr)
    if (is.na(idx_leave)) stop("leave_row_override did not resolve to a valid constraint row.")
  } else {
    pos <- which(valid & (ratios >= -1e-12))
    if (!length(pos)) stop("No nonnegative ratio -> unbounded or cannot pivot.")
    idx_leave <- .choose_leave_index(T0, ratios, row_constr, tol = 1e-12)
    leave_row <- row_constr[idx_leave]
  }
  
  
  pivot <- suppressWarnings(as.numeric(T0[leave_row, enter_col])); if (!is.finite(pivot) || abs(pivot) < 1e-12) stop("Invalid pivot value.")
  
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
  
  T_display <- if (isTRUE(symbolic_M)) bigM_symbolic_annotate(T0, cap, digits = digits, M_label = M_label) else T0
  
  if (length(mask_rows)) {
    mr <- intersect(mask_rows, c("Zj","Zj-Cj"))
    if (length(mr)) for (rr in mr) if (rr %in% rownames(T_display)) T_display[rr, ] <- NA
  }
  if (isTRUE(mask_ratio) && ratio_colname %in% colnames(T_display)) {
    T_display[, ratio_colname] <- NA
  }
  
  tbl <- .simplex_df_to_html(
    T_display, head_left = "TDV", digits = digits,
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
  if (mathjax_wrap) tbl <- wrap_with_mathjax(tbl, title = title)
  tbl
}

simplex_html_step_eliminate_other_rows <- function(
    cap, iter, digits = 2,
    enter_col_style = c("frame","tint","none"),
    highlight_ratio_col = TRUE,
    highlight_pivot = TRUE,
    color_sign = TRUE,
    color_sign_rows = c("Zj","Zj-Cj"),
    highlight_base_row = TRUE,
    highlight_rows_other = character(0),
    mask_rows = character(0),
    mask_ratio = TRUE,
    mark_enter_value = TRUE,
    mark_leave_value = TRUE,
    ratio_enter = "auto",
    choose_enter_if_masked = TRUE,
    rename_leaving_row = TRUE,
    math_labels = FALSE,
    mathjax_wrap = FALSE,
    title = sprintf("Simplex – iteration %d (eliminate)", iter),
    symbolic_M = FALSE, M_label = "M"){
  enter_col_style <- match.arg(enter_col_style)
  stopifnot(is.list(cap), length(cap$steps) >= iter)
  stp <- cap$steps[[iter]]
  
  T0 <- if (iter == 1) stp$before else cap$iterations[[iter - 1]]
  T0_orig <- T0
  if (is.null(T0) || !nrow(T0)) stop("No starting tableau for this iteration.")
  
  enter_col <- stp$pivot_col_name
  leave_row <- stp$pivot_row_name
  # >>> NEW: keep the original leaving-row name for ratio lookup later
  orig_leave_row <- stp$pivot_row_name
  
  if (is.na(enter_col) || is.na(leave_row)) stop("Missing pivot information for this iteration.")
  
  rcols <- grep("^ratio_", colnames(T0), value = TRUE)
  if (!length(rcols)) { T0[["ratio_oran"]] <- NA_real_; rcols <- "ratio_oran" }
  if (length(rcols) > 1L) { keep <- rcols[1]; T0 <- T0[, c(setdiff(colnames(T0), rcols), keep), drop = FALSE]; rcols <- keep }
  if (length(rcols)) {
    cols <- colnames(T0); pos_b <- match("b", cols)
    cols <- append(setdiff(cols, rcols), rcols, after = pos_b)
    T0 <- T0[, cols, drop = FALSE]
  }
  ratio_colname <- rcols[1]
  
  rows_cons <- setdiff(rownames(T0), intersect(c("Cj","Zj","Zj-Cj","z"), rownames(T0)))
  if (length(rows_cons) && enter_col %in% colnames(T0) && "b" %in% colnames(T0)) {
    cur_vals <- suppressWarnings(as.numeric(T0[rows_cons, ratio_colname]))
    if (all(is.na(cur_vals))) {
      a    <- suppressWarnings(as.numeric(T0[rows_cons, enter_col]))
      bRHS <- suppressWarnings(as.numeric(T0[rows_cons, "b"]))
      valid  <- is.finite(a) & (a > 1e-12) & is.finite(bRHS)
      ratios <- rep(NA_real_, length(rows_cons)); names(ratios) <- rows_cons
      ratios[valid] <- bRHS[valid] / a[valid]
      T0[, ratio_colname] <- NA_real_; T0[rows_cons, ratio_colname] <- ratios
      cols  <- colnames(T0); pos_b <- match("b", cols)
      cols  <- append(setdiff(cols, ratio_colname), ratio_colname, after = pos_b)
      T0    <- T0[, cols, drop = FALSE]
    }
  }
  
  aux_rows  <- intersect(c("Cj","Zj","Zj-Cj","z"), rownames(T0))
  rows_cons <- setdiff(rownames(T0), aux_rows)
  
  pivot_val <- suppressWarnings(as.numeric(T0[leave_row, enter_col]))
  if (!is.finite(pivot_val) || abs(pivot_val) < 1e-12) stop("Invalid pivot value.")
  row_vals <- T0[leave_row, , drop = FALSE]
  for (cn in colnames(row_vals)) {
    v <- suppressWarnings(as.numeric(row_vals[1, cn])); if (is.finite(v)) row_vals[1, cn] <- v / pivot_val
  }
  T0[leave_row, ] <- row_vals[1, ]
  
  for (r in setdiff(rows_cons, leave_row)) {
    coeff <- suppressWarnings(as.numeric(T0[r, enter_col]))
    if (!is.finite(coeff) || abs(coeff) < 1e-12) next
    for (cn in colnames(T0)) {
      v_r <- suppressWarnings(as.numeric(T0[r, cn])); v_p <- suppressWarnings(as.numeric(T0[leave_row, cn]))
      if (is.finite(v_r) && is.finite(v_p)) T0[r, cn] <- v_r - coeff * v_p
    }
  }
  
  mask_rows <- intersect(mask_rows, c("Zj","Zj-Cj"))
  if ("Zj" %in% mask_rows && "Zj" %in% rownames(T0))     T0["Zj", ] <- NA
  if ("Zj-Cj" %in% mask_rows && "Zj-Cj" %in% rownames(T0)) T0["Zj-Cj", ] <- NA
  if (isTRUE(mask_ratio)) T0[, ratio_colname] <- NA
  
  if (isTRUE(rename_leaving_row)) {
    cj_val <- suppressWarnings(as.numeric(stp$before["Cj", enter_col]))
    if (is.finite(cj_val)) {
      lab_cost <- if (abs(cj_val - round(cj_val)) < 1e-9) as.integer(round(cj_val)) else formatC(cj_val, format = "f", digits = digits)
      new_name <- sprintf("%s %s", lab_cost, enter_col)
      rn <- rownames(T0); rn[rn == leave_row] <- new_name; rownames(T0) <- rn
      leave_row <- new_name
    }
  }
  
  rows_extra <- character(0)
  if (identical(highlight_rows_other, "others")) rows_extra <- setdiff(rownames(T0), c("Cj","Zj","Zj-Cj", leave_row))
  else if (length(highlight_rows_other)) rows_extra <- intersect(highlight_rows_other, rownames(T0))
  
  # >>> NEW: keep positions BEFORE symbolic rename (so we can remap after)
  idx_leave_before <- match(leave_row, rownames(T0))
  idx_extra_before <- match(rows_extra, rownames(T0))
  
  enter_val <- suppressWarnings(as.numeric(stp$before["Zj-Cj", enter_col]))
  # >>> CHANGED: use the ORIGINAL leave-row name to fetch the ratio value
  leave_val <- if (length(stp$ratios)) stp$ratios[match(orig_leave_row, names(stp$ratios))] else NA_real_
  
  T_display <- if (isTRUE(symbolic_M)) bigM_symbolic_annotate(T0, cap, digits = digits, M_label = M_label) else T0
  
  if (length(mask_rows)) {
    mr <- intersect(mask_rows, c("Zj","Zj-Cj"))
    if (length(mr)) for (rr in mr) if (rr %in% rownames(T_display)) T_display[rr, ] <- NA
  }
  if (isTRUE(mask_ratio) && ratio_colname %in% colnames(T_display)) {
    T_display[, ratio_colname] <- NA
  }
  
  # >>> NEW: remap row names AFTER symbolic rename
  leave_row_disp <- leave_row
  if (!is.na(idx_leave_before) && idx_leave_before >= 1 && idx_leave_before <= nrow(T_display)) {
    leave_row_disp <- rownames(T_display)[idx_leave_before]
  }
  rows_extra_disp <- character(0)
  if (length(idx_extra_before)) {
    keep <- which(!is.na(idx_extra_before) & idx_extra_before >= 1 & idx_extra_before <= nrow(T_display))
    if (length(keep)) rows_extra_disp <- rownames(T_display)[ idx_extra_before[keep] ]
  }
  
  # >>> NEW: build leave-cell using the remapped row name
  leave_cell <- if (isTRUE(mark_leave_value) && !isTRUE(mask_ratio))
    list(row = leave_row_disp, col = ratio_colname, value = leave_val, tol = 1e-9) else NULL
  
  html <- .simplex_df_to_html(
    T_display, head_left = "TDV", digits = digits,
    highlight_col   = enter_col,
    enter_col_style = enter_col_style,
    # >>> CHANGED: use remapped leave_row_disp
    highlight_row   = if (isTRUE(highlight_base_row)) leave_row_disp else NULL,
    highlight_pivot = isTRUE(highlight_pivot),
    color_sign      = color_sign,
    color_sign_rows = color_sign_rows,
    highlight_cell_enter = if (isTRUE(mark_enter_value)) list(row="Zj-Cj", col=enter_col, value=enter_val, tol=1e-9) else NULL,
    # >>> CHANGED: use the leave_cell we built above
    highlight_cell_leave = leave_cell,
    highlight_ratio_col  = if (isTRUE(highlight_ratio_col)) ratio_colname else NULL,
    # >>> CHANGED: pass the remapped set of “other” rows
    highlight_rows_extra = rows_extra_disp,
    math_labels = math_labels,
    leave_fallback = isTRUE(mark_leave_value)
  )
  if (isTRUE(mathjax_wrap)) html <- wrap_with_mathjax(html, title = title)
  html
}


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
    highlight_optimum_value = TRUE,
    math_labels = FALSE,
    mathjax_wrap = FALSE,
    title = "Simplex Tableau",
    symbolic_M = FALSE, M_label = "M",
    leave_row_override = NULL ){
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
    cand <- setdiff(colnames(T0), c("b", ratio_colname, cap$bigM$a_cols))
    if (!is.null(src) && "Zj-Cj" %in% rownames(src) && length(cand)) {
      rc <- suppressWarnings(as.numeric(src["Zj-Cj", cand, drop = TRUE]))
      enter_col <- .bland_enter_from_vals(
        cand, rc,
        maximize = isTRUE(cap$bigM$maximize),
        eps      = 1e-9,
        order    = colnames(T0)
      )
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
  ## NEW (consistent with capture_bigM; allow degeneracy)
  ## choose leave-row (manual override if provided)
  if (!is.null(leave_row_override)) {
    leave_row <- .resolve_leave_row(T0, leave_row_override, rows_cons)
    idx_leave <- match(leave_row, rows_cons)
    if (is.na(idx_leave)) stop("leave_row_override did not resolve to a valid constraint row.")
  } else {
    pos <- which(valid & (ratios >= -1e-12))
    if (!length(pos)) stop("No nonnegative ratio -> unbounded or cannot pivot.")
    idx_leave <- .choose_leave_index(T0, ratios, rows_cons, tol = 1e-12)
    leave_row <- rows_cons[idx_leave]
  }
  
  pivot_val <- suppressWarnings(as.numeric(T0[leave_row, enter_col])); if (!is.finite(pivot_val) || abs(pivot_val) < 1e-12) stop("Invalid pivot value.")
  
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
    if ("Zj-Cj" %in% mask_rows && "Zj-Cj" %in% rownames(T1)) T1["Zj-Cj", ] <- NA
  }
  
  var_cols <- setdiff(names(zmc), c("b"))
  if (isTRUE(cap$bigM$maximize)) {
    optimal <- all(is.na(zmc[var_cols]) | (zmc[var_cols] >= -1e-9))  # max: all >= 0
  } else {
    optimal <- all(is.na(zmc[var_cols]) | (zmc[var_cols] <= +1e-9))  # min: all <= 0
  }
  
  enter_val <- suppressWarnings(as.numeric(T0_orig["Zj-Cj", enter_col]))
  
  opt_cell <- NULL
  if (isTRUE(highlight_optimum_value) && "Zj" %in% rownames(T1) && "b" %in% colnames(T1)) {
    z_val <- suppressWarnings(as.numeric(T1["Zj","b"]))
    if (isTRUE(symbolic_M)) opt_cell <- list(row = "Zj", col = "b") else
      if (is.finite(z_val)) opt_cell <- list(row = "Zj", col = "b", value = z_val, tol = 1e-9)
  }
  
  T_display <- if (isTRUE(symbolic_M)) bigM_symbolic_annotate(T1, cap, digits = digits, M_label = M_label) else T1
  
  if (length(mask_rows)) {
    mr <- intersect(mask_rows, c("Zj","Zj-Cj"))
    if (length(mr)) for (rr in mr) if (rr %in% rownames(T_display)) T_display[rr, ] <- NA
  }
  if (isTRUE(mask_ratio) && ratio_colname %in% colnames(T_display)) {
    T_display[, ratio_colname] <- NA
  }
  
  html <- .simplex_df_to_html(
    T_display, head_left = "TDV", digits = digits,
    highlight_col   = enter_col,
    enter_col_style = enter_col_style,
    highlight_row   = if (isTRUE(highlight_base_row)) leave_row else NULL,
    highlight_pivot = isTRUE(highlight_pivot),
    color_sign      = color_sign,
    color_sign_rows = c("Zj","Zj-Cj"),
    highlight_cell_enter = if (isTRUE(mark_enter_value)) list(row="Zj-Cj", col=enter_col, value=enter_val, tol=1e-9) else NULL,
    highlight_cell_leave = NULL,
    highlight_cell_optimum = opt_cell,
    highlight_ratio_col  = if (isTRUE(highlight_ratio_col)) ratio_colname else NULL,
    math_labels = math_labels,
    leave_fallback = isTRUE(mark_leave_value)
  )
  if (mathjax_wrap) html <- wrap_with_mathjax(html, title = title)
  list(
    html = html,
    optimal = optimal,
    zj = zj,
    zjcj = zmc,
    tableau = T1     # <-- NEW (numeric tableau for chaining)
  )
}

