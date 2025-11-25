# =========================================================
# two_phase_simplex.R — Two-Phase (Two-Step) Simplex
# =========================================================

`%||%` <- function(a,b) if (is.null(a)) b else a

# ---------- numeric format ----------
.fmt_num <- function(x) {
  # Treat tiny values as zero; round to 3 decimals max
  x[abs(x) < 1e-12] <- 0
  formatC(x, digits = 3, format = "f", flag = "-", drop0trailing = TRUE)
}

# Display column names: rename e# -> s#, and renumber artificials consecutively (a1,a2,...)
.disp_col_names <- function(col_names) {
  out <- gsub("^e([0-9]+)$", "s\\1", col_names, ignore.case = TRUE)
  
  a_pos <- which(grepl("^a[0-9]+$", out, ignore.case = TRUE))
  if (length(a_pos)) {
    out[a_pos] <- paste0("a", seq_along(a_pos))
  }
  out
}


# Display row names (constraints) consecutively as A1, A2, ...
.disp_row_names <- function(row_names) {
  m <- length(row_names)
  # If these are default/generated constraint names (c1, c2, ...)
  if (all(grepl("^c[0-9]+$", row_names, ignore.case = TRUE))) {
    return(paste0("A", seq_len(m)))
  }
  # If they are already A# but may have gaps, also renumber
  if (all(grepl("^A[0-9]+$", row_names, ignore.case = TRUE))) {
    return(paste0("A", seq_len(m)))
  }
  # Otherwise, user-specified meaningful names: keep as is
  row_names
}



# ---------- Big-M-like fallback renderer ----------
.bigM_like_render <- function(tab, basis_idx, col_names, row_names,
                              title = NULL, caption = NULL,
                              pivot = NULL, note = NULL,
                              obj_vec = NULL, ratios = NULL,
                              enter_col = NULL, leave_row = NULL,
                              min_mode = FALSE) {
  m <- nrow(tab) - 1; n <- ncol(tab) - 1
  
  # which pages should show Zj and Zj-Cj values?
  is_choose       <- grepl("\\bchoose\\b", title %||% "", ignore.case = TRUE)
  is_optimal      <- grepl("Optimality reached", title %||% "", ignore.case = TRUE)
  is_phase2_init  <- grepl("Phase II - Initial Tableau", title %||% "", ignore.case = TRUE)
  show_metrics    <- is_choose || is_optimal || is_phase2_init
  
  # --- core objective vectors (original order) ---
  c_full_raw <- if (!is.null(obj_vec)) obj_vec[1:n] else (attr(tab, "c_full") %||% rep(0, n))
  cB_raw     <- attr(tab, "cB") %||% rep(0, m)
  
  is_phase1 <- !is.null(title) && grepl("\\bPhase I\\b", title)
  
  # --- identify types in ORIGINAL order ---
  idx_a  <- grep("^a[0-9]+$", col_names)
  idx_s  <- grep("^s[0-9]+$", col_names)
  idx_e  <- grep("^e[0-9]+$", col_names)
  idx_x  <- setdiff(seq_len(n), c(idx_a, idx_s, idx_e))  # decision variables
  
  # Desired DISPLAY order: X..., S...(=s* and e*), A...
  s_like   <- sort(c(idx_s, idx_e))             # group slack+surplus
  a_like   <- sort(idx_a)
  x_like   <- sort(idx_x)
  order_idx <- c(x_like, s_like, a_like)
  
  # map helpers: original j -> display j
  map_to_disp <- function(j) if (is.null(j) || is.na(j)) NA_integer_ else match(j, order_idx)
  
  # display names: rename e# -> s#
  col_names_disp <- .disp_col_names(col_names[order_idx])
  
  
  # artificials in DISPLAY order (for Phase I cosmetics)
  a_idx_disp <- match(idx_a, order_idx)
  
  # Reorder tableau slices for display
  Aview <- tab[1:m, 1:n, drop = FALSE]
  rhs   <- tab[1:m, n + 1]
  
  Aview_disp   <- Aview[, order_idx, drop = FALSE]
  c_full_disp  <- c_full_raw[order_idx]
  
  # FULL display names for ALL columns (with e->s and a-renumbering)
  col_names_disp_full <- .disp_col_names(col_names)
  
  # Basic variable names taken from that full display map
  basic_names_disp <- col_names_disp_full[basis_idx]
  
  
  # Display cB as-is for computation; TDV shows a display-only version below
  cB_disp <- cB_raw
  
  # TDV cost shown (left cell) — flip sign only for Phase II of minimization (visual)
  cB_disp_show <- cB_disp
  
  # Build a DISPLAY vector for Cj (visual only):
  # - Phase I: handled below for artificials
  # - Phase II: if minimization, flip back to original sign for header only
  cj_disp_show <- c_full_disp
  
  # Phase I display convention: artificials’ Cj shown as ±1, Zj−Cj under a* forced to 0
  if (is_phase1 && length(idx_a)) {
    a_sign <- if (min_mode) 1 else -1  # min => +1, max => -1
    c_full_phase <- c_full_raw
    c_full_phase[idx_a] <- a_sign
    c_full_disp  <- c_full_phase[order_idx]
    cj_disp_show <- c_full_disp
    for (i in seq_len(m)) if (basis_idx[i] %in% idx_a) cB_disp[i] <- a_sign
  }
  
  # ---------- CORRECT Zj and (Zj-Cj) from tableau ----------
  rc_internal <- tab[m + 1, 1:n]             # (Zj - Cj) in ORIGINAL order (what solver uses)
  z0_disp     <- tab[m + 1, n + 1]           # Z0 from tableau
  c_internal  <- c_full_raw
  zj_internal <- rc_internal + c_internal
  zj_disp     <- zj_internal[order_idx]
  zjmcj_disp  <- rc_internal[order_idx]
  # Only force RC=0 for artificials that are currently BASIC
  if (is_phase1 && length(idx_a)) {
    a_basic_orig <- intersect(idx_a, basis_idx)     # artificials in basis (original indices)
    a_basic_disp <- match(a_basic_orig, order_idx)  # their display positions
    zjmcj_disp[a_basic_disp] <- 0
  }
  
  # Map highlights to DISPLAY indices
  enter_col_disp <- map_to_disp(enter_col)
  pivot_disp <- if (!is.null(pivot) && length(pivot) == 2 && !any(is.na(pivot))) {
    c(pivot[1], map_to_disp(pivot[2]))
  } else NULL
  
  # Row labels legacy rename if you ever want constraint names (not used anymore for TDV)
  row_names_disp <- .disp_row_names(row_names)
  
  # NA-safe highlight predicates (in DISPLAY space)
  is_enter <- function(j) !is.null(enter_col_disp) && !is.na(enter_col_disp) && j == enter_col_disp
  is_leave <- function(i) !is.null(leave_row) && !is.na(leave_row) && i == leave_row
  
  # --- styles ---
  css <- "<style>
  .sx{
    font-family:system-ui,-apple-system,Segoe UI,Roboto,Arial,sans-serif;
    font-size:1em;   /* or 1.3em / 1.4em */
    line-height:1.25
  }  .sx .t{border-collapse:collapse;margin:4px 0 14px}
  .sx th,.sx td{border:1px solid #d0d0d0;padding:6px 8px}
  .sx thead th{background:#eef3f8}
  .sx .title{font-weight:700;margin:6px 0 4px}
  .sx .cap{font-style:italic;margin:2px 0 10px; font-size: 1em;}
  .sx .tdv{background:#efeafe;font-weight:600}
  .sx .bcol{background:#fff7d6}
  .sx .ratiocol{background:#fff1bf}
  .sx .cjrow th,.sx .cjrow td{background:#f7f7ff;font-weight:600}
  .sx .zjrow th,.sx .zjrow td{background:#eefaf1}
  .sx .diffrow th,.sx .diffrow td{background:#fff3f0}
  .sx .neg{color:#c0392b;font-weight:700}
  .sx .pos{color:#1e8449;font-weight:700}
  /* highlight borders */
  .sx .pivot{outline:3px solid #3b82f6}
  .sx .entercol{background:rgba(0,128,0,0.08);border-left:3px solid #1e8449;border-right:3px solid #1e8449}
  .sx .leaverow>td, .sx .leaverow>th{background:rgba(255,165,0,0.12);border-top:3px solid #e67e22;border-bottom:3px solid #e67e22}
  .sx .note{margin-top:6px;color:#444}
  </style>"
  
  html <- c("<div class='sx'>")
  if (!is.null(title))   html <- c(html, sprintf("<div class='title'>%s</div>", title))
  if (!is.null(caption)) html <- c(html, sprintf("<div class='cap'>%s</div>", caption))
  
  # --- header ---
  html <- c(html, "<table class='t'><thead><tr><th class='tdv'>TDV</th>")
  for (j in seq_len(n)) {
    cls <- if (is_enter(j)) " class='entercol'" else ""
    html <- c(html, sprintf("<th%s>%s</th>", cls, col_names_disp[j]))
  }
  html <- c(html, "<th class='bcol'>b</th><th class='ratiocol'>b/a<sub>j*</sub></th></tr></thead><tbody>")
  
  # --- Cj row (DISPLAY order) ---
  html <- c(html, "<tr class='cjrow'><th>C<sub>j</sub></th>")
  for (j in seq_len(n)) {
    cls <- if (is_enter(j)) " class='entercol'" else ""
    html <- c(html, sprintf("<td%s>%s</td>", cls, .fmt_num(cj_disp_show[j])))
  }
  html <- c(html, "<td class='bcol'>0</td><td class='ratiocol'></td></tr>")
  
  # --- constraint rows (TDV shows cost + basic var name) ---
  for (i in seq_len(m)) {
    row_cls <- if (is_leave(i)) " class='leaverow'" else ""
    tdv <- sprintf("%s&nbsp;%s", .fmt_num(cB_disp_show[i]), basic_names_disp[i])
    html <- c(html, sprintf("<tr%s><th class='tdv'>%s</th>", row_cls, tdv))
    for (j in seq_len(n)) {
      cls <- character()
      if (is_enter(j)) cls <- c(cls, "entercol")
      if (!is.null(pivot_disp) && pivot_disp[1]==i && pivot_disp[2]==j) cls <- c(cls, "pivot")
      attr <- if (length(cls)) sprintf(" class='%s'", paste(cls, collapse=" ")) else ""
      html <- c(html, sprintf("<td%s>%s</td>", attr, .fmt_num(Aview_disp[i, j])))
    }
    # ratios: show only on choose tables
    ratio_cell <- if (!is.null(ratios)) { if (is.finite(ratios[i])) .fmt_num(ratios[i]) else "" } else ""
    html <- c(html, sprintf("<td class='bcol'>%s</td>", .fmt_num(rhs[i])),
              sprintf("<td class='ratiocol'>%s</td></tr>", ratio_cell))
  }
  
  # --- Zj row ---
  # NEW: phase-dependent labels
  zj_label <- if (is_phase1) "Z<sub>j</sub>*" else "Z<sub>j</sub>"
  html <- c(html, sprintf("<tr class='zjrow'><th>%s</th>", zj_label))
  for (j in seq_len(n)) {
    if (show_metrics) {
      val <- zj_disp[j]
      if (is.na(val)) {
        # If value is NA, show empty cell, no sign color
        cls  <- if (is_enter(j)) "entercol" else ""
        cell <- ""
      } else {
        sign_class <- if (val < -1e-12) "neg" else if (val > 1e-12) "pos" else ""
        cls  <- if (is_enter(j)) paste("entercol", sign_class) else sign_class
        cell <- .fmt_num(val)
      }
    } else {
      cls  <- if (is_enter(j)) "entercol" else ""
      cell <- ""
    }
    html <- c(html, sprintf("<td class='%s'>%s</td>", cls, cell))
  }
  
  # Z0 (objective value) – also NA-safe
  if (show_metrics && !is.na(z0_disp)) {
    z0_cell  <- .fmt_num(z0_disp)
    z0_class <- if (z0_disp > 1e-12) "pos" else if (z0_disp < -1e-12) "neg" else ""
  } else {
    z0_cell  <- ""
    z0_class <- ""
  }
  html <- c(html, sprintf("<td class='bcol %s'>%s</td><td class='ratiocol'></td></tr>", z0_class, z0_cell))
  
  # --- Zj − Cj row ---
  # NEW: phase-dependent labels
  diff_label <- if (is_phase1) "Z<sub>j</sub>*-C<sub>j</sub>*" else "Z<sub>j</sub>-C<sub>j</sub>"
  html <- c(html, sprintf("<tr class='diffrow'><th>%s</th>", diff_label))
  for (j in seq_len(n)) {
    if (show_metrics) {
      val <- zjmcj_disp[j]
      if (is.na(val)) {
        cls  <- if (is_enter(j)) "entercol" else ""
        cell <- ""
      } else {
        cls <- if (val < -1e-12) "neg" else if (val > 1e-12) "pos" else ""
        if (is_enter(j)) cls <- paste("entercol", cls)
        cell <- .fmt_num(val)
      }
    } else {
      cls  <- if (is_enter(j)) "entercol" else ""
      cell <- ""
    }
    html <- c(html, sprintf("<td class='%s'>%s</td>", cls, cell))
  }
  html <- c(html, "<td class='bcol'></td><td class='ratiocol'></td></tr>")
  
  html <- c(html, "</tbody></table>")
  if (!is.null(note)) html <- c(html, sprintf("<div class='note'><em>%s</em></div>", note))
  html <- c(html, "</div>")
  paste0(css, paste(html, collapse=""))
}

# ---------- Safe render gateway ----------
.render_tableau_html <- function(tab, basis_idx, col_names, row_names,
                                 title = NULL, caption = NULL,
                                 pivot = NULL, note = NULL,
                                 obj_vec = NULL, ratios = NULL,
                                 enter_col = NULL, leave_row = NULL,
                                 min_mode = FALSE) {
  base_args <- list(
    tab = tab, basis_idx = basis_idx, col_names = col_names, row_names = row_names,
    title = title, caption = caption, pivot = pivot, note = note
  )
  if (exists("simplex_html_initial", mode="function") && grepl("Initial", title %||% "")) {
    return(do.call(get("simplex_html_initial"), base_args))
  }
  if (exists("simplex_html_pivot", mode="function") && grepl("Pivot|Iteration", title %||% "")) {
    return(do.call(get("simplex_html_pivot"), c(base_args, list(pivot=pivot))))
  }
  if (exists("simplex_html_status", mode="function") &&
      grepl("Unbounded|Infeasible|Optimality", title %||% "")) {
    return(do.call(get("simplex_html_status"), base_args))
  }
  if (exists("simplex_html_render_tableau", mode="function")) {
    return(do.call(get("simplex_html_render_tableau"), base_args))
  }
  .bigM_like_render(tab, basis_idx, col_names, row_names,
                    title, caption, pivot, note,
                    obj_vec, ratios, enter_col, leave_row,
                    min_mode = min_mode)
}

# =========================================================
# Core linear algebra
# =========================================================
.make_standard_form <- function(A, b, c, sense, maximize = TRUE,
                                var_names = NULL, cons_names = NULL) {
  A <- as.matrix(A); b <- as.numeric(b); c <- as.numeric(c)
  m <- nrow(A); n <- ncol(A)
  if (is.null(var_names))  var_names  <- paste0("x", seq_len(n))
  if (is.null(cons_names)) cons_names <- paste0("c", seq_len(m))
  
  T <- cbind(A, rhs = b)
  col_names <- var_names
  
  for (i in seq_len(m)) {
    si <- sense[i]
    s_col <- rep(0, m)
    a_col <- rep(0, m)
    if (si == "<=") {
      s_col[i] <- 1
      T <- cbind(T[, -ncol(T), drop = FALSE], s_col, rhs = T[, ncol(T)])
      col_names <- c(col_names, paste0("s", i))
    } else if (si == ">=") {
      s_col[i] <- -1
      T <- cbind(T[, -ncol(T), drop = FALSE], s_col, rhs = T[, ncol(T)])
      col_names <- c(col_names, paste0("e", i))
      a_col[i] <- 1
      T <- cbind(T[, -ncol(T), drop = FALSE], a_col, rhs = T[, ncol(T)])
      col_names <- c(col_names, paste0("a", i))
    } else if (si == "=") {
      a_col[i] <- 1
      T <- cbind(T[, -ncol(T), drop = FALSE], a_col, rhs = T[, ncol(T)])
      col_names <- c(col_names, paste0("a", i))
    } else stop("sense must be one of <=, >=, =")
  }
  
  a_idx <- grep("^a[0-9]+$", col_names)
  
  # Phase I objective: artificials sign depends on problem type
  obj1 <- numeric(length(col_names) + 1)
  if (length(a_idx) > 0) obj1[a_idx] <- if (maximize) -1 else 1
  
  # Phase II objective (internal: always maximize; min -> negate c)
  obj2 <- numeric(length(col_names) + 1)
  obj2[seq_along(var_names)] <- c
  
  list(
    Aext = T[, -ncol(T), drop = FALSE],
    rhs  = T[,  ncol(T)],
    obj1 = obj1,
    obj2 = obj2,
    col_names = col_names,
    row_names = cons_names %||% paste0("c", seq_len(m)),
    var_names = var_names,
    a_idx = a_idx
  )
}

.build_tableau <- function(Aext, rhs, obj) {
  m <- nrow(Aext); n <- ncol(Aext)
  T <- matrix(0, nrow = m + 1, ncol = n + 1)
  T[1:m, 1:n] <- Aext
  T[1:m, n + 1] <- rhs
  T[m + 1, 1:n] <- -obj[1:n]               # store Zj - Cj row
  T[m + 1, n + 1] <- obj[n + 1] %||% 0
  T
}

.recompute_reduced_costs <- function(T, basis_idx, obj_vec) {
  m <- nrow(T) - 1; n <- ncol(T) - 1
  B    <- T[seq_len(m), basis_idx, drop = FALSE]
  Binv <- solve(B)
  A    <- T[seq_len(m), seq_len(n), drop = FALSE]
  rhs  <- T[seq_len(m), n + 1]
  c    <- obj_vec[1:n]
  cB   <- c[basis_idx]
  zj   <- as.numeric(cB %*% Binv %*% A)
  z0   <- as.numeric(cB %*% Binv %*% rhs)
  
  T[m + 1, seq_len(n)] <- zj - c
  T[m + 1, n + 1]      <- z0
  attr(T, "c_full") <- c
  attr(T, "cB")     <- cB
  T
}

# Entering column utility
# - maximize: choose most negative (lowest) Zj−Cj
# - minimize: choose largest positive Zj−Cj
.choose_entering <- function(T, eligible = NULL, tol = 1e-9, choose_negative = FALSE) {
  rc <- T[nrow(T), 1:(ncol(T)-1)]  # Zj − Cj
  
  # limit to eligible cols if given
  if (!is.null(eligible)) {
    mask <- rep(FALSE, length(rc)); mask[eligible] <- TRUE
    rc[!mask] <- NA_real_
  }
  
  # squash tiny numerical noise
  rc[abs(rc) <= tol] <- 0
  
  if (choose_negative) {
    # MAX: look for strictly negative RC
    vmin <- suppressWarnings(min(rc, na.rm = TRUE))
    if (!is.finite(vmin) || vmin >= 0) return(NA_integer_)   # optimal: no negative RC
    which(rc == vmin)[1L]                                    # left-most tie
  } else {
    # MIN: look for strictly positive RC
    vmax <- suppressWarnings(max(rc, na.rm = TRUE))
    if (!is.finite(vmax) || vmax <= 0) return(NA_integer_)   # optimal: no positive RC
    which(rc == vmax)[1L]                                    # left-most tie
  }
}





.pivot <- function(T, i, j) {
  p <- T[i, j]
  T[i, ] <- T[i, ] / p
  for (r in seq_len(nrow(T))) {
    if (r == i) next
    T[r, ] <- T[r, ] - T[r, j] * T[i, ]
  }
  T
}

# One phase loop: emits "choose" and "pivot" tables per iteration
.solve_phase <- function(T, col_names, row_names, basis_idx,
                         caption_prefix, html_steps, step_id_start = 1,
                         eligible_cols = NULL, min_mode = FALSE,
                         choose_negative = FALSE) {
  
  # --- display names for captions (match tableau display) ---
  col_names_disp <- .disp_col_names(col_names)
  row_names_disp <- .disp_row_names(row_names)
  
  step <- step_id_start
  repeat {
    j_enter <- .choose_entering(T, eligible = eligible_cols, choose_negative = choose_negative)
    if (is.na(j_enter)) {
      html_steps[[length(html_steps) + 1]] <- .render_tableau_html(
        T, basis_idx, col_names, row_names,
        title   = sprintf("%s - Optimality reached", caption_prefix),
        caption = sprintf("Iteration %d: No %s reduced costs.",
                          step, if (choose_negative) "negative" else "positive"),
        obj_vec = attr(T, "c_full"),
        min_mode = min_mode
      )
      break
    }
    
    # Decision table
    m <- nrow(T)-1; n <- ncol(T)-1
    col <- T[1:m, j_enter]; rhs <- T[1:m, n+1]
    ratios <- ifelse(col > 1e-12, rhs/col, Inf)
    i_leave <- if (all(is.infinite(ratios))) NA_integer_ else which.min(ratios)
    
    html_steps[[length(html_steps) + 1]] <- .render_tableau_html(
      T, basis_idx, col_names, row_names,
      title   = sprintf("%s - Iteration %d (choose)", caption_prefix, step),
      caption = sprintf("Iteration %d: Enter %s; compute ratios and choose leaving row.",
                        step, col_names_disp[j_enter]),
      obj_vec   = attr(T, "c_full"),
      ratios    = ratios,
      enter_col = j_enter,
      leave_row = if (!is.na(i_leave)) i_leave else NA_integer_,
      min_mode  = min_mode
    )
    
    if (is.na(i_leave)) {
      html_steps[[length(html_steps) + 1]] <- .render_tableau_html(
        T, basis_idx, col_names, row_names,
        title   = sprintf("%s - Unbounded", caption_prefix),
        caption = sprintf("Iteration %d: Column %s has no positive entries; problem is unbounded.",
                          step, col_names_disp[j_enter]),
        obj_vec   = attr(T, "c_full"),
        ratios    = ratios,
        enter_col = j_enter,
        min_mode  = min_mode
      )
      return(list(T=T, basis_idx=basis_idx, html_steps=html_steps,
                  status="unbounded", step=step))
    }
    
    # Pivot table
    basis_idx[i_leave] <- j_enter
    T <- .pivot(T, i_leave, j_enter)
    
    # refresh Zj, Z0, (Zj-Cj), and cB using the current objective
    obj_vec_now <- c(attr(T, "c_full"), T[nrow(T), ncol(T)])
    T <- .recompute_reduced_costs(T, basis_idx, obj_vec_now)
    
    html_steps[[length(html_steps) + 1]] <- .render_tableau_html(
      T, basis_idx, col_names, row_names,
      title   = sprintf("%s - Iteration %d (pivot)", caption_prefix, step),
      caption = sprintf("Iteration %d: Enter %s, leave %s.",
                        step, col_names_disp[j_enter], row_names_disp[i_leave]),
      pivot     = c(i_leave, j_enter),
      obj_vec   = attr(T, "c_full"),
      ratios    = NULL,        # ratios visible only in choose tables
      enter_col = j_enter,
      leave_row = i_leave,
      min_mode  = min_mode
    )
    step <- step + 1
  }
  list(T=T, basis_idx=basis_idx, html_steps=html_steps, status="ok", step=step)
}

# =========================================================
# Public API + viewing helpers
# =========================================================
.two_phase_simplex_html_join <- function(html_steps) paste(html_steps, collapse = "\n\n")

two_phase_simplex_steps_browsable <- function(res, page_title = "Two-Phase Simplex") {
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("Please install 'htmltools': install.packages('htmltools')")
  body <- .two_phase_simplex_html_join(res$html_steps)
  htmltools::browsable(htmltools::tagList(
    htmltools::tags$html(
      htmltools::tags$head(htmltools::tags$title(page_title)),
      htmltools::tags$body(htmltools::HTML(body))
    )
  ))
}

two_phase_simplex_show <- function(res, page_title = "Two-Phase Simplex") {
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("Please install 'htmltools': install.packages('htmltools')")
  b <- two_phase_simplex_steps_browsable(res, page_title)
  htmltools::html_print(b, background = "white")
  invisible(res)
}

two_phase_simplex_save_html <- function(res, file = "two_phase_simplex_report.html",
                                        page_title = "Two-Phase Simplex") {
  body <- .two_phase_simplex_html_join(res$html_steps)
  doc <- sprintf("<!doctype html><html><head><meta charset='utf-8'><title>%s</title></head><body>%s</body></html>",
                 page_title, body)
  writeLines(doc, file)
  normalizePath(file, winslash = "/", mustWork = FALSE)
}

two_phase_simplex_asis <- function(res) { cat(.two_phase_simplex_html_join(res$html_steps)); invisible(res) }

two_phase_simplex_each_table_html <- function(res, page_title_prefix = "Two-Phase Simplex — Step") {
  steps <- res$html_steps %||% character(0)
  if (!length(steps)) return(character(0))
  vapply(seq_along(steps), function(i) {
    sprintf("<!doctype html><html><head><meta charset='utf-8'><title>%s %02d</title></head><body>%s</body></html>",
            page_title_prefix, i, steps[[i]])
  }, character(1))
}

two_phase_simplex_save_steps <- function(res, dir = "two_phase_steps", prefix = "step",
                                         page_title_prefix = "Two-Phase Simplex — Step") {
  docs <- two_phase_simplex_each_table_html(res, page_title_prefix)
  if (!length(docs)) return(character(0))
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(length(docs))
  for (i in seq_along(docs)) {
    fn <- file.path(dir, sprintf("%s-%02d.html", prefix, i))
    writeLines(docs[[i]], fn, useBytes = TRUE)
    paths[i] <- normalizePath(fn, winslash = "/", mustWork = FALSE)
  }
  paths
}

two_phase_simplex_show_steps <- function(res, page_title_prefix = "Two-Phase Simplex — Step") {
  if (!requireNamespace("htmltools", quietly = TRUE))
    stop("Please install 'htmltools': install.packages('htmltools')")
  docs <- two_phase_simplex_each_table_html(res, page_title_prefix)
  invisible(lapply(seq_along(docs), function(i) {
    b <- htmltools::browsable(htmltools::HTML(docs[[i]]))
    htmltools::html_print(b, background = "white")
  }))
}

# =========================================================
# Main solver
# =========================================================
two_phase_simplex_html <- function(A, b, c, sense, maximize = TRUE,
                                   var_names = NULL, cons_names = NULL,
                                   max_iters = 10000) {
  min_mode <- !maximize
  m <- nrow(A)
  sf <- .make_standard_form(A, b, c, sense, maximize, var_names, cons_names)
  
  Aext <- sf$Aext; rhs <- sf$rhs
  col_names <- sf$col_names; row_names <- sf$row_names
  a_idx <- sf$a_idx; var_names <- sf$var_names
  
  html_steps <- list()
  
  # ---------- Phase I ----------
  T1 <- .build_tableau(Aext, rhs, sf$obj1)
  
  # Initial basis
  basis_idx <- integer(m)
  for (i in seq_len(m)) {
    id_found <- FALSE
    for (j in seq_len(ncol(Aext))) {
      if (all(abs(Aext[, j] - c(rep(0, i-1), 1, rep(0, m-i))) < 1e-12)) { basis_idx[i] <- j; id_found <- TRUE; break }
    }
    if (!id_found) {
      ai <- grep(paste0("^a", i, "$"), col_names)
      if (length(ai) == 1) basis_idx[i] <- ai else {
        pos <- which(Aext[i, ] > 1e-12)
        if (length(pos)) basis_idx[i] <- pos[1] else stop("Cannot build initial basis.")
      }
    }
  }
  
  for (i in seq_len(m)) { j <- basis_idx[i]; if (abs(T1[i, j]) > 1e-12) T1 <- .pivot(T1, i, j) }
  T1 <- .recompute_reduced_costs(T1, basis_idx, sf$obj1)
  
  phase1_eligible <- setdiff(seq_len(ncol(T1)-1), sf$a_idx)  # do not enter artificials
  
  html_steps[[length(html_steps) + 1]] <- .render_tableau_html(
    T1, basis_idx, col_names, row_names,
    title   = "Phase I - Initial Tableau",
    caption = sprintf("Construct Phase I with %sΣa as objective and canonicalize basis.",
                      if (min_mode) "+" else "-"),
    obj_vec = sf$obj1, min_mode = min_mode
  )
  
  res1 <- .solve_phase(T1, col_names, row_names, basis_idx,
                       caption_prefix = "Phase I",
                       html_steps = html_steps, step_id_start = 1,
                       eligible_cols = phase1_eligible, min_mode = min_mode,
                       choose_negative = !min_mode
                       )
  T1 <- res1$T; basis_idx <- res1$basis_idx; html_steps <- res1$html_steps
  if (res1$status == "unbounded")
    return(list(status="unbounded", phase=1, tableau=T1,
                basic_vars=col_names[basis_idx], html_steps=html_steps))
  
  # Feasibility check
  z1 <- T1[nrow(T1), ncol(T1)]
  if (abs(z1) > 1e-8) {
    html_steps[[length(html_steps) + 1]] <- .render_tableau_html(
      T1, basis_idx, col_names, row_names,
      title   = "Phase I - Infeasible",
      caption = sprintf("Infeasible: optimal Phase I objective z1 = %.6g (should be 0).", z1),
      obj_vec = sf$obj1, min_mode = min_mode
    )
    return(list(status="infeasible", phase=1, tableau=T1,
                basic_vars=col_names[basis_idx], html_steps=html_steps))
  }
  
  # Remove artificials (pivot out if basic)
  if (length(a_idx) > 0) {
    # 1) Try to pivot artificials out of the basis where possible
    for (ai in a_idx) if (ai %in% basis_idx) {
      i <- which(basis_idx == ai)
      candidates <- setdiff(which(abs(T1[i, 1:(ncol(T1)-1)]) > 1e-12), a_idx)
      if (length(candidates)) {
        j <- candidates[1]
        T1 <- .pivot(T1, i, j)
        basis_idx[i] <- j
      }
    }
    
    # 2) Drop artificial columns
    keep_cols <- setdiff(seq_len(ncol(T1) - 1L), a_idx)
    
    # remap basis indices to the compacted column set
    basis_idx <- match(basis_idx, keep_cols)
    
    # compact tableau and column names
    T1 <- cbind(T1[, keep_cols, drop = FALSE], T1[, ncol(T1), drop = FALSE])
    col_names <- col_names[keep_cols]
    
    # --- NEW: fix rows that lost their basic variable (redundant constraints) ---
    mT <- nrow(T1) - 1L
    nT <- ncol(T1) - 1L
    if (any(is.na(basis_idx))) {
      rows_drop <- integer(0)
      for (i in seq_len(mT)) {
        if (!is.na(basis_idx[i])) next
        
        # Try to find a new basic column j with an identity pattern in row i
        nonbasic <- setdiff(seq_len(nT), basis_idx[!is.na(basis_idx)])
        found <- FALSE
        for (j in nonbasic) {
          col <- T1[1:mT, j]
          if (abs(col[i] - 1) < 1e-9 && all(abs(col[-i]) < 1e-9)) {
            T1 <- .pivot(T1, i, j)
            basis_idx[i] <- j
            found <- TRUE
            break
          }
        }
        
        if (!found) {
          # If the whole row is numerically zero (including RHS), it’s redundant
          if (all(abs(T1[i, 1:nT]) < 1e-9) && abs(T1[i, nT + 1L]) < 1e-9) {
            rows_drop <- c(rows_drop, i)
          } else {
            # Very conservative: treat non-pivotable, no-basic row as redundant as well
            warning(sprintf(
              "Row %d lost its basic variable after removing artificials; treating it as redundant.",
              i
            ))
            rows_drop <- c(rows_drop, i)
          }
        }
      }
      
      if (length(rows_drop)) {
        keep_rows <- c(setdiff(seq_len(mT), rows_drop), mT + 1L)  # keep last (Zj / Zj-Cj) row
        T1 <- T1[keep_rows, , drop = FALSE]
        row_names <- row_names[setdiff(seq_len(mT), rows_drop)]
        basis_idx <- basis_idx[setdiff(seq_len(mT), rows_drop)]
        mT <- nrow(T1) - 1L
      }
    }
    
    # 3) Refresh reduced costs with the Phase-I objective over the compacted columns
    obj1_compact <- numeric(ncol(T1))  # length = n_compact + 1
    obj1_compact[seq_len(ncol(T1) - 1L)] <-
      (attr(T1, "c_full") %||% sf$obj1[seq_len(ncol(T1) - 1L)])
    obj1_compact[ncol(T1)] <- T1[nrow(T1), ncol(T1)]
    T1 <- .recompute_reduced_costs(T1, basis_idx, obj1_compact)
  }
  
  
  
  # ---------- Phase II ----------
  n2 <- length(col_names)
  
  # Use original c for the decision variables; zeros for slacks/artificial
  obj2_full <- numeric(n2 + 1)
  for (j in seq_along(col_names)) {
    nm <- col_names[j]
    obj2_full[j] <- if (nm %in% var_names) c[match(nm, var_names)] else 0
  }
  
  
  T2 <- T1
  T2[nrow(T2), ] <- 0
  T2[nrow(T2), 1:n2] <- -obj2_full[1:n2]
  T2 <- .recompute_reduced_costs(T2, basis_idx, obj2_full)
  
  html_steps[[length(html_steps) + 1]] <- .render_tableau_html(
    T2, basis_idx, col_names, row_names,
    title   = "Phase II - Initial Tableau",
    caption = "Restore original objective; recompute reduced costs.",
    obj_vec = obj2_full, min_mode = min_mode
  )
  
  res2 <- .solve_phase(T2, col_names, row_names, basis_idx,
                       caption_prefix = "Phase II",
                       html_steps = html_steps, step_id_start = 1,
                       eligible_cols = seq_len(ncol(T2)-1), min_mode = min_mode,
                       choose_negative = !min_mode)  
  T2 <- res2$T; basis_idx <- res2$basis_idx; html_steps <- res2$html_steps
  if (res2$status == "unbounded")
    return(list(status="unbounded", phase=2, tableau=T2,
                basic_vars=col_names[basis_idx], html_steps=html_steps))
  
  # Solution for original vars
  n2 <- ncol(T2) - 1
  x <- setNames(numeric(length(var_names)), var_names)
  for (k in seq_along(var_names)) {
    j <- match(var_names[k], col_names)
    if (!is.na(j) && j %in% basis_idx) x[k] <- T2[which(basis_idx == j), n2 + 1]
  }
  z <- T2[nrow(T2), ncol(T2)]
  
  list(status="optimal", phase=2, z_opt=z, x_opt=x,
       basic_vars=col_names[basis_idx], tableau=T2, html_steps=html_steps)
}
