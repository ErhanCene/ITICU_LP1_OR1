# ===== Simplex capture with Cj, Zj, Zj-Cj, basis names-in-rows ("cB var"),
# ===== ratio column, pivot flag, and optional z-row handling =====
capture_simplexR <- function(A, b, c, unique_slacks = TRUE, z_row = c("drop","rename","keep"), ...) {
  z_row <- match.arg(z_row)
  
  out_lines <- utils::capture.output({ res <- simplexR(A, b, c, ...) })
  
  # ---------- utilities ----------
  strip_msg <- function(x) sub('^\\[\\d+\\]\\s+"(.*)"\\s*$', '\\1', x)
  lines_raw   <- out_lines
  lines_logic <- vapply(out_lines, strip_msg, character(1))
  
  .read_block <- function(lines) {
    lines <- lines[nzchar(lines)]
    con <- textConnection(paste0(lines, collapse = "\n"))
    on.exit(close(con), add = TRUE)
    df <- utils::read.table(con, header = TRUE, check.names = FALSE)
    if (all(grepl("^(R\\d+|z)$", df[[1]]))) {
      rn <- df[[1]]; df <- df[-1]; rownames(df) <- rn
    }
    if (unique_slacks) {
      cn <- colnames(df)
      if (any(cn == "s")) {
        idx <- which(cn == "s")
        cn[idx] <- paste0("s", seq_along(idx))
        colnames(df) <- cn
      }
    }
    df
  }
  
  .aux_rows <- c("z","Cj","Zj","Zj-Cj")
  .constr_rows <- function(tbl) setdiff(rownames(tbl), .aux_rows)
  
  .Cj_for_cols <- function(cols) {
    Cj <- setNames(numeric(length(cols)), cols)
    # decision vars x1.. get costs from c
    x_idx <- grep("^x\\d+$", cols)
    if (length(x_idx)) {
      xnums <- as.integer(sub("^x(\\d+)$", "\\1", cols[x_idx]))
      Cj[x_idx] <- c[xnums]
    }
    # slacks and b are 0
    Cj[grepl("^s\\d+$", names(Cj))] <- 0
    Cj[["b"]] <- 0
    Cj
  }
  
  .basis_from_tableau <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(character(0))
    rows_constr <- .constr_rows(tbl)
    cols_vars <- setdiff(colnames(tbl), "b")
    tol <- 1e-8
    basis <- setNames(rep(NA_character_, length(rows_constr)), rows_constr)
    if (!length(rows_constr) || !length(cols_vars)) return(basis)
    M <- as.matrix(tbl[rows_constr, cols_vars, drop = FALSE])
    for (i in seq_along(rows_constr)) {
      col_is_I <- vapply(seq_along(cols_vars), function(j) {
        colvec <- M[, j]
        abs(colvec[i] - 1) < tol && sum(abs(colvec[-i]) < tol) == (length(colvec) - 1)
      }, logical(1))
      if (any(col_is_I)) basis[i] <- cols_vars[which(col_is_I)[1]]
    }
    basis
  }
  
  # Row labels like "3 x1", "0 s2" (c_B then a space, then variable name)
  .apply_cost_row_labels <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(tbl)
    rows_constr <- .constr_rows(tbl)
    if (!length(rows_constr)) return(tbl)
    basis_names <- .basis_from_tableau(tbl)
    Cj <- .Cj_for_cols(colnames(tbl))
    rn <- rownames(tbl)
    for (r in rows_constr) {
      var <- basis_names[[r]]
      if (!is.na(var) && var %in% names(Cj)) {
        rn[rn == r] <- paste0(Cj[[var]], " ", var)
      }
    }
    rownames(tbl) <- rn
    tbl
  }
  
  .augment_prices <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(tbl)
    cols <- colnames(tbl)
    
    # Handle z row
    has_z <- "z" %in% rownames(tbl)
    tbl_work <- tbl
    if (has_z && z_row == "drop") {
      tbl_work <- tbl_work[setdiff(rownames(tbl_work), "z"), , drop = FALSE]
      has_z <- FALSE
    } else if (has_z && z_row == "rename") {
      rn <- rownames(tbl_work); rn[rn == "z"] <- "Zj-Cj"; rownames(tbl_work) <- rn
    }
    
    # Recompute Cj, Zj, Zj-Cj
    Cj <- .Cj_for_cols(cols)
    rows_constr <- .constr_rows(tbl_work)   # after optional z handling
    basis_names <- .basis_from_tableau(tbl_work)
    
    cB <- rep(0, length(rows_constr)); names(cB) <- rows_constr
    for (i in seq_along(rows_constr)) {
      bi <- basis_names[[ rows_constr[i] ]]
      cB[i] <- if (!is.na(bi) && bi %in% names(Cj)) Cj[[bi]] else 0
    }
    
    A_can <- as.matrix(tbl_work[rows_constr, , drop = FALSE])
    Zj <- as.numeric(cB %*% A_can); names(Zj) <- colnames(A_can)
    Zj_minus_Cj <- Zj - Cj[names(Zj)]
    
    add_list <- list(Cj = Cj[cols], Zj = Zj[cols], `Zj-Cj` = Zj_minus_Cj[cols])
    # If we renamed z to Zj-Cj, don't add a duplicate computed Zj-Cj
    if ("Zj-Cj" %in% rownames(tbl_work)) add_list$`Zj-Cj` <- NULL
    add <- do.call(rbind, add_list)
    
    # Bind: Cj on top, then table, then Zj and (maybe) Zj-Cj
    out <- rbind(add["Cj", , drop = FALSE], tbl_work)
    if ("Zj" %in% rownames(add)) out <- rbind(out, add["Zj", , drop = FALSE])
    if ("Zj-Cj" %in% rownames(add)) out <- rbind(out, add["Zj-Cj", , drop = FALSE])
    
    # Finally, rename constraint rows to "cB var"
    out <- .apply_cost_row_labels(out)
    
    # Order rows: Cj, constraints (now labeled), then Zj, Zj-Cj (and/or z if kept)
    final_rows <- rownames(out)
    rows_basic <- setdiff(final_rows, c("Cj","Zj","Zj-Cj","z"))
    ord <- c("Cj", rows_basic, intersect("z", final_rows), intersect("Zj", final_rows), intersect("Zj-Cj", final_rows))
    out[ord, , drop = FALSE]
  }
  
  .extract_state <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl) || !("b" %in% colnames(tbl))) {
      return(list(z = NA_real_, x = numeric(0)))
    }
    # objective value: prefer 'Zj' RHS if present (equals objective), else 'z'
    zval <- NA_real_
    if ("Zj" %in% rownames(tbl)) zval <- suppressWarnings(as.numeric(tbl["Zj","b"]))
    if (is.na(zval) && "z" %in% rownames(tbl)) zval <- suppressWarnings(as.numeric(tbl["z","b"]))
    
    x_cols <- which(grepl("^x\\d+$", colnames(tbl)))
    x_vals <- rep(0, length(x_cols)); names(x_vals) <- colnames(tbl)[x_cols]
    if (length(x_cols)) {
      tol <- 1e-8
      rows_constr <- .constr_rows(tbl)
      M <- as.matrix(tbl[rows_constr, x_cols, drop = FALSE])
      rhs <- as.numeric(tbl[rows_constr, "b"])
      for (j in seq_along(x_cols)) {
        colvec <- M[, j]
        one_at <- which(abs(colvec - 1) < tol)
        zeros  <- which(abs(colvec) < tol)
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
  
  # ---------- find markers ----------
  dash_idx      <- which(grepl("^-{3,}$", lines_logic))
  init_start    <- which(grepl("^Initial Tableau \\(Tableau 0\\)$", lines_logic))
  pivot_col_idx <- which(grepl("^Pivot column:\\s*\\d+$", lines_logic))
  pivot_row_idx <- which(grepl("^Pivot row:\\s*\\d+$", lines_logic))
  new_tbl_idx   <- which(grepl("^New tableau at the end of iteration \\d+$", lines_logic))
  status_idx    <- which(grepl("^Status:", lines_logic))
  
  # ---------- initial tableau ----------
  init_block <- NULL
  if (length(init_start) == 1) {
    next_dash <- dash_idx[dash_idx > init_start]
    next_hdr  <- new_tbl_idx[new_tbl_idx > init_start]
    end_idx <- if (length(next_dash)) next_dash[1] - 1 else if (length(next_hdr)) next_hdr[1] - 1 else length(lines_raw)
    start_idx <- init_start + 1
    init_tbl_lines <- lines_raw[start_idx:end_idx]
    init_tbl_lines <- init_tbl_lines[!grepl('^\\[\\d+\\]\\s*"', init_tbl_lines)]
    init_block <- .read_block(init_tbl_lines)
    # augment and label with "cB var" rows
    init_block <- .augment_prices(init_block)
  }
  
  # ---------- iteration AFTER tableaux ----------
  iter_tbls <- list()
  if (length(new_tbl_idx)) {
    for (i in seq_along(new_tbl_idx)) {
      start_logic <- new_tbl_idx[i] + 1
      next_end <- sort(c(dash_idx[dash_idx > new_tbl_idx[i]], new_tbl_idx[new_tbl_idx > new_tbl_idx[i]]))[1]
      end_logic <- if (length(next_end)) next_end - 1 else length(lines_logic)
      block <- lines_raw[start_logic:end_logic]
      block <- block[!grepl('^\\[\\d+\\]\\s*"', block)]
      Traw <- .read_block(block)
      iter_tbls[[i]] <- .augment_prices(Traw)
    }
    names(iter_tbls) <- paste0("iter_", seq_along(iter_tbls))
  }
  
  # ---------- pivot info ----------
  get_num <- function(x, pat) as.integer(sub(pat, "\\1", x))
  piv_cols <- if (length(pivot_col_idx)) get_num(lines_logic[pivot_col_idx], '^Pivot column:\\s*(\\d+)$') else integer(0)
  piv_rows <- if (length(pivot_row_idx)) get_num(lines_logic[pivot_row_idx], '^Pivot row:\\s*(\\d+)$') else integer(0)
  L <- length(iter_tbls)
  if (length(piv_cols) < L) piv_cols <- c(piv_cols, rep(NA_integer_, L - length(piv_cols)))
  if (length(piv_rows) < L) piv_rows <- c(piv_rows, rep(NA_integer_, L - length(piv_rows)))
  
  # ---------- steps (BEFORE/AFTER, add ratio column to BEFORE) ----------
  steps <- vector("list", L)
  for (k in seq_len(L)) {
    before_tbl <- if (k == 1) init_block else iter_tbls[[k - 1]]
    after_tbl  <- iter_tbls[[k]]
    
    # entering variable name from BEFORE tableau
    enter_var <- if (!is.null(before_tbl) && !is.na(piv_cols[k]) &&
                     piv_cols[k] >= 1 && piv_cols[k] <= ncol(before_tbl)) {
      colnames(before_tbl)[piv_cols[k]]
    } else NA_character_
    
    rows_before <- .constr_rows(before_tbl)
    pivot_row_name <- if (length(rows_before) && !is.na(piv_rows[k]) &&
                          piv_rows[k] >= 1 && piv_rows[k] <= length(rows_before)) {
      rows_before[piv_rows[k]]
    } else NA_character_
    leave_var <- pivot_row_name
    
    # ratios on BEFORE
    ratios <- numeric(0); pivot_row_from_ratio <- NA_integer_
    ratio_colname <- if (!is.na(enter_var)) paste0("ratio_", enter_var) else "ratio"
    if (!is.null(before_tbl) && !is.na(enter_var) && enter_var %in% colnames(before_tbl) && "b" %in% colnames(before_tbl)) {
      a <- as.numeric(before_tbl[rows_before, enter_var])
      bRHS <- as.numeric(before_tbl[rows_before, "b"])
      valid <- a > 1e-12
      ratios <- rep(NA_real_, length(a)); names(ratios) <- rows_before
      ratios[valid] <- bRHS[valid] / a[valid]
      if (any(valid)) pivot_row_from_ratio <- which(valid)[which.min(ratios[valid])]
      
      # insert column safely after 'b'
      before_tbl[[ratio_colname]] <- NA_real_
      before_tbl[rows_before, ratio_colname] <- ratios
      cols <- colnames(before_tbl)
      pos_b <- which(cols == "b")[1]
      cols <- append(cols, ratio_colname, after = pos_b)
      cols <- cols[!duplicated(cols)]
      before_tbl <- before_tbl[, cols, drop = FALSE]
    }
    
    # long formats + pivot flag
    before_long <- .to_long(before_tbl)
    if (nrow(before_long)) before_long$is_pivot_cell <- with(before_long, row == leave_var & col == enter_var)
    after_long <- .to_long(after_tbl); after_long$is_pivot_cell <- FALSE
    
    st <- .extract_state(after_tbl)
    
    steps[[k]] <- list(
      iter = k,
      before = before_tbl,   # constraint rows labeled like "3 x1" etc.
      after  = after_tbl,
      before_long = before_long,
      after_long  = after_long,
      pivot_col_index = piv_cols[k],
      pivot_row_index = piv_rows[k],
      pivot_col_name  = enter_var,
      pivot_row_name  = pivot_row_name,
      enter = enter_var,
      leave = leave_var,
      ratios = ratios,
      ratio_colname = ratio_colname,
      pivot_row_from_ratio_index = pivot_row_from_ratio,
      z_after = st$z,
      x_after = st$x
    )
  }
  
  # summary
  summ_list <- lapply(steps, function(stp) {
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
  
  status <- {
    if (length(status_idx)) sub('^Status:\\s*(.*)$', "\\1", lines_logic[status_idx[length(status_idx)]]) else NA_character_
  }
  attr(summary, "status") <- status
  
  list(
    initial     = init_block,   # augmented; constraint rows labeled "cB var"
    iterations  = iter_tbls,    # AFTER tableaux augmented & labeled
    steps       = steps,        # BEFORE/AFTER + ratios + labels
    summary     = summary,
    result      = res,
    raw_output  = out_lines
  )
}
library(simplexR)

# A <- matrix(c(2,1,
#               1,2,
#               4,1), nrow = 3, byrow = TRUE,
#             dimnames = list(c("R1","R2","R3"), c("x1","x2")))
# b <- c(22, 23, 40)
# c <- c(3, 2)
# 
# # Drop the printed 'z' row to avoid redundancy (you'll have Cj, constraints, Zj, Zj-Cj)
# cap <- capture_simplexR(A, b, c, unique_slacks = TRUE, z_row = "drop")
# 
# # Initial tableau (constraint rows are "0 s1", "0 s2", "0 s3")
# cap$initial
# 
# # BEFORE tableau of iteration 1 shows the ratio column and "cB var" row labels
# cap$steps[[1]]$before
# 
# # AFTER tableau of iteration 1 (augmented, labeled, no ratio column)
# cap$steps[[1]]$after
# 
# # You should see rows like:
# #   Cj
# #   0 s1
# #   0 s2
# #   3 x1     <- once x1 enters basis later
# #   Zj
# #   Zj-Cj
# 
# # Summary remains available
# cap$summary
# attr(cap$summary, "status")
