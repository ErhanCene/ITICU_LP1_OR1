# ===== Robust capture of simplexR() output with basis + enter/leave =====
capture_simplexR <- function(A, b, c, ...) {
  out_lines <- utils::capture.output({
    res <- simplexR(A, b, c, ...)
  })
  
  # Strip R's print prefix [n] " ... " to get a 'logical content' view for markers
  strip_msg <- function(x) {
    # If the whole line is of the form [n] "....", return the inner text; else return as-is
    sub('^\\[\\d+\\]\\s+"(.*)"\\s*$', '\\1', x)
  }
  lines_raw   <- out_lines
  lines_logic <- vapply(out_lines, strip_msg, character(1))
  
  # ---- helpers ----
  .read_block <- function(lines) {
    lines <- lines[nzchar(lines)]
    con <- textConnection(paste0(lines, collapse = "\n"))
    on.exit(close(con), add = TRUE)
    df <- utils::read.table(con, header = TRUE, check.names = FALSE)
    if (all(grepl("^(R\\d+|z)$", df[[1]]))) {
      rn <- df[[1]]
      df <- df[-1]
      rownames(df) <- rn
    }
    df
  }
  .extract_state <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(list(z = NA_real_, x = numeric(0)))
    if (!("z" %in% rownames(tbl)) || !("b" %in% colnames(tbl))) {
      return(list(z = NA_real_, x = numeric(0)))
    }
    z <- suppressWarnings(as.numeric(tbl["z", "b"]))
    x_cols <- which(grepl("^x\\d+$", colnames(tbl)))
    x_vals <- rep(0, length(x_cols))
    names(x_vals) <- colnames(tbl)[x_cols]
    if (length(x_cols)) {
      tol <- 1e-8
      rows_constr <- setdiff(rownames(tbl), "z")
      M <- as.matrix(tbl[rows_constr, x_cols, drop = FALSE])
      rhs <- as.numeric(tbl[rows_constr, "b"])
      for (j in seq_along(x_cols)) {
        colvec <- M[, j]
        one_at <- which(abs(colvec - 1) < tol)
        zeros  <- which(abs(colvec) < tol)
        if (length(one_at) == 1 && length(zeros) == (length(colvec) - 1)) x_vals[j] <- rhs[one_at]
      }
    }
    list(z = z, x = x_vals)
  }
  .basis_from_tableau <- function(tbl) {
    if (is.null(tbl) || !nrow(tbl)) return(character(0))
    rows_constr <- setdiff(rownames(tbl), "z")
    cols_vars <- setdiff(colnames(tbl), "b")
    tol <- 1e-8
    basis <- setNames(rep(NA_character_, length(rows_constr)), rows_constr)
    if (!length(rows_constr) || !length(cols_vars)) return(basis)
    M <- as.matrix(tbl[rows_constr, cols_vars, drop = FALSE])
    for (i in seq_along(rows_constr)) {
      col_is_identity_here <- vapply(seq_along(cols_vars), function(j) {
        colvec <- M[, j]
        abs(colvec[i] - 1) < tol && sum(abs(colvec[-i]) < tol) == (length(colvec) - 1)
      }, logical(1))
      if (any(col_is_identity_here)) basis[i] <- cols_vars[which(col_is_identity_here)[1]]
    }
    basis
  }
  
  # ---- markers in 'logic' view ----
  dash_idx      <- which(grepl("^-{3,}$", lines_logic))
  init_start    <- which(grepl("^Initial Tableau \\(Tableau 0\\)$", lines_logic))
  pivot_col_idx <- which(grepl("^Pivot column:\\s*\\d+$", lines_logic))
  pivot_row_idx <- which(grepl("^Pivot row:\\s*\\d+$", lines_logic))
  new_tbl_idx   <- which(grepl("^New tableau at the end of iteration \\d+$", lines_logic))
  status_idx    <- which(grepl("^Status:", lines_logic))
  
  # ---- initial tableau ----
  init_block <- NULL
  if (length(init_start) == 1) {
    # Prefer the next dashed line; if absent, fall back to the next 'Iteration' or 'New tableau' header
    next_dash <- dash_idx[dash_idx > init_start]
    next_hdr  <- new_tbl_idx[new_tbl_idx > init_start]
    end_idx <- if (length(next_dash)) next_dash[1] - 1 else if (length(next_hdr)) next_hdr[1] - 1 else length(lines_raw)
    start_idx <- init_start + 1
    if (!is.na(start_idx) && !is.na(end_idx) && end_idx >= start_idx) {
      init_tbl_lines <- lines_raw[start_idx:end_idx]
      init_tbl_lines <- init_tbl_lines[!grepl('^\\[\\d+\\]\\s*"', init_tbl_lines)]  # drop quoted messages if any
      init_block <- .read_block(init_tbl_lines)
    }
  }
  
  # ---- iteration tableaux (AFTER each iteration) ----
  iter_tbls <- list()
  if (length(new_tbl_idx)) {
    for (i in seq_along(new_tbl_idx)) {
      start_logic <- new_tbl_idx[i] + 1
      # end at next dashed OR next iteration header OR end of file
      next_end <- sort(c(
        dash_idx[dash_idx > new_tbl_idx[i]],
        new_tbl_idx[new_tbl_idx > new_tbl_idx[i]]
      ))[1]
      end_logic <- if (length(next_end)) next_end - 1 else length(lines_logic)
      # map logic indices to raw lines (same length, so positions align)
      block <- lines_raw[start_logic:end_logic]
      block <- block[!grepl('^\\[\\d+\\]\\s*"', block)]
      iter_tbls[[i]] <- .read_block(block)
    }
    names(iter_tbls) <- paste0("iter_", seq_along(iter_tbls))
  }
  
  # ---- pivot numbers ----
  get_num <- function(x, pat) as.integer(sub(pat, "\\1", x))
  piv_cols <- if (length(pivot_col_idx)) get_num(lines_logic[pivot_col_idx], '^Pivot column:\\s*(\\d+)$') else integer(0)
  piv_rows <- if (length(pivot_row_idx)) get_num(lines_logic[pivot_row_idx], '^Pivot row:\\s*(\\d+)$') else integer(0)
  L <- length(iter_tbls)
  if (length(piv_cols) < L) piv_cols <- c(piv_cols, rep(NA_integer_, L - length(piv_cols)))
  if (length(piv_rows) < L) piv_rows <- c(piv_rows, rep(NA_integer_, L - length(piv_rows)))
  
  # ---- basis ----
  basis_initial <- .basis_from_tableau(init_block)
  basis_iters   <- lapply(iter_tbls, .basis_from_tableau)
  names(basis_iters) <- names(iter_tbls)
  
  # ---- per-iteration summary (with enter/leave) ----
  summ_list <- vector("list", L)
  for (k in seq_len(L)) {
    before_tbl <- if (k == 1) init_block else iter_tbls[[k - 1]]
    after_tbl  <- iter_tbls[[k]]
    
    enter_var <- if (!is.null(before_tbl) && !is.na(piv_cols[k]) &&
                     piv_cols[k] >= 1 && piv_cols[k] <= ncol(before_tbl)) {
      colnames(before_tbl)[piv_cols[k]]
    } else NA_character_
    
    rows_before <- if (!is.null(before_tbl)) setdiff(rownames(before_tbl), "z") else character(0)
    pivot_row_name <- if (length(rows_before) && !is.na(piv_rows[k]) &&
                          piv_rows[k] >= 1 && piv_rows[k] <= length(rows_before)) {
      rows_before[piv_rows[k]]
    } else NA_character_
    basis_before <- if (k == 1) basis_initial else basis_iters[[k - 1]]
    leave_var <- if (length(basis_before) && !is.na(pivot_row_name)) {
      basis_before[[pivot_row_name]]
    } else NA_character_
    
    st <- .extract_state(after_tbl)
    row <- data.frame(
      iter = k,
      pivot_col = piv_cols[k],
      pivot_row = piv_rows[k],
      enter = enter_var,
      leave = leave_var,
      z = st$z,
      check.names = FALSE
    )
    if (length(st$x)) {
      xdf <- as.data.frame(as.list(st$x))
      row <- cbind(row, xdf[, order(names(xdf)), drop = FALSE])
    }
    summ_list[[k]] <- row
  }
  summary <- if (length(summ_list)) do.call(rbind, summ_list) else
    data.frame(iter = integer(0), pivot_col = integer(0), pivot_row = integer(0),
               enter = character(0), leave = character(0), z = numeric(0))
  
  status <- if (length(status_idx)) {
    sub('^Status:\\s*(.*)$', "\\1", lines_logic[status_idx[length(status_idx)]])
  } else NA_character_
  attr(summary, "status") <- status
  
  list(
    initial    = init_block,
    iterations = iter_tbls,
    basis      = list(initial = basis_initial, iterations = basis_iters),
    summary    = summary,
    result     = res
  )
}

# =========================
# Example (your problem)
# =========================
library(simplexR)

# # optimal solution: x = (7, 8), z = 37
# A <- matrix(c(
#   2, 1,
#   1, 2,
#   4, 1
# ), nrow = 3, byrow = TRUE,
# dimnames = list(c("R1","R2","R3"), c("x1","x2")))
# b <- c(22, 23, 40)
# c <- c(3, 2)
# 
# cap <- capture_simplexR(A, b, c)
# 
# # Objects you can use:
# cap$initial                   # initial tableau
# cap$iterations$iter_1         # tableau after iter 1
# cap$basis$initial             # basis before any pivot
# cap$basis$iterations$iter_1   # basis after iter 1
# cap$summary                   # iter, pivot_col/row, enter/leave, z, x's
# attr(cap$summary, "status")   # "End"
# cap$result                    # original simplexR result
