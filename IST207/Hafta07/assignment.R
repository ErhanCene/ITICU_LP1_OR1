# =========================================================
# assignment.R — Hungarian (Kuhn) assignment with
# transportation-like HTML outputs, FULL trace,
# Big-M tokens ("M", "-M"), and visible cover LINES.
# =========================================================

.eps <- 1e-12
`%||%` <- function(a,b) if (is.null(a)) b else a

# -------- Symbolic expressions a*M + b (a in {-1,0,1}, b in Z) --------
.sym_from_string <- function(s) {
  s <- trimws(s)
  if (s %in% c("", "&mdash;")) return(c(0, 0))              # treat blank as 0
  if (s %in% c("M","+M"))        return(c(1, 0))
  if (s == "-M")                 return(c(-1, 0))
  if (grepl("^M[+-][0-9]+$", s)) {
    off <- as.integer(sub("^M([+-][0-9]+)$","\\1", s))
    return(c(1, off))
  }
  # plain integer (we’ll round if given like "15")
  v <- suppressWarnings(as.numeric(s))
  if (is.na(v)) stop(sprintf("Unsupported symbolic token: '%s'", s))
  return(c(0, as.integer(round(v))))
}

.sym_to_string <- function(a, b) {
  # Simplify formatting
  if (a == 0) return(as.character(b))
  if (b == 0) return(if (a==1) "M" else "-M")
  if (a == 1) return(sprintf("M%+d", b))
  if (a == -1) return(sprintf("-M%+d", b))
  # (We don’t generate a ∉ {-1,0,1}, but keep a fallback)
  sprintf("%d*M%+d", a, b)
}

# apply +k / -k to an entire symbolic matrix (k is scalar integer/numeric)
.sym_add_scalar <- function(sym_mat, k) {
  if (k == 0) return(sym_mat)
  sym_mat[,2] <- sym_mat[,2] + as.integer(round(k))
  sym_mat
}

# convert a whole character matrix to 2-column integer matrix [a,b]
.sym_from_disp_matrix <- function(disp) {
  nr <- nrow(disp); nc <- ncol(disp)
  out <- matrix(0L, nr*nc, 2)
  k <- 1L
  for (i in 1:nr) for (j in 1:nc) {
    out[k,] <- .sym_from_string(disp[i,j]); k <- k+1L
  }
  dim(out) <- c(nr*nc, 2)
  # store rowwise then reshape to [nr*nc,2]; we’ll index by linear i
  attr(out, "nr") <- nr; attr(out, "nc") <- nc
  out
}
# reshape helpers
.sym_get_rc <- function(sym, nr, nc) matrix(sym, nrow=nr*nc, ncol=2)
.sym_to_disp <- function(sym, nr, nc) {
  out <- matrix("", nr, nc)
  k <- 1L
  for (i in 1:nr) for (j in 1:nc) {
    out[i,j] <- .sym_to_string(sym[k,1], sym[k,2]); k <- k+1L
  }
  out
}


# ---------------------------------------------------------
# Visuals (transportation style + prime/path + cover lines)
# ---------------------------------------------------------
.assignment_css <- "
<style>
  :root{
    --tbl-font: system-ui, Segoe UI, Roboto, Arial, sans-serif;
    --grid: #d9dee7;
    --head-bg: #eef5ff;
    --head-txt:#233042;
    --cell-txt:#1f2937;
    --muted:#6b7280;
    --alloc-bg:#fff3cd;  /* star */
    --alloc-br:#f1c40f;
    --dead-bg:#fff0f0;   /* light tint for covered rows/cols */
    --zero-bg:#f3fff3;   /* hint for zeros */
    --line: rgba(200, 30, 30, 0.7); /* semi-transparent red */
  }

  table.tg{
    border-collapse:collapse;border-spacing:0;width:auto;
    font-family:var(--tbl-font);font-size:14px; line-height:1.2;
    margin:10px 0;
  }
  .tg th, .tg td{
    border:1px solid var(--grid);
    padding:8px 10px; text-align:center; vertical-align:middle;
    color:var(--cell-txt); white-space:nowrap;
    position:relative;
  }
  .tg thead th{ background:var(--head-bg); color:var(--head-txt); font-weight:600; }
  .row-hdr{ background:var(--head-bg); color:var(--head-txt); font-weight:600; }

  .alloc{ background:var(--alloc-bg); border:2px solid var(--alloc-br); font-weight:700; } /* starred */
  .prime{ border:2px dashed #4285f4; background:#e8f0fe; }                                /* primed */
  .zero{  background:var(--zero-bg); }                                                    /* plain zero */

  /* soft tint when covered (optional) */
  .dead{  background:var(--dead-bg); }

  /* augmenting path outline */
  .path { outline: 2px dashed #16a34a; outline-offset:-2px; }

  /* draw cover LINES through cell centers */
   .line-row::after{
    content:''; position:absolute; left:4px; right:4px; top:50%;
    height:2px; background:var(--line); transform:translateY(-50%);
    pointer-events:none;
    opacity:0.4;
  }
  .line-col::before{
    content:''; position:absolute; top:4px; bottom:4px; left:50%;
    width:2px; background:var(--line); transform:translateX(-50%);
    pointer-events:none;
    opacity:0.4;
  }
  .line-both::after{
    content:''; position:absolute; left:4px; right:4px; top:50%;
    height:2px; background:var(--line); transform:translateY(-50%);
    pointer-events:none;
    opacity:0.4;
  }
  .line-both::before{
    content:''; position:absolute; top:4px; bottom:4px; left:50%;
    width:2px; background:var(--line); transform:translateX(-50%);
    pointer-events:none;
    opacity:0.4;
  }


  .cell-wrap{display:flex; flex-direction:column; align-items:center;}
  .sub{ font-size:12px; color:var(--muted); margin-top:2px; }

  .sum-hdr{ background:var(--head-bg); color:var(--head-txt); font-weight:600; }
  .sum-cell{ font-weight:600; }

  .note{margin:6px 0; color:#374151}
  .badge{display:inline-block;padding:2px 8px;border-radius:999px;background:#eee;font-size:12px}
</style>
"

# ---------------------------------------------------------
# HTML writer (now supports tokenized display & line classes)
# ---------------------------------------------------------
write_html_table <- function(fname, title, mat,
                             stars = NULL, primes = NULL,
                             cover_rows = NULL, cover_cols = NULL,
                             row_labels = NULL, col_labels = NULL,
                             note = NULL,
                             sublabels = NULL,         # optional matrix of strings e.g. "(m=2)"
                             show_dash_for_na = TRUE,  # print — for NA
                             zero_hint = TRUE,
                             path_mask = NULL,         # optional TRUE/FALSE matrix for augmenting path outline
                             display = NULL            # optional same-size matrix of strings to *display*
) {
  
  stopifnot(is.matrix(mat))
  nr <- nrow(mat); nc <- ncol(mat)
  stars       <- stars       %||% matrix(FALSE, nr, nc)
  primes      <- primes      %||% matrix(FALSE, nr, nc)
  cover_rows  <- cover_rows  %||% rep(FALSE, nr)
  cover_cols  <- cover_cols  %||% rep(FALSE, nc)
  row_labels  <- row_labels  %||% paste0("r", seq_len(nr))
  col_labels  <- col_labels  %||% paste0("c", seq_len(nc))
  sublabels   <- sublabels   %||% matrix("", nr, nc)
  path_mask   <- path_mask   %||% matrix(FALSE, nr, nc)
  display     <- display     %||% matrix("", nr, nc)
  
  use_display <- !all(display == "")
  
  cell_html <- function(i,j){
    v <- mat[i,j]
    if (use_display && nzchar(display[i,j])) {
      txt <- display[i,j]
    } else {
      txt <- if (is.na(v) && show_dash_for_na) "&mdash;" else formatC(v, format="fg", digits=8)
    }
    
    cls <- c()
    if (stars[i,j])  cls <- c(cls,"alloc")
    if (primes[i,j]) cls <- c(cls,"prime")
    if (zero_hint && !stars[i,j] && !is.na(v) && abs(v) < .eps) cls <- c(cls,"zero")
    
    # Visual tint for covers
    if (cover_rows[i] || cover_cols[j]) cls <- c(cls,"dead")
    
    # Line classes (draw the actual row/col lines)
    if (cover_rows[i] && cover_cols[j])      cls <- c(cls, "line-both")
    else if (cover_rows[i])                  cls <- c(cls, "line-row")
    else if (cover_cols[j])                  cls <- c(cls, "line-col")
    
    if (path_mask[i,j]) cls <- c(cls,"path")
    
    subtxt <- sublabels[i,j]
    if (nzchar(subtxt)){
      sprintf(
        '<td class="%s"><div class="cell-wrap"><div>%s</div><div class="sub">%s</div></div></td>',
        paste(cls, collapse=" "), txt, subtxt
      )
    } else {
      sprintf('<td class="%s">%s</td>', paste(cls, collapse=" "), txt)
    }
  }
  
  hdr <- paste0(
    "<!doctype html><meta charset='utf-8'>",
    .assignment_css,
    sprintf(""),
    if (!is.null(note)) sprintf("<div class='note'><span class='badge'>note</span> %s</div>", note) else ""
  )
  
  thead <- paste0(
    "<thead><tr><th></th>",
    paste(sprintf('<th class="sum-hdr">%s</th>', col_labels), collapse=""),
    "</tr></thead>"
  )
  
  body_rows <- lapply(seq_len(nr), function(i){
    paste0("<tr>",
           sprintf('<th class="row-hdr">%s</th>', row_labels[i]),
           paste(vapply(seq_len(nc), function(j) cell_html(i,j), ""), collapse=""),
           "</tr>")
  })
  
  html <- paste0(hdr, "<table class='tg'>", thead, "<tbody>",
                 paste(body_rows, collapse=""), "</tbody></table>")
  writeLines(html, fname)
  invisible(fname)
}

# ---------------------------------------------------------
# Big-M parsing
#  - Accepts numeric matrix OR character matrix with tokens:
#    "M", "-M", numbers (e.g. "7"), or blanks (→ NA if desired).
# ---------------------------------------------------------
parse_bigM_matrix <- function(x, bigM=1e9, na_empty=FALSE){
  stopifnot(is.matrix(x))
  if (is.numeric(x)) {
    # numeric: no original display; build from numbers for symbolics (a=0, b=val)
    nr <- nrow(x); nc <- ncol(x)
    disp <- matrix(as.character(as.integer(round(x))), nr, nc)
    num  <- x
  } else {
    nr <- nrow(x); nc <- ncol(x)
    disp <- matrix("", nr, nc)
    num  <- matrix(NA_real_, nr, nc)
    for (i in 1:nr) for (j in 1:nc) {
      s <- trimws(as.character(x[i,j]))
      disp[i,j] <- if (s == "") if (na_empty) "" else "&mdash;" else s
      if (s %in% c("M","+M"))         num[i,j] <-  bigM
      else if (s == "-M")             num[i,j] <- -bigM
      else if (grepl("^M[+-][0-9]+$", s)) {
        off <- as.numeric(sub("^M([+-][0-9]+)$", "\\1", s))
        num[i,j] <- bigM + off
      } else if (s == "" && na_empty) num[i,j] <- NA_real_
      else {
        val <- suppressWarnings(as.numeric(s))
        if (is.na(val)) stop(sprintf("Cannot parse cell (%d,%d): '%s'. Allowed: numbers, 'M', '-M', 'M±k'.", i, j, s))
        num[i,j] <- val
      }
    }
  }
  # symbolic matrix (a*M + b) from display
  sym <- .sym_from_disp_matrix(disp)
  list(num=num, disp=disp, sym=sym)
}




# ---------------------------------------------------------
# Helpers
# ---------------------------------------------------------
assignment_value <- function(cost, assign) {
  i <- seq_along(assign)
  sum(cost[cbind(i, assign)], na.rm=TRUE)
}

pad_to_square <- function(cost, pad_value=0) {
  nr <- nrow(cost); nc <- ncol(cost)
  if (nr == nc) return(cost)
  if (nr < nc) rbind(cost, matrix(pad_value, nc - nr, nc))
  else         cbind(cost, matrix(pad_value, nr, nr - nc))
}


# Enumerate all perfect matchings on a boolean zero graph Z
# Returns a list of integer vectors 'assign' where assign[i] = chosen column for row i
.enum_all_matchings <- function(Z, limit = 2000L) {
  nr <- nrow(Z); nc <- ncol(Z)
  used <- rep(FALSE, nc)
  cur  <- rep(NA_integer_, nr)
  out  <- list()
  
  # Explore rows with fewer options first
  row_order <- order(rowSums(Z))
  Zs <- Z[row_order, , drop = FALSE]
  
  rec <- function(i) {
    if (length(out) >= limit) return(invisible())
    if (i > nr) {
      # IMPORTANT: return the current assignment as-is (already in original row indices)
      out[[length(out) + 1L]] <<- as.integer(cur)
      return(invisible())
    }
    choices <- which(Zs[i, ] & !used)
    if (!length(choices)) return(invisible())
    
    r <- row_order[i]  # original row index for this search depth
    for (j in choices) {
      used[j] <<- TRUE
      cur[r]  <<- j
      rec(i + 1L)
      used[j] <<- FALSE
      cur[r]  <<- NA_integer_
      if (length(out) >= limit) return(invisible())
    }
  }
  rec(1L)
  out
}


# Exhaustive BnB on ORIGINAL cost matrix for small n; returns all assignments with cost == best_obj
.enum_all_matchings_exhaustive <- function(cost, best_obj, tol = 1e-9,
                                           limit = 50000L, n_max = 9L,
                                           inf_cut = 1e15) {
  nr <- nrow(cost); nc <- ncol(cost)
  if (nr != nc || nr > n_max) return(list())
  
  cols_used <- rep(FALSE, nc)
  assign    <- rep(NA_integer_, nr)
  out       <- list()
  
  # Explore rows with smaller mins first
  row_order <- order(apply(cost, 1, min, na.rm = TRUE))
  
  rec <- function(k, partial_sum) {
    if (length(out) >= limit) return(invisible())
    if (k > nr) {
      if (abs(partial_sum - best_obj) <= tol) {
        # IMPORTANT: return 'assign' as-is (already in original row indices)
        out[[length(out) + 1L]] <<- as.integer(assign)
      }
      return(invisible())
    }
    
    # quick LB with remaining rows
    lb <- 0
    for (kk in k:nr) {
      ii <- row_order[kk]
      m <- Inf
      for (j in 1:nc) if (!cols_used[j]) {
        v <- cost[ii, j]
        if (!is.na(v) && v < inf_cut && v < m) m <- v
      }
      lb <- lb + m
      if (!is.finite(m)) { lb <- Inf; break }
    }
    if (!is.finite(lb) || partial_sum + lb - 1e-12 > best_obj + tol) return(invisible())
    
    i <- row_order[k]
    for (j in 1:nc) if (!cols_used[j]) {
      v <- cost[i, j]
      if (is.na(v) || v >= inf_cut) next
      cols_used[j] <<- TRUE
      assign[i]    <<- j
      rec(k + 1L, partial_sum + v)
      cols_used[j] <<- FALSE
      assign[i]    <<- NA_integer_
      if (length(out) >= limit) return(invisible())
    }
  }
  rec(1L, 0)
  out
}


# ---------------------------------------------------------
# Hungarian core for minimization (square) with detailed trace
# ---------------------------------------------------------
hungarian_min_core <- function(cost, disp=NULL, bigM=1e9) {
  n <- nrow(cost); if (n != ncol(cost)) stop("Hungarian requires square matrix.")
  
  # symbolic layer (a*M + b) as a flat [n*n,2] matrix
  if (is.null(disp)) disp <- matrix(as.character(as.integer(round(cost))), n, n)
  sym <- .sym_from_disp_matrix(disp)
  nr <- n; nc <- n
  
  starred <- matrix(FALSE, n, n)
  primed  <- matrix(FALSE,  n, n)
  row_covered <- rep(FALSE, n)
  col_covered <- rep(FALSE, n)
  tr <- list()
  
  # helper to get current display strings from sym
  current_display <- function() .sym_to_disp(sym, nr, nc)
  
  # Step 1: Row reduction → subtract rmin from each row (update numeric and symbolic)
  rmin <- apply(cost, 1, min); cost <- cost - rmin
  for (i in 1:n) { # b := b - rmin[i] for row i
    for (j in 1:n) {
      k <- (i-1)*n + j
      sym[k,2] <- sym[k,2] - as.integer(round(rmin[i]))
    }
  }
  tr[[length(tr)+1]] <- list(stage="", mat=cost, display=current_display())
  
  # Step 2: Column reduction
  cmin <- apply(cost, 2, min); cost <- cost - rep(cmin, each=n)
  for (j in 1:n) {
    for (i in 1:n) {
      k <- (i-1)*n + j
      sym[k,2] <- sym[k,2] - as.integer(round(cmin[j]))
    }
  }
  tr[[length(tr)+1]] <- list(stage="", mat=cost, display=current_display())
  
  # Step 3: Initial independent stars
  used_col <- rep(FALSE, n)
  for (i in 1:n) {
    js <- which(abs(cost[i,]) < .eps & !used_col)
    if (length(js)) { j <- js[1]; starred[i,j] <- TRUE; used_col[j] <- TRUE }
  }
  tr[[length(tr)+1]] <- list(stage="", mat=cost,
                             starred=starred, display=current_display())
  
  # Step 4: Cover columns with starred zeros
  col_covered <- apply(starred, 2, any)
  tr[[length(tr)+1]] <- list(stage="", mat=cost,
                             starred=starred, cover_cols=col_covered, display=current_display())
  
  find_uncovered_zero <- function() {
    for (i in 1:n) if (!row_covered[i]) {
      js <- which(abs(cost[i,]) < .eps & !col_covered)
      if (length(js)) return(c(i, js[1]))
    }
    NULL
  }
  
  while (sum(col_covered) < n) {
    z <- NULL
    repeat {
      z <- find_uncovered_zero()
      if (is.null(z)) break
      i <- z[1]; j <- z[2]; primed[i,j] <- TRUE
      tr[[length(tr)+1]] <- list(stage="", mat=cost,
                                 starred=starred, primed=primed,
                                 cover_rows=row_covered, cover_cols=col_covered,
                                 display=current_display())
      
      if (!any(starred[i,])) {
        # build and flip augmenting path
        path <- list(c(i,j)); ip <- i; jp <- j; done <- FALSE
        while(!done) {
          i_star <- which(starred[, jp])
          if (length(i_star) == 1) {
            path[[length(path)+1]] <- c(i_star, jp)
            j_prime <- which(primed[i_star, ])[1]
            path[[length(path)+1]] <- c(i_star, j_prime)
            ip <- i_star; jp <- j_prime
          } else done <- TRUE
        }
        path_mask <- matrix(FALSE, n, n); for (p in path) path_mask[p[1],p[2]] <- TRUE
        tr[[length(tr)+1]] <- list(stage="", mat=cost,
                                   starred=starred, primed=primed, path_mask=path_mask,
                                   cover_rows=row_covered, cover_cols=col_covered,
                                   display=current_display())
        for (k in seq_along(path)) {
          p <- path[[k]]
          if (k %% 2 == 1) starred[p[1], p[2]] <- TRUE else starred[p[1], p[2]] <- FALSE
        }
        primed[,] <- FALSE; row_covered[] <- FALSE; col_covered[] <- FALSE
        col_covered <- apply(starred, 2, any)
        tr[[length(tr)+1]] <- list(stage="", mat=cost,
                                   starred=starred, cover_cols=col_covered,
                                   display=current_display())
        break
      } else {
        j_star <- which(starred[i,])
        row_covered[i] <- TRUE; col_covered[j_star] <- FALSE
        tr[[length(tr)+1]] <- list(stage="",
                                   mat=cost, starred=starred, primed=primed,
                                   cover_rows=row_covered, cover_cols=col_covered,
                                   display=current_display())
      }
    }
    
    if (is.null(z)) {
      # h adjustment
      uncovered <- cost[!row_covered, !col_covered, drop=FALSE]
      if (!length(uncovered)) break
      h <- min(uncovered)
      cost[!row_covered, ] <- cost[!row_covered, ] - h
      cost[,  col_covered] <- cost[,  col_covered] + h
      # symbolic updates
      for (i in which(!row_covered)) for (j in 1:n) {
        k <- (i-1)*n + j; sym[k,2] <- sym[k,2] - as.integer(round(h))
      }
      for (j in which(col_covered)) for (i in 1:n) {
        k <- (i-1)*n + j; sym[k,2] <- sym[k,2] + as.integer(round(h))
      }
      tr[[length(tr)+1]] <- list(stage="", mat=cost,
                                 starred=starred, primed=primed,
                                 cover_rows=row_covered, cover_cols=col_covered,
                                 display=current_display())
    }
  }
  
  assignment <- integer(n)
  for (i in 1:n) { j <- which(starred[i,]); assignment[i] <- if (length(j)) j[1] else NA_integer_ }
  list(stars=starred, assign=assignment, trace=tr, final_cost = cost)
}


# ---------------------------------------------------------
# Public API
#  - Accepts numeric matrix OR character matrix with "M" and "-M".
#  - bigM controls the numeric magnitude used internally.
# ---------------------------------------------------------
solve_assignment <- function(cost,
                             maximize = FALSE,
                             trace_html = TRUE,
                             file_prefix = "assign",
                             row_labels = NULL,
                             col_labels = NULL,
                             bigM = 1e9,
                             display_M = c("final_and_initial","initial_only","always"),
                             show_notes = FALSE,
                             enumerate_all_optima = TRUE,
                             max_opt_solutions = 2000L,
                             zero_tol = 1e-8) {
  stopifnot(is.matrix(cost))
  display_M <- match.arg(display_M)
  
  # ---------- NEW: capture "raw" display with blanks as '—' ----------
  make_raw_page <- TRUE
  if (!is.numeric(cost)) {
    cost_raw_chr <- as.matrix(cost)
    raw_blanks   <- is.na(cost_raw_chr) | trimws(cost_raw_chr) == ""
    raw_disp     <- matrix("", nrow(cost_raw_chr), ncol(cost_raw_chr))
    for (i in seq_len(nrow(cost_raw_chr))) for (j in seq_len(ncol(cost_raw_chr))) {
      s <- cost_raw_chr[i,j]
      raw_disp[i,j] <- if (is.na(s) || trimws(s) == "") "&mdash;" else as.character(s)
    }
    # auto-fill blanks with M / -M for the solver
    cost_filled_chr <- cost_raw_chr
    if (any(raw_blanks)) cost_filled_chr[raw_blanks] <- if (maximize) "-M" else "M"
    parsed <- parse_bigM_matrix(cost_filled_chr, bigM = bigM)
  } else {
    cost_raw_num <- cost
    raw_blanks   <- is.na(cost_raw_num)
    raw_disp     <- matrix(as.character(as.integer(round(cost_raw_num))), nrow(cost_raw_num), ncol(cost_raw_num))
    if (any(raw_blanks)) raw_disp[raw_blanks] <- "&mdash;"           # show em-dash for students
    # auto-fill NA for the solver
    if (anyNA(cost_raw_num)) cost_raw_num[is.na(cost_raw_num)] <- if (maximize) -bigM else bigM
    parsed <- parse_bigM_matrix(cost_raw_num, bigM = bigM)            # builds numeric+display
  }
  # -------------------------------------------------------------------
  
  num_cost <- parsed$num
  disp0    <- parsed$disp  # symbolic strings for the "initial" & "final" pages
  
  # helper to pad a display matrix to square for internal stages
  pad_display <- function(d, nr, nc, tr){
    if (is.null(d)) return(NULL)
    dd <- d
    if (nrow(d) < tr) dd <- rbind(d, matrix("0", tr - nrow(d), ncol(d)))
    if (ncol(dd) < tr) dd <- cbind(dd, matrix("0", nrow(dd), tr - ncol(dd)))
    dd
  }
  
  nr <- nrow(num_cost); nc <- ncol(num_cost)
  row_labels <- row_labels %||% paste0("r", seq_len(nr))
  col_labels <- col_labels %||% paste0("c", seq_len(nc))
  original_cost <- num_cost
  
  # --- MAX uses row-wise transform: T[i,j] = row_max[i] - original[i,j] ---
  disp_for_solver <- NULL
  if (maximize) {
    rmax <- apply(original_cost, 1, max, na.rm = TRUE)
    
    # numeric matrix for the solver
    num_cost <- matrix(rmax, nrow = nr, ncol = nc) - original_cost
    # (abs() would be redundant; rmax - C >= 0 always)
    
    # symbolic layer for stage pages: (a*M + b) -> (-a)*M + (rmax_i - b)
    if (!is.null(disp0)) {
      sym <- .sym_from_disp_matrix(disp0)   # [nr*nc, 2] holding (a,b)
      disp_new <- matrix("", nr, nc)
      k <- 1L
      for (i in 1:nr) {
        rb <- as.integer(round(rmax[i]))
        for (j in 1:nc) {
          a <- sym[k,1]; b <- sym[k,2]
          disp_new[i,j] <- .sym_to_string(-a, rb - b)
          k <- k + 1L
        }
      }
      disp_for_solver <- disp_new
    }
  } else {
    # MIN: show the original symbolic layer during stages
    disp_for_solver <- disp0
  }
  
  # padded   <- (nrow(num_cost) != ncol(num_cost))
  # cost_sq  <- pad_to_square(num_cost, pad_value = 0)
  # disp_pad <- if (!is.null(disp_for_solver))
  # { dd <- disp_for_solver
  # if (nrow(dd) < nrow(cost_sq)) dd <- rbind(dd, matrix("0", nrow(cost_sq)-nrow(dd), ncol(dd)))
  # if (ncol(dd) < ncol(cost_sq)) dd <- cbind(dd, matrix("0", nrow(dd), ncol(cost_sq)-ncol(dd)))
  # dd
  # } else NULL
  padded <- (nrow(num_cost) != ncol(num_cost))
  
  pad_maxaware <- function(num_mat, disp_mat, maximize, rmax_vec = NULL){
    nr <- nrow(num_mat); nc <- ncol(num_mat)
    if (nr == nc) return(list(num = num_mat, disp = disp_mat))
    
    # how many to add?
    add_rows <- max(0L, nc - nr)
    add_cols <- max(0L, nr - nc)
    
    num_out <- num_mat
    disp_out <- disp_mat
    
    if (add_rows > 0L) {
      # extra dummy rows: cost 0 in transformed space for both MIN and MAX
      num_out <- rbind(num_out, matrix(0, nrow = add_rows, ncol = ncol(num_out)))
      if (!is.null(disp_out)) {
        disp_out <- rbind(disp_out, matrix("0", nrow = add_rows, ncol = ncol(disp_out)))
      }
    }
    if (add_cols > 0L) {
      if (maximize) {
        if (is.null(rmax_vec)) stop("Internal: rmax_vec required for MAX padding.")
        # each dummy column is rmax[i] in row i
        pad_num <- matrix(rep(as.numeric(rmax_vec), add_cols), nrow = nr, ncol = add_cols)
        num_out <- cbind(num_out, pad_num)
        if (!is.null(disp_out)) {
          pad_disp <- matrix(rep(as.character(as.integer(round(rmax_vec))), add_cols),
                             nrow = nr, ncol = add_cols)
          disp_out <- cbind(disp_out, pad_disp)
        }
      } else {
        # MIN: dummy columns are zeros
        num_out <- cbind(num_out, matrix(0, nrow = nrow(num_out), ncol = add_cols))
        if (!is.null(disp_out)) {
          disp_out <- cbind(disp_out, matrix("0", nrow = nrow(disp_out), ncol = add_cols))
        }
      }
    }
    list(num = num_out, disp = disp_out)
  }
  
  # build rmax from ORIGINAL costs when MAX; NULL otherwise
  rmax_vec <- if (maximize) apply(original_cost, 1, max, na.rm = TRUE) else NULL
  pad_res  <- pad_maxaware(num_cost, disp_for_solver, maximize = maximize, rmax_vec = rmax_vec)
  
  cost_sq  <- pad_res$num
  disp_pad <- pad_res$disp
  
  # original matrix padded with zeros (for teaching pages)
  orig_sq      <- pad_to_square(original_cost, pad_value = 0)
  orig_disp_pad <- {
    if (!is.null(disp0)) {
      dd <- disp0
      if (nrow(dd) < nrow(orig_sq)) dd <- rbind(dd, matrix("0", nrow(orig_sq)-nrow(dd), ncol(dd)))
      if (ncol(dd) < ncol(orig_sq)) dd <- cbind(dd, matrix("0", nrow(dd), ncol(orig_sq)-ncol(dd)))
      dd
    } else NULL
  }
  
  pad_row_labels <- if (nrow(cost_sq) > nr) c(row_labels, paste0("dummy_r", seq_len(nrow(cost_sq)-nr))) else row_labels
  pad_col_labels <- if (ncol(cost_sq) > nc) c(col_labels, paste0("dummy_c", seq_len(ncol(cost_sq)-nc))) else col_labels
  
  
  files <- character(0)
  
  # -------------- write the "before initial" raw page --------------
  if (trace_html && make_raw_page) {
    fraw <- sprintf("%s_00_before_initial.html", file_prefix)
    
    # CSS that affects only this file:
    #  - make data cells white
    #  - keep header blues (already in .assignment_css)
    #  - hide the note badge/wrapper so nothing is visible
    css_only <- "
<style>
  /* Only the table immediately following this .note in this file */
  .note + table.tg td { background:#ffffff !important; }

  /* optional: keep the note invisible */
  .note .badge { display:none !important; }
  .note { margin:0 !important; padding:0 !important; height:0 !important; overflow:hidden !important; }
</style>"
    
    
    write_html_table(
      fraw,
      title = "",
      mat = matrix(0, nr, nc),
      row_labels = row_labels,
      col_labels = col_labels,
      note = css_only,         # inject CSS here (no other pages affected)
      display = raw_disp
    )
    files <- c(files, fraw)
  }
  # ----------------------------------------------------------------
  
  
  
  
  
  if (trace_html) {
    f0 <- sprintf("%s_00_initial.html", file_prefix)
    write_html_table(
      f0, sprintf(""),
      mat = original_cost,
      row_labels = row_labels, col_labels = col_labels,
      note = if (show_notes) (if (maximize) "Mode: MAX — internal solver uses transformed costs." else "Mode: MIN") else NULL,
      display = disp0                       # symbolic (M, -M, M±k) after auto-fill
    )
    files <- c(files, f0)
    
    # Extra teaching page: Initial (original domain) WITH dummies shown
    f0a <- sprintf("%s_00a_initial_with_dummies.html", file_prefix)
    write_html_table(
      f0a,
      "",
      mat = orig_sq,
      row_labels = pad_row_labels, col_labels = pad_col_labels,
      note = if (show_notes) "Dummy rows/cols are shown at 0." else NULL,
      display = orig_disp_pad
    )
    files <- c(files, f0a)
    
    
    if (padded) {
      fpad <- sprintf("%s_01_padded_used_by_solver.html", file_prefix)
      pad_row_labels <- if (nrow(cost_sq) > nr) c(row_labels, paste0("dummy_r", seq_len(nrow(cost_sq)-nr))) else row_labels
      pad_col_labels <- if (ncol(cost_sq) > nc) c(col_labels, paste0("dummy_c", seq_len(ncol(cost_sq)-nc))) else col_labels
      write_html_table(
        fpad, "Padded square matrix used internally (after transform if MAX).",
        mat = cost_sq,
        row_labels = pad_row_labels, col_labels = pad_col_labels,
        note = if (show_notes) "Dummy rows/cols added with 0 cost in solver domain." else NULL,
        display = if (display_M == "always") disp_pad else NULL
      )
      files <- c(files, fpad)
    }
  }
  
  # Run core (now returns final reduced matrix too)
  core <- hungarian_min_core(cost_sq, disp = if (!is.null(disp0)) disp_pad else NULL, bigM = bigM)
  
  assign <- core$assign
  if (nrow(num_cost) < ncol(num_cost)) assign <- assign[seq_len(nr)]
  if (ncol(num_cost) < nrow(num_cost)) assign[assign > nc] <- NA_integer_
  
  obj <- assignment_value(original_cost, assign)
  
  
  # ------- NEW: always write the final zero-matrix (solver domain) with M display -------
  if (trace_html) {
    pad_row_labels <- if (nrow(cost_sq) > nr) c(row_labels, paste0("dummy_r", seq_len(nrow(cost_sq)-nr))) else row_labels
    pad_col_labels <- if (ncol(cost_sq) > nc) c(col_labels, paste0("dummy_c", seq_len(ncol(cost_sq)-nc))) else col_labels
    
    # star mask on the padded grid
    make_star_mask_with_dummies <- function(assign_vec, tr, nr, nc) {
      mask <- matrix(FALSE, tr, tr)
      used_dummy_cols <- 0L
      for (i in seq_len(nr)) {
        j <- assign_vec[i]
        if (!is.na(j) && j >= 1L && j <= nc) {
          mask[i, j] <- TRUE
        } else if (tr > nc) {
          used_dummy_cols <- used_dummy_cols + 1L
          mask[i, nc + used_dummy_cols] <- TRUE
        }
      }
      mask
    }
    
    finZ_mat  <- core$final_cost                 # numeric reduced matrix (zeros graph)
    trsz      <- nrow(finZ_mat)
    finZ_star <- make_star_mask_with_dummies(assign, tr = trsz, nr = nr, nc = nc)
    
    # Pull the symbolic display from the LAST trace page
    finZ_disp <- {
      if (length(core$trace)) core$trace[[length(core$trace)]]$display else NULL
    }
    
    fz <- sprintf("%s_%02d_final_zero_matrix.html", file_prefix, length(core$trace)+1)
    write_html_table(
      fz,
      sprintf("Final zero matrix (solver domain) — stars show optimal matching%s",
              if (maximize) " (max transformed to min)" else ""),
      mat        = finZ_mat,
      stars      = finZ_star,
      row_labels = pad_row_labels,
      col_labels = pad_col_labels,
      note       = if (show_notes) "Reduced costs after all adjustments; zeros mark feasible positions. Starred zeros form the optimal assignment." else NULL,
      # >>> This line makes M / -M / M±k appear:
      display    = finZ_disp
    )
    files <- c(files, fz)
  }
  # --------------------------------------------------------------------------------------
  
  
  
  
  if (trace_html) {
    pad_row_labels <- if (nrow(cost_sq) > nr) c(row_labels, paste0("dummy_r", seq_len(nrow(cost_sq)-nr))) else row_labels
    pad_col_labels <- if (ncol(cost_sq) > nc) c(col_labels, paste0("dummy_c", seq_len(ncol(cost_sq)-nc))) else col_labels
    
    for (k in seq_along(core$trace)) {
      tk <- core$trace[[k]]
      fn <- sprintf("%s_%02d_%s.html", file_prefix, k+1, gsub("[^a-z0-9]+","_", tolower(tk$stage)))
      write_html_table(
        fn, sprintf(""),
        mat = tk$mat,
        stars = tk$starred %||% NULL,
        primes = tk$primed  %||% NULL,
        cover_rows = tk$cover_rows %||% NULL,
        cover_cols = tk$cover_cols %||% NULL,
        row_labels = pad_row_labels, col_labels = pad_col_labels,
        note = if (show_notes) (tk$note %||% "") else NULL,
        path_mask = tk$path_mask %||% NULL,
        display = if (display_M == "always") (tk$display %||% NULL) else NULL
      )
      files <- c(files, fn)
    }
  }
  
  # ---------------- enumerate all optimal solutions (dedup fixed) ----------------
  all_assignments <- list(assign)   # include the one we found first
  
  key_of <- function(a) paste(as.integer(a), collapse = ",")
  seen <- new.env(parent = emptyenv())
  assign(key_of(assign), TRUE, envir = seen)  # seed with the primary solution
  
  if (enumerate_all_optima) {
    final_sq <- core$final_cost
    Z <- abs(final_sq[seq_len(nr), seq_len(nc), drop=FALSE]) <= zero_tol
    
    # 1) zeros-graph enumeration
    matchings <- .enum_all_matchings(Z, limit = max_opt_solutions)
    for (m in matchings) {
      k <- key_of(m)
      if (exists(k, envir = seen, inherits = FALSE)) next
      val <- assignment_value(original_cost, m)
      if (is.finite(val) && abs(val - obj) < 1e-9) {
        all_assignments[[length(all_assignments)+1L]] <- m
        assign(k, TRUE, envir = seen)
      }
    }
    
    # 2) fallback exhaustive search (small n) if needed
    if (length(all_assignments) <= 1L) {
      ex_sols <- .enum_all_matchings_exhaustive(
        original_cost, best_obj = obj, tol = 1e-9,
        limit = max_opt_solutions, n_max = 9L, inf_cut = 1e15
      )
      for (m in ex_sols) {
        k <- key_of(m)
        if (exists(k, envir = seen, inherits = FALSE)) next
        all_assignments[[length(all_assignments)+1L]] <- m
        assign(k, TRUE, envir = seen)
      }
    }
  }
  
  # final safety: deduplicate (in case anything slipped through) and
  # present in deterministic lexicographic order
  keys <- vapply(all_assignments, key_of, "")
  ord  <- order(keys)
  keys <- keys[ord]
  all_assignments <- all_assignments[ord]
  # unique by key
  if (length(all_assignments) > 1L) {
    keep <- !duplicated(keys)
    all_assignments <- all_assignments[keep]
  }
  # -------------------------------------------------------------------------------

  # Render finals (one per optimal assignment)
  # Render finals (one per optimal assignment)
  if (trace_html) {
    
    # helper to place stars on the padded original matrix (dummies too)
    make_star_mask_with_dummies <- function(assign_vec, tr, nr, nc) {
      # assign_vec has length nr (real rows), values in 1..nc or NA for dummy
      mask <- matrix(FALSE, tr, tr)
      used_dummy_cols <- 0L
      for (i in seq_len(nr)) {
        j <- assign_vec[i]
        if (!is.na(j) && j >= 1L && j <= nc) {
          mask[i, j] <- TRUE
        } else {
          # place in next dummy column (if any dummy cols exist)
          if (tr > nc) {
            used_dummy_cols <- used_dummy_cols + 1L
            mask[i, nc + used_dummy_cols] <- TRUE
          }
        }
      }
      mask
    }
    
    trsz <- nrow(orig_sq)   # square dimension after padding
    
    for (idx in seq_along(all_assignments)) {
      a <- all_assignments[[idx]]
      
      ## (A) your existing final page on ORIGINAL (no dummies)
      fin <- sprintf("%s_%02d_final_assignment_%02d.html",
                     file_prefix, length(core$trace)+1+idx, idx)
      star_orig <- matrix(FALSE, nr, nc)
      for (i in seq_len(nr)) {
        j <- a[i]
        if (!is.na(j) && j >= 1 && j <= nc) star_orig[i,j] <- TRUE
      }
      write_html_table(
        fin,
        sprintf("Final assignment (%s) — optimal solution %d/%d — objective = %s",
                if (maximize) "maximization" else "minimization",
                idx, length(all_assignments),
                formatC(obj, format="fg", digits=12)),
        mat = original_cost,
        stars = star_orig,
        row_labels = row_labels, col_labels = col_labels,
        note = if (show_notes) NULL else NULL,
        display = disp0
      )
      files <- c(files, fin)
      
      ## (B) EXTRA: final page on ORIGINAL WITH DUMMIES SHOWN
      star_pad <- make_star_mask_with_dummies(a, tr = trsz, nr = nr, nc = nc)
      fpadfin <- sprintf("%s_%02d_final_assignment_with_dummies_%02d.html",
                         file_prefix, length(core$trace)+100, idx)
      write_html_table(
        fpadfin,
        sprintf("Final assignment (original domain) with dummies — optimal solution %d/%d — objective = %s",
                idx, length(all_assignments), formatC(obj, format="fg", digits=12)),
        mat = orig_sq,                        # original matrix padded with zeros
        stars = star_pad,
        row_labels = pad_row_labels, col_labels = pad_col_labels,
        note = if (show_notes) "Dummy rows/cols are shown as 0; stars include dummy matches when needed." else NULL,
        display = orig_disp_pad               # shows zeros in dummy cells
      )
      files <- c(files, fpadfin)
    }
  }
  
  
  list(
    assignment = assign,
    objective  = obj,
    mode       = if (maximize) "max" else "min",
    steps      = vapply(core$trace, function(s) s$stage, ""),
    files      = files,
    all_assignments = all_assignments
  )
}


# Convenience wrappers
solve_assignment_min <- function(cost, ...)   solve_assignment(cost, maximize = FALSE, ...)
solve_assignment_max <- function(profit, ...) solve_assignment(profit, maximize = TRUE,  ...)

