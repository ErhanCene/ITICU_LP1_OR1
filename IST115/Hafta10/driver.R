source('bigM.R')
`%||%` <- function(a, b) if (is.null(a)) b else a


simplex_export_all_panels <- function(
    cap,
    out_dir = ".",
    base_name = "simplex",
    digits = 2,
    # --- initial/before panel (mask Z rows, hide ratio col) ---
    initial_mask_rows = c("Zj","Zj-Cj"),
    initial_mask_ratio = TRUE,
    initial_highlight_ratio_col = TRUE,
    # --- write panel ---
    write_mask_rows = character(0),
    write_mask_ratio = TRUE,         # show ratio values
    # --- enter col panel ---
    
    # --- select panel (show ratios, auto decide enter/leave) ---
    select_ratio_enter = "auto",          # or "x2", ...
    select_mask_rows   = character(0),    # e.g. show Z rows so user sees Zj-Cj
    select_mask_ratio  = FALSE,           # show ratios here
    select_highlight_ratio_col = TRUE,
    select_enter_col_style = "frame",     # "frame"/"tint"/"none"
    # --- update_row panel (normalize pivot row only) ---
    update_row_enter_col_style = "frame",
    update_row_mask_rows = c("Zj","Zj-Cj"),
    update_row_mask_ratio = TRUE,
    # --- eliminate + objective panel ---
    after_mask_rows   = character(0),     # usually show Z rows now
    after_mask_ratio  = TRUE,             # blank ratio col after pivot
    after_enter_col_style = "frame",
    after_highlight_base_row = FALSE,     # often you asked to hide purple band here
    # --- general visuals ---
    highlight_pivot = TRUE,
    color_sign = TRUE,                     # green/red for Zj & Zj-Cj
    mathjax_wrap = TRUE,
    continue_after_optimal = FALSE,  # if TRUE, keep pivoting after optimal
    continue_steps = 0,              # how many extra steps to do
    post_opt_manual = NULL           # list of length <= continue_steps; each:
    #   list(enter="x2", leave="s1") ; 'leave' optional
){
  stopifnot(is.list(cap), length(cap$iterations) >= 1)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  # helper to make a per-iteration "cap_k" that starts from tableau before kth pivot
  get_cap_k <- function(k){
    cap_k <- cap
    if (k == 1) {
      cap_k$initial <- cap$initial
    } else {
      # use AFTER tableau from previous iteration as new "initial"
      cap_k$initial <- cap$iterations[[k-1]]
    }
    cap_k
  }
  
  pad2 <- function(n) sprintf("%02d", n)
  n_iter  <- length(cap$iterations)
  n_steps <- length(cap$steps)          # <-- ADD THIS
  
  
  outputs <- vector("list", n_iter)
  
  for (k in seq_len(n_iter)) {
    has_step <- (k <= n_steps)            # <-- ADD THIS
    cap_k <- get_cap_k(k)
    it_tag <- paste0("iter", pad2(k))
    
    # Panel 1: BEFORE (initial for this round) – mask Z rows, hide ratios
    html_before <- simplex_html_initial(
      cap_k, digits = digits,
      add_ratio_col = TRUE,
      mask_rows     = initial_mask_rows,
      mask_ratio    = initial_mask_ratio,
      ratio_enter   = "auto",                    # we still auto-choose for column shading
      choose_enter_if_masked = TRUE,
      highlight_col = NULL,                    # shade the chosen entering column
      highlight_row = NULL,                      # don't mark leave row yet
      highlight_ratio_col = if (isTRUE(initial_highlight_ratio_col)) TRUE else FALSE,
      highlight_pivot = isTRUE(highlight_pivot),
      color_sign = isTRUE(color_sign),
      # do not box values here
      mark_enter_leave_values = FALSE,
      math_labels = TRUE,
      mathjax_wrap = mathjax_wrap,
      symbolic_M = TRUE,  
      M_label = "M"    
    )
    f_before <- file.path(out_dir, sprintf("%s_%s_01_before.html",base_name, it_tag))
    writeLines(html_before, f_before)
    
    
    # Panel 2: WRITE ZJ CJ
    
    html_write <- simplex_html_initial(
      cap_k, digits = digits,
      add_ratio_col = TRUE,
      mask_rows     = write_mask_rows,
      mask_ratio    = write_mask_ratio,         # show ratio values
      ratio_enter   = 'auto',        # "auto" or "xk"
      auto_compute_ratios = TRUE,
      choose_enter_if_masked = TRUE,
      highlight_col = NULL,
      highlight_row = NULL,                    # auto highlight the leaving row (min positive ratio)
      highlight_pivot = isTRUE(highlight_pivot),
      highlight_ratio_col = TRUE,
      color_sign = isTRUE(color_sign),
      mark_enter_leave_values = FALSE,            # keep for back-compat
      mark_enter_value = FALSE,
      mark_leave_value = FALSE,
      math_labels = TRUE,
      mathjax_wrap = mathjax_wrap,
      symbolic_M = TRUE,  
      M_label = "M"    
    )
    f_write <- file.path(out_dir, sprintf("%s_%s_02_write.html", base_name, it_tag))
    writeLines(html_write, f_write)
    
    
    # Panel 3: ENTER-COL
    
    html_enter_col <- simplex_html_initial(
      cap_k, digits = digits,
      add_ratio_col = TRUE,
      mask_rows     = character(0),
      mask_ratio    = TRUE,         # show ratio values
      ratio_enter   = 'auto',        # "auto" or "xk"
      auto_compute_ratios = TRUE,
      choose_enter_if_masked = TRUE,
      highlight_col = 'auto',
      highlight_row = NULL,                    # auto highlight the leaving row (min positive ratio)
      highlight_pivot = isTRUE(highlight_pivot),
      highlight_ratio_col = TRUE,
      color_sign = isTRUE(color_sign),
      mark_enter_value = TRUE,
      mark_leave_value = FALSE,
      math_labels = TRUE,
      mathjax_wrap = mathjax_wrap,
      symbolic_M = TRUE,  
      M_label = "M"    
    )
    f_enter_col <- file.path(out_dir, sprintf("%s_%s_03_enter_col.html", base_name, it_tag))
    writeLines(html_enter_col, f_enter_col)
    
    
    # Panel 4: SELECT (show ratios, box min Zj-Cj & min ratio)
    html_select <- simplex_html_initial(
      cap_k, digits = digits,
      add_ratio_col = TRUE,
      mask_rows     = select_mask_rows,
      mask_ratio    = select_mask_ratio,         # show ratio values
      ratio_enter   = select_ratio_enter,        # "auto" or "xk"
      auto_compute_ratios = TRUE,
      choose_enter_if_masked = TRUE,
      highlight_col = "auto",
      highlight_row = "auto",                    # auto highlight the leaving row (min positive ratio)
      highlight_pivot = isTRUE(highlight_pivot),
      highlight_ratio_col = TRUE,
      color_sign = isTRUE(color_sign),
      mark_enter_leave_values = TRUE,            # keep for back-compat
      mark_enter_value = TRUE,
      mark_leave_value = TRUE,
      math_labels = TRUE,
      mathjax_wrap = mathjax_wrap,
      symbolic_M = TRUE,  
      M_label = "M"    
    )
    f_select <- file.path(out_dir, sprintf("%s_%s_04_select.html", base_name, it_tag))
    writeLines(html_select, f_select)
    
    # Panel 5: UPDATE_BASE_ROW (normalize pivot row only; keep ratios visible)
    if (has_step) {
      html_update_base <- simplex_html_step_update_enter_row(
        cap_k, digits = digits,
        ratio_enter = 'auto',
        choose_enter_if_masked = TRUE,
        highlight_pivot = isTRUE(highlight_pivot),
        rename_leaving_row = TRUE,
        mask_rows   = update_row_mask_rows,
        mask_ratio  = TRUE,                  # usually FALSE to show min ratio; keep your choice
        highlight_ratio_col = TRUE,
        enter_col_style = update_row_enter_col_style,
        color_sign = isTRUE(color_sign),
        mark_enter_value = FALSE,
        mark_leave_value = FALSE,
        math_labels = TRUE,
        mathjax_wrap = mathjax_wrap,
        symbolic_M = TRUE,  
        M_label = "M"    
      )
    } else {
      # No pivot this iteration -> just show the current tableau (static)
      html_update_base <- simplex_html_initial(
        cap_k, digits = digits,
        add_ratio_col = TRUE,
        mask_rows     = update_row_mask_rows,
        mask_ratio    = TRUE,
        ratio_enter   = "auto",
        choose_enter_if_masked = TRUE,
        highlight_col = NULL,
        highlight_row = NULL,
        highlight_ratio_col = TRUE,
        highlight_pivot = FALSE,
        color_sign = isTRUE(color_sign),
        mark_enter_leave_values = FALSE,
        math_labels = TRUE,
        mathjax_wrap = mathjax_wrap,
        symbolic_M = TRUE,  
        M_label = "M"    
      )
    }
    f_update_base <- file.path(out_dir, sprintf("%s_%s_05_update_base_row.html", base_name, it_tag))
    writeLines(html_update_base, f_update_base)
    
    
    
    # Panel 6: UPDATE_OTHER_ROW
    if (has_step) {
      html_update_other <- simplex_html_step_eliminate_other_rows(
        cap,
        digits = digits,
        iter = k,
        ratio_enter = "auto",
        choose_enter_if_masked = TRUE,
        rename_leaving_row = TRUE,
        highlight_pivot = TRUE,
        enter_col_style = "frame",
        mask_rows = c("Zj","Zj-Cj"),
        mask_ratio = TRUE,
        highlight_ratio_col = TRUE,
        highlight_base_row = TRUE,
        highlight_rows_other = "others",
        math_labels = TRUE,
        mathjax_wrap = mathjax_wrap,
        mark_leave_value = FALSE,
        symbolic_M = TRUE,  
        M_label = "M"    
      )
    } else {
      # No pivot -> static view
      html_update_other <- simplex_html_initial(
        cap_k, digits = digits,
        add_ratio_col = TRUE,
        mask_rows     = c("Zj","Zj-Cj"),
        mask_ratio    = TRUE,
        ratio_enter   = "auto",
        choose_enter_if_masked = TRUE,
        highlight_col = NULL,
        highlight_row = NULL,
        highlight_ratio_col = TRUE,
        highlight_pivot = FALSE,
        color_sign = isTRUE(color_sign),
        mark_enter_leave_values = FALSE,
        math_labels = TRUE,
        mathjax_wrap = mathjax_wrap,
        symbolic_M = TRUE,  
        M_label = "M"    
      )
    }
    f_update_other <- file.path(out_dir, sprintf("%s_%s_06_update_other_row.html", base_name, it_tag))
    writeLines(html_update_other, f_update_other)
    
    
    
    # Panel 7: ELIMINATE + OBJECTIVE
    if (has_step) {
      step_after <- simplex_html_step_after_pivot_and_objective(
        cap_k, digits = digits,
        ratio_enter = select_ratio_enter,
        choose_enter_if_masked = TRUE,
        enter_col_style = after_enter_col_style,
        highlight_ratio_col = TRUE,
        highlight_pivot = isTRUE(highlight_pivot),
        color_sign = isTRUE(color_sign),
        highlight_base_row = isTRUE(after_highlight_base_row),
        mask_rows = after_mask_rows,
        mask_ratio = after_mask_ratio,
        mark_enter_value = TRUE,
        mark_leave_value = FALSE,
        math_labels = TRUE,
        mathjax_wrap = mathjax_wrap,
        symbolic_M = TRUE,  
        M_label = "M"    
      )
      html_after <- step_after$html
      is_opt  <- step_after$optimal
      zj_vals <- step_after$zj
      zjcj    <- step_after$zjcj
    } else {
      # Already optimal before any pivot at this iteration
      # Recompute Z rows once to report zj / zjcj
      comp <- .simplex_recompute_Z_rows(cap_k$initial)
      T1   <- comp$tableau
      # Render a static "final" panel
      html_after <- simplex_html_initial(
        list(initial = T1, bigM = cap$bigM),  # reuse big-M meta for symbols
        digits = digits,
        add_ratio_col = TRUE,
        mask_rows     = character(0),         # show Z rows
        mask_ratio    = TRUE,
        ratio_enter   = "auto",
        choose_enter_if_masked = TRUE,
        highlight_col = NULL,
        highlight_row = NULL,
        highlight_ratio_col = TRUE,
        highlight_pivot = FALSE,
        color_sign = isTRUE(color_sign),
        mark_enter_leave_values = FALSE,
        math_labels = TRUE,
        mathjax_wrap = mathjax_wrap,
        symbolic_M = TRUE, M_label = "M"
      )
      is_opt  <- TRUE
      zj_vals <- comp$Z
      zjcj    <- comp$ZmC
    }
    f_after <- file.path(out_dir, sprintf("%s_%s_07_after_objective.html", base_name, it_tag))
    writeLines(html_after, f_after)
    
    
    outputs[[k]] <- list(
      iter = k,
      files = list(
        before = f_before,
        write = f_write,
        enter_col = f_enter_col,
        select = f_select,
        update_base = f_update_base,
        update_other = f_update_other,
        after_objective = f_after
      ),
      optimal = is_opt,
      zj      = zj_vals,
      zjcj    = zjcj
    )
    
    
    # If optimal reached, you might break here. Comment out if you want all panels anyway.
    if (isTRUE(is_opt)) {
      # message(sprintf("Iteration %d reached optimality; stopping early.", k))
      # break
    }
  }
  
  
  # ----------------------------------------------------------
  # POST-OPT CONTINUATION (manual or semi-manual pivots)
  # ----------------------------------------------------------
  
  if (isTRUE(continue_after_optimal) && continue_steps > 0) {
    # start from the last numeric tableau we have
    T_curr <- if (length(cap$iterations)) cap$iterations[[length(cap$iterations)]] else cap$initial
    
    for (kk in seq_len(continue_steps)) {
      plan <- if (!is.null(post_opt_manual) && kk <= length(post_opt_manual)) post_opt_manual[[kk]] else list()
      enter_hint <- plan$enter %||% "auto"   # prefer Zj-Cj≈0 if 'auto'
      leave_hint <- plan$leave %||% NULL
      
      # cap_k starts from current tableau
      cap_k <- cap
      cap_k$initial <- T_curr
      it_tag <- sprintf("post%02d", kk)
      
      # Decide concrete (enter, leave)
      res <- .resolve_enter_leave(cap_k$initial, cap, enter_hint, leave_hint, eps = 1e-9)
      enter_res <- res$enter
      leave_res <- res$leave
      
      # BEFORE
      html_before <- simplex_html_initial(
        cap_k, digits = digits,
        add_ratio_col = TRUE,
        mask_rows     = c("Zj","Zj-Cj"),
        mask_ratio    = TRUE,
        ratio_enter   = "auto",
        choose_enter_if_masked = TRUE,
        highlight_col = NULL,
        highlight_row = NULL,
        highlight_ratio_col = TRUE,
        highlight_pivot = FALSE,
        color_sign = TRUE,
        mark_enter_leave_values = FALSE,
        math_labels = TRUE,
        mathjax_wrap = mathjax_wrap,
        symbolic_M = TRUE,  M_label = "M"
      )
      writeLines(html_before, file.path(out_dir, sprintf("%s_%s_01_before.html", base_name, it_tag)))
      
      # SELECT — highlight exact row & column so the pivot cell shows
      html_select <- simplex_html_initial(
        cap_k, digits = digits,
        add_ratio_col = TRUE,
        mask_rows     = character(0),
        mask_ratio    = FALSE,
        ratio_enter   = enter_res,             # force entering column
        auto_compute_ratios = TRUE,
        choose_enter_if_masked = TRUE,
        highlight_col = enter_res,             # green band on column
        highlight_row = leave_res,             # purple band on row
        highlight_pivot = TRUE,                # amber pivot cell
        highlight_ratio_col = TRUE,
        color_sign = TRUE,
        mark_enter_leave_values = TRUE,
        mark_enter_value = TRUE,
        mark_leave_value = TRUE,
        math_labels = TRUE,
        mathjax_wrap = mathjax_wrap,
        symbolic_M = TRUE,  M_label = "M"
      )
      writeLines(html_select, file.path(out_dir, sprintf("%s_%s_04_select.html", base_name, it_tag)))
      
      # UPDATE BASE ROW — apply same leave explicitly
      html_update_base <- simplex_html_step_update_enter_row(
        cap_k, digits = digits,
        ratio_enter = enter_res,
        choose_enter_if_masked = TRUE,
        highlight_pivot = TRUE,
        rename_leaving_row = TRUE,
        mask_rows   = c("Zj","Zj-Cj"),
        mask_ratio  = TRUE,
        highlight_ratio_col = TRUE,
        enter_col_style = "frame",
        color_sign = TRUE,
        mark_enter_value = FALSE,
        mark_leave_value = FALSE,
        math_labels = TRUE,
        mathjax_wrap = mathjax_wrap,
        symbolic_M = TRUE,  M_label = "M",
        leave_row_override = leave_res        # <-- key
      )
      writeLines(html_update_base, file.path(out_dir, sprintf("%s_%s_05_update_base_row.html", base_name, it_tag)))
      
      # 06: UPDATE_OTHER_ROW (manual, for post-opt steps)
      html_update_other <- simplex_html_step_eliminate_other_rows_manual(
        cap_k, digits = digits,
        ratio_enter = enter_res,
        choose_enter_if_masked = TRUE,
        leave_row_override = leave_res,
        highlight_pivot = TRUE,
        enter_col_style = "frame",
        mask_rows = c("Zj","Zj-Cj"),
        mask_ratio = TRUE,
        highlight_ratio_col = TRUE,
        highlight_base_row = TRUE,
        highlight_rows_other = "others",
        color_sign = TRUE,
        mark_enter_value = TRUE,
        mark_leave_value = FALSE,
        math_labels = TRUE,
        mathjax_wrap = mathjax_wrap,
        symbolic_M = TRUE,  M_label = "M"
      )
      writeLines(
        html_update_other,
        file.path(out_dir, sprintf("%s_%s_06_update_other_row.html", base_name, it_tag))
      )
      
      
      # ELIMINATE + OBJECTIVE — same leave override; returns next tableau
      step_after <- simplex_html_step_after_pivot_and_objective(
        cap_k, digits = digits,
        ratio_enter = enter_res,
        choose_enter_if_masked = TRUE,
        enter_col_style = "frame",
        highlight_ratio_col = TRUE,
        highlight_pivot = TRUE,
        color_sign = TRUE,
        highlight_base_row = FALSE,
        mask_rows = character(0),
        mask_ratio = TRUE,
        mark_enter_value = TRUE,
        mark_leave_value = FALSE,
        math_labels = TRUE,
        mathjax_wrap = mathjax_wrap,
        symbolic_M = TRUE,  M_label = "M",
        leave_row_override = leave_res        # <-- key
      )
      writeLines(step_after$html, file.path(out_dir, sprintf("%s_%s_07_after_objective.html", base_name, it_tag)))
      
      # Chain to next continuation step
      if (!is.null(step_after$tableau)) T_curr <- step_after$tableau
    }
  }
  
  
  invisible(outputs)
}


