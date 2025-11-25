# Example: maximize c^T x subject to A x (<=, >=, =) b, x >= 0
A <- rbind(
  c( 1,  4, 2),
  c( 3,  2, 1)
)
b <- c(8,6)
c <- c(-2, -3, -1)
sense <- c(">=", ">=")  # mix of >= / <= / =

capBM <- capture_bigM(A, b, c, sense, M = 1e6, maximize = TRUE)
# Status / summary:
attr(capBM$summary, "status")
capBM$summary



# choose entering column from the original Zj-Cj, but render with Zj/Zj-Cj hidden and ratios blank
html_choose_col_hide_ratios <- simplex_html_initial(
  capBM,
  add_ratio_col = TRUE,
  mask_rows  = c('Zj', 'Zj-Cj'),     # hide both rows
  mask_ratio = TRUE,                # keep ratio column blank
  ratio_enter = "auto",             # pick by most-negative Zj-Cj
  choose_enter_if_masked = TRUE,    # <-- key switch
  highlight_ratio_col = TRUE,        # also frame the Oran column
  math_labels = TRUE,
  mathjax_wrap = TRUE,
  symbolic_M = TRUE,  
  M_label = "M",
  highlight_col = FALSE
)
writeLines(html_choose_col_hide_ratios, "01.html")


html_choose_col_hide_ratios <- simplex_html_initial(
  capBM,
  add_ratio_col = TRUE,
  mask_rows  = character(0),     # hide both rows
  mask_ratio = TRUE,                # keep ratio column blank
  highlight_ratio_col = TRUE,        # also frame the Oran column
  mark_enter_value = FALSE,        # <- don't box the min Zj-Cj value
  math_labels = TRUE,
  mathjax_wrap = TRUE,
  symbolic_M = TRUE,  
  M_label = "M"      
)
writeLines(html_choose_col_hide_ratios, "02.html")



html_choose_col_hide_ratios <- simplex_html_initial(
  capBM,
  add_ratio_col = TRUE,
  mask_rows  = character(0),     # hide both rows
  mask_ratio = TRUE,                # keep ratio column blank
  highlight_ratio_col = TRUE,        # also frame the Oran column
  highlight_col = 'auto',
  mark_enter_value = TRUE,
  auto_compute_ratios = TRUE,
  math_labels = TRUE,
  mathjax_wrap = TRUE,
  symbolic_M = TRUE,  
  M_label = "M"      
)
writeLines(html_choose_col_hide_ratios, "03.html")


html_choose_col_hide_ratios <- simplex_html_initial(
  capBM,
  add_ratio_col = TRUE,
  mask_rows  = character(0),     # hide both rows
  mask_ratio = FALSE,                # keep ratio column blank
  highlight_ratio_col = TRUE,        # also frame the Oran column
  highlight_col = 'auto',
  mark_enter_value = TRUE,
  highlight_row = 'auto',
  mark_leave_value = TRUE,
  auto_compute_ratios = TRUE,
  highlight_pivot = TRUE,
  choose_enter_if_masked = TRUE,    # <-- key switch
  math_labels = TRUE,
  mathjax_wrap = TRUE,
  symbolic_M = TRUE,  
  M_label = "M"      
)
writeLines(html_choose_col_hide_ratios, "04.html")


# Update only the entering row, outline (frame) the base column without green fill
html_step1_no_tint <- simplex_html_step_update_enter_row(
  capBM,
  ratio_enter = "auto",
  mask_rows  = c('Zj', 'Zj-Cj'),     # hide both rows
  mask_ratio = TRUE,                # keep ratio column blank
  choose_enter_if_masked = TRUE,
  rename_leaving_row = TRUE,
  highlight_pivot = TRUE,
  highlight_ratio_col = TRUE,
  mark_enter_value = FALSE,
  enter_col_style = "frame",   # << no green tint; just a border
  math_labels = TRUE,
  mathjax_wrap = TRUE,
  mark_leave_value = FALSE,
  symbolic_M = TRUE,  
  M_label = "M"      
)
writeLines(html_step1_no_tint, "05.html")



html_step2_no_base_hi <- simplex_html_step_eliminate_other_rows(
  capBM, iter = 1,
  highlight_rows_other = "others",   # optional orange band on non-pivot rows
  math_labels = TRUE,
  mathjax_wrap = TRUE,
  rename_leaving_row = TRUE,
  mark_leave_value = FALSE,
  mark_enter_value = FALSE,
  symbolic_M = TRUE,  
  M_label = "M"      
  
)
writeLines(html_step2_no_base_hi, "06.html")


step3 <- simplex_html_step_after_pivot_and_objective(
  capBM,
  highlight_base_row = FALSE,   # <- turns off purple band
  math_labels = TRUE,
  mathjax_wrap = TRUE,
  symbolic_M = TRUE,  
  M_label = "M",
  highlight_optimum_value = TRUE
)
writeLines(step3$html, "07.html")



# Tie example: ratios 4 and 4 on x1
A     <- rbind(c(1, 0),
               c(2, 0))
b     <- c(4, 8)
c     <- c(3, 1)        # maximize 3 x1 + 1 x2
sense <- c("<=", "<=")

cap <- capture_bigM(A, b, c, sense, M = 1e6, maximize = TRUE)

# Panel 1: see ratios and the chosen leaving row (s1) highlighted
html1 <- simplex_html_initial(
  cap,
  add_ratio_col = TRUE,
  auto_compute_ratios = TRUE,
  highlight_col = "auto",
  highlight_row = "auto",
  highlight_pivot = TRUE,
  math_labels = TRUE,
  mathjax_wrap = TRUE,
  symbolic_M = TRUE, 
)
cat(html1, file = "tie_panel1.html")

# Next step (row-update view):
html2 <- simplex_html_step_update_enter_row(
  cap,
  highlight_pivot = TRUE,
  math_labels = TRUE,
  mathjax_wrap = TRUE,
  symbolic_M = TRUE, 
)
cat(html2, file = "tie_panel2.html")


# Already optimal at start (all reduced costs >= 0 for maximize)
A <- rbind(c(1,0), c(0,1))
b <- c(4,3)
c <- c(0,0)              # no incentive to enter
sense <- c("<=","<=")

cap0 <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)
attr(cap0$summary, "status")  # "Optimal (Big-M)" and no error

A <- rbind(c(1,0), c(0,1))
b <- c(4,3)
c <- c(5,5)              # Zj-Cj for x1 and x2 equal
sense <- c("<=","<=")

capT <- capture_bigM(A,b,c,sense,M=1e6,maximize=TRUE)
capT$steps[[1]]$enter     # "x1"
