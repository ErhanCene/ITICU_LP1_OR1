


#' Plot and Solve a 2D Linear Programming Problem (Graphical Method)
#'
#' Computes and plots the feasible region, candidate corner points, and optimum
#' solution for a two-variable linear programming (LP) problem using the
#' graphical method.
#'
#' @param A Numeric matrix (n × 2). Coefficients of the constraints.
#' @param b Numeric vector of length `n`. Right-hand side constants.
#' @param dir Character vector of length `n`. Direction of each constraint:
#'   one of `"<="`, `">="`, or `"="`.
#' @param c Numeric vector of length 2. Coefficients for the objective function
#'   \eqn{Z = c_1 x_1 + c_2 x_2}.
#' @param sense Character. `"max"` or `"min"`. Whether to maximize or minimize
#'   the objective function. Default `"max"`.
#' @param bounds List with elements `x1` and `x2`. Each is a numeric vector of
#'   length 2 giving lower and upper bounds. Use `Inf` for unbounded.
#' @param xlim,ylim Numeric vectors of length 2 or `NULL`. Plot limits. If
#'   `NULL`, computed automatically from the feasible region.
#' @param title Character or `NULL`. Plot title. If `NULL`, a title showing the
#'   objective function is generated.
#' @param region_label Character. Label for the feasible region (supports
#'   `"\n"` line breaks). Placed at the polygon centroid.
#' @param tol Numeric tolerance for feasibility checks. Default `1e-9`.
#' @param label_lines Logical. Whether to show constraint equation labels.
#' @param label_size Numeric. Font size for constraint labels.
#' @param label_offset,label_shift Numeric or `NULL`. Offset in perpendicular
#'   direction (data units) and shift along the constraint line (fraction of
#'   span). Computed automatically if `NULL`.
#' @param label_form `"equality"` or `"original"`. Show normalized form or as
#'   originally input.
#' @param label_candidates Logical. Whether to label all candidate corner
#'   points. The optimum is always labeled.
#' @param candidates_size Numeric. Font size for candidate point labels.
#' @param candidates_digits Integer. Number of decimal places for coordinates
#'   and Z values in candidate labels.
#' @param repel_candidates Logical. If `TRUE`, uses \pkg{ggrepel} to avoid
#'   overlapping candidate labels.
#' @param repel_equations Logical. If `TRUE`, uses \pkg{ggrepel} to avoid
#'   overlapping equation labels.
#' @param repel_opts List. Additional arguments for \pkg{ggrepel} functions.
#'   Example: `list(box.padding=0.3, point.padding=0.2, seed=1)`.
#' @param opt_size Numeric or `NULL`. Font size for optimum label. If `NULL`,
#'   defaults to `candidates_size` if candidate labels are shown, otherwise
#'   `0.9 * candidates_size`.
#' @param show Character. What to display immediately:
#'   `"both"` (default) prints the candidates table and draws the plot,
#'   `"plot"` draws only the plot,
#'   `"data"` prints only the table,
#'   `"none"` prints nothing (returns objects invisibly).
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Converts constraints to a common form and applies bounds.
#'   \item Computes all pairwise intersections of boundary lines.
#'   \item Filters feasible points using `tol`.
#'   \item Evaluates the objective at feasible points and finds the optimum.
#'   \item Plots the feasible region (convex polygon), constraints, candidates, and optimum.
#' }
#'
#' Formatting niceties:
#' \itemize{
#'   \item Constraint labels are rewritten to \code{ax1 + bx2 = rhs}; if all coefficients and RHS
#'         are negative, the entire equation is multiplied by \eqn{-1} so you see positive form.
#'   \item The feasible-region label is placed at the polygon centroid to stay inside the region.
#'   \item The optimum is always labeled. When `label_candidates = TRUE`, it is shown in a rounded,
#'         soft-yellow box; otherwise as a simple two-line label.
#' }
#'
#' @return A list with:
#'   \item{plot}{The `ggplot` object.}
#'   \item{x_opt}{Named vector of optimum \eqn{(x_1, x_2)}.}
#'   \item{z_opt}{Optimum objective value.}
#'   \item{corners}{(Reserved; currently `FALSE` for compatibility).}
#'   \item{candidates}{Data frame of all feasible candidate points with their `Z` values.}
#'
#' @examples
#' A <- rbind(c(7,3), c(6,7), c(1,0), c(0,1))
#' b <- c(21, 42, 3, 4)
#' dir <- c("<=", "<=", "<=", "<=")
#'
#' # Draw plot and table
#' res1 <- lp_plot(A, b, dir,
#'                 c=c(6,8), sense="max",
#'                 xlim=c(0,8), ylim=c(0,8),
#'                 label_candidates=TRUE,
#'                 show="both")
#'
#' # Compute quietly and use objects later
#' res2 <- lp_plot(A, b, dir,
#'                 c=c(6,8), sense="max",
#'                 xlim=c(0,8), ylim=c(0,8),
#'                 label_candidates=FALSE,
#'                 show="none")
#' print(res2$plot)
#' res2$candidates
#'
#' @import ggplot2
#' @importFrom scales pretty_breaks hue_pal
#' @importFrom ggrepel geom_text_repel geom_label_repel
#' @importFrom grid unit
#' @export

# install.packages(c("ggplot2","scales","ggrepel"))  # if needed
library(ggplot2)
library(scales)
library(ggrepel)
library(grid)   # for unit() used in label corner radius



lp_plot <- function(
    A, b, dir, c, sense = c("max","min"),
    bounds = list(x1=c(0,Inf), x2=c(0,Inf)),
    xlim = NULL, ylim = NULL,
    title = NULL, region_label = "Uygun \nÇözüm \nBölgesi",
    tol = 1e-9,
    label_lines = TRUE, label_size = 4,
    label_offset = NULL,          # auto if NULL (data units)
    label_shift  = NULL,          # auto if NULL (fraction of span)
    label_form   = c("equality","original"),
    # Candidate labels
    label_candidates = FALSE,
    candidates_size  = 3.5,
    candidates_digits = 3,
    repel_candidates = TRUE,
    # Constraint-equation labels
    repel_equations  = FALSE,
    # ggrepel tuning
    repel_opts = list(box.padding = 0.3, point.padding = 0.2,
                      min.segment.length = 0, max.overlaps = Inf, seed = 1),
    # Optimum label size (NULL = auto: if candidates on -> candidates_size, else 0.9*candidates_size)
    opt_size = NULL,
    # What to display now?
    show = c("both","plot","data","none")
){
  sense <- match.arg(sense)
  label_form <- match.arg(label_form)
  show <- match.arg(show)
  do_plot <- show %in% c("both","plot")
  do_print_table <- show %in% c("both","data")
  
  stopifnot(ncol(A)==2, length(b)==nrow(A), length(dir)==nrow(A), length(c)==2)
  
  `%||%` <- function(x,y) if (is.null(x)) y else x
  clamp  <- function(v, lo, hi) pmin(pmax(v, lo), hi)
  
  # ---- normalize constraints ----
  ineqA <- NULL; ineqb <- NULL; ineq_orig <- list()
  eqA   <- NULL; eqb   <- NULL
  add_ineq <- function(a,rhs,a0=NULL,b0=NULL,d0="<="){
    ineqA <<- rbind(ineqA,a); ineqb <<- c(ineqb,rhs)
    ineq_orig[[length(ineq_orig)+1]] <<- list(a=a0 %||% a, b=b0 %||% rhs, dir=d0)
  }
  add_eq <- function(a,rhs){ eqA <<- rbind(eqA,a); eqb <<- c(eqb,rhs) }
  
  for(i in seq_len(nrow(A))){
    a <- A[i,]; rhs <- b[i]; d <- dir[i]
    if (d=="<=") add_ineq(a,rhs,a,rhs,"<=")
    else if (d==">=") add_ineq(-a,-rhs,a,rhs,">=")
    else if (d=="=")  add_eq(a,rhs)
    else stop("dir must be '<=', '>=', '='")
  }
  L1 <- bounds$x1[1]; U1 <- bounds$x1[2]
  L2 <- bounds$x2[1]; U2 <- bounds$x2[2]
  if (is.finite(U1)) add_ineq(c(1,0),U1,c(1,0),U1,"<=")
  if (is.finite(U2)) add_ineq(c(0,1),U2,c(0,1),U2,"<=")
  if (is.finite(L1)) add_ineq(c(-1,0),-L1,c(1,0),L1,">=")
  if (is.finite(L2)) add_ineq(c(0,-1),-L2,c(0,1),L2,">=")
  
  lines <- list(); meta <- list()
  if (!is.null(ineqA)) for(i in seq_len(nrow(ineqA))){
    lines[[length(lines)+1]] <- list(a=ineqA[i,], b=ineqb[i]); meta[[length(meta)+1]] <- ineq_orig[[i]]
  }
  if (!is.null(eqA)) for(i in seq_len(nrow(eqA))){
    lines[[length(lines)+1]] <- list(a=eqA[i,], b=eqb[i]);     meta[[length(meta)+1]] <- list(a=eqA[i,], b=eqb[i], dir="=")
  }
  nL <- length(lines); if (nL < 2) stop("Not enough boundary lines.")
  
  # ---- intersections → candidate corners ----
  P <- NULL
  for(i in 1:(nL-1)) for(j in (i+1):nL){
    A2 <- rbind(lines[[i]]$a, lines[[j]]$a)
    if (abs(det(A2)) <= 1e-12) next
    P <- rbind(P, as.numeric(solve(A2, c(lines[[i]]$b, lines[[j]]$b))))
  }
  if (is.null(P)) stop("No intersections found.")
  colnames(P) <- c("x1","x2"); P <- unique(P)
  
  feasible <- apply(P,1,function(p){
    ok1 <- if (!is.null(ineqA)) all(ineqA %*% p <= ineqb + tol) else TRUE
    ok2 <- if (!is.null(eqA))   all(abs(eqA %*% p - eqb) <= 1e-7) else TRUE
    ok1 && ok2
  })
  F <- P[feasible,,drop=FALSE]; if (nrow(F)==0) stop("Feasible region empty.")
  
  # ---- autos ----
  auto_lim <- function(vals,L,U,pad_frac=0.05){
    lo <- if (is.finite(L)) L else min(vals)
    hi <- if (is.finite(U)) U else max(vals)
    span <- hi-lo; pad <- pad_frac * (span + 1e-9)
    c(max(0, lo-pad), hi+pad)
  }
  if (is.null(xlim)) xlim <- auto_lim(F[,1], L1, U1, pad_frac = 0.05)
  if (is.null(ylim)) ylim <- auto_lim(F[,2], L2, U2, pad_frac = 0.05)
  xspan <- diff(xlim); yspan <- diff(ylim)
  
  if (is.null(label_offset)) label_offset <- 0.06 * min(xspan, yspan)
  if (is.null(label_shift))  {
    label_shift <- 0.10 + 0.08*log1p(nL)
    label_shift <- max(0.10, min(0.35, label_shift))
  }
  if (is.null(opt_size)) opt_size <- if (label_candidates) candidates_size else 0.9*candidates_size
  
  # ---- corners & objective ----
  ctr <- colMeans(F); ang <- atan2(F[,2]-ctr[2], F[,1]-ctr[1])
  poly <- F[order(ang),,drop=FALSE]
  df_poly <- data.frame(x=poly[,1], y=poly[,2])
  df_pts  <- data.frame(x=F[,1],    y=F[,2])
  
  z <- as.numeric(F %*% c)
  best_idx <- if (sense=="max") which.max(z) else which.min(z)
  x_opt <- F[best_idx,1]; y_opt <- F[best_idx,2]; z_opt <- z[best_idx]
  
  # ---- candidate table output (controlled by `show`) ----
  candidates <- data.frame(x1 = F[,1], x2 = F[,2], Z = z)
  if (sense == "max") candidates <- candidates[order(-candidates$Z, candidates$x1, candidates$x2), ]
  else                candidates <- candidates[order( candidates$Z, candidates$x1, candidates$x2), ]
  if (do_print_table) print(candidates, row.names = FALSE)
  
  # helpers
  fmt <- function(x) as.numeric(format(x, trim = TRUE, digits = 6))
  make_label_text <- function(a1, a2, rhs) {
    # If all coefficients and RHS are negative, flip their signs
    if (a1 < 0 && a2 < 0 && rhs < 0) { a1 <- -a1; a2 <- -a2; rhs <- -rhs }
    if (abs(a2) <= 1e-12) return(sprintf("x1 = %s", fmt(rhs/a1)))
    if (abs(a1) <= 1e-12) return(sprintf("x2 = %s", fmt(rhs/a2)))
    sprintf("%s*x1 %s %s*x2 = %s",
            fmt(a1), if (a2 >= 0) "+" else "-",
            fmt(abs(a2)), fmt(rhs))
  }
  ratio_xy <- yspan / xspan
  angle_for_line <- function(a){
    a1 <- a[1]; a2 <- a[2]
    if (abs(a2) <= 1e-12) ang <- 90
    else if (abs(a1) <= 1e-12) ang <- 0
    else ang <- atan( ratio_xy * (-a1/a2) ) * 180/pi
    if (ang > 90) ang <- ang - 180
    if (ang < -90) ang <- ang + 180
    ang
  }
  unit_perp_along <- function(a){
    n <- sqrt(sum(a^2)); if (n < 1e-12) return(list(perp=c(0,1), along=c(1,0)))
    along <- c(a[2], -a[1]); along <- along / sqrt(sum(along^2))
    perp  <- c(-along[2], along[1])
    list(perp=perp, along=along)
  }
  
  # ---- base plot ----
  p <- ggplot() +
    geom_polygon(data=df_poly, aes(x=x, y=y), fill="grey80", alpha=0.7, color=NA) +
    coord_fixed(ratio = 1, xlim = xlim, ylim = ylim, expand = FALSE, clip = "off") +
    scale_x_continuous(breaks = pretty_breaks(n = 10)) +
    scale_y_continuous(breaks = pretty_breaks(n = 10)) +
    theme_classic(base_size = 12) +
    theme(plot.margin = margin(6, 6, 6, 6))
  
  line_colors <- scales::hue_pal()(nL)
  
  # draw constraints + collect label positions
  label_df <- NULL
  for(i in seq_len(nL)){
    a <- lines[[i]]$a; rhs <- lines[[i]]$b; a1 <- a[1]; a2 <- a[2]
    col_i <- line_colors[i]
    
    if (abs(a2) <= 1e-12) {
      xv <- rhs / a1
      p  <- p + geom_vline(xintercept = xv, linewidth = 0.9, color = col_i)
      xf <- xv; yf <- mean(ylim)
    } else if (abs(a1) <= 1e-12) {
      yv <- rhs / a2
      p  <- p + geom_hline(yintercept = yv, linewidth = 0.9, color = col_i)
      xf <- mean(xlim); yf <- yv
    } else {
      slope <- -a1/a2; intercept <- rhs/a2
      p <- p + geom_abline(slope = slope, intercept = intercept, linewidth = 0.9, color = col_i)
      xf <- xlim[1] + (i/(nL+1)) * xspan
      yf <- (rhs - a1*xf)/a2
      if (yf < ylim[1] || yf > ylim[2]) {
        yf <- ylim[1] + (i/(nL+1)) * yspan
        xf <- (rhs - a2*yf)/a1
        xf <- clamp(xf, xlim[1]+0.02*xspan, xlim[2]-0.02*xspan)
      }
    }
    
    ua <- unit_perp_along(a)
    theta <- atan(-a1/a2)
    w_perp  <- 0.6 + 0.4*abs(cos(theta))
    w_along <- 0.6 + 0.4*abs(sin(theta))
    xf <- xf + (label_offset * w_perp ) * ua$perp[1] + (label_shift * xspan * w_along) * ua$along[1]
    yf <- yf + (label_offset * w_perp ) * ua$perp[2] + (label_shift * yspan * w_along) * ua$along[2]
    xf <- clamp(xf, xlim[1]+0.03*xspan, xlim[2]-0.03*xspan)
    yf <- clamp(yf, ylim[1]+0.05*yspan, ylim[2]-0.05*yspan)
    
    label_df <- rbind(label_df, data.frame(
      x = xf, y = yf,
      lab = make_label_text(a1, a2, rhs),
      angle = angle_for_line(a),
      col = col_i
    ))
  }
  
  # objective line (dashed)
  c1 <- c[1]; c2 <- c[2]
  if (abs(c2) > 1e-12){
    xs <- seq(xlim[1], xlim[2], length.out=200)
    ys <- (z_opt - c1*xs)/c2
    p  <- p + geom_line(data=data.frame(x=xs,y=ys), aes(x=x,y=y),
                        linetype="dashed", linewidth=0.9)
  } else {
    p  <- p + geom_vline(xintercept=z_opt/c1, linetype="dashed", linewidth=0.9)
  }
  
  # draw all candidate points + optimum point
  p <- p + geom_point(data=df_pts, aes(x=x, y=y), size=2.2) +
    annotate("point", x=x_opt, y=y_opt, size=3.2)
  
  # --- polygon centroid for region label (keeps label inside) ---
  poly_centroid <- function(x, y){
    x2 <- c(x, x[1]); y2 <- c(y, y[1])
    cross <- x2[-length(x2)]*y2[-1] - x2[-1]*y2[-length(y2)]
    A <- 0.5 * sum(cross)
    Cx <- (1/(6*A)) * sum( (x2[-length(x2)] + x2[-1]) * cross )
    Cy <- (1/(6*A)) * sum( (y2[-length(y2)] + y2[-1]) * cross )
    c(Cx, Cy)
  }
  ctr_reg <- poly_centroid(df_poly$x, df_poly$y)
  rx <- range(df_poly$x, na.rm=TRUE); ry <- range(df_poly$y, na.rm=TRUE)
  ctr_reg[1] <- max(min(ctr_reg[1], rx[2] - 0.03*diff(rx)), rx[1] + 0.03*diff(rx))
  ctr_reg[2] <- max(min(ctr_reg[2], ry[2] - 0.05*diff(ry)), ry[1] + 0.05*diff(ry))
  p <- p + annotate("text", x=ctr_reg[1], y=ctr_reg[2], label=region_label)
  
  # constraint-equation labels
  if (label_lines && nrow(label_df)){
    if (repel_equations) {
      p <- p + do.call(ggrepel::geom_text_repel, c(
        list(data = label_df,
             mapping = aes(x = x, y = y, label = lab, angle = angle, color = col),
             size = label_size, show.legend = FALSE, inherit.aes = FALSE),
        repel_opts))
    } else {
      p <- p + geom_text(
        data = label_df,
        aes(x = x, y = y, label = lab, angle = angle, color = col),
        size = label_size, show.legend = FALSE
      )
    }
    p <- p + scale_color_identity()
  }
  
  # ===== Optimum label (two lines). Always shown. =====
  opt_lab <- sprintf("Opt (%.3f, %.3f)\nZ* = %.3f", x_opt, y_opt, z_opt)
  opt_df  <- data.frame(x = x_opt, y = y_opt, lab = opt_lab)
  opt_fill   <- "#FFF59D"   # soft yellow
  opt_border <- "black"
  opt_alpha  <- 0.95
  
  if (label_candidates) {
    if (repel_candidates) {
      p <- p + do.call(ggrepel::geom_label_repel, c(
        list(data = opt_df,
             mapping = aes(x = x, y = y, label = lab),
             size = opt_size, fontface = "bold",
             fill = opt_fill, color = opt_border, alpha = opt_alpha,
             label.size = 0.25, label.r = unit(0.15, "lines"),
             inherit.aes = FALSE),
        repel_opts))
    } else {
      p <- p + geom_label(
        data = opt_df,
        aes(x = x, y = y, label = lab),
        size = opt_size, fontface = "bold",
        fill = opt_fill, color = opt_border,
        label.size = 0.25, label.r = unit(0.15, "lines")
      )
    }
  } else {
    p <- p + geom_text(
      data = opt_df,
      aes(x = x, y = y, label = lab),
      size = opt_size, fontface = "bold", vjust = -0.6
    )
  }
  
  # === Candidate labels (exclude optimum to avoid duplication) ===
  if (label_candidates) {
    eps <- 1e-8
    cand_df <- transform(
      candidates,
      is_opt = (abs(x1 - x_opt) < eps & abs(x2 - y_opt) < eps)
    )
    cand_df <- subset(cand_df, !is_opt)
    cand_df$lab <- sprintf("(%.*f, %.*f)\nZ = %.*f",
                           candidates_digits, cand_df$x1,
                           candidates_digits, cand_df$x2,
                           candidates_digits, cand_df$Z)
    
    if (repel_candidates) {
      p <- p + do.call(ggrepel::geom_text_repel, c(
        list(data = cand_df,
             mapping = aes(x = x1, y = x2, label = lab),
             size = candidates_size, inherit.aes = FALSE),
        repel_opts))
    } else {
      p <- p + geom_text(
        data = cand_df,
        aes(x = x1, y = x2, label = lab),
        size = candidates_size, vjust = -0.6
      )
    }
  }
  
  if (is.null(title)) {
    title <- sprintf("Grafik Çözüm: Z = %.2f*x1 + %.2f*x2", c[1], c[2])
  }
  p <- p + labs(x=expression(x[1]), y=expression(x[2]), title=title)
  
  if (do_plot) print(p)
  invisible(list(
    plot = p,
    x_opt = c(x1=x_opt, x2=y_opt),
    z_opt = z_opt,
    corners = F,
    candidates = candidates
  ))
}




# # Problem data
# A   <- rbind(c(7,3), c(6,7), c(1,0), c(0,1))
# b   <- c(21, 42, 3, 4)
# dir <- c("<=", "<=", "<=", "<=")
# 
# # 1) Only optimum (two‑line label, no box)
# res1_optimum <- 
#       lp_plot(A, b, dir,
#         c=c(6,8), sense="max",
#         xlim=c(0,8), ylim=c(0,8),
#         title="Grafik Çözüm: Z = 6*x1 + 8*x2",
#         label_lines=TRUE,
#         label_candidates=FALSE,
#         show = 'none')
# print(res1optimum$plot)                   # draw later
# res1_optimum$candidates                    # or use the data

# 
# # 2) Optimum highlighted + all candidate labels
# lp_plot(A, b, dir,
#         c=c(6,8), sense="max",
#         xlim=c(0,8), ylim=c(0,8),
#         title="Grafik Çözüm: Z = 6*x1 + 8*x2",
#         label_lines=TRUE,
#         label_candidates=TRUE,
#         repel_candidates=TRUE,
#         repel_equations=FALSE,
#         show = 'both') # will print the graph and print the candidates
