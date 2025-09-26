# ==== Adım adım kısıt çizimi + Uygun bölge gölgelendirme (iyileştirilmiş + Objective) ====
library(ggplot2)
library(scales)

`%||%` <- function(x, y) if (is.null(x)) y else x

# --- Geometri yardımcıları ---
.angle_for_line <- function(a, ratio_xy = 1){
  a1 <- a[1]; a2 <- a[2]
  if (abs(a2) <= 1e-12) ang <- 90
  else if (abs(a1) <= 1e-12) ang <- 0
  else ang <- atan(ratio_xy * (-a1/a2)) * 180/pi
  if (ang > 90) ang <- ang - 180
  if (ang < -90) ang <- ang + 180
  ang
}
.unit_perp_along <- function(a){
  n <- sqrt(sum(a^2)); if (n < 1e-12) return(list(perp=c(0,1), along=c(1,0)))
  along <- c(a[2], -a[1]); along <- along / sqrt(sum(along^2))
  perp  <- c(-along[2], along[1])
  list(perp=perp, along=along)
}
.make_label_text <- function(a, rhs, dir) {
  if (a[1] < 0 && a[2] < 0 && rhs < 0) { a <- -a; rhs <- -rhs }
  sym <- if (dir == "<=") "\u2264" else if (dir == ">=") "\u2265" else "="
  fmt <- function(x) sub("\\.0+$", "", formatC(signif(x, 3), format = "fg"))
  a1 <- a[1]; a2 <- a[2]; s2 <- if (a2 >= 0) "+" else "-"
  if (abs(a2) <= 1e-12) sprintf("x1 %s %s", sym, fmt(rhs / a1))
  else if (abs(a1) <= 1e-12) sprintf("x2 %s %s", sym, fmt(rhs / a2))
  else sprintf("%s*x1 %s %s*x2 %s %s", fmt(a1), s2, fmt(abs(a2)), sym, fmt(rhs))
}
.pick_color <- function(g){
  g$idx <- g$idx + 1L
  cols <- g$pal(g$idx)
  list(color = cols[g$idx], idx = g$idx)
}

# --- Uygun bölge poligonunu hesapla ---
.compute_feasible_polygon <- function(g){
  tol <- g$tol
  ineq <- list()
  if (nrow(g$constraints) == 0) return(NULL)
  for (k in seq_len(nrow(g$constraints))){
    a  <- as.numeric(g$constraints[k, c("a1","a2")])
    b  <- as.numeric(g$constraints[k, "b"])
    d  <- as.character(g$constraints[k, "dir"])
    if (d == ">=") { a <- -a; b <- -b; d <- "<=" }
    if (d == "="){
      ineq[[length(ineq)+1]] <- list(a=a,  b=b)
      ineq[[length(ineq)+1]] <- list(a=-a, b=-b)
    } else if (d == "<="){
      ineq[[length(ineq)+1]] <- list(a=a,  b=b)
    }
  }
  xmin <- g$xlim[1]; xmax <- g$xlim[2]
  ymin <- g$ylim[1]; ymax <- g$ylim[2]
  ineq <- c(ineq, list(
    list(a=c( 1, 0), b=xmax),
    list(a=c(-1, 0), b=-xmin),
    list(a=c( 0, 1), b=ymax),
    list(a=c( 0,-1), b=-ymin)
  ))
  P <- NULL
  lines <- ineq
  nL <- length(lines)
  if (nL < 2) return(NULL)
  for (i in 1:(nL-1)){
    for (j in (i+1):nL){
      A2 <- rbind(lines[[i]]$a, lines[[j]]$a)
      if (abs(det(A2)) <= 1e-12) next
      p  <- as.numeric(solve(A2, c(lines[[i]]$b, lines[[j]]$b)))
      if (p[1] < xmin - 1e-7 || p[1] > xmax + 1e-7 ||
          p[2] < ymin - 1e-7 || p[2] > ymax + 1e-7) next
      P <- rbind(P, p)
    }
  }
  if (is.null(P)) return(NULL)
  colnames(P) <- c("x","y")
  ok <- rep(TRUE, nrow(P))
  for (k in seq_len(length(ineq))){
    a <- ineq[[k]]$a; b <- ineq[[k]]$b
    ok <- ok & (P[,1]*a[1] + P[,2]*a[2] <= b + tol)
  }
  P <- P[ok,,drop=FALSE]
  if (!nrow(P)) return(NULL)
  Puniq <- unique(round(P, digits = 9))
  if (nrow(Puniq) < 3) return(NULL)
  ctr <- colMeans(Puniq)
  ang <- atan2(Puniq[,2]-ctr[2], Puniq[,1]-ctr[1])
  as.data.frame(Puniq[order(ang), , drop=FALSE])
}

# --- 1) Tuval başlat (objektif + geriye uyumlu isimler) ---
lp_step_init <- function(xlim = c(0, 120), ylim = c(0, 120),
                         title = "Kısıt Grafiği",
                         pastel = TRUE, fill_feel = "grey90",
                         tol = 1e-9,
                         # her iki isimlendirmeyi de destekle
                         obj = NULL,                # örn. c(20,15)
                         sense = c("max","min"),
                         objective = NULL,          # alternatif isim
                         obj_type  = c("max","min")) {
  
  sense   <- match.arg(sense)
  obj_type<- match.arg(obj_type)
  
  # geriye uyumluluk: verilen hangisiyse onu kullan
  eff_obj   <- if (!is.null(obj)) obj else objective
  eff_sense <- if (!missing(sense)) sense else obj_type
  
  pal <- if (pastel) scales::hue_pal(l = 60, c = 80) else scales::hue_pal()
  p <- ggplot() +
    coord_fixed(ratio = 1, xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_classic(base_size = 12) +
    theme(plot.margin = margin(6, 6, 6, 6)) +
    labs(x = expression(x[1]), y = expression(x[2]), title = title)
  
  structure(list(
    p = p,
    title = title,
    xlim = xlim, ylim = ylim,
    xspan = diff(xlim), yspan = diff(ylim),
    ratio_xy = diff(ylim)/diff(xlim),
    pal = pal, idx = 0L,
    fill_feel = fill_feel,
    tol = tol,
    constraints = data.frame(a1=numeric(0), a2=numeric(0), b=numeric(0),
                             dir=character(0), color=character(0), label=character(0),
                             stringsAsFactors = FALSE),
    # her iki alanı da sakla (kullanımda esneklik için)
    obj = eff_obj,
    sense = eff_sense
  ), class = "lp_step_canvas")
}


# --- 2) Genel kısıt ekle (ve gölgeyi güncelle) ---
lp_step_add <- function(g,
                        coef, rhs, dir = c("<=", ">=", "="),
                        color = NULL, size = 1.1, linetype = "solid",
                        label = NULL, label_nudge = 0.04,
                        shade_alpha = 0.80,
                        repel_equations = FALSE, label_size = 4){
  stopifnot(inherits(g, "lp_step_canvas"), length(coef) == 2)
  dir <- match.arg(dir)
  a1 <- coef[1]; a2 <- coef[2]
  if (is.null(color)) {
    pick <- .pick_color(g)
    color <- pick$color
    g$idx <- pick$idx
  }
  g$constraints <- rbind(
    g$constraints,
    data.frame(a1 = a1, a2 = a2, b = rhs, dir = dir,
               color = color, label = label %||% "", stringsAsFactors = FALSE)
  )
  p <- ggplot() +
    coord_fixed(ratio = 1, xlim = g$xlim, ylim = g$ylim, expand = FALSE) +
    theme_classic(base_size = 12) +
    theme(plot.margin = margin(6, 6, 6, 6)) +
    labs(x = expression(x[1]), y = expression(x[2]), title = g$title)
  df_poly <- .compute_feasible_polygon(g)
  if (!is.null(df_poly)) {
    p <- p + geom_polygon(data = df_poly, aes(x, y),
                          fill = g$fill_feel, alpha = shade_alpha, color = NA)
  }
  for (i in seq_len(nrow(g$constraints))){
    a1i <- g$constraints$a1[i]; a2i <- g$constraints$a2[i]
    bi  <- g$constraints$b[i];  di  <- g$constraints$dir[i]
    coli<- g$constraints$color[i]
    labi<- g$constraints$label[i]; if (identical(labi, "")) labi <- NULL
    if (abs(a2i) <= 1e-12){
      xv <- bi / a1i
      p  <- p + geom_vline(xintercept = xv, colour = coli, linewidth = size, linetype = linetype)
      xf <- xv; yf <- mean(g$ylim)
    } else if (abs(a1i) <= 1e-12){
      yv <- bi / a2i
      p  <- p + geom_hline(yintercept = yv, colour = coli, linewidth = size, linetype = linetype)
      xf <- mean(g$xlim); yf <- yv
    } else {
      slope <- -a1i/a2i; intercept <- bi/a2i
      p <- p + geom_abline(slope = slope, intercept = intercept,
                           colour = coli, linewidth = size, linetype = linetype)
      xf <- mean(g$xlim)
      yf <- (bi - a1i*xf)/a2i
      if (yf < g$ylim[1] || yf > g$ylim[2]){
        yf <- mean(g$ylim)
        xf <- (bi - a2i*yf)/a1i
      }
    }
    ang <- .angle_for_line(c(a1i,a2i), g$ratio_xy)
    ua  <- .unit_perp_along(c(a1i,a2i))
    offs <- label_nudge * min(g$xspan, g$yspan)
    xf <- min(max(xf + offs * ua$perp[1], g$xlim[1]), g$xlim[2])
    yf <- min(max(yf + offs * ua$perp[2], g$ylim[1]), g$ylim[2])
    txt <- labi %||% .make_label_text(c(a1i,a2i), bi, di)
    if (repel_equations && requireNamespace("ggrepel", quietly = TRUE)) {
      p <- p + ggrepel::geom_text_repel(
        data = data.frame(x=xf, y=yf, lab=txt),
        aes(x=x, y=y, label=lab),
        size = label_size, color = coli, min.segment.length = 0, seed = 1
      )
    } else {
      p <- p + annotate("text", x = xf, y = yf, label = txt,
                        angle = ang, colour = coli, size = label_size)
    }
  }
  g$p <- p
  g
}

# --- 3) Kısayollar ---
lp_step_add_x1_bound <- function(g, value, dir = c("<=", ">=", "="), ...){
  lp_step_add(g, coef = c(1,0), rhs = value, dir = match.arg(dir), ...)
}
lp_step_add_x2_bound <- function(g, value, dir = c("<=", ">=", "="), ...){
  lp_step_add(g, coef = c(0,1), rhs = value, dir = match.arg(dir), ...)
}

# --- 4) Aday (köşe) noktalarını getir (Z ve optimum dahil) ---
# --- 2) Aday (köşe) noktalarını getir (Z ve optimum dahil) ---
lp_step_candidates <- function(g, digits = 6){
  df_poly <- .compute_feasible_polygon(g)
  if (is.null(df_poly)) return(data.frame(x=numeric(0), y=numeric(0)))
  out <- unique(round(df_poly, digits = digits))
  names(out) <- c("x","y")
  
  if (!is.null(g$obj) && length(g$obj) == 2){
    out$Z <- g$obj[1]*out$x + g$obj[2]*out$y
    best <- if (g$sense == "max") which.max(out$Z) else which.min(out$Z)
    out$is_opt <- FALSE
    if (length(best)) out$is_opt[best] <- TRUE
  } else {
    out$Z <- NA_real_
    out$is_opt <- FALSE
  }
  out
}


# --- 5) Aday noktalarını işaretle (show_z + highlight_best + iso-line) ---
lp_step_mark_candidates <- function(
    g,
    size = 2.8,
    color = "black",
    label = TRUE,
    digits = 2,
    label_size = 3.5,
    repel = FALSE,
    nudge = 0.02,
    show_z = TRUE,              # KORUNDU
    highlight_best = TRUE,      # KORUNDU
    show_obj_line = TRUE,       # iso-line
    obj_line_color = "darkred",
    obj_line_size  = 1
){
  stopifnot(inherits(g, "lp_step_canvas"))
  
  df_poly <- .compute_feasible_polygon(g)
  if (is.null(df_poly)) {
    message("Uyarı: Halen gölgelenecek/işaretlenecek bir uygun bölge oluşmadı.")
    return(g)
  }
  
  cand <- lp_step_candidates(g, digits = 9)
  if (!nrow(cand)) return(g)
  
  # noktalar
  g$p <- g$p + geom_point(data = cand, aes(x, y), color = color, size = size)
  
  if (label) {
    ctr <- colMeans(cand[,c("x","y")])
    dx <- cand$x - ctr[1]; dy <- cand$y - ctr[2]
    mag <- sqrt(dx^2 + dy^2); mag[mag == 0] <- 1
    kx <- nudge * (g$xspan); ky <- nudge * (g$yspan)
    cand$lx <- cand$x + kx * (dx / mag)
    cand$ly <- cand$y + ky * (dy / mag)
    
    # etiket metni
    if (show_z && !all(is.na(cand$Z))) {
      cand$lab <- paste0(
        "(",
        formatC(cand$x, format = "f", digits = digits),
        ", ",
        formatC(cand$y, format = "f", digits = digits),
        ")\nZ = ",
        formatC(cand$Z, format = "f", digits = digits)
      )
    } else {
      cand$lab <- paste0(
        "(",
        formatC(cand$x, format = "f", digits = digits),
        ", ",
        formatC(cand$y, format = "f", digits = digits),
        ")"
      )
    }
    
    # optimumu ayır ve vurgula
    if (highlight_best && any(cand$is_opt)) {
      opt_df <- subset(cand, is_opt)
      oth_df <- subset(cand, !is_opt)
      
      # diğerleri (normal metin)
      if (nrow(oth_df)) {
        if (repel && requireNamespace("ggrepel", quietly = TRUE)) {
          g$p <- g$p + ggrepel::geom_text_repel(
            data = oth_df, aes(x = x, y = y, label = lab),
            size = label_size, min.segment.length = 0, max.overlaps = Inf, seed = 1
          )
        } else {
          g$p <- g$p + geom_text(
            data = transform(oth_df, x = lx, y = ly),
            aes(x = x, y = y, label = lab),
            size = label_size, vjust = -0.2
          )
        }
      }
      
      # optimum (kutulu)
      if (repel && requireNamespace("ggrepel", quietly = TRUE)) {
        g$p <- g$p + ggrepel::geom_label_repel(
          data = opt_df,
          aes(x = x, y = y, label = lab),
          size = label_size, fontface = "bold",
          fill = "#FFF59D", color = "black", label.size = 0.25,
          min.segment.length = 0, seed = 1
        )
      } else {
        g$p <- g$p + geom_label(
          data = transform(opt_df, x = lx, y = ly),
          aes(x = x, y = y, label = lab),
          size = label_size, fontface = "bold",
          fill = "#FFF59D", color = "black", label.size = 0.25
        )
      }
    } else {
      # hepsi aynı stil
      if (repel && requireNamespace("ggrepel", quietly = TRUE)) {
        g$p <- g$p + ggrepel::geom_text_repel(
          data = cand, aes(x = x, y = y, label = lab),
          size = label_size, min.segment.length = 0, max.overlaps = Inf, seed = 1
        )
      } else {
        g$p <- g$p + geom_text(
          data = transform(cand, x = lx, y = ly),
          aes(x = x, y = y, label = lab),
          size = label_size, vjust = -0.2
        )
      }
    }
  }
  
  # --- Objective iso-line (optimumdan geçen) ---
  if (show_obj_line && !is.null(g$obj) && length(g$obj) == 2 && any(cand$is_opt)) {
    c1 <- g$obj[1]; c2 <- g$obj[2]
    # dikey/ yatay durumlar için koruma
    if (abs(c1) < 1e-12 && abs(c2) < 1e-12) return(g)
    
    opt_pt <- if (g$sense == "max") cand[which.max(cand$Z), ] else cand[which.min(cand$Z), ]
    z_val  <- opt_pt$Z
    
    if (abs(c2) > 1e-12) {
      slope <- -c1/c2
      intercept <- z_val / c2
      g$p <- g$p + geom_abline(slope = slope, intercept = intercept,
                               color = obj_line_color, linewidth = obj_line_size, linetype = "dashed")
    } else {
      # c2 = 0 -> Z = c1*x, iso-line: x = z/c1 (dikey)
      x_iso <- z_val / c1
      g$p <- g$p + geom_vline(xintercept = x_iso,
                              color = obj_line_color, linewidth = obj_line_size, linetype = "dashed")
    }
  }
  
  g
}

# --- Add multiple objective iso-lines (parallel to objective) ---
# If z_values is NULL, it builds a symmetric set around Z*:
#   { Z* + m*step : m in -k,...,k }  (optionally excluding 0 with include_opt=FALSE)
lp_step_add_isolines <- function(
    g,
    z_values = NULL,      # numeric vector of Z-levels to draw (overrides k/step)
    k = 2,                # number of lines on each side of optimum when z_values is NULL
    step = NULL,          # delta-Z between lines; if NULL, auto-chooses ~10% of objective span
    include_opt = TRUE,   # include Z* line in the set (if z_values is NULL)
    color = "darkred",
    linewidth = 0.6,
    linetype = "dashed",
    alpha = 0.8,
    label = TRUE,
    label_size = 3,
    label_nudge = 0.03    # fraction of min(xspan,yspan) to offset label off the line
){
  stopifnot(inherits(g, "lp_step_canvas"))
  if (is.null(g$obj) || length(g$obj) != 2) {
    warning("Objective (g$obj) is not set. Use lp_step_init(..., obj=c(c1,c2)).")
    return(g)
  }
  c1 <- g$obj[1]; c2 <- g$obj[2]
  if (abs(c1) < 1e-12 && abs(c2) < 1e-12) return(g)
  
  # find candidate set (for Z* and to pick sensible step if needed)
  cand <- lp_step_candidates(g, digits = 9)
  if (!nrow(cand)) {
    warning("No candidates yet; add constraints first.")
    return(g)
  }
  # Optimal Z*
  z_star <- if (g$sense == "max") max(cand$Z, na.rm = TRUE) else min(cand$Z, na.rm = TRUE)
  
  # If no explicit z_values, construct around Z*
  if (is.null(z_values)) {
    # auto step: ~10% of observed Z range (fallback to 1 if degenerate)
    if (is.null(step)) {
      z_rng <- range(cand$Z, finite = TRUE)
      step <- max( (diff(z_rng) * 0.10), 1 )
    }
    m <- (-k):k
    if (!include_opt) m <- m[m != 0]
    z_values <- z_star + m * step
  } else {
    z_values <- as.numeric(z_values)
    z_values <- z_values[is.finite(z_values)]
    if (!length(z_values)) return(g)
  }
  
  # Precompute line angle for label rotation & a small perpendicular offset for label placement
  ang <- .angle_for_line(c(c1, c2), g$ratio_xy)
  ua  <- .unit_perp_along(c(c1, c2))
  offs <- label_nudge * min(g$xspan, g$yspan)
  
  # Render each iso-line
  for (z in z_values) {
    if (abs(c2) > 1e-12) {
      # general or horizontal case via abline
      slope <- -c1 / c2
      intercept <- z / c2
      g$p <- g$p + geom_abline(slope = slope, intercept = intercept,
                               color = color, linewidth = linewidth,
                               linetype = linetype, alpha = alpha)
      if (label) {
        # choose an x around center, compute y, then offset slightly off the line
        x0 <- mean(g$xlim)
        y0 <- (z - c1 * x0) / c2
        # If label point is outside panel (e.g., line barely visible), try mid-y instead
        if (y0 < g$ylim[1] || y0 > g$ylim[2]) {
          y0 <- mean(g$ylim)
          x0 <- (z - c2 * y0) / c1
        }
        # final nudge off the line
        x_lab <- min(max(x0 + offs * ua$perp[1], g$xlim[1]), g$xlim[2])
        y_lab <- min(max(y0 + offs * ua$perp[2], g$ylim[1]), g$ylim[2])
        g$p <- g$p + annotate("text", x = x_lab, y = y_lab,
                              label = paste0("Z = ", formatC(z, format = "f", digits = 2)),
                              angle = ang, size = label_size, color = color, alpha = alpha)
      }
    } else {
      # c2 == 0  -> Z = c1*x  -> x = z/c1 (vertical line)
      x_iso <- z / c1
      g$p <- g$p + geom_vline(xintercept = x_iso,
                              color = color, linewidth = linewidth,
                              linetype = linetype, alpha = alpha)
      if (label) {
        x_lab <- x_iso + offs * ua$perp[1]
        y_lab <- mean(g$ylim) + offs * ua$perp[2]
        x_lab <- min(max(x_lab, g$xlim[1]), g$xlim[2])
        y_lab <- min(max(y_lab, g$ylim[1]), g$ylim[2])
        g$p <- g$p + annotate("text", x = x_lab, y = y_lab,
                              label = paste0("Z = ", formatC(z, format = "f", digits = 2)),
                              angle = ang, size = label_size, color = color, alpha = alpha)
      }
    }
  }
  g
}


# # Init with objective (max)
# g <- lp_step_init(xlim = c(-10, 150), ylim = c(-10, 150),
#                   title = "Uygun Bölge (Adım Adım) — Z = 20*x1 + 15*x2 (max)",
#                   pastel = TRUE,
#                   obj = c(20, 15), sense = "max")
# 
# g <- lp_step_add_x1_bound(g, 0,   dir = ">=")              # x1 ≥ 0
# g <- lp_step_add_x2_bound(g, 0,   dir = ">=")              # x2 ≥ 0
# g <- lp_step_add_x1_bound(g, 100, dir = "<=")              # x1 ≤ 100
# g <- lp_step_add_x2_bound(g, 100, dir = "<=")              # x2 ≤ 100
# g <- lp_step_add(g, coef = c(50, 35), rhs = 6000, dir = "<=")
# g <- lp_step_add(g, coef = c(20, 15), rhs = 2000, dir = ">=")  # opsiyonel ek kısıt
# 
# g$p
# 
# 
# g <- lp_step_mark_candidates(g, label = TRUE,
#                              show_z = FALSE,
#                              highlight_best = FALSE,
#                              show_obj_line = FALSE)
# g$p
# 
# 
# # Plot candidates with 2-line labels and Z; highlight best
# g <- lp_step_mark_candidates(g, label = TRUE, show_z = TRUE,
#                              repel = TRUE, highlight_best = TRUE,
#                              show_obj_line = TRUE)
# g$p
# 
# 
# # See candidate table with Z and optimum flag
# lp_step_candidates(g)
# 
# 
# 
# 
# # Objective already set in your g via lp_step_init(..., obj=c(20,15), sense="max")
# 
# # 1) Iso-lines at explicit Z levels:
# g <- lp_step_add_isolines(g, z_values = c(1200, 1600, 2000), color = "firebrick")
# g$p
# # 2) Or ±k lines around optimum with auto step (~10% of Z-range):
# g <- lp_step_add_isolines(g, k = 3, include_opt = TRUE, color = "steelblue")
# g$p
# # 3) Smaller step and exclude the optimum line:
# g <- lp_step_add_isolines(g, k = 4, step = 200, include_opt = FALSE,
#                           color = "darkgreen", label = TRUE)

