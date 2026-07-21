#' CIELAB to sRGB
#'
#' @param L,a,b Numeric matrices of the same dimensions. CIELAB, D65.
#'
#' @return A named list of three matrices `R`, `G`, `B` in **0-1**, not
#'   0-255. Multiply by 255 if you need the byte scale back.
#'
#' @details
#' Out-of-gamut Lab values are clamped at 0 by the sRGB companding, so a
#' round trip through [rgb_to_lab()] returns the original colour only for
#' colours that were in gamut to begin with.
#'
#' @examples
#' lab <- rgb_to_lab(matrix(200, 1, 1), matrix(100, 1, 1), matrix(50, 1, 1))
#' round(lab_to_rgb(lab$L, lab$a, lab$b)$R * 255, 2)
#' @export
lab_to_rgb <- function(L, a, b) {
  v <- .check_bands(L, a, b, names = c("L", "a", "b"))
  L <- v[[1]]; a <- v[[2]]; b <- v[[3]]

  epsilon <- 0.008856
  kappa <- 903.3

  fy <- (L + 16) / 116
  fx <- a / 500 + fy
  fz <- fy - b / 200

  cube <- function(f) {
    f3 <- f^3
    out <- (116 * f - 16) / kappa
    hi <- !is.na(f3) & f3 > epsilon
    out[hi] <- f3[hi]
    out
  }
  X <- cube(fx) * .Xn
  Z <- cube(fz) * .Zn
  Y <- L / kappa
  hi <- !is.na(L) & L > kappa * epsilon
  Y[hi] <- (fy[hi])^3
  Y <- Y * .Yn

  r_lin <-  3.2404542 * X - 1.5371385 * Y - 0.4985314 * Z
  g_lin <- -0.9692660 * X + 1.8760108 * Y + 0.0415560 * Z
  b_lin <-  0.0556434 * X - 0.2040259 * Y + 1.0572252 * Z

  list(R = .linear_to_srgb(r_lin), G = .linear_to_srgb(g_lin),
       B = .linear_to_srgb(b_lin))
}


#' Oklab to sRGB
#'
#' @param L,a,b Numeric matrices of the same dimensions. Oklab.
#'
#' @return A named list of three matrices `R`, `G`, `B` in **0-1**.
#'
#' @examples
#' ok <- rgb_to_oklab(matrix(200, 1, 1), matrix(100, 1, 1), matrix(50, 1, 1))
#' round(oklab_to_rgb(ok$L, ok$a, ok$b)$R * 255, 2)
#' @export
oklab_to_rgb <- function(L, a, b) {
  v <- .check_bands(L, a, b, names = c("L", "a", "b"))
  L <- v[[1]]; a <- v[[2]]; b <- v[[3]]

  l_c <- L + 0.3963377774 * a + 0.2158037573 * b
  m_c <- L - 0.1055613458 * a - 0.0638541728 * b
  s_c <- L - 0.0894841775 * a - 1.2914855480 * b

  l <- l_c^3; m <- m_c^3; s <- s_c^3

  list(R = .linear_to_srgb( 4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s),
       G = .linear_to_srgb(-1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s),
       B = .linear_to_srgb(-0.0041960863 * l - 0.7034186147 * m + 1.7076147010 * s))
}


# HSV and HSL differ only in how chroma and the offset are derived, so the
# sextant assembly lives in one place.
.sextant_to_rgb <- function(H, C, m) {
  Hp <- H / 60
  X <- C * (1 - abs(Hp %% 2 - 1))

  R <- .zeros_like(H); G <- .zeros_like(H); B <- .zeros_like(H)
  sel <- function(lo, hi) !is.na(Hp) & Hp >= lo & Hp < hi

  s <- sel(0, 1); R[s] <- C[s]; G[s] <- X[s]
  s <- sel(1, 2); R[s] <- X[s]; G[s] <- C[s]
  s <- sel(2, 3); G[s] <- C[s]; B[s] <- X[s]
  s <- sel(3, 4); G[s] <- X[s]; B[s] <- C[s]
  s <- sel(4, 5); R[s] <- X[s]; B[s] <- C[s]
  s <- sel(5, 6); R[s] <- C[s]; B[s] <- X[s]

  # H exactly 360 falls outside every sextant above and stays at the m
  # offset, which is grey rather than the red it should be. That matches
  # GeoPalette, so it is left alone deliberately: wrap H yourself with
  # H %% 360 before calling if your data can contain 360.
  list(R = R + m, G = G + m, B = B + m)
}


#' HSV to sRGB
#'
#' @param H Hue in 0-360 degrees.
#' @param S,V Saturation and value, each 0-1.
#'
#' @return A named list of three matrices `R`, `G`, `B` in **0-1**.
#'
#' @details
#' `H` is not wrapped. A value of exactly 360 falls outside every sextant
#' and returns grey; pass `H %% 360` if your data can contain it.
#'
#' @examples
#' hsv <- rgb_to_hsv(matrix(200, 1, 1), matrix(100, 1, 1), matrix(50, 1, 1))
#' round(hsv_to_rgb(hsv$H, hsv$S, hsv$V)$R * 255, 2)
#' @export
hsv_to_rgb <- function(H, S, V) {
  v <- .check_bands(H, S, V, names = c("H", "S", "V"))
  H <- v[[1]]; S <- v[[2]]; V <- v[[3]]
  C <- V * S
  .sextant_to_rgb(H, C, V - C)
}


#' HSL to sRGB
#'
#' @param H Hue in 0-360 degrees.
#' @param S,L Saturation and lightness, each 0-1.
#'
#' @return A named list of three matrices `R`, `G`, `B` in **0-1**.
#'
#' @details
#' `H` is not wrapped; see [hsv_to_rgb()].
#'
#' @examples
#' hsl <- rgb_to_hsl(matrix(200, 1, 1), matrix(100, 1, 1), matrix(50, 1, 1))
#' round(hsl_to_rgb(hsl$H, hsl$S, hsl$L)$R * 255, 2)
#' @export
hsl_to_rgb <- function(H, S, L) {
  v <- .check_bands(H, S, L, names = c("H", "S", "L"))
  H <- v[[1]]; S <- v[[2]]; L <- v[[3]]
  C <- (1 - abs(2 * L - 1)) * S
  .sextant_to_rgb(H, C, L - C / 2)
}
