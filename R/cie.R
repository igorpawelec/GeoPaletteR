#' RGB to CIELAB
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `L` (0-100), `a` and `b`.
#'
#' @details
#' D65 illuminant, sRGB primaries, with proper sRGB linearisation before the
#' XYZ transform. The cube root keeps its sign, so out-of-gamut values stay
#' finite instead of becoming `NaN`.
#'
#' @examples
#' R <- matrix(c(255, 0, 128), 1, 3)
#' G <- matrix(c(255, 0, 128), 1, 3)
#' B <- matrix(c(255, 0, 128), 1, 3)
#' round(rgb_to_lab(R, G, B)$L, 3)
#' @export
rgb_to_lab <- function(R, G, B) {
  b <- .check_bands(R, G, B)
  xyz <- .rgb_to_xyz(b[[1]], b[[2]], b[[3]])
  .xyz_to_lab(xyz$X, xyz$Y, xyz$Z)
}

# Shared by rgb_to_lab() and rgb_to_dlab().
.xyz_to_lab <- function(X, Y, Z) {
  epsilon <- 0.008856
  kappa <- 903.3

  xr <- X / .Xn; yr <- Y / .Yn; zr <- Z / .Zn
  f <- function(t) {
    out <- (kappa * t + 16) / 116
    hi <- !is.na(t) & t > epsilon
    out[hi] <- .safe_cbrt(t[hi])
    out
  }
  fx <- f(xr); fy <- f(yr); fz <- f(zr)

  list(L = 116 * fy - 16, a = 500 * (fx - fy), b = 200 * (fy - fz))
}


#' RGB to CIELAB plus DIN99
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of six matrices: `L`, `a`, `b` (CIELAB) and `L99`,
#'   `a99`, `b99` (DIN99).
#'
#' @details
#' DIN99 rescales CIELAB so that a unit step is closer to a perceptual step.
#' The second axis is compressed by 0.7 per DIN 6176; leaving that out puts
#' `a99` and `b99` off by several units, which defeats the point of the
#' space.
#'
#' Neutral pixels have `G = 0` and therefore no defined DIN99 hue; they
#' return `a99 = b99 = 0`.
#'
#' @examples
#' R <- matrix(200, 1, 1); G <- matrix(120, 1, 1); B <- matrix(60, 1, 1)
#' round(rgb_to_dlab(R, G, B)$L99, 3)
#' @export
rgb_to_dlab <- function(R, G, B) {
  lab <- rgb_to_lab(R, G, B)
  L_lab <- lab$L; a_lab <- lab$a; b_lab <- lab$b

  L99 <- 105.51 * log1p(0.0158 * L_lab)

  angle <- 16 * pi / 180
  e <- a_lab * cos(angle) + b_lab * sin(angle)
  f <- 0.7 * (-a_lab * sin(angle) + b_lab * cos(angle))

  G_val <- sqrt(e^2 + f^2)
  k_val <- log1p(0.045 * G_val) / 0.045

  a99 <- .zeros_like(G_val)
  b99 <- .zeros_like(G_val)
  nz <- !is.na(G_val) & G_val != 0
  a99[nz] <- (k_val * e)[nz] / G_val[nz]
  b99[nz] <- (k_val * f)[nz] / G_val[nz]

  list(L = L_lab, a = a_lab, b = b_lab, L99 = L99, a99 = a99, b99 = b99)
}


#' RGB to Oklab
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `L`, `a` and `b`.
#'
#' @details
#' Applies sRGB linearisation before the Oklab matrices, which many
#' implementations skip; without it the lightness is wrong across the whole
#' range.
#'
#' @examples
#' R <- matrix(255, 1, 1); G <- matrix(255, 1, 1); B <- matrix(255, 1, 1)
#' round(rgb_to_oklab(R, G, B)$L, 4)
#' @export
rgb_to_oklab <- function(R, G, B) {
  bb <- .check_bands(R, G, B)
  lin <- .rgb_to_linear(bb[[1]], bb[[2]], bb[[3]])
  r <- lin[[1]]; g <- lin[[2]]; b <- lin[[3]]

  l <- 0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b
  m <- 0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b
  s <- 0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b

  l_c <- .safe_cbrt(l); m_c <- .safe_cbrt(m); s_c <- .safe_cbrt(s)

  list(L = 0.2104542553 * l_c + 0.7936177850 * m_c - 0.0040720468 * s_c,
       a = 1.9779984951 * l_c - 2.4285922050 * m_c + 0.4505937099 * s_c,
       b = 0.0259040371 * l_c + 0.7827717662 * m_c - 0.8086757660 * s_c)
}


#' RGB to CIELUV
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `L` (0-100), `u` and `v`.
#'
#' @details
#' Black has `X + 15Y + 3Z = 0`, where the u' and v' chromaticities are
#' undefined. Those pixels take the reference white's chromaticity, which
#' makes `u` and `v` fall out to 0 rather than `NaN`.
#'
#' @examples
#' R <- matrix(c(0, 255), 1, 2); G <- matrix(c(0, 255), 1, 2)
#' B <- matrix(c(0, 255), 1, 2)
#' round(rgb_to_luv(R, G, B)$L, 3)
#' @export
rgb_to_luv <- function(R, G, B) {
  bb <- .check_bands(R, G, B)
  xyz <- .rgb_to_xyz(bb[[1]], bb[[2]], bb[[3]])
  X <- xyz$X; Y <- xyz$Y; Z <- xyz$Z

  epsilon <- 0.008856
  kappa <- 903.3

  yr <- Y / .Yn
  L <- kappa * yr
  hi <- !is.na(yr) & yr > epsilon
  L[hi] <- 116 * .safe_cbrt(yr[hi]) - 16

  denom <- X + 15 * Y + 3 * Z
  denom_ref <- .Xn + 15 * .Yn + 3 * .Zn
  u_ref <- 4 * .Xn / denom_ref
  v_ref <- 9 * .Yn / denom_ref

  u_prime <- .zeros_like(X) + u_ref
  v_prime <- .zeros_like(Y) + v_ref
  nz <- !is.na(denom) & denom != 0
  u_prime[nz] <- 4 * X[nz] / denom[nz]
  v_prime[nz] <- 9 * Y[nz] / denom[nz]

  list(L = L, u = 13 * L * (u_prime - u_ref), v = 13 * L * (v_prime - v_ref))
}


#' RGB to LCH(ab)
#'
#' Cylindrical CIELAB.
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `L`, `C` and `Hab` (0-360
#'   degrees).
#'
#' @examples
#' R <- matrix(200, 1, 1); G <- matrix(50, 1, 1); B <- matrix(50, 1, 1)
#' round(rgb_to_lchab(R, G, B)$C, 3)
#' @export
rgb_to_lchab <- function(R, G, B) {
  lab <- rgb_to_lab(R, G, B)
  ch <- .to_cylindrical(lab$a, lab$b)
  list(L = lab$L, C = ch$C, Hab = ch$H)
}


#' RGB to LCH(uv)
#'
#' Cylindrical CIELUV.
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `L`, `C` and `Huv` (0-360
#'   degrees).
#'
#' @examples
#' R <- matrix(200, 1, 1); G <- matrix(50, 1, 1); B <- matrix(50, 1, 1)
#' round(rgb_to_lchuv(R, G, B)$C, 3)
#' @export
rgb_to_lchuv <- function(R, G, B) {
  luv <- rgb_to_luv(R, G, B)
  ch <- .to_cylindrical(luv$u, luv$v)
  list(L = luv$L, C = ch$C, Huv = ch$H)
}

# Opponent pair -> chroma and hue angle in [0, 360).
.to_cylindrical <- function(a, b) {
  C <- sqrt(a^2 + b^2)
  H <- atan2(b, a) * 180 / pi
  neg <- !is.na(H) & H < 0
  H[neg] <- H[neg] + 360
  list(C = C, H = H)
}


#' RGB to CIE xyY
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `x` and `y_ch` (chromaticity) and
#'   `Y_lum` (luminance).
#'
#' @details
#' The component names carry the `_ch` and `_lum` suffixes that GeoPalette
#' uses, so that band files written from either package line up. `y_ch` is a
#' chromaticity in 0-1 and `Y_lum` is a luminance; they are not the same
#' quantity despite the letters.
#'
#' Black has `X + Y + Z = 0` and no defined chromaticity; those pixels get
#' `x = y = 1/3`, the equal-energy point.
#'
#' @examples
#' R <- matrix(c(0, 255), 1, 2); G <- matrix(c(0, 255), 1, 2)
#' B <- matrix(c(0, 255), 1, 2)
#' round(rgb_to_xyY(R, G, B)$x, 4)
#' @export
rgb_to_xyY <- function(R, G, B) {
  bb <- .check_bands(R, G, B)
  xyz <- .rgb_to_xyz(bb[[1]], bb[[2]], bb[[3]])
  X <- xyz$X; Y <- xyz$Y; Z <- xyz$Z

  s <- X + Y + Z
  x <- .zeros_like(X) + 1 / 3
  y <- .zeros_like(Y) + 1 / 3
  nz <- !is.na(s) & s != 0
  x[nz] <- X[nz] / s[nz]
  y[nz] <- Y[nz] / s[nz]

  list(x = x, y_ch = y, Y_lum = Y)
}


#' RGB to YCbCr
#'
#' BT.601 **studio swing**.
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `Y` (16-235), `Cb` and `Cr`
#'   (16-240).
#'
#' @details
#' Studio range, not full range: black maps to `Y = 16` and white to
#' `Y = 235`, matching the ITU-R BT.601 broadcast convention. For the full
#' 0-255 swing, scale afterwards with `Y_full <- (Y - 16) * 255 / 219`.
#'
#' @examples
#' R <- matrix(c(0, 255), 1, 2); G <- matrix(c(0, 255), 1, 2)
#' B <- matrix(c(0, 255), 1, 2)
#' round(rgb_to_ycbcr(R, G, B)$Y, 3)
#' @export
rgb_to_ycbcr <- function(R, G, B) {
  b <- .check_bands(R, G, B)
  Rf <- b[[1]]; Gf <- b[[2]]; Bf <- b[[3]]

  list(Y  = 16 + (65.481 * Rf + 128.553 * Gf + 24.966 * Bf) / 255,
       Cb = 128 + (-37.797 * Rf - 74.203 * Gf + 112.000 * Bf) / 255,
       Cr = 128 + (112.000 * Rf - 93.786 * Gf - 18.214 * Bf) / 255)
}
