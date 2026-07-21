#' RGB to HSL
#'
#' @param R,G,B Numeric matrices of the same dimensions, sRGB on the 0-255
#'   scale. Integer matrices are accepted and converted.
#'
#' @return A named list of three matrices: `H` (0-360 degrees), `S` (0-1)
#'   and `L` (0-1), each shaped like the input.
#'
#' @details
#' Operates on gamma-encoded values, as HSL is defined to. `NA` propagates.
#' Achromatic pixels — grey, white, black, and most water and shadow — have
#' no defined hue; they return `H = 0` rather than `NaN`.
#'
#' @examples
#' R <- matrix(c(255, 0, 0, 128), 2, 2)
#' G <- matrix(c(0, 255, 0, 128), 2, 2)
#' B <- matrix(c(0, 0, 255, 128), 2, 2)
#' rgb_to_hsl(R, G, B)$H
#' @export
rgb_to_hsl <- function(R, G, B) {
  b <- .check_bands(R, G, B)
  Rn <- b[[1]] / 255; Gn <- b[[2]] / 255; Bn <- b[[3]] / 255

  cmax <- pmax(Rn, Gn, Bn)
  cmin <- pmin(Rn, Gn, Bn)
  delta <- cmax - cmin

  L <- (cmax + cmin) / 2
  H <- .hue_sextant(Rn, Gn, Bn, cmax, delta)

  S <- .zeros_like(cmax)
  denom <- 1 - abs(2 * L - 1)
  nz <- !is.na(delta) & delta != 0 & denom != 0
  S[nz] <- delta[nz] / denom[nz]

  list(H = H, S = S, L = L)
}


#' RGB to HSV
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `H` (0-360 degrees), `S` (0-1)
#'   and `V` (0-1).
#'
#' @details
#' Saturation is `delta / cmax`, so it is 0 for black, where `cmax` is 0 and
#' the ratio is undefined.
#'
#' @examples
#' R <- matrix(c(255, 0), 1, 2); G <- matrix(c(0, 255), 1, 2)
#' B <- matrix(c(0, 0), 1, 2)
#' rgb_to_hsv(R, G, B)$V
#' @export
rgb_to_hsv <- function(R, G, B) {
  b <- .check_bands(R, G, B)
  Rn <- b[[1]] / 255; Gn <- b[[2]] / 255; Bn <- b[[3]] / 255

  cmax <- pmax(Rn, Gn, Bn)
  cmin <- pmin(Rn, Gn, Bn)
  delta <- cmax - cmin

  H <- .hue_sextant(Rn, Gn, Bn, cmax, delta)

  S <- .zeros_like(cmax)
  nz <- !is.na(cmax) & cmax != 0
  S[nz] <- delta[nz] / cmax[nz]

  list(H = H, S = S, V = cmax)
}


#' RGB to HSI
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `H` (0-360 degrees), `S` (per
#'   cent, 0-100) and `I` (0-255).
#'
#' @details
#' Note the scales, which differ from [rgb_to_hsl()] and [rgb_to_hsv()]:
#' saturation is a percentage and intensity keeps the 0-255 range of the
#' input rather than being normalised.
#'
#' @examples
#' R <- matrix(c(255, 10), 1, 2); G <- matrix(c(0, 10), 1, 2)
#' B <- matrix(c(0, 10), 1, 2)
#' rgb_to_hsi(R, G, B)$S
#' @export
rgb_to_hsi <- function(R, G, B) {
  b <- .check_bands(R, G, B)
  Rf <- b[[1]]; Gf <- b[[2]]; Bf <- b[[3]]

  sum_rgb <- Rf + Gf + Bf
  sum_rgb[!is.na(sum_rgb) & sum_rgb == 0] <- 1
  r <- Rf / sum_rgb; g <- Gf / sum_rgb; bb <- Bf / sum_rgb

  num <- 0.5 * ((r - g) + (r - bb))
  den <- sqrt((r - g)^2 + (r - bb) * (g - bb))
  den[!is.na(den) & den == 0] <- NaN

  h_rad <- acos(pmin(pmax(num / den, -1), 1))
  flip <- !is.na(bb) & !is.na(g) & bb > g
  h_rad[flip] <- 2 * pi - h_rad[flip]
  H <- h_rad * 180 / pi
  # Achromatic pixels reach here as NaN by construction (den forced to NaN
  # above) and become 0, matching GeoPalette's nan_to_num. Genuine NA in the
  # input is left alone: is.nan() is FALSE for NA.
  H[is.nan(H)] <- 0

  S <- (1 - 3 * pmin(r, g, bb)) * 100
  I <- (Rf + Gf + Bf) / 3

  list(H = H, S = S, I = I)
}


#' RGB to a simplified JCH
#'
#' A cheap lightness/chroma/hue split. **This is not CIECAM02** — use
#' [rgb_to_cam02()] when the values have to mean something.
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `J` (0-100), `C` (0-100) and `H`
#'   (0-324 degrees).
#'
#' @details
#' Built from relative luminance plus HSV-style chroma and hue, with no
#' chromatic adaptation, surround or background term. Against a real
#' CIECAM02 transform it tracks J at about r = 0.98 and C at about
#' r = 0.89, but the hue can be off by as much as 70 degrees.
#'
#' Note the hue range. `H` is an HSV hue scaled by 0.9, so it spans 0-324
#' degrees — neither the 0-360 of a hue angle nor the 0-400 of CIECAM02 hue
#' quadrature. Do not feed it to anything expecting either.
#'
#' @seealso [rgb_to_cam02()] for the real appearance model.
#'
#' @examples
#' R <- matrix(200, 1, 1); G <- matrix(100, 1, 1); B <- matrix(50, 1, 1)
#' rgb_to_jch(R, G, B)$J
#' @export
rgb_to_jch <- function(R, G, B) {
  b <- .check_bands(R, G, B)
  xyz <- .rgb_to_xyz(b[[1]], b[[2]], b[[3]])
  J <- xyz$Y * 100

  Rn <- b[[1]] / 255; Gn <- b[[2]] / 255; Bn <- b[[3]] / 255
  cmax <- pmax(Rn, Gn, Bn)
  cmin <- pmin(Rn, Gn, Bn)
  delta <- cmax - cmin

  C <- delta * 100
  H <- .hue_sextant(Rn, Gn, Bn, cmax, delta) * 0.9

  list(J = J, C = C, H = H)
}
