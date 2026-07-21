#' RGB to Jzazbz
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `Jz`, `az` and `bz`.
#'
#' @details
#' Safdar et al. (2017), "Perceptually uniform color space for image signals
#' including high dynamic range and wide gamut". Absolute luminance assumes
#' an SDR peak of about 203 cd/m^2.
#'
#' Two details are easy to get wrong and are worth stating, because both
#' produce plausible-looking output:
#'
#' * X and Y are pre-scaled before the LMS matrix (eq. 8-9). Without it `Jz`
#'   is out by roughly 80 per cent of its own range, since the matrix is
#'   defined against the primed values rather than raw XYZ.
#' * `Jz` uses two constants, not one. `d` shapes the curve and `d0` only
#'   offsets it; using `d0` in place of `d` collapses the result to
#'   `Jz` approximately `Iz`, because `d0` is about 1.6e-11.
#'
#' The PQ curve carries an exponent of 134, which amplifies small input
#' differences. That is why this space needs a looser tolerance than the
#' others when comparing against GeoPalette — see `?GeoPaletteR-agreement`.
#'
#' @examples
#' R <- matrix(c(255, 0), 1, 2); G <- matrix(c(255, 0), 1, 2)
#' B <- matrix(c(255, 0), 1, 2)
#' signif(rgb_to_jzazbz(R, G, B)$Jz, 5)
#' @export
rgb_to_jzazbz <- function(R, G, B) {
  bb <- .check_bands(R, G, B)
  xyz <- .rgb_to_xyz(bb[[1]], bb[[2]], bb[[3]])

  X_abs <- xyz$X * 203
  Y_abs <- xyz$Y * 203
  Z_abs <- xyz$Z * 203

  b_pre <- 1.15
  g_pre <- 0.66
  X_p <- b_pre * X_abs - (b_pre - 1) * Z_abs
  Y_p <- g_pre * Y_abs - (g_pre - 1) * X_abs

  Lp <-  0.41478972 * X_p + 0.579999 * Y_p + 0.01464800 * Z_abs
  Mp <- -0.20151000 * X_p + 1.120649 * Y_p + 0.05310080 * Z_abs
  Sp <- -0.01660080 * X_p + 0.264800 * Y_p + 0.66847990 * Z_abs

  c1 <- 3424 / 4096
  c2 <- 2413 / 128
  c3 <- 2392 / 128
  nn <- 2610 / 16384
  pp <- 1.7 * 2523 / 32

  pq <- function(x) {
    x <- pmax(x / 10000, 0)
    xn <- x^nn
    ((c1 + c2 * xn) / (1 + c3 * xn))^pp
  }

  L_pq <- pq(Lp); M_pq <- pq(Mp); S_pq <- pq(Sp)

  Iz <- 0.5 * L_pq + 0.5 * M_pq
  az <- 3.524000 * L_pq - 4.066708 * M_pq + 0.542708 * S_pq
  bz <- 0.199076 * L_pq + 1.096799 * M_pq - 1.295875 * S_pq

  d <- -0.56
  d0 <- 1.6295499532821566e-11
  Jz <- ((1 + d) * Iz) / (1 + d * Iz) - d0

  list(Jz = Jz, az = az, bz = bz)
}


#' RGB to JzCzHz
#'
#' Cylindrical [rgb_to_jzazbz()].
#'
#' @inheritParams rgb_to_hsl
#'
#' @return A named list of three matrices: `Jz`, `Cz` and `hz` (0-360
#'   degrees).
#'
#' @examples
#' R <- matrix(200, 1, 1); G <- matrix(50, 1, 1); B <- matrix(50, 1, 1)
#' signif(rgb_to_jzczhz(R, G, B)$Cz, 5)
#' @export
rgb_to_jzczhz <- function(R, G, B) {
  j <- rgb_to_jzazbz(R, G, B)
  ch <- .to_cylindrical(j$az, j$bz)
  list(Jz = j$Jz, Cz = ch$C, hz = ch$H)
}
