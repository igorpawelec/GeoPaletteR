# CAT02 chromatic adaptation matrix (Moroney et al. 2002)
.MCAT02 <- matrix(c( 0.7328,  0.4296, -0.1624,
                    -0.7036,  1.6975,  0.0061,
                     0.0030,  0.0136,  0.9834), nrow = 3, byrow = TRUE)

# Hunt-Pointer-Estevez matrix (post-adaptation cone space)
.MHPE <- matrix(c( 0.38971, 0.68898, -0.07868,
                  -0.22981, 1.18340,  0.04641,
                   0.00000, 0.00000,  1.00000), nrow = 3, byrow = TRUE)

# (F, c, N_c) per surround, CIECAM02 Table A1
.CAM02_SURROUND <- list(average = c(1.0, 0.690, 1.00),
                        dim     = c(0.9, 0.590, 0.90),
                        dark    = c(0.8, 0.525, 0.80))

# D65 white point on the 0-100 scale, 2 degree observer
.CAM02_WHITE_D65 <- c(95.047, 100.0, 108.883)

# Apply a 3x3 to three parallel arrays. The Python does xyz %*% t(M) on a
# stacked (.., 3) array; keeping the bands separate avoids materialising
# that stack, which on a large raster is a third copy of the image.
.mat3 <- function(M, x, y, z) {
  list(M[1, 1] * x + M[1, 2] * y + M[1, 3] * z,
       M[2, 1] * x + M[2, 2] * y + M[2, 3] * z,
       M[3, 1] * x + M[3, 2] * y + M[3, 3] * z)
}


#' RGB to CIECAM02
#'
#' The real CIECAM02 forward model — full CAT02 chromatic adaptation, the
#' surround terms and the background induction. Not the [rgb_to_jch()]
#' stand-in.
#'
#' @inheritParams rgb_to_hsl
#' @param L_A Adapting field luminance in cd/m^2. Rule of thumb: about a
#'   fifth of the scene white luminance. 64 suits a screen; reach for about
#'   318 for imagery meant to represent a sunlit outdoor scene.
#' @param Y_b Relative luminance of the background. 20 is the usual
#'   grey-world assumption.
#' @param surround One of `"average"` (normal viewing), `"dim"` (a lit room)
#'   or `"dark"` (a darkened one).
#' @param whitepoint Reference white as XYZ on the 0-100 scale. Defaults to
#'   D65.
#'
#' @return A named list of three matrices: `J` (lightness), `C` (chroma) and
#'   `h` (hue angle, 0-360 degrees).
#'
#' @details
#' Unlike every other conversion here, CIECAM02 is not a fixed function of
#' RGB — it models an *observer*, so it needs the viewing conditions. They
#' genuinely move the numbers: `J` shifts by roughly 8 units between
#' `"average"` and `"dark"`. Report the parameters you used, or the values
#' are not reproducible.
#'
#' Achromatic pixels stay finite without special-casing. The model adds 0.1
#' to every adapted cone response, so the chroma denominator is at least
#' 0.305 for any pixel, black included, and black falls out to `J = 0`,
#' `C = 0`, `h = 0` on its own.
#'
#' @seealso [rgb_to_jch()] when you only need a cheap lightness/chroma/hue
#'   split and the values do not have to mean anything.
#'
#' @examples
#' R <- matrix(c(255, 0, 128), 1, 3)
#' G <- matrix(c(255, 0, 64), 1, 3)
#' B <- matrix(c(255, 0, 32), 1, 3)
#' round(rgb_to_cam02(R, G, B)$J, 3)
#' # viewing conditions are not a free choice
#' round(rgb_to_cam02(R, G, B, surround = "dark")$J, 3)
#' @export
rgb_to_cam02 <- function(R, G, B, L_A = 64, Y_b = 20, surround = "average",
                         whitepoint = NULL) {
  if (!is.character(surround) || length(surround) != 1L ||
      !surround %in% names(.CAM02_SURROUND))
    stop("surround must be one of ",
         paste(sQuote(names(.CAM02_SURROUND)), collapse = ", "),
         call. = FALSE)
  # Fs, not F: F is R's abbreviation for FALSE, and shadowing it inside a
  # function is legal but reads as a bug to anyone scanning the code.
  sp <- .CAM02_SURROUND[[surround]]
  Fs <- sp[1]; cc <- sp[2]; N_c <- sp[3]

  XYZ_w <- if (is.null(whitepoint)) .CAM02_WHITE_D65 else as.numeric(whitepoint)
  if (length(XYZ_w) != 3L || anyNA(XYZ_w))
    stop("whitepoint must be 3 finite values (XYZ on the 0-100 scale)",
         call. = FALSE)

  bands <- .check_bands(R, G, B)
  xyz <- .rgb_to_xyz(bands[[1]], bands[[2]], bands[[3]])
  X <- xyz$X * 100; Y <- xyz$Y * 100; Z <- xyz$Z * 100

  Yw <- XYZ_w[2]

  # viewing-condition constants
  rgb_w <- as.numeric(.MCAT02 %*% XYZ_w)
  D <- min(max(Fs * (1 - (1 / 3.6) * exp((-L_A - 42) / 92)), 0), 1)
  D_rgb <- D * Yw / rgb_w + (1 - D)

  k <- 1 / (5 * L_A + 1)
  F_L <- 0.2 * k^4 * (5 * L_A) + 0.1 * (1 - k^4)^2 * (5 * L_A)^(1 / 3)
  n <- Y_b / Yw
  N_bb <- 0.725 * (1 / n)^0.2
  N_cb <- N_bb
  z <- 1.48 + sqrt(n)

  # Hoisted: adapt() runs once for the image and once for the white point,
  # and this inverse does not depend on either.
  P <- .MHPE %*% solve(.MCAT02)

  adapt <- function(x, y, zz) {
    rgb <- .mat3(.MCAT02, x, y, zz)
    rc1 <- rgb[[1]] * D_rgb[1]
    rc2 <- rgb[[2]] * D_rgb[2]
    rc3 <- rgb[[3]] * D_rgb[3]
    rp <- .mat3(P, rc1, rc2, rc3)
    lapply(rp, function(v) {
      t <- (F_L * abs(v) / 100)^0.42
      sign(v) * 400 * t / (27.13 + t) + 0.1
    })
  }
  achromatic <- function(a) (2 * a[[1]] + a[[2]] + a[[3]] / 20 - 0.305) * N_bb

  rgb_a <- adapt(X, Y, Z)
  A_w <- achromatic(adapt(XYZ_w[1], XYZ_w[2], XYZ_w[3]))

  a <- rgb_a[[1]] - 12 * rgb_a[[2]] / 11 + rgb_a[[3]] / 11
  b <- (rgb_a[[1]] + rgb_a[[2]] - 2 * rgb_a[[3]]) / 9
  A <- achromatic(rgb_a)

  J <- 100 * sign(A) * (abs(A) / A_w)^(cc * z)

  h <- (atan2(b, a) * 180 / pi) %% 360

  e_t <- 0.25 * (cos(h * pi / 180 + 2) + 3.8)
  denom <- rgb_a[[1]] + rgb_a[[2]] + 21 * rgb_a[[3]] / 20
  t <- (50000 / 13) * N_c * N_cb * e_t * sqrt(a^2 + b^2) / denom

  C <- t^0.9 * sqrt(pmax(J, 0) / 100) * (1.64 - 0.29^n)^0.73

  list(J = J, C = C, h = h)
}
