# Internal helpers shared by every conversion.
#
# GeoPalette (Python) stores intermediates as float32. R has no float32 —
# every numeric is a double — so this package computes the whole chain in
# double and agrees with GeoPalette only to that storage precision. See
# ?GeoPaletteR-agreement for the measured tolerances; do not "fix" a
# fifth-decimal difference against the Python output, it is expected.

# D65 white point, sRGB reference.
.Xn <- 0.95047
.Yn <- 1.00000
.Zn <- 1.08883

# Every entry point takes three bands, so the checking lives in one place.
# Returns the bands as doubles, dimensions intact.
.check_bands <- function(R, G, B, names = c("R", "G", "B")) {
  bands <- list(R, G, B)
  for (i in seq_along(bands)) {
    x <- bands[[i]]
    if (!is.numeric(x))
      stop(names[i], " must be numeric, got ", class(x)[1], call. = FALSE)
    bands[[i]] <- x * 1.0          # integer -> double, keeps dim and dimnames
  }
  d <- lapply(bands, dim)
  if (!identical(d[[1]], d[[2]]) || !identical(d[[1]], d[[3]]))
    stop("R, G and B must have identical dimensions", call. = FALSE)
  bands
}

# Cube root that keeps the sign.
#
# R's `x^(1/3)` is NaN for negative x, so the naive translation of numpy's
# cbrt silently turns every out-of-gamut Lab pixel into NaN. Matches
# geopalette._safe_cbrt.
.safe_cbrt <- function(x) sign(x) * abs(x)^(1 / 3)

# Inverse sRGB companding: gamma-encoded [0,1] -> linear [0,1].
.srgb_to_linear <- function(c) {
  out <- ((c + 0.055) / 1.055)^2.4
  lo <- !is.na(c) & c <= 0.04045
  out[lo] <- c[lo] / 12.92
  out
}

# sRGB companding: linear [0,1] -> gamma-encoded [0,1].
.linear_to_srgb <- function(c) {
  c <- pmax(c, 0)
  out <- 1.055 * c^(1 / 2.4) - 0.055
  lo <- !is.na(c) & c <= 0.0031308
  out[lo] <- 12.92 * c[lo]
  out
}

# 0-255 sRGB -> linearized RGB.
.rgb_to_linear <- function(R, G, B) {
  list(.srgb_to_linear(R / 255), .srgb_to_linear(G / 255),
       .srgb_to_linear(B / 255))
}

# Linear RGB [0,1] -> CIE XYZ, D65 illuminant with sRGB primaries.
.linear_rgb_to_xyz <- function(r, g, b) {
  list(X = 0.4124564 * r + 0.3575761 * g + 0.1804375 * b,
       Y = 0.2126729 * r + 0.7151522 * g + 0.0721750 * b,
       Z = 0.0193339 * r + 0.1191920 * g + 0.9503041 * b)
}

# 0-255 sRGB -> CIE XYZ (D65), the full pipeline.
.rgb_to_xyz <- function(R, G, B) {
  lin <- .rgb_to_linear(R, G, B)
  .linear_rgb_to_xyz(lin[[1]], lin[[2]], lin[[3]])
}

# Hue sextant shared by HSL, HSV and the JCH stand-in. Returns degrees in
# [0, 360).
#
# Indexed assignment rather than ifelse() on purpose: the divisor is the
# max-min delta, which is exactly 0 on every achromatic pixel — grey,
# white, black, shadow, water. Dividing the whole array and selecting
# afterwards computes 0/0 on all of them and only discards it later.
#
# On ties this deliberately differs from GeoPalette in form but not in
# result. There the three masks overlap and are assigned in order, so when
# two channels are both the maximum the *last* branch wins; the masks here
# are exclusive, so the *first* wins. The values coincide either way:
#   R==B max -> mr gives -60, mb gives 300, and the wrap below maps -60 to 300
#   R==G max -> both branches give 60
#   G==B max -> both branches give 180
# Only the mr branch can go negative, which is why one wrap at the end is
# enough, and why HSL, HSV and JCH can share this despite folding the
# wrap into three different places upstream.
.hue_sextant <- function(Rn, Gn, Bn, cmax, delta) {
  H <- .zeros_like(cmax)
  m <- !is.na(delta) & delta != 0
  mr <- m & cmax == Rn
  mg <- m & !mr & cmax == Gn
  mb <- m & !mr & !mg & cmax == Bn
  H[mr] <- 60 * ((Gn - Bn)[mr] / delta[mr])
  H[mg] <- 60 * (((Bn - Rn)[mg] / delta[mg]) + 2)
  H[mb] <- 60 * (((Rn - Gn)[mb] / delta[mb]) + 4)
  neg <- !is.na(H) & H < 0
  H[neg] <- H[neg] + 360
  H
}

# Zero array shaped like x. Not array(0, dim = dim(x)): dim() is NULL for a
# plain vector and array() errors on a zero-length dim, so a package that
# only ever saw matrices in testing would break on the first vector input.
.zeros_like <- function(x) {
  out <- numeric(length(x))
  if (!is.null(dim(x))) dim(out) <- dim(x)
  out
}
