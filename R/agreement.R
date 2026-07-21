#' How closely this package matches GeoPalette
#'
#' GeoPaletteR is the R twin of the GeoPalette Python package. They
#' implement the same transforms, but they are **not** bit-for-bit
#' identical and cannot be, so this page says what the agreement actually
#' is and where it stops.
#'
#' @section Why exact agreement is impossible:
#' GeoPalette stores its intermediates as single-precision floats and casts
#' back to single precision at every step. R has no single-precision
#' numeric type, so this package computes the whole chain in double. The
#' two therefore part company at roughly the seventh significant figure by
#' construction.
#'
#' This is not a defect on either side. Two single-precision pipelines that
#' differ only in *where* they round disagree by about 3e-4 in CIELAB `a`,
#' which is larger than the gap measured here — the double-precision result
#' is the more accurate of the two, and sits closer to reference
#' implementations such as `colour-science`.
#'
#' @section Measured agreement:
#' Across 1600 pixels, including black, white, greys, near-greys and all
#' six primaries and secondaries, every one of the 60 components of the 15
#' forward spaces and 4 inverse transforms agrees with GeoPalette 0.4.0 to
#' better than **1.7e-6 of the component's own range**. In absolute terms:
#'
#' * Oklab, xyY, Jzazbz — 1e-7 or better
#' * CIELAB, CIELUV, DIN99, HSL, HSV, JCH, YCbCr, CIECAM02 — 1e-4 or better
#' * hue angles — 6e-4 degrees or better, with the exception below
#' * inverse transforms — 7e-6 or better, on the 0-1 output scale
#'
#' Reproduce with `tools/generate_geopalette_reference.py` followed by
#' `tools/cross_validate_against_geopalette.R`.
#'
#' @section Hue is undefined on the neutral axis:
#' The one place the two packages genuinely disagree is the hue angle of a
#' colour that has no hue. `Hab`, `Huv`, `hz` and CIECAM02 `h` are computed
#' with `atan2()` over an opponent pair that is exactly 0 for a neutral
#' pixel, so the angle is decided by floating-point noise. On pure greys
#' GeoPalette reports 0, 90 or 141.34 depending on the pixel; this package
#' reports 158.199. Both are artefacts, neither is meaningful, and the
#' difference reaches 158 degrees.
#'
#' It affects only pixels with essentially zero chroma — in the run above,
#' 5 pixels out of 1600, all with chroma below 1.8e-5. Above a chroma of
#' 0.01 the largest hue difference is 4.4e-4 degrees.
#'
#' The practical consequence: **do not segment or classify on a hue band
#' without masking low-chroma pixels first.** Grey, white, black, deep
#' shadow and still water are all neutral, so this is common in real
#' imagery rather than a corner case. The hues from [rgb_to_hsl()],
#' [rgb_to_hsv()], [rgb_to_hsi()] and [rgb_to_jch()] are not affected:
#' those come from a max-minus-min sextant and both packages force them to
#' 0 when the pixel is achromatic.
#'
#' @name GeoPaletteR-agreement
NULL
