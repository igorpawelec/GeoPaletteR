#' @keywords internal
"_PACKAGE"

#' Colour space conversions for raster bands
#'
#' Every conversion takes three bands as numeric matrices on the 0-255 sRGB
#' scale and returns a named list of matrices, one per component, shaped
#' like the input. Nothing loops over pixels; the arithmetic is vectorised
#' over whole bands, which is why the package needs no compiled code.
#'
#' Use [available_spaces()] for the list of spaces and [convertbands()] to
#' dispatch on a name chosen at runtime.
#'
#' @section Scales differ between spaces:
#' There is no single convention, and the differences bite when bands are
#' stacked or compared:
#'
#' * `hsl`, `hsv` — hue 0-360, the rest 0-1
#' * `hsi` — hue 0-360, saturation 0-100 **per cent**, intensity 0-255
#' * `lab`, `luv`, `lchab`, `lchuv` — L 0-100, the rest unbounded
#' * `jch` — hue spans 0-**324**, being an HSV hue scaled by 0.9
#' * `cam02` — hue 0-360, J and C unbounded
#' * `ycbcr` — studio swing, Y 16-235
#' * inverse conversions return 0-**1**, not 0-255
#'
#' @section Nodata:
#' `NA` propagates through the arithmetic and comes out as `NA`. Pixels
#' that are merely *achromatic* are a different matter: their hue is
#' genuinely undefined, and every function here returns 0 for it rather
#' than `NaN`, matching GeoPalette. Grey, white, black, deep shadow and
#' still water are all achromatic, so this is not an edge case in real
#' imagery.
#'
#' @name GeoPaletteR-conventions
NULL
