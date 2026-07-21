#' Read the RGB bands of a raster
#'
#' @param x A `SpatRaster` or a path to a raster file. Bands 1-3 are taken
#'   as R, G and B; further bands are ignored.
#'
#' @return A list with matrices `R`, `G`, `B` and the `template`
#'   `SpatRaster` the geometry came from.
#'
#' @details
#' Requires \pkg{terra}, which is only a Suggests: every conversion in this
#' package works on plain matrices and has no spatial dependency at all.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   r <- terra::rast(nrows = 4, ncols = 4, nlyrs = 3, vals = runif(48, 0, 255))
#'   bands <- read_rgb(r)
#'   dim(bands$R)
#' }
#' }
#' @export
read_rgb <- function(x) {
  .need_terra()
  r <- if (inherits(x, "SpatRaster")) x else terra::rast(x)
  if (terra::nlyr(r) < 3L)
    stop("need at least 3 bands for R, G and B, got ", terra::nlyr(r),
         call. = FALSE)
  band <- function(i) terra::as.matrix(r[[i]], wide = TRUE)
  list(R = band(1), G = band(2), B = band(3), template = r)
}


#' Convert a raster to a colour space and write it out
#'
#' @param input A `SpatRaster` or a path to an RGB raster.
#' @param output Path for the multi-band output raster. If `NULL`, nothing
#'   is written and the components are returned only.
#' @param space One of [available_spaces()].
#' @param ... Passed to the conversion; see [convertbands()].
#'
#' @return Invisibly, the named list of component matrices.
#'
#' @details
#' Band names in the output are the component names of the space, so a
#' file written here is self-describing and lines up with what GeoPalette
#' writes for the same scene.
#'
#' Requires \pkg{terra}.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   r <- terra::rast(nrows = 4, ncols = 4, nlyrs = 3, vals = runif(48, 0, 255))
#'   out <- convert_raster(r, output = NULL, space = "lab")
#'   names(out)
#' }
#' }
#' @export
convert_raster <- function(input, output = NULL, space = "lab", ...) {
  .need_terra()
  bands <- read_rgb(input)
  comps <- convertbands(bands$R, bands$G, bands$B, space, ...)

  if (!is.null(output)) {
    tmpl <- bands$template
    layers <- lapply(comps, function(m) {
      r <- terra::rast(tmpl[[1]])
      # t(): terra fills a layer row-major, and as.matrix(wide = TRUE)
      # handed us row-major too, so the transpose puts them back in step.
      # Without it the output is the transpose of the input geometry, which
      # looks plausible on a square raster and is wrong on every other one.
      terra::values(r) <- as.vector(t(m))
      r
    })
    out <- terra::rast(layers)
    names(out) <- names(comps)
    terra::writeRaster(out, output, overwrite = TRUE)
  }
  invisible(comps)
}

.need_terra <- function() {
  if (!requireNamespace("terra", quietly = TRUE))
    stop("this function needs the 'terra' package; ",
         "the conversions themselves do not", call. = FALSE)
}
