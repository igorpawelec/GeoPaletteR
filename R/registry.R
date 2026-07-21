# Space name -> conversion. The names match GeoPalette's registry exactly,
# so a band file written from either package can be read by the other
# without a translation table.
.CONVERSIONS <- list(
  dlab   = "rgb_to_dlab",
  hsl    = "rgb_to_hsl",
  hsi    = "rgb_to_hsi",
  hsv    = "rgb_to_hsv",
  jch    = "rgb_to_jch",
  cam02  = "rgb_to_cam02",
  jzazbz = "rgb_to_jzazbz",
  jzczhz = "rgb_to_jzczhz",
  lab    = "rgb_to_lab",
  lchab  = "rgb_to_lchab",
  lchuv  = "rgb_to_lchuv",
  luv    = "rgb_to_luv",
  oklab  = "rgb_to_oklab",
  xyY    = "rgb_to_xyY",
  ycbcr  = "rgb_to_ycbcr"
)


#' Supported colour spaces
#'
#' @return A character vector of space names, sorted.
#'
#' @details
#' The same 15 names GeoPalette uses, so `available_spaces()` agrees across
#' the two packages.
#'
#' @examples
#' available_spaces()
#' @export
available_spaces <- function() sort(names(.CONVERSIONS))


#' Convert RGB bands to a named colour space
#'
#' A dispatcher over every `rgb_to_*` function, for when the target space is
#' chosen at runtime.
#'
#' @inheritParams rgb_to_hsl
#' @param space One of [available_spaces()].
#' @param ... Passed to the conversion. Only [rgb_to_cam02()] takes extra
#'   arguments (`L_A`, `Y_b`, `surround`, `whitepoint`); supplying them for
#'   any other space is an error rather than a silent no-op.
#'
#' @return A named list of matrices. The names are the component names for
#'   that space — `L`, `a`, `b` for `"lab"`, and so on — which is also what
#'   GeoPalette returns alongside its arrays.
#'
#' @examples
#' R <- matrix(c(255, 0, 128), 1, 3)
#' G <- matrix(c(255, 0, 64), 1, 3)
#' B <- matrix(c(255, 0, 32), 1, 3)
#' names(convertbands(R, G, B, "lab"))
#' names(convertbands(R, G, B, "dlab"))
#' @export
convertbands <- function(R, G, B, space, ...) {
  if (!is.character(space) || length(space) != 1L)
    stop("space must be a single string", call. = FALSE)
  fn <- .CONVERSIONS[[space]]
  if (is.null(fn))
    stop("Unknown space '", space, "'. Available: ",
         paste(available_spaces(), collapse = ", "), call. = FALSE)
  # Reject stray arguments here rather than letting them vanish: passing
  # surround = "dark" to "lab" is a misunderstanding worth reporting, and
  # do.call would otherwise fail with a message about the wrong function.
  if (...length() > 0L && space != "cam02")
    stop("'", space, "' takes no extra arguments; only 'cam02' does",
         call. = FALSE)
  do.call(fn, c(list(R, G, B), list(...)))
}
