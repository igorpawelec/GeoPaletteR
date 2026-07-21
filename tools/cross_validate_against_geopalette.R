# Measure how far GeoPaletteR sits from GeoPalette, component by component.
#
# This cannot be an equality check and never will be. GeoPalette stores its
# intermediates as float32; R has no float32, so this package computes the
# whole chain in double. The two therefore disagree at roughly the seventh
# significant figure by construction, and the gap is amplified by whatever
# the space does downstream — the PQ curve in Jzazbz carries an exponent of
# 134, so it amplifies hard.
#
# The point of this script is to put a number on that per component, so the
# figures quoted in ?GeoPaletteR-agreement and in the README are measured
# rather than guessed. It lives in tools/ and not tests/ because it needs
# Python and GeoPalette installed, which CRAN machines do not have.
#
#   python3 tools/generate_geopalette_reference.py
#   Rscript tools/cross_validate_against_geopalette.R
#
# Copyright (C) 2025 Igor Pawelec. Licence: GPLv3.

library(GeoPaletteR)

dir <- file.path("tools", "reference")
if (!dir.exists(dir))
  stop("run tools/generate_geopalette_reference.py first", call. = FALSE)

rd <- function(f) as.matrix(utils::read.csv(file.path(dir, paste0(f, ".csv")),
                                            header = FALSE))
R <- rd("R"); G <- rd("G"); B <- rd("B")

spaces <- utils::read.csv(file.path(dir, "spaces.csv"), header = FALSE,
                          col.names = c("space", "comps"),
                          colClasses = "character")

# Hue angles derived with atan2 are undefined on the neutral axis, where
# the opponent pair is 0 and only float noise decides the angle. Comparing
# them there measures nothing: on a pure grey GeoPalette reports 0, 90 or
# 141.34 depending on the pixel, and this package reports 158.199. Both are
# artefacts. So each such hue is compared only where its own chroma is
# meaningful, and the skipped pixels are counted rather than hidden.
#
# The hues of HSL, HSV, HSI and JCH are not in this list on purpose: those
# are computed from a max-min sextant, and both packages force them to 0
# when the pixel is achromatic, so they agree everywhere.
.HUE_OF <- list(lchab = c(hue = "Hab", chroma = "C"),
                lchuv = c(hue = "Huv", chroma = "C"),
                jzczhz = c(hue = "hz", chroma = "Cz"),
                cam02 = c(hue = "h", chroma = "C"))

cat(sprintf("%-14s %-7s %12s %12s %12s %7s\n",
            "space", "comp", "max|diff|", "max rel", "range", "skipped"))
cat(strrep("-", 70), "\n")

worst <- list()
rel_worst <- list()
report <- function(tag, comp, got, ref, keep = NULL) {
  n_skip <- 0L
  if (!is.null(keep)) {
    n_skip <- sum(!keep)
    got <- got[keep]; ref <- ref[keep]
  }
  d <- abs(got - ref)
  span <- max(ref) - min(ref)
  rel <- if (span > 0) max(d) / span else 0
  cat(sprintf("%-14s %-7s %12.3e %12.3e %12.4g %7d\n",
              tag, comp, max(d), rel, span, n_skip))
  worst[[paste(tag, comp)]] <<- max(d)
  rel_worst[[paste(tag, comp)]] <<- rel
}

for (i in seq_len(nrow(spaces))) {
  sp <- spaces$space[i]
  comps <- strsplit(spaces$comps[i], "|", fixed = TRUE)[[1]]
  got <- convertbands(R, G, B, sp)
  if (!identical(names(got), comps))
    stop("component names differ for '", sp, "': R has ",
         paste(names(got), collapse = ","), ", Python has ",
         paste(comps, collapse = ","), call. = FALSE)
  hue <- .HUE_OF[[sp]]
  keep <- NULL
  if (!is.null(hue)) {
    chroma_ref <- rd(paste0(sp, "_", hue[["chroma"]]))
    # 1e-3 of the chroma range: well above the float noise that produces a
    # spurious angle, well below any chroma a reader would call a colour.
    keep <- chroma_ref > 1e-3 * max(chroma_ref)
  }
  for (nm in comps) {
    k <- if (!is.null(hue) && nm == hue[["hue"]]) keep else NULL
    report(sp, nm, got[[nm]], rd(paste0(sp, "_", nm)), keep = k)
  }
}

inverses <- list(
  lab_to_rgb   = list(fn = lab_to_rgb,   pre = "in_lab",   arg = c("L", "a", "b")),
  oklab_to_rgb = list(fn = oklab_to_rgb, pre = "in_oklab", arg = c("L", "a", "b")),
  hsv_to_rgb   = list(fn = hsv_to_rgb,   pre = "in_hsv",   arg = c("H", "S", "V")),
  hsl_to_rgb   = list(fn = hsl_to_rgb,   pre = "in_hsl",   arg = c("H", "S", "L"))
)
for (tag in names(inverses)) {
  spec <- inverses[[tag]]
  args <- lapply(spec$arg, function(a) rd(paste0(spec$pre, "_", a)))
  got <- do.call(spec$fn, args)
  for (nm in c("R", "G", "B")) report(tag, nm, got[[nm]], rd(paste0(tag, "_", nm)))
}

cat(strrep("-", 70), "\n")
w <- unlist(worst)
rw <- unlist(rel_worst)
cat(sprintf("largest absolute difference: %.3e  (%s)\n",
            max(w), names(w)[which.max(w)]))
cat(sprintf("largest relative difference: %.3e  (%s)\n",
            max(rw), names(rw)[which.max(rw)]))
cat(sprintf("components compared: %d\n", length(w)))

# Enforce, do not merely report. Without this the CI job is green whatever
# the numbers say, which is worse than having no job at all.
#
# The bound is relative to each component's own range: the scales differ by
# orders of magnitude between Jzazbz (range 0.22) and YCbCr (range 224), and
# no single absolute tolerance serves both. 1e-5 is about six times the
# largest value measured for the 0.1.0 release (1.7e-6) — room for platform
# differences in pow() and exp(), but not for a real regression.
TOL_REL <- 1e-5
bad <- rw[rw > TOL_REL]
if (length(bad)) {
  cat("\nOVER TOLERANCE:\n")
  for (nm in names(bad)) cat(sprintf("  %-24s %.3e\n", nm, bad[[nm]]))
  stop("GeoPaletteR has drifted from GeoPalette by more than ", TOL_REL,
       " of a component's range", call. = FALSE)
}
cat(sprintf("\nall %d components agree to within %g of their range\n",
            length(w), TOL_REL))
