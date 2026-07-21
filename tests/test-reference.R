# Check the conversions against an independent implementation.
#
# Agreement with GeoPalette alone proves only that the port is faithful —
# if the Python had a coefficient wrong, this package would reproduce it.
# farver is a separate codebase with separate constants, so it catches the
# class of error that a port cannot catch by construction.
#
# farver is a Suggests, so this file is a no-op when it is absent, which is
# also what happens on a CRAN machine that chooses not to install it.

library(GeoPaletteR)

if (!requireNamespace("farver", quietly = TRUE)) {
  cat("farver not installed; skipping the independent reference check\n")
} else {

ok <- function(cond, what) {
  if (!isTRUE(cond)) stop("FAILED: ", what, call. = FALSE)
  cat("  ok:", what, "\n")
}
near <- function(a, b, tol, what) {
  d <- max(abs(a - b))
  if (!(d < tol))
    stop("FAILED: ", what, " — max|diff| = ", format(d, digits = 4),
         ", tolerance ", tol, call. = FALSE)
  cat(sprintf("  ok: %-46s max|diff| = %.2e\n", what, d))
}

set.seed(4)
n <- 300
rgb <- cbind(sample(0:255, n, TRUE), sample(0:255, n, TRUE),
             sample(0:255, n, TRUE))
R <- rgb[, 1]; G <- rgb[, 2]; B <- rgb[, 3]

# ── CIELAB ───────────────────────────────────────────────────────────
ref <- farver::convert_colour(rgb, from = "rgb", to = "lab")
got <- rgb_to_lab(R, G, B)
# 0.01 is farver's own D65 white point differing in the last decimals, not
# a disagreement about the transform: the residual is flat across the range
# rather than growing with lightness.
near(got$L, ref[, 1], 0.01, "CIELAB L* matches farver")
near(got$a, ref[, 2], 0.05, "CIELAB a* matches farver")
near(got$b, ref[, 3], 0.05, "CIELAB b* matches farver")

# ── CIELUV ───────────────────────────────────────────────────────────
ref <- farver::convert_colour(rgb, from = "rgb", to = "luv")
got <- rgb_to_luv(R, G, B)
near(got$L, ref[, 1], 0.01, "CIELUV L* matches farver")
near(got$u, ref[, 2], 0.05, "CIELUV u* matches farver")
near(got$v, ref[, 3], 0.05, "CIELUV v* matches farver")

# ── HSL and HSV ──────────────────────────────────────────────────────
ref <- farver::convert_colour(rgb, from = "rgb", to = "hsl")
got <- rgb_to_hsl(R, G, B)
near(got$H, ref[, 1], 1e-3, "HSL hue matches farver")
near(got$S * 100, ref[, 2], 1e-3, "HSL saturation matches farver")
near(got$L * 100, ref[, 3], 1e-3, "HSL lightness matches farver")

# No * 100 here, unlike HSL above. farver reports HSL saturation and
# lightness on 0-100 but HSV saturation and value on 0-1 — an inconsistency
# in farver, not here, and one that silently turns a passing test into a
# failure of 99 units if you assume the scales match.
ref <- farver::convert_colour(rgb, from = "rgb", to = "hsv")
got <- rgb_to_hsv(R, G, B)
near(got$H, ref[, 1], 1e-3, "HSV hue matches farver")
near(got$S, ref[, 2], 1e-6, "HSV saturation matches farver")
near(got$V, ref[, 3], 1e-6, "HSV value matches farver")

# ── LCH(ab) ──────────────────────────────────────────────────────────
ref <- farver::convert_colour(rgb, from = "rgb", to = "lch")
got <- rgb_to_lchab(R, G, B)
near(got$C, ref[, 2], 0.05, "LCHab chroma matches farver")
# Hue only where there is a hue to compare — see ?GeoPaletteR-agreement.
keep <- got$C > 1
near(got$Hab[keep], ref[keep, 3], 0.1, "LCHab hue matches farver above C = 1")

# ── XYZ, via xyY ─────────────────────────────────────────────────────
ref <- farver::convert_colour(rgb, from = "rgb", to = "xyz")
got <- rgb_to_xyY(R, G, B)
# farver reports XYZ on the 0-100 scale; this package keeps Y in 0-1.
near(got$Y_lum * 100, ref[, 2], 0.01, "luminance Y matches farver")

cat("\nall reference tests passed\n")
}
