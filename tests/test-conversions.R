# Plain-R tests: any error fails R CMD check. No testthat dependency, so
# the package stays free of test-only requirements.

library(GeoPaletteR)

ok <- function(cond, what) {
  if (!isTRUE(cond)) stop("FAILED: ", what, call. = FALSE)
  cat("  ok:", what, "\n")
}
near <- function(a, b, tol, what) ok(all(abs(a - b) < tol), what)

m <- function(...) matrix(c(...), nrow = 1)

# ── structure ────────────────────────────────────────────────────────
cat("structure\n")

R <- m(255, 0, 0, 255, 128, 0)
G <- m(255, 0, 255, 0, 128, 0)
B <- m(255, 0, 0, 0, 128, 255)

ok(length(available_spaces()) == 15L, "15 spaces registered")
ok(!is.unsorted(available_spaces()), "space list is sorted")

for (s in available_spaces()) {
  out <- convertbands(R, G, B, s)
  ok(is.list(out) && !is.null(names(out)), paste(s, "returns a named list"))
  ok(all(vapply(out, function(x) identical(dim(x), dim(R)), logical(1))),
     paste(s, "preserves band dimensions"))
  ok(all(is.finite(unlist(out))), paste(s, "is finite on primaries and greys"))
}
ok(length(convertbands(R, G, B, "dlab")) == 6L, "dlab returns six components")

# Component names are the contract shared with GeoPalette; a rename here
# silently breaks anyone reading band files written by the other package.
ok(identical(names(convertbands(R, G, B, "lab")), c("L", "a", "b")),
   "lab components are L, a, b")
ok(identical(names(convertbands(R, G, B, "xyY")), c("x", "y_ch", "Y_lum")),
   "xyY keeps the _ch and _lum suffixes")

# ── shapes other than matrices ───────────────────────────────────────
cat("input shapes\n")

v <- rgb_to_lab(c(255, 0), c(255, 0), c(255, 0))
ok(is.null(dim(v$L)) && length(v$L) == 2L, "plain vectors work and stay vectors")

a3 <- array(128, dim = c(2, 2, 2))
ok(identical(dim(rgb_to_lab(a3, a3, a3)$L), c(2L, 2L, 2L)),
   "3-D arrays keep their shape")

# ── known values ─────────────────────────────────────────────────────
cat("known values\n")

w <- m(255); bl <- m(0)
near(rgb_to_lab(w, w, w)$L, 100, 1e-3, "white is L* = 100")
near(rgb_to_lab(w, w, w)$a, 0, 0.01, "white has a* = 0")
near(rgb_to_lab(bl, bl, bl)$L, 0, 1e-9, "black is L* = 0")

hsv_red <- rgb_to_hsv(m(255), m(0), m(0))
near(hsv_red$H, 0, 1e-9, "pure red is hue 0")
near(hsv_red$S, 1, 1e-9, "pure red is fully saturated")
near(hsv_red$V, 1, 1e-9, "pure red has value 1")

near(rgb_to_hsv(m(0), m(255), m(0))$H, 120, 1e-6, "pure green is hue 120")
near(rgb_to_hsv(m(0), m(0), m(255))$H, 240, 1e-6, "pure blue is hue 240")
near(rgb_to_hsv(m(0), m(255), m(255))$H, 180, 1e-6, "cyan is hue 180")

near(rgb_to_ycbcr(bl, bl, bl)$Y, 16, 1e-9, "YCbCr black is 16, studio swing")
near(rgb_to_ycbcr(w, w, w)$Y, 235, 1e-3, "YCbCr white is 235, studio swing")
near(rgb_to_ycbcr(bl, bl, bl)$Cb, 128, 1e-9, "YCbCr neutral Cb is 128")

near(rgb_to_xyY(w, w, w)$x, 0.3127, 1e-3, "white sits at the D65 chromaticity")
near(rgb_to_xyY(bl, bl, bl)$x, 1 / 3, 1e-9, "black falls back to equal energy")

near(rgb_to_oklab(w, w, w)$L, 1, 1e-4, "Oklab white is L = 1")
near(rgb_to_oklab(bl, bl, bl)$L, 0, 1e-9, "Oklab black is L = 0")

# ── the achromatic axis ──────────────────────────────────────────────
cat("achromatic pixels\n")

g <- m(128)
ok(rgb_to_hsl(g, g, g)$H == 0, "grey has HSL hue 0, not NaN")
ok(rgb_to_hsv(g, g, g)$H == 0, "grey has HSV hue 0, not NaN")
ok(rgb_to_hsi(g, g, g)$H == 0, "grey has HSI hue 0, not NaN")
ok(rgb_to_jch(g, g, g)$H == 0, "grey has JCH hue 0, not NaN")
ok(rgb_to_hsl(bl, bl, bl)$S == 0, "black has zero saturation")
ok(rgb_to_hsv(bl, bl, bl)$S == 0, "black has zero HSV saturation")
ok(is.finite(rgb_to_luv(bl, bl, bl)$u), "black CIELUV u is finite")
ok(is.finite(rgb_to_cam02(bl, bl, bl)$C), "black CIECAM02 chroma is finite")
near(rgb_to_cam02(bl, bl, bl)$J, 0, 1e-9, "black is CIECAM02 J = 0")

# Neutral chroma must be ~0 even though the hue angle beside it is noise —
# see ?GeoPaletteR-agreement.
ok(rgb_to_lchab(g, g, g)$C < 1e-4, "grey has essentially zero LCH chroma")

# ── hue is a sextant, including on ties ──────────────────────────────
cat("hue ties\n")

# R == B is the tie where the two packages take different branches and must
# still land on the same angle.
near(rgb_to_hsv(m(255), m(0), m(255))$H, 300, 1e-6, "magenta (R == B) is hue 300")
near(rgb_to_hsv(m(255), m(255), m(0))$H, 60, 1e-6, "yellow (R == G) is hue 60")
near(rgb_to_hsv(m(0), m(255), m(255))$H, 180, 1e-6, "cyan (G == B) is hue 180")
ok(all(rgb_to_hsv(m(10, 200), m(10, 200), m(10, 200))$H >= 0), "no negative hue")

# ── round trips ──────────────────────────────────────────────────────
cat("round trips\n")

set.seed(1)
rr <- matrix(sample(0:255, 200, TRUE), 20, 10)
gg <- matrix(sample(0:255, 200, TRUE), 20, 10)
bb <- matrix(sample(0:255, 200, TRUE), 20, 10)

lab <- rgb_to_lab(rr, gg, bb)
back <- lab_to_rgb(lab$L, lab$a, lab$b)
near(back$R * 255, rr, 1e-3, "lab -> rgb -> lab returns the original R")
near(back$G * 255, gg, 1e-3, "lab round trip returns the original G")
near(back$B * 255, bb, 1e-3, "lab round trip returns the original B")

okl <- rgb_to_oklab(rr, gg, bb)
back <- oklab_to_rgb(okl$L, okl$a, okl$b)
near(back$R * 255, rr, 1e-3, "oklab round trip returns the original R")

hsv <- rgb_to_hsv(rr, gg, bb)
back <- hsv_to_rgb(hsv$H, hsv$S, hsv$V)
near(back$R * 255, rr, 1e-3, "hsv round trip returns the original R")
near(back$B * 255, bb, 1e-3, "hsv round trip returns the original B")

hsl <- rgb_to_hsl(rr, gg, bb)
back <- hsl_to_rgb(hsl$H, hsl$S, hsl$L)
near(back$G * 255, gg, 1e-3, "hsl round trip returns the original G")

# ── cylindrical spaces agree with their cartesian parents ────────────
cat("cylindrical consistency\n")

lc <- rgb_to_lchab(rr, gg, bb)
near(lc$C, sqrt(lab$a^2 + lab$b^2), 1e-9, "LCHab chroma matches Lab")
near(lc$L, lab$L, 1e-12, "LCHab lightness matches Lab")
ok(all(lc$Hab >= 0 & lc$Hab < 360), "LCHab hue stays in [0, 360)")

luv <- rgb_to_luv(rr, gg, bb)
lu <- rgb_to_lchuv(rr, gg, bb)
near(lu$C, sqrt(luv$u^2 + luv$v^2), 1e-9, "LCHuv chroma matches Luv")

jz <- rgb_to_jzazbz(rr, gg, bb)
jc <- rgb_to_jzczhz(rr, gg, bb)
near(jc$Cz, sqrt(jz$az^2 + jz$bz^2), 1e-12, "JzCzHz chroma matches Jzazbz")
near(jc$Jz, jz$Jz, 1e-15, "JzCzHz lightness matches Jzazbz")

dl <- rgb_to_dlab(rr, gg, bb)
near(dl$L, lab$L, 1e-12, "dlab carries CIELAB through unchanged")

# ── CIECAM02 viewing conditions actually do something ────────────────
cat("CIECAM02\n")

avg <- rgb_to_cam02(rr, gg, bb)
drk <- rgb_to_cam02(rr, gg, bb, surround = "dark")
ok(max(abs(avg$J - drk$J)) > 1,
   "surround moves J by more than a unit, so it is not ignored")
lit <- rgb_to_cam02(rr, gg, bb, L_A = 318)
ok(max(abs(avg$J - lit$J)) > 0.1, "L_A changes the result")
ok(all(avg$h >= 0 & avg$h < 360), "CIECAM02 hue stays in [0, 360)")

# ── NA handling ──────────────────────────────────────────────────────
cat("NA\n")

rn <- m(255, NA); gn <- m(0, 128); bn <- m(0, 128)
lab_na <- rgb_to_lab(rn, gn, bn)
ok(is.na(lab_na$L[2]), "NA in a band propagates to the output")
ok(!is.na(lab_na$L[1]), "NA does not contaminate its neighbours")

# ── errors ───────────────────────────────────────────────────────────
cat("validation\n")

err <- function(expr, what) {
  ok(inherits(try(expr, silent = TRUE), "try-error"), what)
}
err(convertbands(R, G, B, "nope"), "an unknown space is rejected")
err(convertbands(R, G, B, c("lab", "luv")), "a vector of spaces is rejected")
err(rgb_to_lab(m(1, 2), m(1), m(1)), "mismatched band dimensions are rejected")
err(rgb_to_lab("a", "b", "c"), "non-numeric bands are rejected")
err(rgb_to_cam02(R, G, B, surround = "bright"), "an unknown surround is rejected")
err(rgb_to_cam02(R, G, B, whitepoint = c(1, 2)), "a short whitepoint is rejected")
# Passing viewing conditions to a space that has none is a misunderstanding,
# not a no-op.
err(convertbands(R, G, B, "lab", surround = "dark"),
    "extra arguments to a space that takes none are rejected")

cat("\nall conversion tests passed\n")
