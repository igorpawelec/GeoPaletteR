# The terra bridge. Small, but the one place in the package where the data
# changes shape, and therefore the one place a silent transpose can hide.
#
# terra is a Suggests, so this file is a no-op when it is absent.

library(GeoPaletteR)

if (!requireNamespace("terra", quietly = TRUE)) {
  cat("terra not installed; skipping the raster bridge tests\n")
} else {

ok <- function(cond, what) {
  if (!isTRUE(cond)) stop("FAILED: ", what, call. = FALSE)
  cat("  ok:", what, "\n")
}

# Deliberately not square, and deliberately not symmetric in value.
#
# A 4x4 raster of random numbers cannot tell a correct write from a
# transposed one: the dimensions still match and nothing looks wrong. Here
# the cell value encodes its own position, so a transpose is visible in the
# numbers, and the non-square shape makes it an error rather than a
# rearrangement.
NR <- 4L; NC <- 7L
pos <- outer(seq_len(NR), seq_len(NC), function(r, c) r * 10 + c)

mk <- function() {
  r <- terra::rast(nrows = NR, ncols = NC, nlyrs = 3,
                   xmin = 0, xmax = NC, ymin = 0, ymax = NR)
  terra::values(r) <- cbind(as.vector(t(pos)),           # R band
                            as.vector(t(pos)) + 100,     # G band
                            as.vector(t(pos)) + 200)     # B band
  r
}

cat("read_rgb\n")

r <- mk()
b <- read_rgb(r)
ok(identical(dim(b$R), c(NR, NC)), "bands come back with the raster's shape")
ok(identical(b$R, pos), "band 1 values land in the right cells")
ok(identical(b$G, pos + 100), "band 2 is the second layer, not the first")
ok(identical(b$B, pos + 200), "band 3 is the third layer")
ok(inherits(b$template, "SpatRaster"), "the template comes back")

two <- terra::rast(nrows = NR, ncols = NC, nlyrs = 2, vals = 1)
ok(inherits(try(read_rgb(two), silent = TRUE), "try-error"),
   "a 2-band raster is rejected")

cat("convert_raster\n")

comps <- convert_raster(r, output = NULL, space = "lab")
ok(identical(names(comps), c("L", "a", "b")), "components are named for the space")
ok(identical(dim(comps$L), c(NR, NC)), "components keep the raster's shape")

direct <- convertbands(b$R, b$G, b$B, "lab")
ok(identical(comps$L, direct$L), "convert_raster agrees with convertbands")

# The round trip. This is the test that matters: write the result, read it
# back with a fresh terra call, and require the values to be where they were
# before. The comment in R/io.R says a missing t() looks plausible on a
# square raster; 4x7 is where it stops looking plausible.
f <- tempfile(fileext = ".tif")
on.exit(unlink(f), add = TRUE)
invisible(convert_raster(r, output = f, space = "lab"))
ok(file.exists(f), "an output file is written")

back <- terra::rast(f)
ok(terra::nlyr(back) == 3L, "the output has one layer per component")
ok(identical(names(back), c("L", "a", "b")), "layer names survive the write")
for (i in seq_len(3)) {
  got <- terra::as.matrix(back[[i]], wide = TRUE)
  # The shape check alone proves nothing: terra keeps the raster geometry
  # whatever order the values arrive in, so a transposed write still comes
  # back 4x7. Verified by removing the t() in R/io.R — this assertion still
  # passed and only the value comparison below caught it.
  ok(identical(dim(got), c(NR, NC)),
     paste0("layer ", i, " keeps its ", NR, "x", NC, " geometry"))
  ok(max(abs(got - comps[[i]])) < 1e-4,
     paste0("layer ", i, " round-trips to the same values, in the same cells"))
}

cat("dispatch and errors\n")

ok(length(convert_raster(r, output = NULL, space = "dlab")) == 6L,
   "a six-component space works through the raster path")
ok(inherits(try(convert_raster(r, output = NULL, space = "nope"),
                silent = TRUE), "try-error"),
   "an unknown space is rejected through the raster path")

# cam02 is the only space taking extra arguments, and they must reach it
# through two layers of dispatch.
avg <- convert_raster(r, output = NULL, space = "cam02")
drk <- convert_raster(r, output = NULL, space = "cam02", surround = "dark")
ok(max(abs(avg$J - drk$J)) > 1e-6,
   "viewing conditions reach cam02 through convert_raster")

cat("\nall raster bridge tests passed\n")
}
