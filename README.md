# GeoPaletteR

[![R-CMD-check](https://github.com/igorpawelec/GeoPaletteR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/igorpawelec/GeoPaletteR/actions/workflows/R-CMD-check.yaml)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](LICENSE)
[![R](https://img.shields.io/badge/R-%3E%3D%203.6-blue.svg)](https://www.r-project.org)

**Colour space conversions for geospatial raster data.**

Plain R. No compiled code, no Rcpp, no required dependencies, and `terra`
only if you want to read files.

> **Python users:** the same conversions are in
> [GeoPalette](https://github.com/igorpawelec/GeoPalette). The two are
> separate repositories because their tooling and idioms do not mix. They
> are close, but — unlike the pyHRG/rHRG pair — they are **not identical**,
> and cannot be. See [Agreement with GeoPalette](#agreement-with-geopalette).

## Install

```r
# install.packages("remotes")
remotes::install_github("igorpawelec/GeoPaletteR")
```

## Use

Every conversion takes three bands as numeric matrices on the 0-255 sRGB
scale and returns a named list of matrices, one per component:

```r
library(GeoPaletteR)

lab <- rgb_to_lab(R, G, B)
lab$L        # 0-100
lab$a        # unbounded

available_spaces()
#> "cam02" "dlab" "hsi" "hsl" "hsv" "jch" "jzazbz" "jzczhz" "lab"
#> "lchab" "lchuv" "luv" "oklab" "xyY" "ycbcr"

# dispatch on a name chosen at runtime
comps <- convertbands(R, G, B, "oklab")
```

Nothing loops over pixels — the arithmetic is vectorised over whole bands,
which is why the package needs no C.

With a raster:

```r
convert_raster("scene.tif", "scene_lab.tif", space = "lab")
```

### CIECAM02 is a model of an observer

`rgb_to_cam02()` is the real thing, not the `rgb_to_jch()` stand-in, so it
takes viewing conditions — and they are not a free choice:

```r
rgb_to_cam02(R, G, B)                      # average surround
rgb_to_cam02(R, G, B, surround = "dark")   # J moves by several units
```

Report the parameters you used, or the numbers are not reproducible.

### Scales differ between spaces

There is no single convention, and it bites when bands are stacked:

| space | components |
|---|---|
| `hsl`, `hsv` | hue 0-360, rest 0-1 |
| `hsi` | hue 0-360, saturation 0-100 **per cent**, intensity 0-255 |
| `lab`, `luv`, `lchab`, `lchuv` | L 0-100, rest unbounded |
| `jch` | hue spans 0-**324** — an HSV hue scaled by 0.9 |
| `ycbcr` | studio swing, Y 16-235 |
| inverses | return 0-**1**, not 0-255 |

## Agreement with GeoPalette

Not bit-for-bit, and this is stated rather than glossed over. GeoPalette
stores its intermediates in single precision; R has no single-precision
numeric type, so this package computes in double throughout. The two part
company at about the seventh significant figure by construction.

Measured across 1600 pixels — including black, white, greys, near-greys and
all six primaries and secondaries — every one of the 60 components of the
15 forward spaces and 4 inverses agrees to better than **1.7e-6 of the
component's own range**. Reproduce it:

```sh
python3 tools/generate_geopalette_reference.py
Rscript tools/cross_validate_against_geopalette.R
```

The double-precision result is the *more* accurate of the two: CIELAB `L*`
matches the independent `farver` implementation to 1.4e-14 here.

### One genuine divergence: hue on the neutral axis

`Hab`, `Huv`, `hz` and CIECAM02 `h` come from `atan2()` over an opponent
pair that is exactly zero for a neutral pixel, so the angle is decided by
floating-point noise. On pure greys GeoPalette reports 0, 90 or 141.34
depending on the pixel; this package reports 158.199. Both are artefacts,
neither is meaningful, and the difference reaches 158 degrees.

It touches only pixels with essentially zero chroma — 5 of 1600 in the run
above, all with chroma below 1.8e-5. Above a chroma of 0.01 the largest hue
difference is 4.4e-4 degrees.

**Do not segment or classify on a hue band without masking low-chroma
pixels first.** Grey, white, black, deep shadow and still water are all
neutral, so this is ordinary in real imagery, not a corner case. The hues
from `rgb_to_hsl()`, `rgb_to_hsv()`, `rgb_to_hsi()` and `rgb_to_jch()` are
unaffected: they come from a max-minus-min sextant and are forced to 0 for
achromatic pixels in both packages.

## Citation

Pawelec, I. (2025). *GeoPaletteR: Colour Space Conversions for Geospatial
Raster Data*. R package version 0.1.0.

## Licence

GPL-3.
