"""Write reference bands from GeoPalette for cross_validate_against_geopalette.R.

    pip install geopalette
    python3 tools/generate_geopalette_reference.py

Deliberately covers the awkward pixels as well as random ones: pure
primaries, black, white, greys and near-greys are where hue is undefined
and where a port most easily diverges without any test noticing.

Copyright (C) 2025 Igor Pawelec. Licence: GPLv3.
"""
import os

import numpy as np
import geopalette as gp

OUT = os.path.join("tools", "reference")
os.makedirs(OUT, exist_ok=True)

rng = np.random.default_rng(11)
N = 40

R = rng.integers(0, 256, (N, N)).astype(np.uint8)
G = rng.integers(0, 256, (N, N)).astype(np.uint8)
B = rng.integers(0, 256, (N, N)).astype(np.uint8)

# Rows 0-2 are hand-set: the achromatic and saturated cases that random
# sampling almost never produces but real imagery is full of.
edge = [
    (0, 0, 0), (255, 255, 255), (128, 128, 128),      # black, white, grey
    (255, 0, 0), (0, 255, 0), (0, 0, 255),            # primaries
    (255, 255, 0), (0, 255, 255), (255, 0, 255),      # secondaries
    (1, 1, 1), (254, 254, 254), (128, 128, 129),      # near-grey
]
for i, (r, g, b) in enumerate(edge):
    R[0, i], G[0, i], B[0, i] = r, g, b


def save(name, arr):
    np.savetxt(os.path.join(OUT, name + ".csv"), np.asarray(arr, np.float64),
               delimiter=",", fmt="%.17g")


save("R", R)
save("G", G)
save("B", B)

spaces = gp.available_spaces()
with open(os.path.join(OUT, "spaces.csv"), "w") as fh:
    for space in spaces:
        comps, names = gp.convertbands(R, G, B, space)
        fh.write("%s,%s\n" % (space, "|".join(names)))
        for nm, arr in zip(names, comps):
            save("%s_%s" % (space, nm), arr)

# Inverse transforms, fed from the forward result so the inputs are real
# colours rather than arbitrary numbers.
lab = gp.rgb_to_lab(R, G, B)
ok = gp.rgb_to_oklab(R, G, B)
hsv = gp.rgb_to_hsv(R, G, B)
hsl = gp.rgb_to_hsl(R, G, B)

for nm, arr in zip(("L", "a", "b"), lab):
    save("in_lab_%s" % nm, arr)
for nm, arr in zip(("L", "a", "b"), ok):
    save("in_oklab_%s" % nm, arr)
for nm, arr in zip(("H", "S", "V"), hsv):
    save("in_hsv_%s" % nm, arr)
for nm, arr in zip(("H", "S", "L"), hsl):
    save("in_hsl_%s" % nm, arr)

for tag, out in (("lab_to_rgb", gp.lab_to_rgb(*lab)),
                 ("oklab_to_rgb", gp.oklab_to_rgb(*ok)),
                 ("hsv_to_rgb", gp.hsv_to_rgb(*hsv)),
                 ("hsl_to_rgb", gp.hsl_to_rgb(*hsl))):
    for nm, arr in zip(("R", "G", "B"), out):
        save("%s_%s" % (tag, nm), arr)

print("reference for %d spaces + 4 inverses written to %s/" %
      (len(spaces), OUT))
print("geopalette %s" % gp.__version__)
