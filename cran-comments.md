# cran-comments

## Test environments

* Windows 11, R 4.5.2 (local), `R CMD check --as-cran`, with TinyTeX
  installed so that `checking PDF version of manual` runs rather than
  being skipped
* GitHub Actions: ubuntu-latest (devel, release, oldrel-1),
  macos-latest (release), windows-latest (release)

## R CMD check results

0 errors, 0 warnings, 1 note.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Igor Pawelec <igor.pawelec@urk.edu.pl>'
New submission
```

Expected for a first submission.

The remaining local notes are environmental rather than properties of the
package: `checking top-level files` reports that README.md and NEWS.md
cannot be checked without pandoc, and `checking for future file
timestamps` reports that it could not reach the network time service.
Neither appears on the CI runners, which install pandoc.

`checking PDF version of manual` passes. Note that the CI workflow runs
with `--no-manual` to avoid requiring a LaTeX install on five runners, so
that particular check is covered locally rather than in CI.

## Notes for the reviewer

The package has no compiled code and no required dependencies beyond base
R. `terra` and `farver` are Suggests: `terra` is used only by `read_rgb()`
and `convert_raster()`, and `farver` only by `tests/test-reference.R`,
which is a no-op when it is absent. Every colour space conversion works on
plain numeric matrices with no spatial or graphical dependency.

`tools/` is excluded from the build via `.Rbuildignore`. It holds a
cross-check against the GeoPalette Python package, which needs Python and
so cannot run on a CRAN machine. The agreement it measures is documented
in `?GeoPaletteR-agreement`.

`\donttest{}` wraps the two examples that need `terra`, so the examples
run without it.
