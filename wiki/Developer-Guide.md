# Developer Guide

## Roxygen2 documentation

This repo uses **roxygen2**. Edit comments above each function and run:

```r
devtools::document()
```

That regenerates **NAMESPACE** and `man/*.Rd`. A CI job verifies docs are up-to-date.

## CI/CD

- **R-CMD-check** runs on Ubuntu, macOS, Windows, and multiple R versions.
- **pkgdown** builds and deploys a website to GitHub Pages from `docs/`.
- **release** builds a source `.tar.gz` on tags like `v0.2.0` and attaches it to a GitHub Release.

See the **CI-CD** page for workflow details.

## Local development

```r
# Install deps and run checks
remotes::install_deps(dependencies = TRUE)
devtools::check()
```

## Contributing

- Use 4-space indents.
- Keep plots spoiler-free by default.
- Prefer EPSG:27700 for metric operations; plot in WGS84.
