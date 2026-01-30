# CI & CD

## Workflows

- **R-CMD-check** (`.github/workflows/R-CMD-check.yaml`): matrix checks across OS/R versions.
- **pkgdown** (`.github/workflows/pkgdown.yaml`): builds site and deploys to GitHub Pages.
- **release** (`.github/workflows/release.yml`): on tag `v*`, builds source tarball and publishes a GitHub Release.
- **roxygen-check** (`.github/workflows/roxygen-check.yaml`): fails the build if roxygen docs are stale.

## GitHub Pages

Enable Pages in repo **Settings → Pages → Build and deployment → GitHub Actions**.

The pkgdown workflow uploads `docs/` as a Pages artifact and deploys it. The site URL is configured in `_pkgdown.yml` (change `OWNER`).

## Releasing

1. Bump version in `DESCRIPTION`.
2. Create a tag like `v0.2.0` and push.
3. The **release** action builds `alex_bot_0.2.0.tar.gz` and attaches it to the GitHub Release.

Users can install from GitHub directly:

```r
remotes::install_github("OWNER/alex_bot")
```

Or download the tarball from the Release and install offline:

```r
install.packages("alex_bot_0.2.0.tar.gz", repos = NULL, type = "source")
```
