# Getting Started

## Installation

```r
# install.packages("remotes")
remotes::install_github("OWNER/alex_bot")
library(alex_bot)
```

## Play a round

```r
LA_game()  # uses defaults: England LADs, BGC resolution, hints at 15/30/60 km
```

**Commands** during a round: type your guess, or `hint`, `options`, `reveal`, `quit`.

## Manual flow

```r
lad <- ons_get_boundaries("lad", "BUC", c(E=TRUE,N=FALSE,S=FALSE,W=FALSE))
sel <- pick_entry(lad, random = TRUE, seed = 42)
plot_selected(sel)
plot_with_surroundings(sel, distance_km = 15)
guess_options(sel, n_distractors = 3, max_distance_km = 60, seed = 1)
```
