# alex_bot

ONS Boundary Guessing Game tools for R.
Totally vibe coded (not tested) in the length of time it takes to make and drink a cuppa.

## Install from local source

1. Download the ZIP (`alex_bot.zip`) and unzip it.
2. In R:
   ```r
   # install.packages("remotes")
   remotes::install_local("alex_bot")
   # or devtools::install_local("alex_bot")
   ```

## Quick start

### Fast game:
```r
alex_bot::LA_game()
```

### More manual game:
```r
library(alex_bot)

# Download England LADs (low-res)
lad <- ons_get_boundaries(
    area_type = "lad",
    resolution = "BUC",
    nations = c(E=TRUE, N=FALSE, S=FALSE, W=FALSE)
)

# Pick random target
sel <- pick_entry(lad, random = TRUE, seed = 42)

# Silhouette (no spoilers)
plot_selected(sel)

# Hint: surroundings 15 km (no title)
plot_with_surroundings(sel, distance_km = 15)

# Options (correct + nearby distractors)
guess_options(sel, n_distractors = 3, max_distance_km = 50, seed = 1)

# One-command terminal game with defaults
LA_game()
```
