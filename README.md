# AlexBot

ONS Boundary Guessing Game tools for R.
Totally vibe coded (not tested) in the length of time it takes to make and drink a cuppa.

## Install from GitHub

    ```r
    remotes::install_github("pd-luhc/alex_bot")
    ```
You might need to `install.packages("remotes")` if "remotes" is not installed
## Quick start

### Fast game:
```r
AlexBot::LA_game()
```

### More manual game:
```r
library(AlexBot)

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
