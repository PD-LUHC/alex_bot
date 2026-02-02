# AlexBot

ONS Boundary Guessing Game tools for R.
Totally vibe-coded (and not at all tested) in the length of time it takes to make and drink a cuppa.

### What rubbish is this?
It's a playful way to engage with ONS mapping information if you are an R user.
The code is really rough - what do you expect from something that was vibe-coded in half an hour?
The code seems to work but is a bit buggy and issue prone - what do you expect from something that was vibe-coded in half an hour(raise an issue ticket if you care that much)?

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
    resolution = "BGC",
    nations = c(E=TRUE, N=FALSE, S=FALSE, W=FALSE)
)

# Pick random target
sel <- pick_entry(lad, random = TRUE)

# Silhouette (no spoilers)
plot_selected(sel)

# Hint: surroundings 15 km (no title)
plot_with_surroundings(sel, distance_km = 15)

# Options (correct + nearby distractors)
guess_options(sel, n_distractors = 3, max_distance_km = 50, seed = 1)

# One-command terminal game with defaults
LA_game()
```
