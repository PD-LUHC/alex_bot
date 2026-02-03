# AlexBot

ONS Boundary Guessing Game tools for R.
Totally vibe-coded (and not at all tested) in the length of time it takes to make and drink a cuppa.

### What rubbish is this?
It's a playful way to engage with ONS mapping information if you are an R user.
The code is really rough - what do you expect from something that was vibe-coded in half an hour?
The code seems to work but is a bit buggy and issue prone - what do you expect from something that was vibe-coded in half an hour(raise an issue ticket if you care that much)?

### Known VS Code httpgd fudge
As VS Code users need httpgd for plot displays there is an additional layer that adds some fudge to the experience.
`LA_game()` won't necessarily display plots if httpgd hasn't been launched already.
Until a workable fix is figured out, VS Code users should just do a dummy run of `LA_game()`, httpgd will launch after *"reveal"* or *"quit"* have been entered.

## Install from GitHub

```r
remotes::install_github("pd-luhc/alex_bot")
```
You might need to `install.packages("remotes")` if "remotes" is not installed

## Quick start

### Fast game:
```r
AlexBot::LA_game() # defaults to LADs
```
or if you prefer parliamentary constituencies:
```r
AlexBot::LA_game("pcon")
```
additionally, if you like things chattier:
```r
AlexBot::LA_game(area_type = "pcon", chatty = TRUE)
```

### More manual game:
```r
library(AlexBot)

# Download England LADs (low-res)
lad <- ons_get_boundaries(
    area_type = "lad", # other district types available
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
