# Example script to play one round from the installed package
library(alex_bot)

set.seed(2026)
lad <- ons_get_boundaries(
    area_type = "lad",
    resolution = "BUC",
    nations = c(E=TRUE, N=FALSE, S=FALSE, W=FALSE)
)

sel <- pick_entry(lad, random = TRUE)

# Initial silhouette
print(plot_selected(sel))

# Hints
print(plot_with_surroundings(sel, distance_km = 15))
print(plot_with_surroundings(sel, distance_km = 30))

# Options
print(guess_options(sel, n_distractors = 3, max_distance_km = 50, seed = 1))

# Reveal
print(plot_selected(sel, show_title = TRUE, title_position = "top", title_size = 22))

# Or just run the game with defaults
# LA_game()
