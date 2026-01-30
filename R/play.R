#' Play an interactive terminal round (LA_game)
#' @export
LA_game <- function(
    area_type = "lad",
    resolution = "BGC",
    nations = c(E = TRUE, N = FALSE, S = FALSE, W = FALSE),
    random_seed = 99,
    buffers_km = c(15, 30, 60),
    n_options = 4,
    options_radius_km = 60
) {
    .require_pkgs(c("sf", "ggplot2"))
    set.seed(random_seed)

    bnd <- ons_get_boundaries(
        area_type = area_type,
        resolution = resolution,
        nations = nations,
        quiet = TRUE
    )

    sel <- pick_entry(bnd, random = TRUE, seed = random_seed)
    correct <- selected_name(sel)

    message("
--- GUESS THE REGION ---")
    message("Type your guess, 'hint', 'options', 'reveal', or 'quit'.")
    message("Hints radii: ", paste(buffers_km, collapse = " -> "), " km")

    print(plot_selected(sel))

    hint_i <- 0L
    repeat {
        ans <- readline("
Your input: ")
        low <- tolower(trimws(ans))

        if (low %in% c("quit", "exit")) {
            message("You quit. The answer was: ", correct)
            break
        }
        if (low == "reveal") {
            message("Reveal: ", correct)
            print(plot_selected(sel, show_title = TRUE, title_position = "top", title_size = 22))
            break
        }
        if (low == "hint") {
            hint_i <- hint_i + 1L
            if (hint_i > length(buffers_km)) {
                message("No more hints. Try 'reveal' or make a guess.")
            } else {
                d <- buffers_km[hint_i]
                message("Showing surroundings at ", d, " km.")
                print(plot_with_surroundings(sel, distance_km = d, hint_label = paste0("HINT ", hint_i, " – ", d, " km")))
            }
            next
        }
        if (low == "options") {
            opts <- guess_options(sel, n_distractors = max(0, n_options - 1L), max_distance_km = options_radius_km, seed = random_seed)
            cat("
Options:
")
            for (i in seq_along(opts)) cat(sprintf("  %d) %s
", i, opts[i]))
            next
        }

        if (nchar(low) == 0) next
        if (low == tolower(correct)) {
            message("✅ Correct! ", correct)
            print(plot_selected(sel, show_title = TRUE, title_position = "top", title_size = 22))
            break
        } else {
            message("❌ Not correct. Try 'hint', 'options', or 'reveal'.")
        }
    }
}
