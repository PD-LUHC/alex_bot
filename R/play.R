#' Play an interactive terminal round (LA_game)
#' @export
LA_game <- function(
    area_type = "lad",
    resolution = "BGC",
    nations = c(E = TRUE, N = FALSE, S = FALSE, W = FALSE),
    random_seed = NULL,                     # NULL = random each run; set a number for reproducibility
    hint_mode = c("proportional", "fixed_km"),
    buffers_km = c(15, 30, 60),             # used when hint_mode == "fixed_km"
    buffer_scales = c(1, 2.5, 5),       # used when hint_mode == "proportional"
    buffer_basis = c("maxdim", "diag", "area_radius"),
    min_buffer_km = 5,                      # floor so tiny districts still get a sensible first hint
    n_options = 4,
    options_radius_km = 60
) {
    .require_pkgs(c("sf", "ggplot2"))
    hint_mode <- match.arg(hint_mode)
    buffer_basis <- match.arg(buffer_basis)

    # Only fix the RNG if a seed is provided
    if (!is.null(random_seed)) set.seed(random_seed)

    # Download boundaries and pick a target at random (no seed => truly random by default)
    bnd <- ons_get_boundaries(
        area_type = area_type,
        resolution = resolution,
        nations = nations,
        quiet = TRUE
    )

    sel <- pick_entry(bnd, random = TRUE)

    # Compute the hint distances in km
    if (hint_mode == "proportional") {
        unit_km <- .la_size_unit_km(sel, basis = buffer_basis)
        computed_buffers <- pmax(buffer_scales * unit_km, min_buffer_km)
    } else {
        computed_buffers <- buffers_km
    }

    correct <- selected_name(sel)

    message("\n--- GUESS THE REGION ---")
    message("Type your guess, 'hint', 'options', 'reveal', or 'quit'.")
    message(
        if (hint_mode == "proportional") {
            paste0(
                "Hints (proportional, basis = ", buffer_basis, "): ",
                paste(round(computed_buffers), collapse = " → "), " km"
            )
        } else {
            paste0("Hints (fixed): ", paste(computed_buffers, collapse = " → "), " km")
        }
    )

    # Always ensure a device exists and is active, then plot immediately
    .ensure_plot_device()
    print(plot_selected(sel))

    hint_i <- 0L
    repeat {
        ans <- readline("\nYour input: ")
        low <- tolower(trimws(ans))

        if (low %in% c("quit", "exit")) {
            message("You quit. The answer was: ", correct)
            break
        }

        if (low == "reveal") {
            message("Reveal: ", correct)
            .ensure_plot_device()
            print(plot_selected(sel, show_title = TRUE, title_position = "top", title_size = 22))
            break
        }

        if (low == "hint") {
            hint_i <- hint_i + 1L
            if (hint_i > length(computed_buffers)) {
                message("No more hints. Try 'reveal' or make a guess.")
            } else {
                d <- computed_buffers[hint_i]
                message("Showing surroundings at ", round(d, 1), " km.")
                .ensure_plot_device()
                print(plot_with_surroundings(
                    sel,
                    distance_km = d,
                    hint_label = paste0("HINT ", hint_i, " – ", round(d, 1), " km")
                ))
            }
            next
        }

        if (low == "options") {
            # Keep options random unless the user asked for reproducibility via random_seed
            opts <- guess_options(
                sel,
                n_distractors = max(0, n_options - 1L),
                max_distance_km = options_radius_km,
                seed = if (!is.null(random_seed)) random_seed else NULL
            )
            cat("\nOptions:\n")
            for (i in seq_along(opts)) cat(sprintf("  %d) %s\n", i, opts[i]))
            next
        }

        if (nchar(low) == 0) next

        if (low == tolower(correct)) {
            message("✅ Correct! ", correct)
            .ensure_plot_device()
            print(plot_selected(sel, show_title = TRUE, title_position = "top", title_size = 22))
            break
        } else {
            message("❌ Not correct. Try 'hint', 'options', or 'reveal'.")
        }
    }
}