#' Pick a specific or random entry from boundaries
#'
#' @param boundaries An sf object from ons_get_boundaries().
#' @param value Name or code to match (optional).
#' @param by Match by 'name' or 'code'.
#' @param random If TRUE, pick a random entry.
#' @param seed Optional seed for reproducibility.
#' @return A list with 'selected' (sf row) and 'boundaries' (sf).
#' @export
pick_entry <- function(boundaries,
                       value = NULL,
                       by = c("name","code"),
                       random = FALSE,
                       seed = NULL) {
    stopifnot(inherits(boundaries, "sf"))
    by <- match.arg(by)
    if (!is.null(seed)) set.seed(seed)

    idx <- NULL
    if (!is.null(value)) {
        idx <- .match_entry(boundaries, value, by = by)
    } else if (isTRUE(random)) {
        idx <- sample(seq_len(nrow(boundaries)), 1L)
    } else {
        stop("Provide a 'value' (name/code) or set random = TRUE.")
    }

    sel <- boundaries[idx, , drop = FALSE]
    list(
        selected = sel,
        boundaries = boundaries
    )
}

#' Return canonical name for selection
#' @export
selected_name <- function(selection) {
    stopifnot(is.list(selection), inherits(selection$selected, "sf"))
    selection$selected$name[1]
}

#' Build multiple-choice options: correct + nearby distractors
#' @export
guess_options <- function(selection,
                          n_distractors = 3,
                          max_distance_km = Inf,
                          seed = NULL,
                          shuffle = TRUE) {
    .require_pkgs(c("sf"))
    stopifnot(is.list(selection), inherits(selection$selected, "sf"))
    if (!is.null(seed)) set.seed(seed)

    sel <- selection$selected
    all <- selection$boundaries

    sel_bng <- .to_bng(sel)
    all_bng <- .to_bng(all)

    sel_ctr <- sf::st_centroid(sel_bng)
    all_ctr <- sf::st_centroid(all_bng)

    dists_m <- as.numeric(sf::st_distance(sel_ctr, all_ctr)[1, ])
    ord <- order(dists_m)
    ord <- ord[all$code[ord] != sel$code[1]]

    if (is.finite(max_distance_km)) {
        keep <- which(dists_m[ord] <= (max_distance_km * 1000))
        ord <- ord[keep]
    }
    if (length(ord) < n_distractors) {
        ord <- order(dists_m)[all$code[order(dists_m)] != sel$code[1]]
    }

    distractors <- head(all$name[ord], n_distractors)
    opts <- c(sel$name[1], distractors)
    if (isTRUE(shuffle)) opts <- sample(opts, length(opts))
    opts
}
