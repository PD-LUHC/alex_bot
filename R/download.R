#' Download ONS boundaries
#'
#' @param area_type One of 'lad', 'ctyua', 'msoa'. Defaults to 'lad' if not provided.
#' @param resolution One of 'BUC', 'BSC', 'BGC'.
#' @param nations Named logical vector for nation prefixes (E/N/S/W). Only those TRUE are kept.
#' @param quiet Logical; if FALSE, prints the fetch URL.
#' @return An sf object with 'code' and 'name' columns added.
#' @export
ons_get_boundaries <- function(area_type = c("lad", "ctyua", "msoa"),
                               resolution = c("BUC", "BSC", "BGC"),
                               nations = c(E = TRUE, N = FALSE, S = FALSE, W = FALSE),
                               quiet = TRUE) {
    .require_pkgs(c("sf"))
    area_type <- if (length(area_type) == 1 && !area_type %in% c("lad","ctyua","msoa")) "lad" else match.arg(area_type)
    resolution <- match.arg(resolution)

    catl <- .ons_catalogue(area_type, resolution)

    full_url <- paste0(
        catl$url,
        "?where=1%3D1",
        "&outFields=*",
        "&outSR=4326",
        "&f=geojson"
    )

    message("Fetching ONS boundaries, may take a moment")
    if (!quiet) {
        message(full_url)
    }

    sf_obj <- tryCatch(
        {
            sf::st_read(full_url, quiet = quiet)
        },
        error = function(e) {
            stop(
                "Failed to read boundaries via sf::st_read().
",
                "URL: ", full_url, "

",
                "Original error: ", conditionMessage(e)
            )
        }
    )

    sf_obj <- .normalize_cols(sf_obj, catl$code_col, catl$name_col)

    valid_n <- names(nations) %in% catl$nations_prefix
    nations_eff <- nations[valid_n]
    if (length(nations_eff) == 0) {
        stop("Chosen area_type only supports nations: ", paste(catl$nations_prefix, collapse = ", "))
    }
    sf_obj <- .filter_nations(sf_obj, nations_eff, catl$code_col)

    keep_cols <- c("code", "name", catl$code_col, catl$name_col, setdiff(names(sf_obj), c("code", "name")))
    sf_obj <- sf_obj[, intersect(keep_cols, names(sf_obj))]

    sf::st_zm(sf_obj, drop = TRUE, what = "ZM")
}
