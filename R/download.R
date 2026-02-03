#' Download ONS boundaries (with caching)
#'
#' @param area_type 'lad','ctyua','msoa','pcon'
#' @param resolution 'BUC','BSC','BGC' (optionally 'BFC','BFE' if enabled)
#' @param nations named logical vector for nation prefixes (E/N/S/W)
#' @param quiet logical; if FALSE prints the fetch URL (URL printed only when quiet = FALSE)
#' @param use_cache logical; if TRUE cache results on disk and reuse across sessions
#' @param cache_max_age_days numeric; max age before re-downloading (Inf = never expire)
#' @return sf object
#' @export
ons_get_boundaries <- function(area_type = c("lad", "ctyua", "msoa", "pcon"),
                               resolution = c("BUC", "BSC", "BGC"),
                               nations = c(E = TRUE, N = FALSE, S = FALSE, W = FALSE),
                               quiet = TRUE,
                               use_cache = TRUE,
                               cache_max_age_days = 30) {
    .require_pkgs(c("sf"))
    area_type <- if (length(area_type) == 1 && !area_type %in% c("lad","ctyua","msoa","pcon")) "lad" else match.arg(area_type)
    resolution <- match.arg(resolution)

    catl <- .ons_catalogue(area_type, resolution)

    # Build a stable cache key and try to load cached result
    key <- .cache_key(catl, nations)

    if (isTRUE(use_cache)) {
        cached <- .cache_load(key, max_age_days = cache_max_age_days)
        if (!is.null(cached)) {
            # Always show the short message; URL only when quiet = FALSE
            message("Fetching cached ONS boundaries")
            if (!quiet) message(catl$url)
            return(cached)
        }
    }

    # Build full URL; same as before
    full_url <- paste0(
        catl$url, "?where=1%3D1",
        "&outFields=*",
        "&outSR=4326",
        "&f=geojson"
    )

    message("Fetching ONS boundaries, may take a moment")
    if (!quiet) {
        message(full_url)
    }

    # Read from FeatureServer
    sf_obj <- tryCatch(
        sf::st_read(full_url, quiet = quiet),
        error = function(e) {
            stop(
                "Failed to read boundaries via sf::st_read().\n",
                "URL: ", full_url, "\n\n",
                "Original error: ", conditionMessage(e)
            )
        }
    )

    # Normalise & filter (unchanged)
    sf_obj <- .normalize_cols(sf_obj, catl$code_col, catl$name_col)

    valid_n <- names(nations) %in% catl$nations_prefix
    nations_eff <- nations[valid_n]
    if (length(nations_eff) == 0) {
        stop("Chosen area_type only supports nations: ", paste(catl$nations_prefix, collapse = ", "))
    }
    sf_obj <- .filter_nations(sf_obj, nations_eff, catl$code_col)

    keep_cols <- c("code", "name", catl$code_col, catl$name_col, setdiff(names(sf_obj), c("code", "name")))
    sf_obj <- sf_obj[, intersect(keep_cols, names(sf_obj))]
    sf_obj <- sf::st_zm(sf_obj, drop = TRUE, what = "ZM")

    # Save to cache if enabled
    if (isTRUE(use_cache)) {
        .cache_save(key, sf_obj)
    }

    sf_obj
}