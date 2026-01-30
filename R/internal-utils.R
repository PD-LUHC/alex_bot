.require_pkgs <- function(pkgs) {
    missing <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    if (length(missing) > 0) {
        stop(
            "The following packages are required but not installed: ",
            paste(missing, collapse = ", "),
            "
Please install them using install.packages()."
        )
    }
}

.ons_catalogue <- function(area_type = c("lad", "ctyua", "msoa"),
                           resolution = c("BUC", "BSC", "BGC")) {
    area_type <- match.arg(area_type)
    resolution <- match.arg(resolution)
    base <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/"

    if (area_type == "lad") {
        service <- paste0("Local_Authority_Districts_December_2024_Boundaries_UK_", resolution)
        code_col <- "LAD24CD"
        name_col <- "LAD24NM"
        nations_prefix <- c("E","N","S","W")
    } else if (area_type == "ctyua") {
        service <- paste0("Counties_and_Unitary_Authorities_December_2024_Boundaries_UK_", resolution)
        code_col <- "CTYUA24CD"
        name_col <- "CTYUA24NM"
        nations_prefix <- c("E","N","S","W")
    } else {
        res <- if (resolution == "BUC") "BSC_V3" else paste0(resolution, "_V3")
        service <- paste0("Middle_layer_Super_Output_Areas_December_2021_Boundaries_EW_", res)
        code_col <- "MSOA21CD"
        name_col <- "MSOA21NM"
        nations_prefix <- c("E","W")
    }

    list(
        url = paste0(base, service, "/FeatureServer/0/query"),
        code_col = code_col,
        name_col = name_col,
        nations_prefix = nations_prefix
    )
}

.normalize_cols <- function(sf_obj, code_col, name_col) {
    sf_obj$code <- sf_obj[[code_col]]
    sf_obj$name <- sf_obj[[name_col]]
    sf_obj
}

.filter_nations <- function(sf_obj, nations, code_col) {
    enabled <- names(nations)[unlist(nations)]
    if (length(enabled) == 0) {
        stop("At least one nation must be TRUE (e.g., E=TRUE).")
    }
    keep <- Reduce(`|`, lapply(enabled, function(pref) grepl(paste0("^", pref), sf_obj[[code_col]])))
    sf_obj[keep, ]
}

.to_bng <- function(sf_obj) sf::st_transform(sf_obj, 27700)
.to_wgs84 <- function(sf_obj) sf::st_transform(sf_obj, 4326)

.match_entry <- function(sf_obj, value, by = c("name","code")) {
    by <- match.arg(by)
    if (by == "code") {
        hit <- which(toupper(sf_obj$code) == toupper(value))
    } else {
        exact <- which(tolower(sf_obj$name) == tolower(value))
        if (length(exact) == 1) return(exact)
        hit <- grep(paste0("(^|\b)", tolower(value)), tolower(sf_obj$name))
    }
    if (length(hit) == 0) stop("No match found for '", value, "' in column ", by, ".")
    if (length(hit) > 1) {
        hit <- hit[which.min(nchar(sf_obj$name[hit]))]
    }
    hit
}

# Ensure a plotting device exists and is active
.ensure_plot_device <- function() {
    # If no device is open, open one; otherwise ensure the current device is active
    if (is.null(grDevices::dev.list())) {
        grDevices::dev.new()
    } else {
        try(grDevices::dev.set(which = grDevices::dev.cur()), silent = TRUE)
    }
}

# Measure an LA's characteristic size in km (in BNG meters internally)
# basis = "maxdim": max(width, height) of bbox
# basis = "diag":   diagonal length of bbox
# basis = "area_radius": diameter of a circle with same area (2 * sqrt(area/pi))
.la_size_unit_km <- function(selection, basis = c("maxdim", "diag", "area_radius")) {
    basis <- match.arg(basis)
    sel_bng <- .to_bng(selection$selected)

    bb <- sf::st_bbox(sel_bng)
    w <- as.numeric(bb["xmax"] - bb["xmin"])  # meters
    h <- as.numeric(bb["ymax"] - bb["ymin"])  # meters

    if (basis == "maxdim") {
        unit_m <- max(w, h)
    } else if (basis == "diag") {
        unit_m <- sqrt(w * w + h * h)
    } else {
        a <- as.numeric(sf::st_area(sel_bng))  # m^2
        unit_m <- 2 * sqrt(a / pi)             # diameter-equivalent
    }

    unit_m / 1000  # return km
}
