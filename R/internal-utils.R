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

.ons_catalogue <- function(area_type = c("lad", "ctyua", "msoa", "pcon"),
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
    } else if (area_type == "pcon") {
        # Westminster Parliamentary Constituencies (July 2024), UK
        service <- paste0("Westminster_Parliamentary_Constituencies_July_2024_Boundaries_UK_", resolution)
        code_col <- "PCON24CD"
        name_col <- "PCON24NM"
        nations_prefix <- c("E","N","S","W")  # UK-wide
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

# ---- Cache helpers --------------------------------------------------------
# Default cache directory (OS-specific, user-writable)
# On R >= 4.0, tools::R_user_dir() gives a nice per-package cache location.
.cache_dir <- function() {
    dir <- tryCatch(
        tools::R_user_dir("AlexBot", which = "cache"),
        error = function(e) file.path(tempdir(), "AlexBot-cache")  # fallback
    )
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    dir
}

# Construct a stable cache key for a boundaries request
# Include service URL (from .ons_catalogue), resolution, area_type, nations filter.
.cache_key <- function(catl, nations) {
    # Only keep the nation flags that are valid for this layer
    valid_n <- names(nations) %in% catl$nations_prefix
    nations_eff <- nations[valid_n]
    paste0(
        "area=", sub(".*/services/", "", catl$url),    # unique-ish layer identifier
        "|nations=", paste(names(nations_eff)[nations_eff], collapse = "-")
    )
}

.cache_path <- function(key) {
    # Sanitize for filesystem and add an extension that signals RDS
    file.path(.cache_dir(), paste0(gsub("[^A-Za-z0-9._-]", "_", key), ".rds"))
}

# Save / load cached sf object + metadata
.cache_save <- function(key, sf_obj) {
    path <- .cache_path(key)
    info <- list(
        saved_at = Sys.time(),
        sf_obj = sf_obj
    )
    saveRDS(info, file = path)
    invisible(path)
}

.cache_load <- function(key, max_age_days = Inf) {
    path <- .cache_path(key)
    if (!file.exists(path)) return(NULL)
    info <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(info) || is.null(info$saved_at) || is.null(info$sf_obj)) return(NULL)
    if (is.finite(max_age_days)) {
        age <- as.numeric(difftime(Sys.time(), info$saved_at, units = "days"))
        if (age > max_age_days) return(NULL)
    }
    info$sf_obj
}

# Public utilities (optional): clear & inspect cache
clear_alexbot_cache <- function() {
    dir <- .cache_dir()
    files <- list.files(dir, full.names = TRUE)
    if (length(files)) file.remove(files)
    invisible(files)
}

alexbot_cache_info <- function() {
    dir <- .cache_dir()
    files <- list.files(dir, full.names = TRUE)
    data.frame(
        file = basename(files),
        size_mb = round(file.info(files)$size / 1024^2, 2),
        modified = file.info(files)$mtime,
        stringsAsFactors = FALSE
    )
}

# ---- package version check ----
#' @keywords internal
#' @noRd
.check_latest_version_remotes <- function(
    repo = getOption(
        "alex_bot.github_repo",
        getOption("Config/alex_bot/github_repo", "PD-LUHC/alex_bot")
    ),
    ref = getOption( # ref kept for future use
        "alex_bot.github_ref",
        getOption("Config/alex_bot/github_ref", "main")
    ),
    quiet = TRUE,
    once_per_session = TRUE,
    include_prerelease = FALSE) {
    # Helper to emit a single failure message per session and exit cleanly

    fail <- function(reason = NULL) {
        # emit at most once per session
        if (!isTRUE(getOption("alex_bot._update_failed", FALSE))) {
            msg <- "Unable to check for updates (AlexBot)."
            if (!quiet && nzchar(reason)) msg <- sprintf("%s Reason: %s", msg, reason)
            message(msg) # runtime-appropriate
            options(alex_bot._update_failed = TRUE)
        }
        invisible(NULL)
    }

    # Allow opt-out (useful in CI/tests/offline builds) — not a "failure"
    if (identical(getOption("alex_bot.check_updates", TRUE), FALSE)) {
        return(invisible(NULL))
    }

    # Avoid repeating the message within the same R session
    if (isTRUE(once_per_session) && isTRUE(getOption("alex_bot._update_nudged", FALSE))) {
        return(invisible(NULL))
    }

    # Require {remotes}; if missing we consider that a failure to check
    if (!requireNamespace("remotes", quietly = TRUE)) {
        return(fail("package 'remotes' not installed"))
    }

    pkg <- "AlexBot"
    installed <- tryCatch(utils::packageVersion(pkg), error = function(e) NA)

    # If the installed version couldn’t be determined, tell the user once and exit
    if (is.na(installed)) {
        return(fail("installed version unknown (could not determine utils::packageVersion)."))
    }

    # Cache the last attempt result (success value or NA) to avoid repeated work
    latest <- getOption("alex_bot._cached_latest_tag", NULL)
    if (is.null(latest)) {
        # Need to attempt retrieval
        if (!nzchar(Sys.which("git"))) {
            return(fail("git not found on PATH"))
        }

        repo_url <- paste0("https://github.com/", repo, ".git")

        # Try to list remote tags
        tags_raw <- tryCatch(
            {
                out <- suppressWarnings(system2(
                    "git",
                    c("ls-remote", "--tags", repo_url),
                    stdout = TRUE, stderr = TRUE
                ))
                if (!is.null(attr(out, "status")) && attr(out, "status") != 0) {
                    return(NULL)
                }
                out
            },
            error = function(e) NULL
        )

        if (is.null(tags_raw)) {
            options(alex_bot._cached_latest_tag = NA_character_)
            return(fail("git ls-remote failed"))
        }

        # Extract tag names and filter to semver-like
        tag_names <- sub("^.+\\trefs/tags/", "", tags_raw)
        tag_names <- sub("\\^\\{\\}$", "", tag_names) # peel annotations
        semver_pat <- "^v?\\d+\\.\\d+\\.\\d+(?:[-+].*)?$"
        tag_names <- tag_names[grepl(semver_pat, tag_names, perl = TRUE)]

        if (!isTRUE(include_prerelease)) {
            tag_names <- tag_names[!grepl("^v?\\d+\\.\\d+\\.\\d+-", tag_names)]
        }

        if (!length(tag_names)) {
            options(alex_bot._cached_latest_tag = NA_character_)
            return(fail("no semver tags found"))
        }

        semvers <- sub("^v", "", tag_names)
        pv <- tryCatch(
            package_version(semvers),
            error = function(e) rep(NA, length(semvers))
        )
        if (all(is.na(pv))) {
            options(alex_bot._cached_latest_tag = NA_character_)
            return(fail("could not parse tag versions"))
        }

        o <- order(pv, decreasing = TRUE, na.last = NA)
        latest <- semvers[o][1]
        options(alex_bot._cached_latest_tag = latest)
    }

    # If the remote latest couldn’t be determined, tell the user once and exit
    if (is.na(latest)) {
        # Cache the failure so we don’t keep retrying this session
        options(alex_bot._cached_latest_tag = NA_character_)
        return(fail("remote latest version unknown (unable to determine tags or parse versions)."))
    }

    # Compare installed vs latest and nudge
    cmp <- tryCatch(
        utils::compareVersion(as.character(installed), as.character(latest)),
        error = function(e) NA_integer_
    )
    if (!is.na(cmp) && cmp < 0) {
        msg <- sprintf(
            paste0(
                "A newer version of %s is available (%s -> %s).\n",
                "To update (after game is over):\n  remotes::install_github('%s')"
            ),
            pkg, installed, latest, repo
        )
        if (isTRUE(quiet)) packageStartupMessage(msg) else message(msg)
        options(alex_bot._update_nudged = TRUE)
    }
    invisible(NULL)
}

# ---- Typing effect: prints text character-by-character with optional blinking cursor ----
# ---- ANSI helpers ----
ansi <- list(
    hide_cursor = "\033[?25l",
    show_cursor = "\033[?25h",
    clear_line = "\033[2K",
    carriage = "\r"
)

.flush <- function() {
    try(flush.console(), silent = TRUE)
}

.colour_check <- function() {
    term <- tolower(Sys.getenv("TERM", ""))
    colorterm <- tolower(Sys.getenv("COLORTERM", ""))

    startsWith(term, "xterm") ||
        identical(colorterm, "truecolor")
}

# ---- typewrite with only "none" or "after" for ellipsis ----
# Arguments:
# - text: text to type
# - delay: per-character delay (seconds)
# - cursor: show a blinking block cursor during typing
# - cursor_char: the fake cursor character (full block default)
# - blink_every: blink cycle for the fake cursor (seconds)
# - ellipsis: one of c("none", "after")
# - ellipsis_ticks: how many steps the ellipsis should animate after typing
# - ellipsis_step: delay between ellipsis frames
# - ellipsis_max: max number of dots before reset (typically 3)
typewrite <- function(
    text,
    chatty = TRUE,
    delay = 0.02,
    cursor = TRUE, # doesn't work as expected if false
    cursor_char = "\u2588",
    blink_every = 0.12,
    ellipsis = TRUE,
    ellipsis_ticks = 8,
    ellipsis_step = 0.2,
    ellipsis_max = 3) {
    cat(ansi$hide_cursor)
    on.exit(cat(ansi$show_cursor), add = TRUE)

    chars <- strsplit(text, "", fixed = TRUE)[[1]]
    acc <- ""
    last_blink <- Sys.time()

    # chatty override
    if (!chatty) {
        delay <- 0
        ellipsis <- FALSE
    }
    if (.colour_check()) {
        cat("\033[32m")
    }
    # Normal typing
    for (i in seq_along(chars)) {
        acc <- paste0(acc, chars[i])
        if (cursor) {
            now <- Sys.time()
            show <- as.numeric(difftime(now, last_blink, units = "secs")) %% (2 * blink_every) < blink_every
            display <- if (show) paste0(acc, cursor_char) else paste0(acc, " ")
            cat(ansi$carriage, ansi$clear_line, display, sep = "")
        } else {
            cat(chars[i])
        }
        .flush()
        Sys.sleep(delay)
    }

    # Stabilize the final line (remove fake cursor)
    cat(ansi$carriage, ansi$clear_line, acc, sep = "")
    .flush()

    # Optional ellipsis AFTER typing
    if (ellipsis) {
        # Important: ellipsis() also hides/shows the cursor; that’s fine since
        # we restored the line above and it will re-hide/re-show during its run.
        ellipsis(base = acc, ticks = ellipsis_ticks, step = ellipsis_step)
        # Ensure we leave the base text visible and then newline if you want:
    }
    if (!ellipsis) {
        cat("\n")
    }
    if (.colour_check()) {
        cat("\033[0m")
    }
}

# ---- Ellipses animation: prints `base` then animated ... then returns to a clean line ----
ellipsis <- function(base = "Thinking", ticks = 6, step = 0.3) {
    cat(ansi$hide_cursor)
    on.exit(cat(ansi$show_cursor), add = TRUE)

    for (i in seq_len(ticks)) {
        dots <- paste(rep(".", i %% 4), collapse = "")
        line <- sprintf("%s%s", base, dots)
        cat(ansi$carriage, ansi$clear_line, line, sep = "")
        .flush()
        Sys.sleep(step)
    }
    # Clear and end with the base phrase completed
    cat(ansi$carriage, ansi$clear_line, base, "\n", sep = "")
    .flush()
}

# ---- Temporary flashing cursor at line end (decorative) ----
flash_cursor <- function(times = 6, cursor_char = "\u2588", on = 0.25, off = 0.25) {
    cat(ansi$hide_cursor)
    on.exit(cat(ansi$show_cursor), add = TRUE)

    for (i in seq_len(times)) {
        cat(cursor_char)
        .flush()
        Sys.sleep(on)
        cat(ansi$carriage, ansi$clear_line, sep = "")
        .flush()
        Sys.sleep(off)
    }
}