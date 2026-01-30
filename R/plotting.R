#' Plot just the selected outline (silhouette)
#' @export
plot_selected <- function(selection,
                          fill = "#4C78A8",
                          border = "grey20",
                          size = 0.4,
                          title = NULL,
                          show_title = FALSE,
                          title_size = 18,
                          title_position = c("top", "center"),
                          title_alpha = 1) {
    .require_pkgs(c("ggplot2", "sf"))
    stopifnot(is.list(selection), inherits(selection$selected, "sf"))
    sel <- selection$selected
    title_position <- match.arg(title_position)

    if (isTRUE(show_title)) {
        if (is.null(title)) title <- sel$name[1]
    } else {
        title <- NULL
    }

    p <- ggplot2::ggplot(sel) +
        ggplot2::geom_sf(fill = fill, color = border, linewidth = size) +
        ggplot2::coord_sf(expand = FALSE) +
        ggplot2::theme_void()

    if (!is.null(title)) {
        if (title_position == "top") {
            p <- p +
                ggplot2::labs(title = title) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(
                        hjust = 0.5,
                        size = title_size,
                        face = "bold",
                        colour = "black"
                    )
                )
        } else {
            bbox <- sf::st_bbox(sel)
            cx <- (bbox["xmin"] + bbox["xmax"]) / 2
            cy <- (bbox["ymin"] + bbox["ymax"]) / 2
            p <- p +
                ggplot2::annotate(
                    "label",
                    x = cx, y = cy,
                    label = title,
                    size = title_size / 3.5,
                    alpha = title_alpha,
                    label.size = NA,
                    fill = "grey50",
                    colour = "white"
                )
        }
    }
    p
}

#' Plot selected + surrounding up to a distance (km)
#' @export
plot_with_surroundings <- function(selection,
                                   distance_km = 15,
                                   fill_selected = "#4C78A8",
                                   fill_surroundings = "#A0C4FF",
                                   border = "grey20",
                                   size = 0.4,
                                   title = NULL,
                                   show_title = FALSE,
                                   title_size = 18,
                                   title_position = c("top", "center"),
                                   title_alpha = 1,
                                   hint_label = NULL,
                                   hint_cex = 6,
                                   hint_alpha = 0.15) {
    .require_pkgs(c("ggplot2", "sf"))
    stopifnot(is.list(selection), inherits(selection$selected, "sf"))
    sel <- selection$selected
    all <- selection$boundaries
    title_position <- match.arg(title_position)

    if (isTRUE(show_title)) {
        if (is.null(title)) {
            title <- paste0("Surroundings (", distance_km, " km)")
        }
    } else {
        title <- NULL
    }

    sel_bng <- .to_bng(sel)
    all_bng <- .to_bng(all)

    buf <- sf::st_buffer(sel_bng, dist = distance_km * 1000)
    within <- all_bng[sf::st_intersects(all_bng, buf, sparse = FALSE)[, 1], ]

    within$is_selected <- within$code == sel$code[1]
    within <- .to_wgs84(within)

    p <- ggplot2::ggplot(within) +
        ggplot2::geom_sf(
            ggplot2::aes(fill = factor(is_selected, levels = c(FALSE, TRUE))),
            color = border, linewidth = size
        ) +
        ggplot2::scale_fill_manual(
            values = c(`FALSE` = fill_surroundings, `TRUE` = fill_selected),
            guide = "none"
        ) +
        ggplot2::coord_sf(expand = FALSE) +
        ggplot2::theme_void()

    if (!is.null(title)) {
        if (title_position == "top") {
            p <- p +
                ggplot2::labs(title = title) +
                ggplot2::theme(
                    plot.title = ggplot2::element_text(
                        hjust = 0.5,
                        size = title_size,
                        face = "bold",
                        colour = "black"
                    )
                )
        } else {
            bbox <- sf::st_bbox(within)
            cx <- (bbox["xmin"] + bbox["xmax"]) / 2
            cy <- (bbox["ymin"] + bbox["ymax"]) / 2
            p <- p +
                ggplot2::annotate(
                    "label",
                    x = cx, y = cy,
                    label = title,
                    size = title_size / 3.5,
                    alpha = title_alpha,
                    label.size = NA,
                    fill = "grey50",
                    colour = "white"
                )
        }
    }

    if (!is.null(hint_label)) {
        p <- p +
            ggplot2::annotate(
                "label",
                x = Inf, y = Inf,
                label = hint_label,
                hjust = 1, vjust = 1,
                size = hint_cex,
                alpha = hint_alpha,
                label.size = NA,
                fill = "grey50",
                color = "white"
            )
    }
    p
}
