#' @export
plot_violin1 <- function(x, colors = get_colors(), cex = 1, cex_main = 12 * cex, cex_sub = 10 * cex) {
    p <- ggviolin(
        get_melt(x),
        x = "name",
        y = "value",
        fill = "name",
        add = "boxplot",
        add.params = list(fill = "white")
    )
    theme_perso(p, colors = colors, cex = cex, cex_main = cex_main, cex_sub = cex_sub)
}

#' @export
plot_violin <- function(x, colors = get_colors(), cex = 1, cex_main = 12 * cex, cex_sub = 10 * cex) {
    p <- ggbetweenstats(
        get_melt(x),
        name,
        value,
        outlier.tagging = TRUE,
        results.subtitle = FALSE,
        pairwise.comparisons = FALSE
    )
    theme_perso(p, colors = colors, cex = cex, cex_main = cex_main, cex_sub = cex_sub)
    # theme(strip.background = element_blank(), strip.text.x = element_blank())
}

#' @export
plot_violin2 <- function(x, colors = get_colors(), cex = 1, cex_main = 12 * cex, cex_sub = 10 * cex) {
    p <- ggplot(
        getmelt0(x),
        aes(
            x = name,
            y = value,
            color = name
        )
    ) +
        geom_violin(
            trim = FALSE,
            # draw_quantiles = c(0.25, 0.5, 0.75)
        ) +
        geom_boxplot() +
        geom_dotplot(
            binaxis = "y",
            stackdir = "center",
            alpha = .1,
            color = "gray",
            drop = TRUE,
            width = .5
        )
    theme_perso(p, colors = colors, cex = cex, cex_main = cex_main, cex_sub = cex_sub)
}

#' @export
plot_violin3 <- function(x, colors = get_colors(), cex = 1, cex_main = 12 * cex, cex_sub = 10 * cex) {
    p <- ggplot(
        getmelt0(x),
        aes(
            x = name,
            y = value,
            fill = name,
            color = name
        )
    ) +
        geom_violindot(
            trim = FALSE,
            draw_quantiles = c(0.25, 0.5, 0.75),
            binwidth = 2
        )
    theme_perso0(p, colors = colors)
}

theme_perso0 <- function(p, colors = get_colors()) {
    p +
        xlab("") +
        ylab("") +
        facet_wrap(~name, scales = "free") +
        theme_classic() +
        guides(color = "none", fill = "none", x = "none") +
        scale_color_manual(values = colors, na.value = "black") +
        scale_fill_manual(values = colors, na.value = "black")
}

theme_perso <- function(p, colors = get_colors(), cex = 1, cex_main = 12 * cex, cex_sub = 10 * cex) {
    theme_perso0(p, colors = colors) +
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size = 10 * cex),
            axis.title = element_text(face = "bold.italic", size = cex_sub),
            strip.text = element_text(
                size = cex_main,
                face = "bold",
                hjust = 0.5,
                margin = margin(0.5, 0.5, 0.5, 0.5)
            )
        )
}
