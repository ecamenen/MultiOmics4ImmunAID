#' @export
plot_histo <- function(
    x,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
    p <- gghistogram(
        get_melt(x),
        x = "value",
        bins = 30,
        add = "mean",
        rug = TRUE,
        color = "grey",
        fill = "name"
    ) +
        facet_wrap(~name, scales = "free") +
        guides(color = "none", fill = "none")
    theme_perso0((p + theme_perso(cex, cex_main, cex_sub)), colors = colors)
}

# plot_histo2(elisa, "Calprotectin", cex = 3, color = c("yellow", "blue"))
plot_histo2 <- function(
    x,
    y,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
    gghistostats(
        data = x,
        x = .data[[y]],
        results.subtitle = FALSE,
        bar.fill = colors[1],
        centrality.line.args = list(size = 1, color = colors[1])
    ) +
        theme_bw() +
        theme_perso(cex, cex_main, cex_sub)
}

#' @export
plot_violin1 <- function(
    x,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
    p <- ggviolin(
        get_melt(x),
        x = "name",
        y = "value",
        fill = "name",
        add = "boxplot",
        add.params = list(fill = "white")
    )
    theme_violin(
        p,
        colors = colors,
        cex = cex,
        cex_main = cex_main,
        cex_sub = cex_sub
    )
}

#' @export
plot_violin <- function(
    x,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
    p <- ggbetweenstats(
        get_melt(x),
        name,
        value,
        outlier.tagging = TRUE,
        outlier.label = value,
        results.subtitle = FALSE,
        pairwise.comparisons = FALSE,
        centrality.point.args = list(size = 3, color = "red"),
        outlier.color = "white"
    )
    theme_violin(
        p,
        colors = colors,
        cex = cex,
        cex_main = cex_main,
        cex_sub = cex_sub
    )
}

#' @export
plot_violin2 <- function(
    x,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
    p <- ggplot(
        getmelt0(x),
        aes(
            x = name,
            y = value,
            color = name
        )
    ) +
        geom_violin(trim = FALSE) +
        geom_boxplot() +
        geom_dotplot(
            binaxis = "y",
            stackdir = "center",
            alpha = .1,
            color = "gray",
            drop = TRUE,
            width = .5
        )
    theme_violin(
        p,
        colors = colors,
        cex = cex,
        cex_main = cex_main,
        cex_sub = cex_sub
    )
}

#' @export
plot_violin3 <- function(
    x,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
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
    theme_violin0(p, colors = colors)
}
