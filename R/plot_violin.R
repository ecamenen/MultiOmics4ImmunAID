#' @export
plot_violin <- function(x) {
    p <- ggbetweenstats(
        get_melt(x),
        name,
        value,
        outlier.tagging = TRUE,
        results.subtitle = FALSE,
        pairwise.comparisons = FALSE
    )
    theme_perso(p)
    # theme(strip.background = element_blank(), strip.text.x = element_blank())
}

#' @export
plot_violin2 <- function(x) {
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
            draw_quantiles = c(0.25, 0.5, 0.75)
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
    theme_perso(p)
}

#' @export
plot_violin3 <- function(x) {
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
    theme_perso0(p)
}

theme_perso0 <- function(p) {
    p +
        xlab("") +
        ylab("") +
        facet_wrap(~name, scales = "free") +
        theme_bw() +
        guides(color = "none", fill = "none", x = "none")
}

theme_perso <- function(p) {
    theme_perso0(p) +
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        )
}
