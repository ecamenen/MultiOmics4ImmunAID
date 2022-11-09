#' @export
plot_piechart <- function(x, hsize = 1.2, cex = 15, colour = get_colors(), wrap = 5, lwd = 4) {
    df <- fct_drop(x) %>%
        fct_infreq() %>%
        fct_count() %>%
        mutate(
            hsize = hsize,
            pos = rev(cumsum(rev(n))),
            pos = n / 2 + lead(pos, 1),
            pos = if_else(is.na(pos), n / 2, pos),
            label = str_wrap(str_glue("{f} (N={n})"), wrap)
        )
    ggplot(df, aes(x = hsize, y = n, fill = f)) +
        geom_col(width = .8, color = "white", lwd = lwd) +
        geom_text(
            color = "white",
            size = cex / 2.5,
            aes(label = scales::percent(n / sum(n), .1)),
            position = position_stack(vjust = 0.5)
        ) +
        coord_polar(theta = "y", clip = "off") +
        scale_fill_manual(values = colour, na.value = "black") +
        scale_y_continuous(breaks = df$pos, labels = df$label) +
        guides(fill = guide_legend(title = "Group")) +
        theme(
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(size = cex),
            axis.text.y = element_blank(),
            legend.position = "none",
            panel.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, -1, -1, -1), "cm")
        ) +
        xlim(0.5, hsize + 0.5)
}

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

plot_histo0 <- function(df, x, color = "red", dotsize = 1, binwidth = 1.5, method = "histodot") {
    gghistogram(
        df,
        x = x,
        fill = color,
        binwidth = binwidth,
        add = "mean",
        rug = TRUE
    ) +
        geom_density(
            lwd = 1,
            colour = color,
            fill = color,
            alpha = 0.25
        ) +
        geom_dotplot(
            fill = color,
            color = color,
            binwidth = binwidth,
            dotsize = dotsize,
            method = method
        )
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
plot_violin0 <- function(
    x,
    colour = "red",
    lwd = 1,
    size = 1,
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
    df <- data.frame(value = x, name = 1)
    m <- median(df$value, na.rm = TRUE)
    iqr <- 1.5 * IQR(df$value, na.rm = TRUE)
    p <- ggplot(df, aes(x = name, y = value)) +
        geom_errorbar(
            width = .1,
            lwd = lwd,
            colour = colour,
            aes(
                ymin = m - iqr,
                ymax = m + iqr
            )
        ) +
        geom_boxplot(
            coef = 0,
            outlier.shape = NA,
            colour = "white",
            fill = colour,
            lwd = lwd
        ) +
        geom_violin(alpha = 0.1, fill = colour, colour = NA) +
        geom_sina(size = size) +
        theme_bw()
    theme_violin1(
        p,
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
        # outlier.label = value,
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
