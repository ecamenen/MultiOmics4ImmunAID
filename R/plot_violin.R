plot_violin <- function(x) {
    x <- get_melt(x)
    ggbetweenstats(
        x,
        key,
        value,
        outlier.tagging = TRUE,
        results.subtitle = FALSE,
        pairwise.comparisons = FALSE
    ) +
        xlab("") +
        ylab("") + facet_wrap(~key, scale = "free") +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    # theme(strip.background = element_blank(), strip.text.x = element_blank())
}

plot_violin2 <- function(x) {
    x <- melt(x) %>% select(variable, value)
    ggplot(x, aes(x = variable, y = value, color = variable)) +
        geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) +
        geom_boxplot() +
        geom_dotplot(
            binaxis = "y",
            stackdir = "center",
            alpha = .1,
            color = "gray",
            drop = TRUE,
            width = .5
        ) +
        facet_wrap(~variable, scale = "free") +
        theme_bw() +
        guides(color = "none", fill = "none") +
        xlab("") +
        ylab("") +
        theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
}
plot_violin3 <- function(x) {
    x <- melt(x) %>% select(variable, value)
    ggplot(x, aes(
        x = variable,
        y = value,
        fill = variable,
        color = variable
    )) +
        geom_violindot(
            trim = FALSE,
            draw_quantiles = c(0.25, 0.5, 0.75),
            binwidth = 2
        ) +
        facet_wrap(~variable, scale = "free") +
        theme_bw() +
        guides(color = "none", fill = "none", x = "none") +
        xlab("") +
        ylab("")
}
