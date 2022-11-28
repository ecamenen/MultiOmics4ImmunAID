#' @export
plot_piechart <- function(
    x,
    df0 = NULL,
    hsize = 1.2,
    cex = 15,
    colour = get_colors(),
    wrap = 5,
    lwd = 4,
    dec = .1,
    label = TRUE,
    threshold = 5,
    title = NULL,
    wrap_title = 20
    ) {
    df <- unlist(x) %>%
      as.factor() %>%
        fct_drop() %>%
        fct_infreq() %>%
        fct_relabel(~ str_replace_all(.x, "\\s*\\([^\\)]+\\)", "")) %>%
        fct_count()
    if (!is.null(df0)) {
        df <- rbind(data.frame(f = NA, n = c(nrow(df0) - sum(df$n))), df)
    }
    df <- mutate(
        df,
        hsize = hsize,
        pos = rev(cumsum(rev(n))),
        pos = n / 2 + lead(pos, 1),
        pos = if_else(is.na(pos), n / 2, pos),
        label = str_wrap(str_glue("{f} (N={n})"), wrap),
        text = scales::percent(n / sum(n), dec)
    )
    if (is.null(title)) {
        title <- deparse(substitute(x))
    }
    i <- df$n / sum(df$n) <= threshold / 100
    df$text[i] <- ""
    if (!isTRUE(label)) {
        df$label <- rep("", nrow(df))
    }
    ggplot(df, aes(x = hsize, y = n, fill = f)) +
        geom_col(width = 1, color = NA, lwd = lwd) +
        geom_text(
            color = "white",
            size = cex / 2.5,
            aes(label = text),
            position = position_stack(vjust = 0.5)
        ) +
        coord_polar(theta = "y", clip = "off") +
        scale_fill_manual(values = colour, na.value = colour[nrow(df)]) +
        scale_y_continuous(breaks = df$pos, labels = df$label) +
        guides(fill = guide_legend(title = "Group")) +
        ggtitle(str_wrap(title, wrap_title)) +
        theme(
            plot.title = element_text(hjust = 0.5, vjust = -4, size = cex * 1.25, face = "bold"),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(size = cex, colour = colour),
            axis.text.y = element_blank(),
            # legend.text = element_text(size = cex),
            legend.position = "none",
            panel.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0, -1, -1), "cm")
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
        color = NA,
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
plot_histo0 <- function(
    x,
    color = "red",
    dotsize = 1,
    binwidth = 1.5,
    method = "histodot",
    probs = c(.25, .75)
    ) {
    df <- data.frame(x) %>% get_melt()
    quant <- quantile(x, probs, na.rm = TRUE)
    p <- gghistogram(
        df,
        x = "value",
        color = NA,
        fill = "name",
        palette = color,
        binwidth = binwidth,
        add = "median",
        rug = TRUE,
        add.params = list(linetype = "solid", size = 1),
    ) +
        geom_density(
            lwd = 1,
            colour = color,
            fill = color,
            alpha = 0.25
        # ) +
        # geom_dotplot(
        #     fill = color,
        #     color = color,
        #     binwidth = binwidth,
        #     dotsize = dotsize,
        #     method = method
        ) +
        geom_vline(xintercept = quant, color = "red", lty = 2) +
        theme_minimal() +
        scale_x_continuous(expand = c(0.01, 0), labels = function(x) paste0(x)) +
        guides(color = "none", fill = "none")
    theme_violin1(p, guide = TRUE)
}

#' @export
plot_violin1 <- function(
    x,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
  x <- data.frame(value = x, name = 1)
    p <- ggviolin(
        x,
        x = "name",
        y = "value",
        fill = colors[1],
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
    size = cex,
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex,
    alpha = 0.1,
    title = NULL,
    wrap_title = 20,
    probs = c(.25, .75),
    coef = 1.5,
    pch_colour = "#828282",
    pch_alpha = 1,
    class = 1,
    value = 1,
    guide = FALSE,
    subtitle = NULL,
    caption = NULL
    ) {
    colour_fill <- colour
    if (!(class(x) %in% c("data.frame", "tibble"))) {
        df <- data.frame(value = x, name = class)
    } else {
        df <- data.frame(value = pull(x, value), name = pull(x, class))
        x <- df$value
        colour <- factor(df$name, labels = colour)
    }
    quant <- group_by(df, name) %>%
        summarise(quantile(value, probs, na.rm = TRUE)) %>%
        pull(2)
    quant0 <- group_by(df, name) %>%
        summarise(quantile(value, c(0, 1), na.rm = TRUE)) %>%
        pull(2)
    iqr <- quant - c(-1, 1) * (1 - coef) * quant
    if (iqr[1] < quant0[1]) {
          iqr[1] <- quant0[1]
      }
    if (iqr[2] > quant0[2]) {
          iqr[2] <- quant0[2]
      }
    if (is.null(title)) {
        title <- deparse(substitute(x))
    }
    p <- ggplot(df, aes(x = name, y = value)) +
        geom_errorbar(
            width = .1,
            lwd = lwd,
            colour = colour,
            aes(
                ymin = iqr[1],
                ymax = iqr[2]
            )
        ) +
        geom_boxplot(
            coef = 0,
            outlier.shape = NA,
            colour = "white",
            fill = colour_fill,
            lwd = lwd
        ) +
        # geom_violin(alpha = alpha, fill = colour_fill, colour = NA) +
        geom_sina(size = size, colour = pch_colour, alpha = pch_alpha) +
        ggtitle(str_wrap(title, wrap_title)) +
        theme_minimal() +
        labs(subtitle = subtitle, caption = caption)
        # scale_y_continuous(expand = c(0, 0))
    theme_violin1(
        p,
        cex = cex,
        cex_main = cex_main,
        cex_sub = cex_sub,
        guide = guide
    )  %>% suppressMessages()
}

#' @export
plot_violin <- function(
    x,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
    x <- data.frame(value = x, name = 1)
    p <- ggbetweenstats(
        x,
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
    x <- data.frame(value = x, name = 1)
    p <- ggplot(
        x,
        aes(
            x = name,
            y = value,
            color = colors
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
    x <- data.frame(value = x, name = 1)
    p <- ggplot(
        x,
        aes(
            x = name,
            y = value,
            fill = colors[1],
            color = colors[1]
        )
    ) +
        geom_violindot(
            trim = FALSE,
            draw_quantiles = c(0.25, 0.5, 0.75),
            binwidth = 2
        )
    theme_violin0(p, colors = colors)
}
