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
    wrap_title = 20,
    legend = TRUE,
    parse = FALSE,
    collapse = FALSE,
    label_name = NULL,
    legend_name = NULL
) {
    df <- count_cat(x, parse = parse, wrap = wrap, collapse = collapse, label = label_name)
    if (!is.null(df0)) {
        df <- rbind(df, data.frame(f = NA, n = c(nrow(df0) - sum(df$n))))
    }
    if (!is.null(legend) && !is.logical(legend)) {
        df$f <- factor(df$f, labels = legend)
    }
    df <- mutate(
        df,
        hsize = hsize,
        pos = rev(cumsum(rev(n))),
        pos = n / 2 + lead(pos, 1),
        pos = if_else(is.na(pos), n / 2, pos),
        label = str_wrap(str_glue("{f}"), wrap),
        text = scales::percent(n / sum(n), dec)
    )
    if (is.null(title)) {
        title <- deparse(substitute(x))
    }
    i <- df$n / sum(df$n) <= threshold / 100
    df$text[i] <- ""
    if (!is.null(legend_name)) {
        df$legend <- str_replace_all(legend_name, "\\(", "\\\\(") %>% str_replace_all("\\)", "\\\\)") %>% c("NA")
        i <- sapply(df$legend, function(i) paste(df$f) %>% str_replace_all("\n", " ") %>% str_which(i))
        colour0 <- c(rev(colour), "gray")[i]
        colour <- colour0
        df$legend <- df$f[i]
    } else {
        df$legend <- df$f
        colour0 <- c(colour[seq(length(levels(df$f)))], "gray")
    }
    df$legend0 <- paste0(df$legend, ": ", df$n)
    if (!isTRUE(label)) {
        df$label <- rep("", nrow(df))
    }
    df$legend[df$legend == "NA"] <- NA
    p <- ggplot(df, aes(x = hsize, y = n, fill = f)) +
        geom_col(width = 1, color = NA, lwd = lwd) +
        geom_text(
            color = "white",
            size = cex / 2.5,
            aes(label = text),
            position = position_stack(vjust = 0.5)
        ) +
        coord_polar(theta = "y", clip = "off") +
        scale_fill_manual(values = colour, na.value = "gray", labels = df$legend0, breaks = df$legend, name = "") +
        scale_y_continuous(breaks = df$pos, labels = df$label) +
        ggtitle(str_wrap(title, wrap_title)) +
        theme(
            plot.title = element_text(hjust = 0.5, vjust = -4, size = cex * 1.25, face = "bold"),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            axis.text.x = element_text(size = cex, colour = colour0),
            axis.text.y = element_blank(),
            legend.text = element_text(size = cex * 0.85),
            legend.key = element_blank(),
            panel.background = element_rect(fill = "white"),
            plot.margin = unit(c(0, 0, -1, -1), "cm")
        ) +
        xlim(0.5, hsize + 0.5)
    if (is.null(legend) || legend == FALSE) {
        p + theme(legend.position = "none")
    } else {
        p
    }
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
    probs = c(.25, .75),
    subtitle = NULL
) {
    df <- data.frame(x) %>% get_melt()
    quant <- quantile(x, probs, na.rm = TRUE)
    if (!is.null(subtitle)) {
        subtitle <- paste0(print_stats0(df$value), ", N=", length(na.omit(df$value)))
    }
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
        labs(subtitle = subtitle, title = title) +
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
        cex = 1,
        size = cex,
        cex_main = 21 * cex,
        cex_sub = 15 * cex,
        cex_axis = 17 * cex,
        alpha = 0.1,
        title = NULL,
        wrap_title = 20,
        probs = c(.25, .75),
        coef = 1.5,
        pch_colour = "#828282",
        pch_alpha = 1,
        class = 1,
        value = 1,
        subtitle = FALSE,
        caption = NULL,
        color_title = "black",
        ratio = 5,
        title_center = 0.5,
        lim1 = NULL,
        lim2 = NULL,
        method = "anova",
        ylab = NULL,
        method_adjust = "BH",
        wrap = 20,
        ratio_y = 7
) {
    set.seed(1)
    if (is.null(title)) {
        title <- paste0(deparse(substitute(x)))
    }
    if (isFALSE(subtitle)) {
        if (!(class(x) %in% c("data.frame", "tibble"))) {
            subtitle <- paste0(print_stats0(x), ", N=", length(na.omit(x)))
        } else {
            subtitle <- get_melt(x) %>%
                get(paste0(method, "_test"))(value ~ name) %>%
                print_mean_test(dec_p = 3)
        }
    }
    color_subtitle <- colour
    if (!(class(x) %in% c("data.frame", "tibble"))) {
        df <- data.frame(value = x, name = class)
        colour_fill <- colour
        sub_labs <- ""
        guide <- FALSE
    } else {
        df <- get_melt(x)
        # df$name <- factor(df$name, labels = colnames(x)) %>% as.numeric()
        # x <- df$value
        colour_fill <- sort(colnames(x)) %>% match(., colnames(x)) %>% colour[.]
        colour <- factor(df$name, labels = colour)
        sub_labs <- group_by(df, name) %>%
            dplyr::summarise(label = paste0(print_stats0(value), ", N=", length(na.omit(value)))) %>%
            mutate(label = paste(name, label, sep = "\n") %>% str_wrap(wrap)) %>%
            pull(label)
        sub_labs <- sort(colnames(x)) %>% match(colnames(x), .) %>% sub_labs[.]
        guide <- TRUE
    }
    colour_fill1 <- factor(df$name, labels = colour_fill)
    colour_fill0 <- colour
    if (is.null(lim1))
        lim1 <- min(df$value, na.rm = TRUE)
    if (is.null(lim2))
        lim2 <- max(df$value, na.rm = TRUE)
    quant <- group_by(df, name) %>%
        dplyr::summarise(quantile(value, probs, na.rm = TRUE)) %>%
        pull(2)
    quant0 <- group_by(df, name) %>%
        dplyr::summarise(quantile(value, c(0, 1), na.rm = TRUE)) %>%
        pull(2)
    iqr <- quant - c(-1, 1) * (1 - coef) * quant
    iqr_even <- function(x) {
        which(seq(length(iqr)) %% 2 == x)
    }
    for (i in iqr_even(1)) {
        if (iqr[i] < quant0[i]) {
            iqr[i] <- quant0[i]
        }
    }
    for (i in iqr_even(0)) {
        if (iqr[i] > quant0[i]) {
            iqr[i] <- quant0[i]
        }
    }
    get_iqr <- function(x) {
        factor(df$name, labels = iqr[iqr_even(x)]) %>% as.character() %>% as.numeric()
    }
    p <- ggplot(df, aes(x = as.character(name), y = as.numeric(value))) +
        # expand_limits(x = 1 + 1 / ratio) +
        geom_errorbar(
            width = .1,
            lwd = lwd,
            colour = colour_fill1,
            aes(
                ymin = get_iqr(1),
                ymax = get_iqr(0)
            )
        ) +
        geom_boxplot(
            coef = 0,
            outlier.shape = NA,
            colour = "white",
            aes(fill = colour_fill0),
            lwd = lwd * 0.25
        ) +
        geom_violin(alpha = alpha, aes(fill = colour_fill0), colour = NA) +
        theme_minimal() +
        labs(
            title = str_wrap(title, wrap_title),
            subtitle = subtitle,
            caption = caption,
            y = ylab
        ) +
        scale_fill_manual(values = colour_fill) +
        scale_x_discrete(limits = colnames(x), labels = sub_labs) +
        xlab("")
    if ((class(x) %in% c("data.frame", "tibble")) && ncol(x) > 2) {
        stats <- dunn_test(df, value ~ name, p.adjust.method = method_adjust) %>%
            filter(p.adj.signif <= 0.05) %>%
            mutate(y.position = max(value, na.rm = TRUE) + (as.numeric(rownames(.)) * max(value, na.rm = TRUE) / ratio_y))
        p <- p + ggpubr::stat_pvalue_manual(
            stats,
            label = "p.adj.signif",
            color = "gray50",
            bracket.size = 0.7,
            size = cex * 6,
            hide.ns = TRUE,
            tip.length = 0
        )
        max_stats <- pull(stats, y.position) %>% max()
        if (lim2 < max_stats)
            lim2 <- max_stats
    }
    p <- p + scale_y_continuous(limits = c(lim1, lim2))
    if (length(na.omit(df$value)) > 3)
        p <- p + geom_sina(size = size, colour = pch_colour, alpha = pch_alpha, seed = 1)
    else
        p <- p + geom_point(size = size, colour = pch_colour, alpha = pch_alpha)
    theme_violin1(
        p,
        cex = cex,
        cex_main = cex_main,
        cex_sub = cex_sub,
        cex_axis = cex_axis,
        guide = guide,
        color_title = color_title,
        title_center = title_center,
        color_subtitle = color_subtitle
    ) %>% suppressMessages()
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
