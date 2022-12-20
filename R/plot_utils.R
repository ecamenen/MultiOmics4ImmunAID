#' @export
plot_corr <- function(
    x,
    y = NULL,
    clean_name = TRUE,
    mat = NULL,
    p_mat = NULL,
    col = brewer.pal(n = 7, name = "RdBu"),
    method = "pearson"
) {
    if (clean_name) {
        colnames(x) <- get_var_names(colnames(x), y)
        x <- clean_names(x)
    }
    x <- as.data.frame(x)
    if (is.null(mat)) {
        mat <- get_corr(x, TRUE, method = method)
    }
    if (is.null(p_mat)) {
        p_mat <- get_corr(x, method = method)
    }

    corrplot(
        mat,
        # method="color",
        col = col,
        type = "upper",
        order = "original",
        # addCoef.col = "black",
        # Ajout du coefficient de corrélation
        tl.col = "black",
        tl.srt = 45,
        tl.cex = .5,
        # Rotation des etiquettes de textes
        # Combiner avec le niveau de significativité
        p.mat = p_mat,
        sig.level = 0.05,
        addgrid.col = NA,
        insig = "pch",
        pch = 4,
        pch.cex = 2,
        pch.col = "white",
        # Cacher les coefficients de corrélation sur la diagonale
        diag = FALSE,
        na.label = " "
    )
}

#' @export
plot_normal <- function(x) {
    x %>%
        select(get_not_normal(x)$vars) %>%
        plot_normality()
}

#' @export
get_colors <- function() {
    c(
        brewer.pal(9, "Set1"),
        brewer.pal(9, "Pastel1")
    )
}

#' @export
theme_perso0 <- function(p, colors = get_colors()) {
    p +
        theme_classic() +
        scale_color_manual(values = colors, na.value = "black") +
        scale_fill_manual(values = colors, na.value = "black")
}

theme_violin0 <- function(p, colors = get_colors()) {
    theme_perso0(
        p +
            xlab("") +
            ylab("") +
            facet_wrap(~name, scales = "free") +
            guides(
                color = "none",
                fill = "none",
                x = "none"
            ),
        colors = colors
    )
}

theme_violin1 <- function(
    p,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex,
    guide = FALSE,
    grid = FALSE
    ) {
    p <- p +
        xlab("") +
        ylab("") +
        guides(
            color = "none",
            fill = "none"
        ) +
        theme(
            plot.title = element_text(
                hjust = 0.5,
                size = cex * 15,
                face = "bold"
            ),
            plot.subtitle = element_text(
                hjust = 1,
                size = cex * 10
            ),
            plot.caption = element_text(
                hjust = 1,
                size = cex * 11,
                color = "gray"
            )
        ) +
        theme_perso(cex, cex_main, cex_sub)
    if (!isTRUE(guide)) {
        p + guides(x = "none")
    }
    if (!isTRUE(grid)) {
        p + theme(
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )
    }
    else {
        p
    }
}

theme_violin <- function(
    p,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
    theme_violin0(p, colors = colors) +
        theme(
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank()
        ) +
        theme_perso(cex, cex_main, cex_sub)
}

#' @export
theme_perso <- function(
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex,
    cex_axis = 10 * cex
    ) {
    # cex_axis
    theme(
        axis.text = element_text(size = cex_axis * cex),
        axis.title = element_text(face = "bold.italic", size = cex_sub),
        strip.text = element_text(
            size = cex_main,
            face = "bold",
            hjust = 0.5,
            margin = margin(0.5, 0.5, 0.5, 0.5)
        )
    )
}

theme_histo <- function(
    p,
    colors = get_colors(),
    cex = 1,
    cex_main = 12 * cex,
    cex_sub = 10 * cex) {
    theme_perso0(p + theme_perso(cex, cex_main, cex_sub), colors = colors)
}

#' @export
plot_venn <- function(x, snames = "", ilcs = 2, sncs = 3, plotsize = 15) {
    venn::venn(
        x,
        ilabels = TRUE,
        zcolor = "style",
        box = FALSE,
        ilcs = ilcs,
        sncs = sncs,
        snames = snames,
        # borders = FALSE,
        # ellipse = TRUE,
        plotsize = plotsize
    )
}

plot_venn0 <- function(x) {
    ggvenn::ggvenn(
        x,
        stroke_size = NA,
        fill_color = get_colors()[-4],
        text_color = "white",
        digits = 0
    )
}


plot_venn1 <- function(x, n = 4, color = get_colors()[-4], cex = 1, vjust = 0.5) {
    data <- Venn(x) %>% process_data()
    data@region <- data@region %>%
        mutate(
            percent = (count * 100 / sum(count)) %>%
                round(digits = 0) %>%
                paste0("%")
        ) %>%
        mutate(
            label = paste0("(", percent, ")") %>%
                paste(count, ., sep = "\n")
        )
    data@region$label[data@region$item %>% list.which(length(.) == 0)] <- ""
    i <- data@region$item %>% list.which(length(.) <= n & length(.) > 0)
    data@region$label[unlist(i)] <- data@region$item %>%
        list.search(length(.) <= n & length(.) > 0) %>%
        list.mapv(paste(., collapse = "\n"))
    p <- ggplot() +
        geom_sf(aes(fill = count), data = venn_region(data)) +
        geom_sf(aes(color = id), data = venn_setedge(data), show.legend = FALSE) +
        geom_sf_text(aes(label = name), color = color[seq(length(x))], vjust = vjust, data = venn_setlabel(data), size = cex * 8) +
        geom_sf_label(aes(label = label), data = venn_region(data), alpha = 0.5, label.size = NA, size = cex * 4) +
        theme_void() +
        scale_fill_gradient(low = "white", high = "red") +
        scale_color_manual(values = color) +
        theme(
            legend.title = element_text(face = "bold.italic", size = cex * 13),
            legend.text = element_text(size = cex * 9)
        )
    p$labels$fill <- "N"
    return(p)
}

#' Histogram settings
#'
#' Default font for a vertical barplot.
#'
#' @param p A ggplot object.
#' @param df A dataframe with a column named "order"
#' @param title A character string giving a graphic title
#' @param color A vector of character giving the colors for the rows
#' @examples
#' df <- data.frame(x = runif(30), order = 30:1)
#' library("ggplot2")
#' p <- ggplot(df, aes(order, x))
#' plotHistogram(p, df, "This is my title", "red")
#' # Add colors per levels of a variable
#' df$color <- rep(c(1, 2, 3), each = 10)
#' p <- ggplot(df, aes(order, x, fill = color))
#' plotHistogram(p, df, "Histogram", as.character(df$color))
#' @export plotHistogram
plotHistogram <- function(p = NULL, df = NULL, hjust = 0, vjust = 0.5, n = 100, title = "", color = "black", color_gradient = c("#FFF5F0", "#99000D"), cex = 1) {
    if (is.null(p)) {
        df0 <- as.data.frame(df)
        colnames(df0)[1] <- "val"
        if (n > nrow(df0)) {
            n <- nrow(df0)
        }
        df <- (
            df0 %>%
                arrange(desc(val)) %>%
                mutate(order = rev(seq(nrow(df0))))
        )[seq(n), ]
        p <- ggplot(df, aes(order, val, fill = order)) +
            theme_classic()
    }
    p +
        # TODO: if NB_ROW > X, uncomment this
        # geom_hline(yintercept = c(-.5,.5), col="grey", linetype="dotted", size=1) +
        geom_hline(yintercept = 0, col = "grey", size = 1) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_x_continuous(breaks = df$order, labels = rownames(df)) +
        labs(
            title = title,
            x = "", y = ""
        ) +
        # theme_classic() +
        # theme_perso() +
        theme(
            axis.text.y = element_text(size = cex * 10, face = "italic", color = color),
            axis.text.x = element_text(size = cex * 10, face = "italic", color = "darkgrey"),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")
        ) +
        geom_text(aes(label = round(..y.., 2)), hjust = hjust, vjust = vjust, size = cex * 4) +
        theme(legend.position = "none") +
        scale_fill_gradient(low = color_gradient[1], high = color_gradient[2])
}


#' @export
plot_qq0 <- function(
    x,
    ggtheme = theme_classic(),
    geom_qq_args = list(color = "blue"),
    geom_qq_line_args = list(color = "red"),
    theme_config = theme_perso(),
    nrow = 3,
    ncol = 3
) {
    plot_qq(
        x,
        ggtheme = ggtheme,
        geom_qq_args = geom_qq_args,
        geom_qq_line_args = geom_qq_line_args,
        theme_config = theme_config,
        nrow = nrow,
        ncol = ncol
    )
}

#' @export
plot_corr0 <- function(
    x,
    n = ncol(x),
    cols = c(brewer.pal(n = 9, name = "RdBu")),
    method = "spearman",
    p_adjust = "holm"
) {
    res <- correlation(x[, seq(n)], method = method, p_adjust = p_adjust)
    plot_corr(
        x[, seq(n)],
        NULL,
        FALSE,
        as.matrix(select(res, matches("^parameter|^r"))),
        as.matrix(select(res, matches("^parameter|^p"))),
        col = cols
    )
}

#' @export
spec_color2 <- function(x, alpha = 1, begin = 0, end = 1,
                        direction = 1, option = "D",
                        na_color = "#BBBBBB", scale_from = NULL,
                        palette = colorRampPalette(c("blue", "gray", "red"))(100)) {
    n <- length(palette)
    if (is.null(scale_from)) {
        x <- round(scales::rescale(x, c(1, n)))
    } else {
        x <- round(
            scales::rescale(
                x,
                to = c(1, n),
                from = scale_from
            )
        )
    }

    color_code <- palette[x]
    color_code[is.na(color_code)] <- na_color
    return(color_code)
}

#' @export
# plot_bar_mcat(clinic_tot0$C_2643_3798, colorRampPalette(c("blue", "gray", "red"))(16))
plot_bar_mcat <- function(
    x,
    colors = c("blue", "gray", "#cd5b45"),
    hjust = -0.1,
    vjust = 0.5,
    ratio = 5,
    cex = 10,
    title = NULL,
    wrap = 20,
    parse = FALSE,
    collapse = FALSE,
    n = 5,
    result = FALSE,
    label = NULL,
    color_title = "black"
    ) {
    if (is.null(title)) {
        title <- deparse(substitute(x))
    }
    x0 <- as.data.frame(x) -> x
    if (ncol(x0) > 1) {
        x <- sapply(seq(ncol(x0)), function(i) rep(colnames(x0)[i], colSums(x0, na.rm = TRUE)[i]))
    }
    x <- unlist(x) %>%
        stri_trans_general("latin-ascii") %>%
        str_replace_all("\n", " ")
    if (parse) {
        x <- str_to_title(x)
    }
    df <- factor(x) %>%
        fct_relabel(~ str_replace_all(.x, "\\s*\\([^\\)]+\\)", "")) %>%
        fct_relabel(~ str_replace_all(.x, "\\$\\$[^\\)]+", "")) %>%
        fct_relabel(~ str_replace_all(.x, "^0$", "No")) %>%
        fct_relabel(~ str_replace_all(.x, "^1$", "Yes")) %>%
        str_wrap(wrap) %>%
        fct_infreq() %>%
        fct_rev() %>%
        fct_count()
    if (collapse) {
        df <- group_by(df, n) %>%
            summarise(f = paste(f, collapse = ", ") %>% str_wrap(wrap)) %>%
            mutate(f = factor(f))
        df$f <- reorder(df$f, df$n)
        levels(df$f)[str_count(levels(df$f), "\\w+") > n] <- "..."
    }
    if (!is.null(label)) {
        df$f <- factor(df$f, labels = rev(str_wrap(label, wrap)))
    }
    if (result) {
        return(df)
    }
    df <- data.frame(df, order = as.numeric(rownames(df)))
    colors <- colorRampPalette(colors)(nrow(df))
    x_lab <- (round(df$n, 2) / nrow(x0) * 100) %>%
        round(1) %>%
        paste("%")
    df$x_lab <- x_lab # paste0(df$n, "\n   (", x_lab, ")")
    df$y_lab <- df$n / 2
    # i <- df$y_lab < threshold
    # df$x_lab[i] <- ""
    (ggplot(df, aes(f, n, fill = order, label = n)) +
        geom_bar(stat = "identity") +
        expand_limits(y = max(df$n) + max(df$n) / ratio) +
        coord_flip()
    ) %>% theme_perso_bar(colors = colors, cat = FALSE) +
        geom_text(
            aes(color = I("white"), y = y_lab),
            size = cex
        ) +
        geom_text(aes(label = x_lab, color = colors), data = df, hjust = hjust, vjust = vjust, size = cex) +
        ggtitle(str_wrap(title, wrap)) +
        theme(
            plot.title = element_text(hjust = 0, vjust = 0, size = cex * 4, face = "bold", color = color_title),
            axis.text.y = element_text(colour = colors, size = cex * 3),
            axis.text.x = element_text(size = cex * 1.75),
            plot.margin = unit(c(-0, 0, 0, 0.5), "cm"),
            panel.grid.major.y = element_blank()
        )
}

#' @export
theme_perso_bar <- function(p, y = NULL, colors = c(brewer.pal(n = 9, name = "Set1")[-6], brewer.pal(n = 9, name = "Pastel2")), cat = TRUE) {
    p <- p +
        theme_minimal() +
        theme_perso() +
        theme(
            # axis.text.y = element_text(angle = 45, vjust = 1, hjust = 1),
            legend.position = "none"
        ) +
        labs(x = "", y = "")
    if (cat) {
        p + scale_fill_manual(p, values = colors, na.value = "black")
    } else {
        p + scale_fill_gradientn(colors = colors, na.value = "black")
    }
}
