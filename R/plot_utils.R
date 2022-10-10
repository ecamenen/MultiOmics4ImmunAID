#' @export
plot_corr <- function(
    x,
    y,
    clean_name = TRUE,
    mat = NULL,
    p_mat = NULL,
    col = brewer.pal(n = 7, name = "RdBu")
) {
    if (clean_name) {
        colnames(x) <- get_var_names(colnames(x), y)
        x <- clean_names(x)
    }
    x <- as.data.frame(x)
    if (is.null(mat)) {
        mat <- get_corr(x, TRUE)
    }
    if (is.null(p_mat)) {
        p_mat <- get_corr(x)
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

get_colors <- function() {
    c(
        "#cd5b45",
        "#71ad65",
        "#3c78b4",
        "#ffc600",
        "#b448af",
        "#9d9d9d",
        "#abcaef",
        "#4a6f43",
        "#f0e500",
        "#efb8f0",
        "black",
        "#d6d6d6"
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
        plotsize = plotsize
    )
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


plot_corr0 <- function(
    x,
    n = ncol(x),
    cols = c("black", brewer.pal(n = 7, name = "RdBu"), "#000000"),
    method = "pearson",
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
        x <- round(scales::rescale(x, to = c(1, n),
                                   from = scale_from))
    }

    color_code <- palette[x]
    color_code[is.na(color_code)] <- na_color
    return(color_code)
}
