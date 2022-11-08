# Creates the nodes for a design matrix
#
# @return A dataframe with x in rows and the number of variables, of rows
# and tau or sparsity in columns
get_nodes <- function(x) {
    if (is.null(names(x))) {
        names(x) <- seq_along(x)
    }
    dims <- sapply(x, dim)
    values <- list(names(x), dims[2, ], dims[1, ])
    nodes <- as.data.frame(matrix(unlist(values), length(x), length(values)))
    colnames(nodes) <- c("id", "p", "n")
    return(nodes)
}

# Creates the edges for a design matrix
#
# @return A dataframe with tuples of connected x
get_edges <- function(x, C, p = NULL) {
    J <- NCOL(C)
    edges <- list()

    k <- 0
    for (j in seq(J)) {
        for (i in seq(J)) {
            if (i > k && abs(C[i, j]) > 0) {
                if (is.null(p)) {
                    d <- NULL
                } else {
                    d <- p[i, j]
                }
                edges[[length(edges) + 1]] <- c(names(x)[j], names(x)[i], C[i, j], d)
            }
        }
        k <- k + 1
    }

    n <- length(edges[[1]])
    edges <- as.data.frame(t(matrix(unlist(edges), n, length(edges))))
    colnames(edges) <- c("from", "to", "weight", "p")[seq(n)]
    edges[, 3] <- as.numeric(edges[, 3])
    if (!is.null(p)) {
        edges[, 4] <- as.numeric(edges[, 4])
    }

    return(edges)
}

#' Plot the connection between blocks
#'
#' @return A dataframe with tuples of connected blocks
#' @export
plot_network <- function(
    x,
    C = 1 - diag(length(x)),
    title = paste0(
        "Common rows between blocks : ",
        length(Reduce(intersect, lapply(x, row.names)))
    ),
    cex = 1,
    cex_main = 14 * cex,
    cex_point = 3 * cex,
    cex_nodes = 2 * cex,
    color = c("#eee685", "gray60"),
    shape = "square",
    dashes = TRUE,
    nodes = NULL,
    edges = NULL
) {
    stopifnot(is(x, "list"))
    stopifnot(all(sapply(x, function(i) is(i, "data.frame"))))
    title <- paste0(title, collapse = " ")
    RGCCA:::check_colors(color)
    for (i in c("cex_main", "cex_point")) {
        RGCCA:::check_integer(i, get(i))
    }
    RGCCA:::check_integer("cex", cex, float = TRUE)

    RGCCA:::load_libraries("igraph")

    # Avoid random
    set.seed(1)
    `V<-` <- igraph::`V<-`
    `E<-` <- igraph::`E<-`
    V <- E <- NULL

    if (is.null(nodes)) {
        nodes <- get_nodes(x)
    }
    if (is.null(edges)) {
        edges <- get_edges(x, C)
    }

    net <- igraph::graph_from_data_frame(
        d = edges,
        vertices = nodes,
        directed = FALSE
    )

    V(net)$color <- as.vector(color[1])
    V(net)$label <- paste(
        nodes$id,
        "\nP =",
        nodes$p,
        "\nN =",
        nodes$n,
        sep = " "
    )
    if (shape == "dot") {
        shape <- "circle"
    }

    V(net)$shape <- shape


    E(net)$width <- E(net)$weight * cex_nodes
    plot(
        net,
        edge.color = color[2],
        edge.lty = 2,
        vertex.frame.color = color[2],
        vertex.label.cex = cex,
        vertex.label.color = "black",
        vertex.label.dist = 6,
        vertex.label.degree = 1.5,
        vertex.size = cex_point * 7.5,
        margin = c(0.1, 0, 0, 0)
    )
    title(title, cex.main = cex_main * 0.1)
}

plot_corr_network <- function(x, cutoff = 0.75, ...) {
    C <- get_corr(x, TRUE)
    title <- round(mean(C, na.rm = TRUE), 2)
    C[abs(C) < cutoff] <- 0 -> diag(C)
    C[is.na(C)] <- 0

    p <- get_corr(x)

    edges <- get_edges(x, C, p)
    # edges <- adjust_pvalue(edges, "p")
    # font <- "14px arial black"
    # edges$font.bold.mod <- ifelse(edges$p.adj < 0.05, paste(font, "bold"), font)
    edges$title <- round(edges[, 3], 2) -> edges$label
    id <- unlist(edges[, seq(2)])
    nodes <- data.frame(id) %>%
        group_by(id) %>%
        summarise(size = n() * 6) %>%
        as.data.frame()

    color_node <- ifelse(edges$weight > 0, "green", "red") -> edges$color

    plot_network2(
        x,
        C,
        shape = "dot",
        dashes = FALSE,
        nodes = nodes,
        edges = edges,
        cex_nodes = edges$weight * 20,
        title = paste("Mean correlation between variables = ", title * 2),
        ...
    )
}


#' Plot the connection between blocks (dynamic plot)
#'
#' @return A dataframe with tuples of connected blocks
#' @export
plot_network2 <- function(
    x,
    C = 1 - diag(length(x)),
    title = paste0(
        "Common rows between blocks : ",
        length(Reduce(intersect, lapply(x, row.names)))
    ),
    cex = 1,
    cex_main = 14 * cex,
    cex_point = 3 * cex,
    cex_nodes = 2 * cex,
    color = c("#eee685", "gray"),
    shape = "square",
    dashes = TRUE,
    nodes = NULL,
    edges = NULL
) {
    # stopifnot(is(x, "list"))
    # stopifnot(all(sapply(x, function(i) is(i, "data.frame"))))
    title <- paste0(title, collapse = " ")
    RGCCA:::check_colors(color)
    for (i in c("cex_main", "cex_point")) {
        RGCCA:::check_integer(i, get(i))
    }
    RGCCA:::check_integer("cex", cex, float = TRUE)
    if (length(color) < 2) {
        color <- c(color, "gray")
    }

    # load_libraries("visNetwork")
    `%>%` <- magrittr::`%>%`

    if (is.null(nodes)) {
        nodes <- get_nodes(x)
        nodes$label <- paste(
            nodes$id,
            "\nP =",
            nodes$p,
            "\nN =",
            nodes$n,
            sep = " "
        )
    }
    nodes$title <- nodes$id -> nodes$label
    nodes$color.background <- rep(as.vector(color[1]), nrow(nodes))

    if (is.null(edges)) {
        edges <- get_edges(x, C)
    }
    edges$width <- edges$weight * cex_nodes

    visNetwork::visNetwork(
        nodes,
        edges,
        main = list(
            text = title,
            style = paste0(
                "font-family:sans;font-weight:bold;font-size:",
                cex_main * 1.4,
                "px;text-align:center;"
            )
        )
    ) %>%
        visNetwork::visNodes(
            borderWidth = 2,
            shape = shape,
            shadow = TRUE,
            size = cex_point * 7.5,
            font = list(size = cex * 14),
            color = list(
                border = color[2],
                highlight = list(background = "black", border = "darkred")
            )
        ) %>%
        visNetwork::visEdges(
            smooth = FALSE,
            shadow = TRUE,
            dashes = dashes,
            color = list(color = color[2], highlight = "darkred")
        )
}
