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
    colnames(nodes) <- c("id", "P", "N")
    return(nodes)
}

# Creates the edges for a design matrix
#
# @return A dataframe with tuples of connected x
get_edges <- function(x, C) {
    J <- NCOL(C)
    edges <- list()

    k <- 0
    for (j in seq(J)) {
        for (i in seq(J)) {
            if (i > k && C[i, j] > 0) {
                edges[[length(edges) + 1]] <- c(names(x)[j], names(x)[i], C[i, j])
            }
        }
        k <- k + 1
    }

    edges <- as.data.frame(t(matrix(unlist(edges), 3, length(edges))))
    colnames(edges) <- c("from", "to", "weight")
    edges[, 3] <- as.numeric(edges[, 3])

    return(edges)
}

#' Plot the connection between blocks
#'
#' @return A dataframe with tuples of connected blocks
#' @examples
#' library(igraph)
#' library(RGCCA)
#' data("Russett")
#' blocks <- list(
#'     agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11]
#' )
#' plot_network(blocks)
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
    color = c("#eee685", "gray60")
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

    nodes <- get_nodes(x)
    edges <- get_edges(x, C)

    net <- igraph::graph_from_data_frame(
        d = edges,
        vertices = nodes,
        directed = FALSE
    )

    V(net)$color <- as.vector(color[1])
    V(net)$label <- paste(
        nodes$id,
        "\nP =",
        nodes$P,
        "\nN =",
        nodes$N,
        sep = " "
    )
    V(net)$shape <- "square"
    E(net)$width <- E(net)$weight * 2
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

#' Plot the connection between blocks (dynamic plot)
#'
#' @return A dataframe with tuples of connected blocks
#' @examples
#' library(visNetwork)
#' library(RGCCA)
#' data("Russett")
#' blocks <- list(
#'     agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11]
#' )
#' plot_network2(blocks)
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
    color = c("#eee685", "gray")
) {
    stopifnot(is(x, "list"))
    stopifnot(all(sapply(x, function(i) is(i, "data.frame"))))
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

    nodes <- get_nodes(x)
    edges <- get_edges(x, C)

    nodes$title <- nodes$id
    nodes$label <- paste(
        nodes$id,
        "\nP =",
        nodes$P,
        "\nN =",
        nodes$N,
        sep = " "
    )

    edges$width <- edges$weight * 2
    nodes$color.background <- rep(as.vector(color[1]), length(x))

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
            shape = "square",
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
            dashes = TRUE,
            color = list(color = color[2], highlight = "darkred")
        )
}
