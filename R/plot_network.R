# Creates the nodes for a design matrix
#
# @inheritParams plot_var_2D
# @return A dataframe with rgcca_res$call$blocks in rows and the number of variables, of rows
# and tau or sparsity in columns
get_nodes <- function(rgcca_res) {
    if (tolower(rgcca_res$call$method) %in% c("sgcca", "spls", "spca")) {
        par_rgcca <- "sparsity"
        par_name <- "sparsity"
    } else {
          par_rgcca <- par_name <- "tau"
      }

    if (is.matrix(rgcca_res$call[[par_rgcca]])) {
          penalty <- unlist(
              lapply(
                  seq(NCOL(rgcca_res$call[[par_rgcca]])),
                  function(x) {
                        Reduce(paste, round(rgcca_res$call[[par_rgcca]][, x], 2))
                    }
              )
          )
      } else {
          penalty <- round(rgcca_res$call[[par_rgcca]], 2)
      }

    nrow <- unlist(lapply(rgcca_res$call$blocks, function(x) {
          ifelse(
              is.null(attributes(x)$nrow),
              NROW(rgcca_res$call$blocks[[1]]),
              attributes(x)$nrow
          )
      }))

    values <- list(names(rgcca_res$call$blocks), unlist(lapply(rgcca_res$call$blocks, NCOL)), nrow, penalty)
    nodes <- as.data.frame(matrix(unlist(values), length(rgcca_res$call$blocks), length(values)))
    colnames(nodes) <- c("id", "P", "nrow", par_name)

    return(nodes)
}

# Creates the edges for a design matrix
#
# @inheritParams plot_ind
# @return A dataframe with tuples of connected rgcca_res$call$blocks
get_edges <- function(rgcca_res) {
    J <- NCOL(rgcca_res$call$connection)
    edges <- list()

    k <- 0
    for (j in seq(J)) {
        for (i in seq(J)) {
            if (i > k && rgcca_res$call$connection[i, j] > 0) {
                  edges[[length(edges) + 1]] <-
                      c(names(rgcca_res$call$blocks)[j], names(rgcca_res$call$blocks)[i], rgcca_res$call$connection[i, j])
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
#' @inheritParams plot_ind
#' @inheritParams plot2D
#' @return A dataframe with tuples of connected blocks
#' @examples
#' library(igraph)
#' data("Russett")
#' blocks <- list(
#'     agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11]
#' )
#' rgcca_out <- rgcca(blocks)
#' plot_network(rgcca_out)
#' @export
plot_network <- function(
  rgcca_res,
  title = paste0("Common rows between blocks : ",
                 NROW(rgcca_res$call$blocks[[1]])),
  cex = 1,
  cex_main = 14 * cex,
  cex_point = 3 * cex,
  colors =  c("#eee685", "gray60")) {
    stopifnot(is(rgcca_res, "rgcca"))
    title <- paste0(title, collapse = " ")
    check_colors(colors)
    for (i in c("cex_main", "cex_point")) {
          check_integer(i, get(i))
      }
    check_integer("cex", cex, float = TRUE)

    load_libraries("igraph")

    # Avoid random
    set.seed(1)
    `V<-` <- igraph::`V<-`
    `E<-` <- igraph::`E<-`
    V <- E <- NULL

    nodes <- get_nodes(rgcca_res)
    edges <- get_edges(rgcca_res)

    par <- ifelse("sparsity" %in% names(nodes), "sparsity", "tau")

    net <- igraph::graph_from_data_frame(
        d = edges,
        vertices = nodes,
        directed = FALSE
    )

    V(net)$color <- as.vector(colors[1])
    V(net)$label <- paste(
        nodes$id,
        "\nP =",
        nodes$P,
        paste0("\n", par, " ="),
        nodes[, par],
        "\nN =",
        nodes$nrow,
        sep = " "
    )
    V(net)$shape <- "square"
    E(net)$width <- E(net)$weight * 2
    plot(
        net,
        edge.color = colors[2],
        edge.lty = 2,
        vertex.frame.color = colors[2],
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
#' @inheritParams plot_ind
#' @inheritParams plot2D
#' @return A dataframe with tuples of connected blocks
#' @examples
#' library(visNetwork)
#' data("Russett")
#' blocks <- list(
#'     agriculture = Russett[, seq(3)], industry = Russett[, 4:5],
#'     politic = Russett[, 6:11]
#' )
#' rgcca_out <- rgcca(blocks)
#' plot_network2(rgcca_out)
#' @export
plot_network2 <- function(
  rgcca_res,
  title = paste0("Common rows between blocks : ",
                 NROW(rgcca_res$call$blocks[[1]])),
  cex = 1,
  cex_main = 14 * cex,
  cex_point = 3 * cex,
  colors =  c("#eee685", "gray")) {
    stopifnot(is(rgcca_res, "rgcca"))
    title <- paste0(title, collapse = " ")
    check_colors(colors)
    if (length(colors) < 2) {
          colors <- c(colors, "gray")
      }

    load_libraries("visNetwork")
    `%>%` <- magrittr::`%>%`

    nodes <- get_nodes(rgcca_res)
    edges <- get_edges(rgcca_res)

    par <- ifelse("sparsity" %in% names(nodes), "sparsity", "tau")

    nodes$title <- nodes$id
    nodes$label <- paste(nodes$id,
        "\nP =",
        nodes$P,
        paste0("\n", par, " ="),
        nodes[, par],
        "\nN =",
        nodes$nrow,
        sep = " "
    )

    edges$width <- edges$weight * 2
    nodes$color.background <- rep(as.vector(colors[1]), length(rgcca_res$call$blocks))

    visNetwork::visNetwork(
        nodes,
        edges,
        main = list(
            text = title,
            style = paste0("font-family:sans;font-weight:bold;font-size:", cex_main * 1.4, "px;text-align:center;")
        )
    ) %>%
        visNetwork::visNodes(
            borderWidth = 2,
            shape = "square",
            shadow = TRUE,
            size = cex_point * 7.5,
            font = list(size = cex * 14),
            color = list(
                border = colors[2],
                highlight = list(background = "black", border = "darkred")
            )
        ) %>%
        visNetwork::visEdges(
            smooth = FALSE,
            shadow = TRUE,
            dashes = TRUE,
            color = list(color = colors[2], highlight = "darkred")
        )
}
