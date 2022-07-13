#' @export
clean_data <- function(
    x,
    clean_name = TRUE,
    row_number = 1
    ) {
    if (clean_name) {
        x <- clean_names(x)
    }
    x %>%
        remove_empty(c("rows", "cols"), quiet = FALSE) %>%
        remove_constant(na.rm = TRUE, quiet = FALSE)
}

#' @export
get_name_num <- function(x) {
    colnames(x)[
        sapply(
            colnames(x),
            function(i) {
                is.numeric(c(x[, i])[[1]])
            }
        )
    ]
}

#' @export
get_var_names <- function(x, y) {
    (y %>%
        filter(
            str_detect(
                y[, 1],
                paste0(c(x), collapse = "|")
            )
        )
    )$item_name
}

#' @export
get_melt <- function(x) {
    x %>%
        select(all_of(get_name_num(x))) %>%
        pivot_longer(everything())
}

#' @export
getmelt0 <- function(x) {
    melt(x) %>%
        select(variable, value) %>%
        rename(name = variable)
}

#' @export
get_intersection <- function(x) {
    res <- gplots::venn(x)
    intersections <- attributes(res)$intersections
    intersections[grepl(":", names(intersections))]
}
