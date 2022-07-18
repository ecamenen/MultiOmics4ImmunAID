#' @export
clean_data <- function(
    x,
    clean_name = TRUE,
    clean_constant = TRUE,
    clean_duplicates = TRUE
    ) {
    if (clean_name) {
        x <- clean_names(x)
    }
    if (clean_constant) {
        x <- remove_constant(x, na.rm = TRUE, quiet = FALSE)
    }
    if (clean_duplicates) {
        res <- get_dup_name(x)
        if (length(res) > 0) {
            msg <- paste0(
                "Deletion of duplicated lines with identifiers ",
                paste(res, collapse = ", "),
                "."
            )
            message(msg)
        }
        x <- distinct(x)
    }
    remove_empty(x, c("rows", "cols"), quiet = FALSE)
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
    y <- as.data.frame(y)
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

get_intersection0 <- function(x) {
    res <- gplots::venn(x)
    attributes(res)$intersections
}

#' @export
get_intersection <- function(x) {
    intersections <- get_intersection0(x)
    intersections[grepl(":", names(intersections))]
}

#' @export
get_difference <- function(x) {
    intersections <- get_intersection0(x)
    intersections[!grepl(":", names(intersections))]
}

#' @export
get_dup_bool <- function(x) {
    duplicated(x[, 1])
}

#' @export
get_dup_name <- function(x) {
    as.data.frame(x)[get_dup_bool(x), 1]
}

#' @export
get_dup <- function(x) {
    filter(x, x[, 1] == get_dup_name(x))
}

#' @export
get_codes_levels <- function(
    x,
    y = c("ImmunAID identifier", "Form completed on:")) {
    (filter(
        x[[1]],
        !str_detect(item_name, paste0(y, collapse = "|")))
    )$column_code
}

#' @export
get_levels <- function(x) {
    na.omit(sort(unique(x)))
}
