

clean_data <- function(x,
                       clean_name = TRUE,
                       row_number = 1) {
    if (clean_name) {
        x <- clean_names(x)
    }
    x %>%
        remove_empty(c("rows", "cols"), quiet = FALSE) %>%
        remove_constant(na.rm = TRUE, quiet = FALSE)
}

get_name_num <- function(x) {
    colnames(x)[
      sapply(
        colnames(x),
        function(i) {
              is.numeric(c(x[, i])[[1]])
          }
    )]
}

get_var_names <- function(x) {
    (stats %>%
       filter(
         str_detect(
           column_code,
           paste0(c(x), collapse = "|")
           )
         )
     )$item_name
}

get_melt <- function(x) {
    (x %>% select(all_of(get_name_num(x))) %>% gather())
}

get_intersection <- function(x) {
    res <- gplots::venn(x)
    intersections <- attributes(res)$intersections
    intersections[grepl(":", names(intersections))]
}
