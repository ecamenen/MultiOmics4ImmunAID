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
              suppressWarnings(
                isTRUE(
                  unique(!is.na(as.numeric(na.omit(c(x[, i])[[1]]))))
                )
              )
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

#' @export
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

#' @export
reformat <- function(x) {
    unique(str_trim(na.omit(unlist(x))))
}

#' @export
parse_levels0 <- function(x) {
    n <- str_count(x, pattern = ";") %>% max()
    as.data.frame(x) %>%
        separate(1, sep = ";", into = paste0("X", seq(n + 1))) %>%
        reformat() %>%
        sort()
}

#' @export
get_group_occ <- function(x, y = "haq") {
    haq_groups <- unique(gsub(paste(y, "_"), "", x))
    haq_groups0 <- unique(gsub("(V\\d_)|(_2)", "", haq_groups))
    sapply(haq_groups0, function(i) {
          length(x[str_detect(haq_groups0, i)])
      })
}

#' @export
parse_levels <- function(x, y) {
    distinct(parse_levels1(x, y)[, -1])
}

#' @export
parse_levels1 <- function(x, y) {
    level_cols <- x %>%
        select(all_of(get_codes_levels(y, "Form completed on:"))) %>%
        select(-one_of(get_name_num(x)))
    ldply(sapply(level_cols, get_levels), rbind)
}

#' @export
col_sim <- function(x, y) {
    l <- parse_levels1(x, y)
    l_level_cols <- parse_levels(x, y)
    sapply(
        seq(nrow(l_level_cols)),
        function(j) {
            l[
                sapply(
                    seq(nrow(l)),
                    function(i) {
                          identical(c(l[i, -1]), c(l_level_cols[j, ]))
                      }
                ),
                1
            ]
        }
    )
}

#' @export
get_table_occ <- function(x, y, i_rows) {
    level_tot <- parse_levels0(
        parse_levels0(parse_levels(x, y)[-c(i_rows), ])
    )
    sapply(
        unlist(col_sim(x, y)[-i_rows]),
        function(i) {
            sapply(
                level_tot,
                function(j) {
                    nrow(
                        filter(x, str_detect(as.data.frame(x[, i])[, 1], j)) %>%
                            select(all_of(i))
                    )
                }
            )
        }
    )
}


detect_difficulty0 <- function(x, pattern = "difficulty") {
    filter(
        x,
        if_any(
            everything(),
            ~ str_detect(., regex(pattern, ignore_case = TRUE))
        )
    )
}

#' @export
detect_difficulty <- function(x, pattern = "difficulty") {
    which(
        sapply(
            seq(nrow(x)),
            function(i) {
                str_detect(
                    paste(as.data.frame(x)[i, ], collapse = " "),
                    regex(pattern, ignore_case = TRUE)
                )
            }
        )
    )
}

#' @export
replace_levels <- function(x, keys, values) {
    for (i in seq_along(keys)) {
        x <- mutate_all(x, funs(str_replace_all(., keys[i], values[i])))
    }
    return(x)
}
