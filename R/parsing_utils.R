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
        res <- distinct(x)
        if (nrow(res) < nrow(x)) {
            msg <- paste0(
                "Deletion of duplicated lines",
                # paste(row.names(x)[get_dup_bool(x)], collapse = ", "),
                "."
            )
            message(msg)
        }
        x <- res
    }
    remove_empty(x, c("rows", "cols"), quiet = FALSE)
}

get_name_num0 <- function(x) {
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
get_name_num <- function(x) {
    x <- as.data.frame(x)
    colnames(x)[
        sapply(
            colnames(x),
            function(i) {
                !any(
                    x[, i] %>%
                        na.omit() %>%
                        as.character() %>%
                        as.numeric() %>%
                        is.na() %>%
                        unique() %>%
                        suppressWarnings()
                )
            }
        )
    ]
}

#' @export
rename0 <- function(x, sep = " ") {
    str_to_title(x) %>% gsub("_", sep, .)
}

#' @export
get_var_names <- function(x, y) {
    y <- as.data.frame(y)
    (y %>%
        filter(
            str_detect(
                y[, 1],
                paste0("^", c(x), "$", collapse = "|")
            )
        )
    )$item_name
}

#' @export
get_melt <- function(x) {
    as.data.frame(x) %>%
        select(all_of(get_name_num0(x))) %>%
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
        !str_detect(item_name, paste0(y, collapse = "|"))
    )
    )$column_code
}

#' @export
get_levels <- function(x) {
    na.omit(sort(unique(x)))
}

#' @export
reformat <- function(x) {
    sort(unique(str_trim(na.omit(unlist(x)))))
}

#' @export
parse_levels0 <- function(x) {
    n <- str_count(x, pattern = ";") %>% max()
    as.data.frame(x, drop = FALSE) %>%
        separate(1, sep = ";", into = paste0("X", seq(n + 1))) %>%
        reformat() %>%
        sort()
}

#' @export
get_group_occ <- function(x, y = "haq") {
    haq_groups <- unique(gsub(paste(y, "_"), "", x))
    haq_groups0 <- unique(gsub("(V\\d_)|(_2)", "", haq_groups))
    sapply(
        haq_groups0,
        function(i) {
            length(x[str_detect(x, i)])
        }
    )
}

#' @export
get_level_name <- function(x, y) {
    lapply(seq(x), function(i) get_var_names(x[[i]], y))
}

#' @export
get_level_name0 <- function(x, y, i_rows) {
    select(x, all_of(unlist(y[as.numeric(i_rows)]))) %>%
        colnames()
}

#' @export
parse_levels <- function(x, y, form = TRUE) {
    distinct(parse_levels1(x, y, form = form)[, -1])
}

#' @export
parse_levels1 <- function(x, y, form = TRUE) {
    if (form) {
        x <- select(x, all_of(get_codes_levels(y, "Form completed on:")))
    }
    level_cols <- select(x, -one_of(get_name_num(x)))
    ldply(sapply(level_cols, get_levels), rbind)
}

#' @export
col_sim <- function(x, y, form = TRUE) {
    l <- parse_levels1(x, y, form = form)
    l_level_cols <- parse_levels(x, y, form = form)
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
get_table_occ <- function(x, y, i_rows, form = TRUE) {
    res <- parse_levels(x, y, form = form)[-c(i_rows), ]
    level_tot <- parse_levels0(res[!is.na(res)])
    sapply(
        unlist(col_sim(x, y, form = form)[-i_rows]),
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

#' @export
detect_difficulty <- function(x, pattern = "difficulty") {
    which(
        sapply(
            seq(nrow(x)),
            function(i) {
                str_detect(
                    paste(as.character(as.matrix(x[i, ])), collapse = "\t"),
                    regex(pattern, ignore_case = TRUE)
                )
            }
        )
    )
}

#' @export
replace_levels <- function(x, keys, values) {
    for (i in seq_along(keys)) {
        x <- mutate_all(
            x,
            ~ str_replace_all(., regex(keys[i], ignore_case = TRUE), values[i])
        )
    }
    return(x)
}

#' @export
get_patient_id <- function(x) {
    str_replace(
        rownames(x),
        "(\\d{5})[:upper:]\\d{2}[:upper:]{2}\\d",
        "\\1"
    ) %>%
        as.numeric() %>%
        suppressWarnings() %>%
        sort()
}

#' @export
t2tibble <- function(x) {
    tibble(x) %>%
        update_columns(ncol(.), as.character)
    # as.character to fix t bug
}

#' @export
get_level_pattern <- function(levels, regex) {
    i_rows <- detect_difficulty(levels, regex)
    reformat(levels[i_rows, ])
}

#' @export
get_table_occ0 <- function(x, y, levels, regex) {
    i_rows <- seq(nrow(levels))[-detect_difficulty(levels, regex)]
    get_table_occ(x, y, i_rows, FALSE)
}

#' @export
complete_date <- function(x, sep = "/") {
    r_y <- "((?:19|20)\\d{2})"
    r_m <- "([01]\\d)"
    r_d <- "(\\d{2})"
    values0 <- sapply(2:0, function(x) paste(rep(paste0("01", sep), x), collapse = ""))
    values <- sapply(seq(3), function(x) paste0("\\", seq(x), collapse = sep))
    values <- mapply(paste0, values0, values)
    keys <- NULL
    res <- NULL
    k <- c(r_y, r_m, r_d)
    for (i in seq_along(k)) {
        res <- c(k[i], res)
        keys[i] <- paste0("^", paste(res, collapse = sep), "$")
    }
    for (i in seq_along(keys)) {
        x <- str_replace_all(x, keys[i], values[i])
    }
    return(x)
}

#' @export
detect_row <- function(x, p) {
    str_detect(rownames(x), paste0("^(\\d{5})", p))
}

#' @export
length_detect_row <- function(x, p) {
    length(which(detect_row(x, p)))
}

#' @export
finalise_data0 <- function(x, y, n = 50) {
    x <- best_na_percent(x, n) %>%
        clean_data(clean_name = FALSE)
    rownames(x) <- y$immun_aid_identifier
    colnames(x) %>%
        sort() %>%
        select(x, .)
}

#' @export
finalise_data <- function(x, col_name = NULL, clean_name = TRUE, row_name = 1) {
    if (!is.null(col_name)) {
        colnames(x) <- col_name
    }
    x <- clean_data(x, clean_name)
    rownames(x) <- as.data.frame(x[, row_name])[[1]]
    return(x)
}

#' @export
ordinal_variables <- function(x, n = nrow(x) / 2^3) {
    which(
        sapply(
            colnames(x),
            function(i) length(unique(sort(as.data.frame(x[, i])[, 1])))
        ) <= n
    )
}

ordinal_variables0 <- function(x, n = nrow(x) / 2^3) {
    which(
        sapply(
            colnames(x),
            function(i) length(unique(sort(as.data.frame(x[, i])[, 1])))
        ) == n
    )
}

#' @export
find_dates <- function(x) {
    x <- as.data.frame(x)
    sapply(
        seq_along(x),
        function(i) any(str_detect(na.omit(x[, i]), "(([0-3])?\\d/)?(0|1)?\\d/(19|20)\\d{2}"))
    )
}

#' @export
replace_parenthesis <- function(x) {
    str_replace_all(x, "\\(", "\\\\(") %>%
        str_replace_all("\\)", "\\\\)")
}

#' @export
extract_codes <- function(x, y) {
    slice(
        y,
        sapply(
            x,
            function(i) str_which(y$column_code, paste0("^", i, "$"))
        )
    )
}

#' @export
extract_codes0 <- function(x) {
    (attributes(clinic)$codes %>% filter(name == x))$codes
}

#' @export
set_qualitative <- function(x, n = 2) {
    names(ordinal_variables0(x, 2)) %>%
        x[, .] %>%
        mutate_all(factor)
}

#' @export
collapse_mcat <- function(x) {
    lvls <- Reduce(unique, lapply(x, unique))
    sapply(
        na.omit(lvls),
        function(i) {
            sapply(
                seq(nrow(x)),
                function(j) {
                    if (isTRUE(any(x[j, ] == i))) {
                        1
                    } else {
                        0
                    }
                }
            )
        }
    )
}

#' @export
immu_manif <- function(x, df1 = clinic_code[[1]], df2 = clinic_tot1) {
    temp <- filter(df1, str_detect(item_code, paste0("^IMA_", x, "$")))
    temp <- select(df2, temp$column_code) %>%
        set_colnames(temp$item_name) %>%
        clean_names()
    n <- str_extract_all(temp, " ; ") %>%
        map_int(~ length(na.omit(.))) %>%
        max() + 1
    separate(temp, 1, LETTERS[seq(n)], sep = " ; ")
}
