#' @export
best_na_percent <- function(x, threshold = 50) {
    (df_na <- diagnose(x) %>%
        filter(missing_percent > threshold) %>%
        arrange(desc(missing_percent))
    )
    var_na <- data.frame(df_na %>% select(variables))[, 1]
    x %>% select(-one_of(var_na))
}

#' @export
get_correlation <- function(x, threshold = 0.5, half = TRUE) {
    y <- correlate(x) %>%
        filter(abs(coef_corr) > threshold)
    if (half) {
        y %>% filter(as.integer(var1) > as.integer(var2))
    } else {
        y
    }
}

#' @export
# n = 3; df = matrix(rep(c(runif(n), NA), 2), n +1, 2); get_corr(df)
get_corr <- function(x, pval = FALSE, method = "pearson") {
    x <- as.data.frame(x)
    vars <- seq(ncol(x))
    sapply(
        vars,
        function(i) {
            sapply(
                vars,
                function(j) {
                    if (is.numeric(x[, i]) & is.numeric(x[, j])) {
                        tryCatch(
                            {
                                res <- cor.test(x[, i], x[, j], method = method) %>%
                                    suppressWarnings()
                                if (pval) {
                                    res$estimate
                                } else {
                                    res$p.value
                                }
                            },
                            error = function(e) NA
                        )
                        # return(res)
                    } else {
                        NA
                    }
                }
            )
        }
    )
}

#' @export
explore_outliers <- function(x) {
    colnames(x)[find_outliers(x)]
}

#' @export
get_outliers <- function(
    x,
    y,
    probs = c(0.25, 0.75),
    method = "iqr",
    c = 1.5
    ) {
    stopifnot(method %in% c("iqr", "percentiles", "hampel", "mad", "sd"))
    y <- as.data.frame(x)[, y]
    # outliers <- boxplot(y)$out
    # percentiles: probs = c(0.025, 0.975)
    quant <- quantile(y, probs = probs, na.rm = TRUE)
    up <- quant[2]
    low <- quant[1]
    if (method == "iqr") {
        # interquartile range: 1.5 * IQR
        iqr <- IQR(y, na.rm = TRUE)
        up <- up + c * iqr
        low <- low - c * iqr
    }
    if (method %in% c("hampel", "mad", "sd")) {
        if (method %in% c("hampel", "mad")) {
            # mediane absolute deviation: 3 * MAD
            med <- median(y, na.rm = TRUE)
            mad3 <- c * mad(y, na.rm = TRUE, constant = 1)
        } else {
            med <- median(y, na.rm = TRUE)
            mad3 <- c * sd(y, na.rm = TRUE)
        }
        up <- med + mad3
        low <- med - mad3
    }
    x %>% filter(y < low | y > up)
}

get_not_normal <- function(x, p_value = 0.05) {
    normality(x) %>%
        filter(p_value <= p_value) %>%
        arrange(abs(p_value))
}

perc_autocorr <- function(x, threshold = 0.8) {
    round(nrow(get_correlation(x, threshold)) / (ncol(x)^2 - ncol(x)), 2)
}

#' @export
get_not_normal0 <- function(x, threshold = 0.05) {
    sapply(
        x, function(i) {
            tryCatch(
                shapiro_test(as.data.frame(i)[[1]])$p.value, # < threshold,
                error = function(e) NA
            )
        }
    )
}

percent_not_normal <- function(x, threshold = 0.05) {
    round(length(which(get_not_normal0(x, threshold))) / ncol(x) * 100, 2)
}


get_corr0 <- function(x, p = NULL) {
    r <- get_corr(x, TRUE)
    r[is.na(r)] <- 0
    colnames(r) <- colnames(x) -> row.names(r)
    if (!is.null(p)) {
        p <- get_corr(x)
        p[is.na(p)] <- 1
    }
    list(r, p)
}

#' @export
calculate_samplesize <- function(muA, muB, sd, kappa = 1, alpha = 0.05, beta = 0.2) {
    ceiling((1 + 1 / kappa) * (sd * (qnorm(1 - alpha / 2) + qnorm(1 - beta)) / (muA - muB)) ^ 2)
}

#' @export
calculate_effectsize <- function(muA, muB, sd) {
    (muA - muB) / sd
}

#' @export
remove_cofunding <- function(x, vars, block = 1) {
    # All cofunding variables in numeric
    # for (i in vars) {
    #     x[[block]][, i] <- as.numeric(as.character(x[[block]][, i]))
    # }

    # Remove missing samples from cofunding variables
    to_remove <- sapply(vars, function(i) which(is.na(x[[block]][, i])))
    to_remove <- unique(Reduce(c, to_remove))
    if (length(to_remove) > 0) {
          x <- lapply(x, function(i) i[-to_remove, ])
      }
    cl <- x[[block]]
    # x0 <- lapply(x, log1p)

    # Weight by the cofunding effect residuals
    blocks.df <- lapply(
        x,
        function(i) {
            lapply(
                seq(ncol(i)),
                function(j) {
                    form <- as.formula(paste0("i[, j] ~", paste0("cl$", vars, collapse = "+")))
                    lm(form, na.action = "na.exclude")$residuals
                }
            )
        }
    )

    # Position of the NA values in blocks
    for (k in seq(length(x))) {
        listNA <- which(is.na(x[[k]]), arr.ind = TRUE)

        # Insert NA
        for (i in unique(listNA[, 2])) {
            for (j in listNA[which(listNA[, 2] == i), 1] - 1) {
                blocks.df[[k]][[i]] <- append(
                    blocks.df[[k]][[i]],
                    NA,
                    j
                )
            }
        }

        # Convert in matrix
        blocks.df[[k]] <- as.data.frame(
            matrix(
                unlist(blocks.df[[k]]),
                nrow = nrow(x[[k]]),
                ncol = ncol(x[[k]]),
                dimnames = list(row.names(x[[k]]), colnames(x[[k]]))
            )
        )
    }

    return(blocks.df)
}
