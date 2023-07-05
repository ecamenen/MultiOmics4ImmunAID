#' @export
save_tiff <- function(f, filename = "violinplot_clin.tiff") {
    tiff(
        filename,
        units = "px",
        width = 2000,
        height = 2000,
        res = 300
    )
    f
    dev.off()
}

#' @export
kable0 <- function(x, align = "c", rownames = TRUE, color = "#a9a9a9") {
    res <- kbl(x, escape = FALSE, align = align) %>%
        kable_minimal(full_width = FALSE)
    if (isTRUE(rownames)) {
        column_spec(res, 1, bold = TRUE, color = color)
    } else {
        return(res)
    }
}

save_tsv <- function(x, filename = NULL, col_names = TRUE, cwd = golem::get_golem_wd()) {
    if (is.null(filename)) {
        filename <- substitute(x)
    }
    write_tsv(
        as.data.frame(x),
        file.path(cwd, filename),
        col_names = col_names
    )
}

#' Read the clinical files
#' 
#' Read the clinical files
#' @param data type of ImmunAID data (default: Clinical)
#' @param type export or code data type (default: export)
#' @param n_batch number of batch to parse (default: 4)
#' @param path directory root for the clinical data (required)
#' @export
read_immunaid <- function(
    data = "Clinical",
    type = "export",
    date = "20230103",
    n_batch = 4,
    path = file.path(
        golem::get_golem_wd(),
        "inst",
        "extdata",
        "clinical",
        "20230103"
    )
) {
    lapply(
        seq(n_batch),
        function(i) {
            read_tsv(file.path(
                path,
                paste0(
                    "ImmunAID_",
                    data,
                    "_",
                    date,
                    "_batch",
                    i,
                    "_eCRF_",
                    type,
                    ".csv"
                )
            ))
        }
    )
}
