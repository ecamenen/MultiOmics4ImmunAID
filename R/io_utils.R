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
kable0 <- function(x, align = "c") {
    kbl(x, escape = F, align = align) %>%
        kable_minimal(full_width = F)
}

save_tsv <- function(x, filename = NULL, col_names = TRUE) {
    if (is.null(filename)) {
        filename <- substitute(x)
    }
    write_tsv(
        as.data.frame(x),
        file.path(golem::get_golem_wd(), filename),
        col_names = col_names
    )
}

#' @export
read_batches <- function(
    k = "Clinical",
    export = "export",
    date = "20220512",
    n_batch = 3,
    clin_path = file.path(
        golem::get_golem_wd(),
        "inst",
        "extdata",
        "clinical",
        "20220501"
    )
) {
    lapply(
        seq(n_batch),
        function(i) {
            read_tsv(file.path(
                clin_path,
                paste0(
                    "ImmunAID_",
                    k,
                    "_",
                    date,
                    "_batch",
                    i,
                    "_eCRF_",
                    export,
                    ".csv"
                )
            ))
        }
    )
}
