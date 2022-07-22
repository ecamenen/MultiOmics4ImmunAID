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
