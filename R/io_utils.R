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
