read_batches <- function(files, export = "export") {
  batches <- list()
  for (i in seq_along(files)) {
    batches[[i]] <- read_tsv(file.path(
      clin_path,
      paste0(
        "ImmunAID_",
        blocks[k],
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
  return(batches)
}

save_tiff <- function(f, filename = "violinplot_clin.tiff") {
  tiff(filename, units = "px", width = 2000, height = 2000, res = 300)
  f
  dev.off()
}
