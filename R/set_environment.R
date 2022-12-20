#' @export
set_environment <- function() {
    libs <- desc_get_deps()$package
    load_libraries(libs[libs != "R"])
    path <- file.path(golem::get_golem_wd(), "inst", "results")
    path_img <<- file.path(path, "img")
    path_tsv <<- file.path(path, "tsv")
    file.path(golem::get_golem_wd(), "inst", "extdata")
}
