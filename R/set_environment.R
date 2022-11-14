#' @export
set_environment <- function() {
    libs <- desc::desc_get_deps()$package
    load_libraries(libs[libs != "R"])
    file.path(golem::get_golem_wd(), "inst", "extdata")
}
