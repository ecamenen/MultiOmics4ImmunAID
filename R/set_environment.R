#' @export
set_environment <- function() {
    load_libraries(desc::desc_get_deps()$package)
    file.path(golem::get_golem_wd(), "inst", "extdata")
}
