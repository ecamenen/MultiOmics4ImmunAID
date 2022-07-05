library(devtools)

name <- "MultiOmics4ImmunAID"
# available::available(name)
path <- paste0("~/bin/", name)
if (!file.exists(path)) {
    dir.create(file.path(path))
}
setwd(path)
create_project(path)
create_package(path)
use_git(message = "Add configuration files for packaging")
use_github()
use_description(
    fields = list(
        Title = "Multi-omics data analysis for Auto-Inflammatory Diseases",
        `Authors@R` = c(
            person("Etienne", "Camenen", email = "etienne.camenen@gmail.com", role = c("aut", "cre")),
            person("Daniel", "Herrero-Saboya", role = "aut")
        ),
        Description = "Multi-omics data analysis for the ImmunAID project (H2020): identify potential biomarkers for Auto-Inflammatory Diseases (AID).",
        License = "GPL-3",
        Version = "0.1.0"
    )
)
use_readme_rmd()
use_cran_badge()
use_package_doc()
use_pipe()
# promptPackage(name)
for (f in c(".idea/", "dev/", "inst/extdata/")) {
    use_git_ignore(f)
    use_build_ignore(f)
}
# use_vignette("Tutorial")
use_testthat()
use_spell_check()
for (p in c(
    "openxlsx",
    "dplyr",
    "ggplot2",
    "reshape2",
    "readr",
    "data.table",
    "ggpubr",
    "rstatix"
)) {
    use_package(p)
}
use_coverage()
use_github_actions()
use_github_actions_badge()
use_data_raw()
