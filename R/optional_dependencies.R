.brathering_bioconductor_packages <- c(
    "IRanges",
    "limma",
    "MOFA2"
)

.brathering_pak_packages <- c(
    colrr = "close-your-eyes/colrr",
    deunicode = "close-your-eyes/deunicode",
    dismay = "skinnider/dismay",
    fcexpr = "close-your-eyes/fcexpr",
    hdos = "close-your-eyes/hdos",
    scattermore = "exaexa/scattermore"
)

.brathering_cran_packages <- c(
    "aricode",
    "bench",
    "BiocManager",
    "cluster",
    "ComplexUpset",
    "crayon",
    "dbscan",
    "digest",
    "farver",
    "foreign",
    "forcats",
    "fs",
    "gganimate",
    "ggforce",
    "ggplot2",
    "ggrepel",
    "Gmisc",
    "gt",
    "gtools",
    "HDoutliers",
    "irlba",
    "knitr",
    "magick",
    "MASS",
    "Matrix",
    "mclust",
    "mvoutlier",
    "officer",
    "pak",
    "parallel",
    "patchwork",
    "plotly",
    "ragg",
    "readr",
    "rgl",
    "Rlof",
    "robustbase",
    "rpart",
    "rsvd",
    "scales",
    "Seurat",
    "shiny",
    "stringdist",
    "stringi",
    "styler"
)

.brathering_optional_packages <- c(
    .brathering_bioconductor_packages,
    names(.brathering_pak_packages),
    .brathering_cran_packages
)

.ensure_package <- function(package) {
    if (!package %in% .brathering_optional_packages) {
        stop("Unknown optional package: ", package, call. = FALSE)
    }
    if (requireNamespace(package, quietly = TRUE)) {
        return(invisible(TRUE))
    }

    message("Installing optional package '", package, "'.")
    if (package %in% .brathering_bioconductor_packages) {
        if (!requireNamespace("BiocManager", quietly = TRUE)) {
            utils::install.packages("BiocManager")
        }
        BiocManager::install(package, ask = FALSE, update = FALSE)
    } else if (package %in% names(.brathering_pak_packages)) {
        if (!requireNamespace("pak", quietly = TRUE)) {
            utils::install.packages("pak")
        }
        pak::pak(unname(.brathering_pak_packages[[package]]))
    } else {
        utils::install.packages(package)
    }

    if (!requireNamespace(package, quietly = TRUE)) {
        stop(
            "Package '", package, "' is required for this function but could not be installed.",
            call. = FALSE
        )
    }
    invisible(TRUE)
}

.ensure_packages <- function(packages) {
    invisible(lapply(unique(packages), .ensure_package))
}
