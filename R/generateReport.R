#' Create a report a given set of packages
#'
#' @description The function compiles statistics for Bioconductor packages that
#'   have GitHub repositories. It uses functionality in `BiocPkgTools` to
#'   extract commit and issue history. A token is required to access the
#'   GitHub commit and issue history.
#'
#' @section Authentication:
#'   The package uses the `gh` package calls from `BiocPkgTools`. Users must
#'   authenticate with a GitHub Fine Grained Token and add the token using
#'   `gitcreds::gitcreds_set()`.
#'
#' @param packages `character()` A vector of valid package names
#'
#' @param gh_org `character(1)` The GitHub organization from which to read
#'   issue and commit data from.
#'
#' @param since_date `character(1)` The date from when to start looking at
#'   commit and issue history. This should be specified in the year, month, and
#'   day format, 'YYYY-MM-DD'.
#'
#' @param outdir `character(1)` The directory in which to place rendered
#'   RMarkdown documents, by default they will be placed in the current working
#'   directory.
#'
#' @param overwrite `logical(1)` Whether to overwrite an existing rendered
#'   product, i.e., a runnable RMarkdown document.
#'
#' @examples
#' if (interactive()) {
#'
#' generateReport(
#'     c(
#'         "MultiAssayExperiment", "cBioPortalData", "SingleCellMultiModal"
#'     ),
#'     gh_org = "waldronlab",
#'     since_date = "2017-05-01",
#'     overwrite = TRUE
#' )
#'
#' generateReport(
#'     "RaggedExperiment",
#'     gh_org = "Bioconductor",
#'     since_date = "2017-05-01",
#'     overwrite = TRUE
#' )
#'
#' }
#'
#' @export
generateReport <- function(
    packages, gh_org, since_date, outdir = ".",
    overwrite = FALSE
) {
    stopifnot(
        BiocBaseUtils::isCharacter(packages),
        BiocBaseUtils::isScalarCharacter(gh_org),
        BiocBaseUtils::isScalarCharacter(template),
        BiocBaseUtils::isTRUEorFALSE(overwrite)
    )

    template <- system.file(
        package = "BiocPkgStats", "template", "package_stats.Rmd",
        mustWork = TRUE
    )
    temp_char <- readLines(template)

    for (pkg in packages) {

    message("Working on: ", pkg)
    rendered_path <- file.path(
        outdir, paste0(pkg, "_", basename(template))
    )

    type <- .get_pkg_type(pkg)

    rendered <- whisker::whisker.render(
        template = temp_char,
        data = list(
            package = pkg, org = gh_org, sinceDate = since_date,
            pkgType = type, anyPkgs = TRUE
        )
    )

    if (!overwrite && file.exists(rendered_path))
        stop(
            "path to rendered file exists and 'overwrite = FALSE'\n",
            "    rendered file path: '", rendered_path, "'"
        )

    writeLines(rendered, rendered_path)

    }

    invisible(outdir)
}

.get_pkg_type <- function(package) {
    bv <- utils::packageDescription(pkg = package)$biocViews
    terms <- unlist(strsplit(packageDescription(package)$biocViews, ",\\s+|\n"))
    type <- biocViews::guessPackageType(terms)
}
