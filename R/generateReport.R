#' Create a report a given set of packages
#'
#' @description The function compiles statistics for Bioconductor packages that
#'   have GitHub repositories. It uses functionality in `BiocPkgTools` to
#'   extract commit and issue history. A token is required to access the
#'   GitHub commit and issue history.
#'
#' @details Note that packages must be installed in order to determine the
#' package type via the `biocViews` field.
#'
#' @section Authentication:
#'   The package uses the `gh` package calls from `BiocPkgTools`. Users must
#'   authenticate with a GitHub Fine Grained Token and add the token using
#'   `gitcreds::gitcreds_set()`.
#'
#' @param packages `character()` A vector of valid package names that are
#'   installed
#'
#' @param gh_org `character()` The GitHub organization from which to read issue
#'   and commit data from. It can be either a scalar character to be recycled to
#'   the length of packages or a vector the length of `packages` for instances
#'   where packages are hosted under different GitHub organizations.
#'
#' @param since_date `character()` The date from when to start looking at
#'   commit and issue history. This should be specified in the year, month, and
#'   day format, 'YYYY-MM-DD'. It can be a vector of dates that match the
#'   length `packages`.
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
#' }
#'
#' @export
generateReport <- function(
    packages, gh_org, since_date, outdir = ".",
    overwrite = FALSE
) {
    stopifnot(
        BiocBaseUtils::isCharacter(packages),
        BiocBaseUtils::isCharacter(gh_org),
        BiocBaseUtils::isTRUEorFALSE(overwrite)
    )
    if (!identical(length(gh_org), length(packages)) &&
        !identical(length(gh_org), 1L))
        stop("'gh_org' should be length one or the same length as 'packages'")

    template <- system.file(
        package = "BiocPkgStats", "template", "stats_report.Rmd",
        mustWork = TRUE
    )
    temp_char <- readLines(template)

    rendered_path <- file.path(
        outdir, paste0("packages_", basename(template))
    )

    pkgdata <- .get_pkg_data(packages, gh_org, since_date)
    datalist <- unname(split(pkgdata, pkgdata[["package"]]))
    rendered <- whisker::whisker.render(
        template = temp_char,
        data = list(packages = datalist)
    )

    if (!overwrite && file.exists(rendered_path))
        stop(
            "path to rendered file exists and 'overwrite = FALSE'\n",
            "    rendered file path: '", rendered_path, "'"
        )

    writeLines(rendered, rendered_path)

    invisible(outdir)
}

.get_pkg_type <- function(package) {
    bv <- utils::packageDescription(pkg = package)[["biocViews"]]
    terms <- unlist(strsplit(bv, ",\\s+|\n"))
    biocViews::guessPackageType(terms)
}

.get_pkg_data <- function(packages, gh_org, since_date) {
    .arePkgsInstalled(packages)
    types <- vapply(packages, .get_pkg_type, character(1L))

    data.frame(
        package = packages, pkgType = types,
        org = gh_org, sinceDate = since_date,
        row.names = NULL
    )
}
