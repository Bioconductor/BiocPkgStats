#' Create a table of package metrics
#'
#' @description The function compiles statistics for Bioconductor packages that
#'   have GitHub repositories. It uses functionality in `BiocPkgTools` to
#'   extract commit and issue history. A token is required to access the
#'   GitHub commit and issue history. See the `.token` argument in `?gh::gh`
#'   for details on its use.
#'
#' @details
#' Note that pull requests are a type of "issue" on GitHub; therefore, issues
#' and pull requests are combined when reporting the number of closed issues
#' since a specific date. For more information on how the queries are preformed,
#' see the GitHub API documentation for the `/repos/{owner}/{repo}/issues`
#' endpoint at \url{https://docs.github.com/en/rest/issues} and the `gh` R
#' package. The following list describes each column in the returned
#' `data.frame`:
#'
#' * download.rank: The percentile rank of the package in its respective
#'   repository, e.g., software, data-experiment etc.
#' * avg.downloads: The average number of monthly downloads from distinct IP
#'   addresses since the given date
#' * med.downloads: The median number of montly downloads from distinct IP
#'   addresses since the given date
#' * num.revdeps: The number of "all" reverse dependencies including "Depends",
#'   "Imports", "LinkingTo", "Suggests", and "Enhances"
#' * issues.since: The number of closed issues (including pull requests) since
#'   the date given
#' * commits.since: The number of commits since the given date
#'
#' @return A `data.frame` of metrics, see the `details` section for specifics
#'
#' @inheritParams generateReport
#'
#' @examples
#' if (interactive()) {
#'
#' generateTable(
#'     packages = c(
#'         "MultiAssayExperiment", "cBioPortalData", "SingleCellMultiModal"
#'     ),
#'     gh_org = "waldronlab",
#'     since_date = "2019-05-01"
#' )
#'
#' }
#' @export
generateTable <- function(packages, gh_org, since_date) {
    stopifnot(
        BiocBaseUtils::isCharacter(packages),
        BiocBaseUtils::isCharacter(gh_org)
    )
    if (!identical(length(gh_org), length(packages)) &&
        !identical(length(gh_org), 1L))
        stop("'gh_org' should be length one or the same length as 'packages'")

    pkgdata <- .get_pkg_data(packages, gh_org, since_date)
    allrows <- apply(pkgdata, 1L, function(pkgrow) {
        structure(list(
            .get_stats(
                package = pkgrow[['package']],
                pkgType = pkgrow[['pkgType']],
                since_date = pkgrow[['sinceDate']],
                gh_org = pkgrow[["org"]]
            )
        ), .Names = pkgrow[['package']])
    })
    allrows <- unlist(allrows, recursive = FALSE)
    stats_res <- do.call(rbind, allrows)
    cbind(stats_res, since.date = since_date)
}

.get_stats <- function(package, pkgType, since_date, gh_org) {
    gh_repo <- paste(gh_org, package, sep = "/")
    pkgType <- typeTranslate(pkgType)
    dls <- .avg_median_dls(package, pkgType, since_date)
    data.frame(
        download.rank = .rel_dl_rank(package, pkgType, since_date),
        avg.downloads = dls['avg'],
        med.downloads = dls['med'],
        num.revdeps = .num_revdeps(package),
        issues.since = .activity_since(gh_repo, since_date, "issues"),
        commits.since = .activity_since(gh_repo, since_date, "commits")
    )
}

## Release Download Rank
.rel_dl_rank <- function(package, pkgType, since_date) {
    since_date <- lubridate::as_date(since_date)
    smonth <- lubridate::month(since_date, abbr = FALSE, label = TRUE)
    syear <- lubridate::year(since_date)
    dlrank <- BiocPkgTools::pkgDownloadRank(pkg = package, pkgType = pkgType)
    round(dlrank, 0)
}

.avg_median_dls <- function(package, pkgType, since_date) {
    now <- lubridate::year(Sys.time())
    syear <- lubridate::year(since_date)
    ## Average Downloads
    dls <- BiocPkgTools::pkgDownloadStats(
        package, pkgType = pkgType, years = syear:now
    )
    avgdls <- mean(dls[["Nb_of_distinct_IPs"]])
    meddls <- median(dls[["Nb_of_distinct_IPs"]])
    c(avg = round(avgdls, 0), med = round(meddls, 0))
}

.num_revdeps <- function(package) {
    ## Number of reverse dependencies
    db <- utils::available.packages(repos = BiocManager::repositories())
    revdeps <- tools::package_dependencies(
        packages = package, db = db, reverse = TRUE, which = "all"
    )[[1]]
    length(revdeps)
}

.activity_since <- function(gh_repo, since_date, activity) {
    ## Activity since date: either "issues" or "commits"
    suppressMessages({
        activity <- BiocPkgTools::activitySince(
            gh_repo,
            activity,
            "closed",
            since_date
        )
    })
    nrow(activity)
}
