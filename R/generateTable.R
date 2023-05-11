generateTable <- function(packages, gh_org, since_date) {
    stopifnot(
        BiocBaseUtils::isCharacter(packages),
        BiocBaseUtils::isCharacter(gh_org),
    )
    if (!identical(length(gh_org), length(packages)) &&
        !identical(length(gh_org), 1L))
        stop("'gh_org' should be length one or the same length as 'packages'")

    pkgdata <- .get_pkg_data(pacakges, gh_org, since_date)
    apply(pkgdata, 1L, .get_stats)
    # TODO: compile stats from stats_report
    gh_repo <- paste(gh_org, package, sep = "/")
    pkgType <- typeTranslate(pkgType)
    packageType <- typeEnglish(pkgType)
}

## Release Download Rank
.rel_dl_rank <- function(since_date, version) {
    since_date <- as_date(since_date)
    smonth <- lubridate::month(since_date, abbr = FALSE, label = TRUE)
    syear <- lubridate::year(since_date)
    dlrank <- pkgDownloadRank(pkg = package, pkgType = pkgType)
    round(dlrank, 0)
}
