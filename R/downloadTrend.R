#' Plot the download activity for a package
#'
#' @param include_recent `logical(1)` Whether to include the latest month of
#'   download data. Usually, the data for the most recent month has incomplete
#'   numbers and is excluded (default is `FALSE`).
#'
#' @inheritParams generateReport
#'
#' @examples
#'
#' downloadTrend("MultiAssayExperiment", "2017-05-01")
#' downloadTrend("cBioPortalData", "2017-05-01")
#' downloadTrend("RaggedExperiment", "2017-05-01")
#'
#'
#' downloadTrend("SingleCellMultiModal", "2017-05-01")
#' downloadTrend("curatedTCGAData", "2017-05-01")
#'
#' # low downloads
#' downloadTrend("TENxIO", "2017-05-01")
#' downloadTrend("terraTCGAdata", "2017-05-01")
#'
#' @export
downloadTrend <- function(package, since_date, include_recent = FALSE) {
    if (requireNamespace("lubridate", quietly = TRUE))
        syear <- lubridate::year(lubridate::as_date(since_date))
    else
        syear <- format(as.Date.character(since_date), "%Y")
    now <- Sys.time()
    this_year <- format(now, "%Y")
    this_month <- format(now, "%b")
    ## Average Downloads
    type <- .get_pkg_type(package)
    pkgType <- typeTranslate(type)
    dls <- BiocPkgTools::pkgDownloadStats(
        package, pkgType = pkgType, years = syear:now
    )
    if (!include_recent)
        dls <- dls[!(dls$Year == this_year & dls$Month == this_month), ]
    x <- seq(nrow(dls))
    y <- dls[["Nb_of_distinct_IPs"]]
    plot(
        x, y, type = "l", xaxt = "n",
        xlab = "", ylab = "No. downloads (distinct IP)",
        main = paste0(package, ": Download Activity")
    )
    ## basic selector of months
    vals <- x[c(TRUE, FALSE)]
    tickmo <- dls[["Month"]][c(TRUE, FALSE)]
    mtext(tickmo, side = 1, line = 1, at = vals, las = 2, cex = 0.8)
    pos <- tapply(x, dls[["Year"]], median)
    mtext(text = names(pos), side = 1, at = pos, line = 2.5)
}
