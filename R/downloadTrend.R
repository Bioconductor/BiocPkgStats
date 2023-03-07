#' Plot the download activity for a package
#'
#' @inheritParams generateReport
#'
#' @examples
#'
#' downloadTrend("MultiAssayExperiment", "2017-05-01")
#' downloadTrend("cBioPortalData", "2017-05-01")
#' downloadTrend("RaggedExperiment", "2017-05-01")
#'
#' downloadTrend("SingleCellMultiModal", "2017-05-01")
#'
#' @export
downloadTrend <- function(package, since_date) {
    if (requireNamespace("lubridate", quietly = TRUE))
        syear <- lubridate::year(lubridate::as_date(since_date))
    else
        syear <- format(as.Date.character("2021-01-02"), "%Y")
    now <- format(Sys.time(), "%Y")
    ## Average Downloads
    type <- .get_pkg_type(package)
    pkgType <- typeTranslate(type)
    dls <- BiocPkgTools::pkgDownloadStats(
        package, pkgType = pkgType, years = syear:now
    )
    x <- seq(nrow(dls))
    y <- dls[["Nb_of_distinct_IPs"]]
    plot(
        x, y, type = "l", xaxt = "n",
        xlab = "", ylab = "No. downloads (distinct IP)",
        main = paste0(package, ": Download Activity")
    )
    vals <- x[c(TRUE, FALSE)]
    tickmo <- dls[["Month"]][c(TRUE, FALSE)]
    mtext(tickmo, side = 1, line = 1, at = vals, las = 2, cex = 0.8)
    pos <- tapply(x, dls[["Year"]], median)
    mtext(text = names(pos), side = 1, at = pos, line = 2.5)
}
