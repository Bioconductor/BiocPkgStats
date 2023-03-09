.SUPPORT_SITE_BASE_URL <- "https://support.bioconductor.org/api/stats/date/"

#' Gather support site statistics for a given period
#'
#' @param from `character(1)` date in ISO 8601 format ("YYYY-MM-DD")
#' @param to `character(1)` date in ISO 8601 format ("YYYY-MM-DD")
#'
#' @return a `bioc_support_stats` list class with components userdiff,
#'   toplevdiff, questdiff, and respdiff
#'
#' @author Vincent J. Carey
#'
#' @examples
#'
#' supportSiteStats()
#'
#' @export
supportSiteStats <-
    function(from = "2021-01-01", to = "2021-12-31")
{
    base <- .SUPPORT_SITE_BASE_URL
    from <- paste0(gsub("-", "/", from), "/")
    to <- paste0(gsub("-", "/", to), "/")
    stat0 <- httr::GET(paste0(base, from)) |> httr::content()
    stat1 <- httr::GET(paste0(base, to)) |> httr::content()

    snames <- c("usediff", "toplevel", "questions", "respdiff", "from", "to")
    stats <- structure(
        vector("list", length = length(snames)),
        .Names = snames
    )
    stats[["userdiff"]] <- stat1$users - stat0$users
    stats[["toplevdiff"]] <- stat1$toplevel - stat0$toplevel
    stats[["questdiff"]] <- stat1$questions - stat0$questions
    stats[["respdiff"]] <-
        (stat1$answers + stat1$comments) - (stat0$answers + stat0$comments)
    stats[["from"]] <- format(as.Date(substr(from, 1, 10)), "%B %d, %Y")
    stats[["to"]] <- format(as.Date(substr(to, 1, 10)), "%B %d, %Y")
    class(stats) <- c("bioc_support_stats", "list")
    stats
}

#' @rdname supportSiteStats
#' @export
print.bioc_support_stats = function(x, ...) {
    cat("Bioconductor support site usage increments from\n")
    cat(sprintf("  %s to %s\n", x$from, x$to))
    cat(sprintf("    Users added: %d\n", x$userdiff))
    cat(sprintf("    Top-level posts added: %d\n", x$toplevdiff))
    cat(sprintf("    Questions added: %d\n", x$questdiff))
    cat(sprintf("    Answers/comments added: %d\n", x$respdiff))
}

