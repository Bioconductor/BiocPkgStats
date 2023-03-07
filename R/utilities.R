#' @name BiocPkgStats-helpers
#'
#' @title Internal helpers to translate values to text
#'
#' @param type_term `character(1)` A term to be translated obtained from
#'   `biocViews::guessPackageType()`
#'
#' @param packageType `character(1)` The term given by `typeTranslate` and
#'   often used throughout the project, e.g., "software", "data-experiment",
#'   etc.
#'
#' @description `typeTranslate` will take a package's `biocViews` terms and run
#'   them through `biocViews::guessPackageType()` to obtain the package type
#'   value. `typeEnglish` will take the package type value and convert it
#'   into standard English text.
#'
#' @keywords internal
#' @export
typeTranslate <- function(type_term) {
    # integrate with BiocPkgTools:::repo_short_names?
    result <- switch(
        type_term,
        Software = "software",
        ExperimentData = "data-experiment",
        AnnotationData = "data-annotation",
        Workflow = "workflows",
        NULL
    )
    if (is.null(result))
        stop("Term could not be matched")
    result
}

#' @rdname BiocPkgStats-helpers
#'
#' @keywords internal
#' @export
typeEnglish <- function(package_type) {
    if (endsWith(package_type, "s"))
        package_type <-
            substr(package_type, nchar(package_type), nchar(package_type))
    gsub("-", " ", package_type)
}
