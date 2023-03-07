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

#' @keywords internal
#' @export
typeEnglish <- function(package_type) {
    if (endsWith(package_type, "s"))
        package_type <-
            substr(package_type, nchar(package_type), nchar(package_type))
    gsub("-", " ", package_type)
}
