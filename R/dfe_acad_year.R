#' @title Change the style of academic year
#'
#' @description The \code{dfe_acad_year} function converts academic year numbers e.g. 201213 into strings with a forwward slash "2012/13"
#'
#'
#'
#' @details The registers are sorted by publication date and name by
#'  alphabetical order. The top five registers are output as a character string
#'  including commas and an and, for inclusion in the report.
#'
#' @param year is the acadmic year we'll input, e.g. 201213 which will be converted into 2012/13
#'
#' @return Returns a character string
#'
#' @examples
#'
#' library(dferap)
#' dfe_acad_year(201213)
#' "2012/13"
#'
#'
#' @export
#'

dfe_acad_year <- function(year) {

    if (!grepl("^[0-9]{6,6}$",year))
      stop("year parameter must be a six didgit number e.g. 201617")

    sub("(.{4})(.*)", "\\1/\\2", year)

}
