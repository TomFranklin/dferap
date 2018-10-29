#' @title minimal tidy data set for regregrap report production.
#'
#' @description \code{check_consistency_absence_one_term} is the class used for the creation of all
#'  figures and tables in the made up RAP report (it's a demo for the RAP MOOC).
#'
#'
#' @details The \code{check_consistency_absence_one_term} class expects a \code{data.frame} with at
#' least four columns: term, level, year, sess_overall.
#'
#'   Once inititated, the class has five slots: \code{df}: the basic
#'   \code{data.frame}, \code{colnames}: a character vector containing the
#'   column names from the \code{df},
#'
#' @param x Input dataframe, see details.
#' @param log_level The severity level at which log messages are written from
#' least to most serious: TRACE, DEBUG, INFO, WARN, ERROR, FATAL. Default is
#' level is INFO. See \code{?flog.threshold()} for additional details.
#' @param log_appender Defaults to write the log to "console", alternatively you
#' can provide a character string to specify a filename to also write to. See
#' for additional details \code{?futile.logger::appender.tee()}.
#'
#' @return If the class is not instantiated correctly, nothing is returned.
#'
#' @examples
#'
#' library(dferap)
#'
#' df <- check_consistency_absence_one_term(absence_one_term_data)$df
#'
#' @export

check_consistency_absence_one_term <- function(x, log_level = futile.logger::WARN,
                                               log_appender = "console") {

  # Set logger severity threshold, defaults to
  # high level use (only flags warnings and errors)
  # Set log_level argument to futile.logger::TRACE for full info
  futile.logger::flog.threshold(log_level)

  # Set where to write the log to
  if (log_appender != "console")
  {
    # if not console then to a file called...
    futile.logger::flog.appender(futile.logger::appender.file(log_appender))
  }

  # Checks
  futile.logger::flog.info('Initiating check_consistency_absence_one_term class.
                           \n\nExpects a data.frame with at
                           least four columns: term, level, year, sess_overall.
                           More information on the format expected by
                           this class is given by ?check_consistency_absence_one_term().')

  # Integrity checks on incoming data ----

  # Check the structure of the data is as expected: data.frame containing no
  # missing values and at least three columns, containing phase, date and register name.

  futile.logger::flog.info('\n*** Running integrity checks on input dataframe (x):')
  futile.logger::flog.debug('\nChecking input is properly formatted...')

  futile.logger::flog.debug('Checking x is a data.frame...')
  if (!is.data.frame(x))
  {
    futile.logger::flog.error("x must be a data.frame",
                              x, capture = TRUE)
  }

  futile.logger::flog.debug('Checking x has correct columns...')
  if (length(colnames(x)) < 3)
  {
    futile.logger::flog.error("x must have at least four columns: term, level, year, sess_overall")
  }

  futile.logger::flog.debug('Checking x contains a date column...')
  if (!'term' %in% colnames(x)) stop("x must contain term column")

  futile.logger::flog.debug('Checking x contains a phase column...')
  if (!'level' %in% colnames(x)) stop("x must contain level column")

  futile.logger::flog.debug('Checking x contains a date column...')
  if (!'year' %in% colnames(x)) stop("x must contain year column")

  futile.logger::flog.debug('Checking x contains a phase column...')
  if (!'sess_overall' %in% colnames(x)) stop("x must contain sess_overall column")

  futile.logger::flog.debug('Checking for the correct number of rows...')
  if (nrow(x) < 10) {
    futile.logger::flog.warn("x does not appear to be well formed. nrow(x) should be
                             greater than 10 as of early 2018.")
  }



  futile.logger::flog.info('...passed')

  # User assertr to run statistical tests on the data itself ----

  futile.logger::flog.info("\n***Running statistical checks on input dataframe (x)")

  futile.logger::flog.trace("These tests are implemented using the package assertr see:
                            https://cran.r-project.org/web/packages/assertr for more details.")



  # Check that the correct levels are in phase


  # Reset threshold to package default
  futile.logger::flog.threshold(futile.logger::INFO)
  # Reset so that log is appended to console (the package default)
  futile.logger::flog.appender(futile.logger::appender.console())

  # Message required to pass a test
  message("Checks completed successfully:
          object of 'check_consistency_absence_one_term' class produced!")


  # Drop unnecessary columns
  x <- x[, c("term", "level", "year", "sess_overall")]
  # Define the class here ----

  structure(
    list(
      df = x,
      colnames = colnames(x),
      type = colnames(x)[!colnames(x) %in% c("term", "level", "year", "sess_overall")]
    ),
    class = "check_consistency_absence_one_term")
  }
