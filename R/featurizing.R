
################################################################################
# Utilities for extracting features from the original data
################################################################################

#' @title Get prefixes up to a character in a vector of strings
#' @importFrom stringr str_extract
#' @export
#' @examples
#' get_prefix("this_is_a&test", "&")
get_prefix <- function(x, stop_char) {
  str_extract(x, paste0("[^", stop_char, "]+"))
}

#' @title Get features associated with a POSIXct date
#' @importFrom data.table data.table month year yday mday hour
#' @export
#' @examples
#' Sys.Date() %>%
#'   as.POSIXct() %>%
#'   get_date_features()
get_date_features <- function(x) {
  stopifnot("POSIXt" %in% class(x))
  data.table(year = year(x), month = month(x), day_of_year = yday(x),
             day_of_month = mday(x), day_of_week = weekdays(x),
             hour = hour(x))
}
