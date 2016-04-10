
################################################################################
# Utilities for reshaping data
################################################################################

#' @title Wrapper for casting tables from long to wide
#' @param X The data set to cast
#' @param row_var The variable whose unique values will determine the rows.
#' @param col_var The variable whose unique values will determine the columns.
#' @param value_var The variable to use to fill in the cells of the wide matrix.
#' @param aggregate_fun The name of a function to use to summarize many elements
#' with the same row_var / col_var identification. Must be one of
#' @return The wide version of X.
#' @importFrom data.table data.table dcast.data.table
#' @export
cast_wrapper <- function(X, row_var, col_var, value_var = NULL,
                         aggregate_fun = "sum") {
  aggregate_fun <- match.arg(aggregate_fun, c("mean", "count", "sum", "max",
                                              "first", "sum_na", "mean_na",
                                              "max_na"))

  # convert character value vars to factors before casting
  X <- data.table(X)
  y <- X[, value_var, with = F]
  if(is.character(y)) {
    X[, value_var] <- as.factor(y)
  }

  # setup for cast
  cast_fmla <- formula(paste0(row_var, "~", col_var))
  if(is.null(value_var)) {
    library("data.table")
    value_var <- data.table:::guess(X)
  }

  # non-standard evaluation...can't just pass in name
  switch(aggregate_fun,
         "mean" = dcast.data.table(X, cast_fmla, value.var = value_var, fun.aggregate = mean),
         "count" = dcast.data.table(X, cast_fmla, value.var = value_var, fun.aggregate = length),
         "sum" = dcast.data.table(X, cast_fmla, value.var = value_var, fun.aggregate = sum),
         "max" = dcast.data.table(X, cast_fmla, value.var = value_var, fun.aggregate = max),
         "first" = dcast.data.table(X, cast_fmla, value.var = value_var, fun.aggregate = function(x) x[1]),
         "sum_na" = dcast.data.table(X, cast_fmla, value.var = value_var, fun.aggregate = sum_na),
         "mean_na" = dcast.data.table(X, cast_fmla, value.var = value_var, fun.aggregate = function(x) mean(x, na.rm = T)),
         "max_na" = dcast.data.table(X, cast_fmla, value.var = value_var, fun.aggregate = function(x) max(x, na.rm = T)))
}
