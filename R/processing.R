
################################################################################
# General tools for processing data
################################################################################

# preprocessing-wrapper --------------------------------------------------------
################################################################################
# Function to wrap all the preprocesssing functions below
################################################################################

#' @title Merge processing options
#' @export
merge_process_opts <- function(opts = list()) {
  default_opts <- list()
  default_opts$sanitize_colnames <- TRUE
  default_opts$sort_rows <- TRUE
  default_opts$convert_class <- TRUE
  default_opts$classes_df <- data.frame(name = colnames(X),
                                        class = rep("character", ncol(X)))
  default_opts$blanks_to_nas <- TRUE
  default_opts$remove_constant_cols <- TRUE
  default_opts$levels_to_int <- TRUE
  default_opts$clean_ages <- FALSE
  default_opts$clean_dates <- FALSE
  modifyList(default_opts, opts)
}

#' @title Wrapper to preprocess raw data tables
#' @param opts A list with the following options:
#' @importFrom magrittr %>%
#' @importFrom dplyr arrange_
#' @importFrom plyr colwise
#' @importFrom data.table data.table is.data.table
#' @export
preprocess_data <- function(X, opts = list()) {
  opts <- merge_process_opts(opts)
  is_dt <- is.data.table(X)
  X <- as.data.table(X)

  # convert columns to hand-labeled classes
  if(opts$convert_class) {
    X <- convert_classes(X, opts$classes_df)
  }

  # sort rows by the first column
  if(opts$sort_rows) {
    X <- X[order(X[, 1, with = F]), ]
  }

  # convert blank factor values to NA
  if(opts$blanks_to_nas) {
    X <- as.data.table(colwise(blanks_to_nas)(X))
  }

  # recode factor names as integer factors
  if(opts$levels_to_int) {
    not_id <- !grepl("id", tolower(colnames(X)))
    recode_result <- levels_to_int(X[, not_id, with = F])
    X <- data.table(X[, !not_id, with = F], recode_result$X)
    levels_recode <- recode_result$levels
  } else {
    levels_recode <- NULL
  }

  # remove constant columns
  if(opts$remove_constant_cols) {
    X <- remove_constant_cols(X)
  }

  # clean age field
  if(opts$clean_ages) {
    X <- clean_ages(X)
  }

  # clean date field
  if(opts$clean_dates) {
    X <- clean_dates(X)
  }

  # sanitize names last, to make sure previous preprocessing works
  if(opts$sanitize_colnames) {
    X <- sanitize_colnames(X)
  }

  if(!is_dt) X <- data.frame(X)
  list(X = X, levels_recode = levels_recode)
}

# generic-cleaning -------------------------------------------------------------
################################################################################
# Functions that can be used to clean generic data sets.
################################################################################

#' @title Functional to get type conversion functions
#' @param type A string specifying the function to retrieve
#' @param date_formats A vector of date formatting types to try when convert
#' dates to POSIXlt class. If none provided, defaults to
#' c("%Y-%m-%d %H:%M:%S", "%m/%d/%Y", "%Y-%m-%d")
#' @return convert_fun A function that can be used to convert types.
#' @examples
#' get_convert_fun("numeric")
get_convert_fun <- function(type, date_formats = NULL) {
  if(type != "POSIXct") {
    convert_fun <- get(paste0("as.", type))
  } else {
    if(is.null(date_formats)) {
      date_formats <- c("%Y-%m-%d %H:%M:%S", "%m/%d/%Y", "%Y-%m-%d")
    }
    convert_fun <- function(x) {
      for (cur_format in date_formats) {
        test_convert <- try(as.POSIXct(x, format = cur_format))
        if (!(all(is.na(test_convert)))) {
          return(test_convert)
        }
      }
      stop("None of the provided date formats are applicable")
    }
  }
  convert_fun
}

#' @title Convert the columns in a data.table to specified types
#' @description Given a data.frame with incorrect types, convert them to
#' the types in a prespecified classes_df data frame.
#' @param X The data.frame or data.table whose columns we need to change classes
#' of.
#' @param classes_df A data.frame with two columns, "name" and "class",
#' specifying the desired class for each column name in X.
#' @param date_formats A vector of date formatting types to try when convert
#' dates to POSIXlt class.
#' @return X The version of X with classes as specified in classes_df.
#' @examples
#' classes_df <- data.frame(name = c("A", "B", "C"), class = c("numeric", "factor", "character"))
#' X <- data.table(cbind(A = c("1", "121", "31", "132"), B = c("U", "U", "V", "W"), C = c(14, 12, 1, 1))) # starts all character
#' convert_classes(X, classes_df)
#' @importFrom data.table data.table
#' @importFrom plyr colwise
#' @importFrom dplyr filter_ select
#' @importFrom magrittr %>%
#' @export
convert_classes <- function(X, classes_df, date_formats = NULL) {
  is_dt <- is.data.table(X)
  X <- data.table(X)
  for(type in unique(classes_df$class)) {
    # get current group of columns to convert
    filt_ix <- paste0("class == '", type, "'")
    cur_cols <- filter_(classes_df, filt_ix) %>%
      select(name) %>%
      unlist()
    cur_cols_ix <- which(colnames(X) %in% cur_cols)

    # convert the columns
    convert_fun <- get_convert_fun(type, date_formats)
    X[, cur_cols_ix] <- colwise(convert_fun)(X[, cur_cols_ix, with = F])
  }

  # return in original form
  if(!is_dt) X <- data.frame(X)
  X
}

#' @title Convert long factor names to integers
#' @param X The data.table or data.frame whose factors we want to convert
#' to integer factors.
#' @return A list containing the following elements, \cr
#'    $X The original data, but with factor levels recoded to integer factors. \cr
#'    $levels_code A data.frame describing the mapping between original factor
#'     levels and new integer factor levels.
#' @export
levels_to_int <- function(X) {
  levels_code <- list()
  is_dt <- is.data.table(X)
  X <- data.table(X)
  factor_ix <- which(sapply(X, is.factor))

  # loop over factor columns, converting to integer factors
  for (j in factor_ix) {
    old_x <- unlist(X[, j, with = F])
    new_x <- as.factor(as.numeric(old_x))
    X[, j] <- new_x

    # could be all NAs
    if (nlevels(new_x) > 0) {
      levels_code[[colnames(X)[j]]] <- data.table(factor_name = colnames(X)[j],
                                                  original_name = levels(old_x),
                                                  new_code = levels(new_x))
    }
  }

  # return in the form provided
  if(!is_dt) X <- data.frame(X)
  list(X = X, levels_code = do.call(rbind, levels_code))
}

#' @title Convert factor levels == "" to NA
#' @description It's probably better to directly use the na.strings argument in
#' read.csv or fread
#' @param x A factor vector to relevel in the specified way.
#' @examples
#' x = factor(c("A", "A", "B", "", "A", "B"))
#' blanks_to_nas(x)
#' @export
blanks_to_nas <- function(x) {
  if(is.factor(x)) {
    x[x == ""] <- NA
    x <- droplevels(x)
  }
  x
}

#' @title Remove columns of X that are at least some proportion NA
#' @param X A data.table or data.frame whose all NA columns we want to remove
#' @param na_thresh The proportion of elements in the column that have to be NA
#' before we remove it. Defaults to 1.
#' @importFrom data.table data.table
#' @export
remove_na_cols <- function(X, na_thresh = 1) {
  all_na_cols <- sapply(X, function(x) sum(is.na(x)) / length(x) >= na_thresh)
  cat(sprintf("Removing columns: %s \n", paste0(colnames(X)[all_na_cols], collapse = ", ")))
  X[, !all_na_cols]
}

#' @title Remove columns that all one number / level / all NA
#' @param X A data.table or data.frame whose constant columns we want to remove.
#' @return X_trim A version of X without the constant columns.
#' @export
remove_constant_cols <- function(X) {
  is_constant <- function(x) all(x == na.omit(x)[1], na.rm = T)
  constant_ix <- sapply(X, is_constant)
  if(is.data.table(X)) {
    X_trim <- X[, as.logical(!constant_ix), with = F]
  } else {
    X_trim <- X[, as.logical(!constant_ix)]
  }
  X_trim
}

#' @title Sanitize character vector
#' @description Removes punctuation, converts to lower case, replaces spaces
#' with _
#' @importFrom stringr str_trim
#' @export
sanitize_names <- function(x) {
  x <- make.names(gsub("-|'", "", x))
  tolower(gsub(" ", "_", str_trim(gsub("\\.+", " ", x))))
}

#' @title Sanitize column names of a matrix
#' @description Wraps sanitize_names().
#' @export
sanitize_colnames <- function(X) {
  colnames(X) <- sanitize_names(colnames(X))
  X
}

#' @title Remove negative ages
#' @description This only happens in one sample, but it is annoying.
clean_ages <- function(X) {
  if("Age" %in% colnames(X)) {
    X$Age[X$Age < 0] <- NA
  }
  if("age" %in% colnames(X)) {
    X$age[X$age < 0] <- NA
  }
  X
}

#' @title Clean date columns. Right now only caps dates at tomorrow's date.
#' @importFrom data.table data.table
#' @importFrom plyr colwise
clean_dates <- function(X) {
  cap_date <- function(x) {
    if("POSIXct" %in% class(x)) {
      x[x > as.POSIXct(Sys.Date() + 1, format = "%m/%d/%Y")] <- NA
    }
    x
  }
  data.table(colwise(cap_date)(X))
}

# misc-funs --------------------------------------------------------------------
################################################################################
# Miscellaneous functions used in the preprocessing step
################################################################################

#' @title Wrapper for sum() that gives NA when all elements are 0
#' @description We don't want the sums of NAs to be 0.
#' @export
#' @examples
#' sum_na(c(NA, NA, NA))
#' sum_na(c(0, 1, NA))
#' sum(c(NA, NA, NA), na.rm = T)
#' sum(c(0, 1, NA), na.rm = T)
sum_na <- function(x) {
  if(all(is.na(x))) {
    return(as.numeric(NA))
  }
  sum(x, na.rm = T)
}
