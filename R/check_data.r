#' Check primary keys are unique
#'
#' @param dt a data.table, for which to check the primary key values.
#' @param colnames a character vector, giving names of columns in the primary
#'   key.
#'
#' @return NULL, if no duplicates found.
#' @export
check_primary_keys_unique <- function(
  dt,
  colnames
) {
  if (length(colnames) == 0)
    stop("colnames cannot be length zero")
  stop_if_nonempty(
    setdiff(colnames, colnames(dt)),
    "columns not in dt"
  )
  dup <- duplicated(dt, by = colnames)
  stop_if_nonempty(
    unique(dt[dup, ..colnames]),
    "there are duplicated primary keys"
  )
}

#' Check foreign key values are in reference columns
#'
#' @param dt a data.table, for which to check foreign key values.
#' @param ref a data.table, which includes the reference columns to check
#'   against.
#' @param keys a character vector, giving names of foreign key columns in
#'   \code{dt}.
#' @param ref_keys a character vector, giving names of reference columns in
#'   \code{ref}. These should be in the same order as the foreign key columns
#'   they are references for. Defaults to \code{keys}.
#' @param optional a logical, or logical vector, indicating whether foreign key
#'   values can be missing. Note that this is different from the reference
#'   columns being nullable. The length must be one, or equal to the length of
#'   \code{keys}. If length one, the single logical is applied to all keys.
#'   Defaults to FALSE.
#'
#' @return NULL, if no reference errors found.
#' @export
check_foreign_keys <- function(
  dt,
  ref,
  keys,
  ref_keys = keys,
  optional = FALSE
) {
  if (length(keys) == 0)
    stop("require at least one key")
  if (length(keys) != length(ref_keys))
    stop("keys and ref_keys must be same length")
  if (length(optional) == 1)
    optional <- rep(optional, length(keys))
  if (length(optional) != length(keys))
    stop("optional must be length one or same length as keys")
  stop_if_nonempty(
    setdiff(keys, colnames(dt)),
    "foreign key columns not found in dt"
  )
  stop_if_nonempty(
    setdiff(ref_keys, colnames(ref)),
    "reference key columns not found in ref"
  )
  value_miss <- stats::setNames(
    Map(
      function(key, ref_key, optional) {
        setdiff(
          dt[[key]],
          c(
            ref[[ref_key]],
            if (optional) NA
          )
        )
      },
      keys,
      ref_keys,
      optional
    ),
    keys
  )
  stop_if_nonempty(
    remove_empty(value_miss),
    "foreign key values not found in reference columns"
  )
}

#' Check for missing entries in non-nullable columns
#'
#' @param dt a data.table, for which to check for missing entries.
#' @param optional a character vector, containing names of nullable columns in
#'   \code{dt}. These columns are not checked.
#'
#' @return NULL, if no missing non-nullable entries are found.
#' @export
check_no_required_values_missing <- function(
  dt,
  optional = character()
) {
  missing <- lapply(dt[, -..optional], function(x) which(is.na(x)))
  stop_if_nonempty(
    remove_empty(missing),
    "there are missing required values in the following rows"
  )
}

remove_empty <- function(
  lst
) {
  lst[vapply(lst, length, integer(1)) > 0]
}

toString.list <- function(lst, ...) {
  paste(
    names(lst),
    vapply(lst, toString, character(1)),
    sep = ": ",
    collapse = "\n"
  )
}

toString.data.frame <- function(dt, ...) {
  paste(
    apply(dt, 1L, toString.list),
    collapse = "\n\n"
  )
}

stop_if_nonempty <- function(
  x,
  err
) {
  UseMethod("stop_if_nonempty")
}

stop_if_nonempty.default <- function(
  x,
  err
) {
  if (length(x) > 0)
    stop(paste(err, toString(x), sep = ": "))
}

stop_if_nonempty.list <- function(
  x,
  err
) {
  if (length(x) > 0)
    stop(paste(err, toString.list(x), sep = ":\n"))
}

stop_if_nonempty.data.frame <- function(
  x,
  err
) {
  if (nrow(x) > 0)
    stop(paste(err, toString.data.frame(x), sep = ":\n"))
}
