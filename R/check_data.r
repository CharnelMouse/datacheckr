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
  stop_if_nonempty(
    setdiff(keys, colnames(dt)),
    "there are keys not in dt"
  )
  stop_if_nonempty(
    setdiff(ref_keys, colnames(ref)),
    "there are keys not in ref"
  )
  value_miss <- stats::setNames(
    Map(
      function(key, ref_key) {
        setdiff(
          dt[[key]],
          c(
            ref[[ref_key]],
            if (optional) NA
          )
        )
      },
      keys,
      ref_keys
    ),
    keys
  )
  stop_if_nonempty(
    remove_empty(value_miss),
    "there are values not in ref"
  )
}

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
