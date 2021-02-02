check_primary_keys_unique <- function(
  dt,
  colnames
) {
  if (length(colnames) == 0)
    stop("colnames cannot be length zero")
  if (any(!is.element(colnames, colnames(dt))))
    stop("colnames must exist in dt")
  primary <- dt[, ..colnames]
  dup <- duplicated(primary)
  if (any(dup)) {
    dups <- unique(primary[dup])
    error_list <- paste(
      apply(
        dups,
        1L,
        function(y) paste(colnames, y, sep = ": ", collapse = "\n")
      ),
      collapse = "\n\n"
    )
    stop(paste0("there are duplicated primary keys:\n", error_list))
  }
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
  dt_miss <- setdiff(keys, colnames(dt))
  if (length(dt_miss) > 0)
    stop(paste("there are keys not in dt:", paste(dt_miss, collapse = ", ")))
  ref_miss <- setdiff(ref_keys, colnames(ref))
  if (length(ref_miss) > 0)
    stop(paste("there are keys not in ref:", paste(ref_miss, collapse = ", ")))
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
  value_miss <- value_miss[vapply(value_miss, length, integer(1)) > 0L]
  if (length(unlist(value_miss)) > 0)
    stop(paste0(
      "there are values not in ref:\n",
      paste(
        names(value_miss),
        vapply(value_miss, paste, character(1), collapse = ", "),
        sep = ": ",
        collapse = "\n"
      )
    ))
}

check_no_required_values_missing <- function(
  dt,
  optional = character()
) {
  missing <- lapply(dt[, -..optional], function(x) which(is.na(x)))
  stripped_missing <- missing[vapply(missing, length, integer(1)) > 0L]
  if (length((stripped_missing)) > 0)
    stop(paste0(
      "there are missing required values in the following rows:\n",
      paste(
        names(stripped_missing),
        vapply(stripped_missing, paste, character(1), collapse = ", "),
        sep = ": ",
        collapse = "\n"
      )
    ))
}
