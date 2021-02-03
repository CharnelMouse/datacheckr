describe("check_primary_keys_unique()", {
  it("expects at least one column name for primary key", {
    expect_exerr(
      check_primary_keys_unique(data.table(a = integer()), character()),
      "colnames cannot be length zero"
    )
  })
  it("expects colnames to be in dt", {
    expect_exerr(
      check_primary_keys_unique(data.table(a = integer()), c("a", "b")),
      "columns not in dt: b"
    )
  })
  it("returns NULL (pass) if dt has no rows", {
    expect_null(check_primary_keys_unique(data.table(a = integer()), "a"))
  })
  it("returns NULL (pass) if dt has no rows where primary key is duplicated", {
    expect_null(check_primary_keys_unique(data.table(a = 1L), "a"))
    expect_null(check_primary_keys_unique(data.table(a = 1:2, b = 1L), "a"))
  })
  it("returns duplicated keys if any in error message", {
    expect_exerr(
      check_primary_keys_unique(data.table(a = rep(1L, 2L)), "a"),
      "there are duplicated primary keys:\na: 1"
    )
    expect_exerr(
      check_primary_keys_unique(data.table(a = rep(1L, 3L)), "a"),
      "there are duplicated primary keys:\na: 1"
    )
    expect_exerr(
      check_primary_keys_unique(data.table(a = rep(1:2, 2L)), "a"),
      "there are duplicated primary keys:\na: 1\n\na: 2"
    )
  })
  it("checks for unique sets over multiple-column keys", {
    expect_exerr(
      check_primary_keys_unique(
        data.table(
          a = rep(1:2, 3L),
          b = c(1L, 1L, 2L, 2L, 2L, 3L)
        ),
        c("a", "b")
      ),
      "there are duplicated primary keys:\na: 1\nb: 2"
    )
  })
})

describe("check_foreign_keys()", {
  it("expects at least one key name", {
    expect_exerr(
      check_foreign_keys(
        data.table(a = character()),
        data.table(a = character()),
        character()
      ),
      "require at least one key"
    )
  })
  it("expects key to exist in table and reference table", {
    expect_exerr(
      check_foreign_keys(
        data.table(a = character()),
        data.table(b = character()),
        "b"
      ),
      "foreign key columns not found in dt: b"
    )
    expect_exerr(
      check_foreign_keys(
        data.table(a = character()),
        data.table(b = character()),
        "a"
      ),
      "reference key columns not found in ref: a"
    )
  })
  it("returns error with values in dt that don't exist in ref", {
    expect_exerr(
      check_foreign_keys(
        data.table(a = 1:3),
        data.table(a = 1:2),
        "a"
      ),
      "foreign key values not found in reference columns:\na: 3"
    )
    expect_exerr(
      check_foreign_keys(
        data.table(a = 1:4,
                   b = 1:2),
        data.table(a = c(1:2, 1L),
                   b = 1:3),
        c("a", "b")
      ),
      "foreign key values not found in reference columns:\na: 3, 4"
    )
  })
  it("returns NULL if foreign keys all pull values out of ref", {
    expect_null(
      check_foreign_keys(
        data.table(a = integer()),
        data.table(a = integer()),
        "a"
      )
    )
  })
  it("returns error for NA values by default", {
    expect_exerr(
      check_foreign_keys(
        data.table(a = NA_character_),
        data.table(a = "a"),
        "a"
      ),
      "foreign key values not found in reference columns:\na: NA"
    )
  })
  it("expects optional to be length one or same length as keys", {
    expect_exerr(
      check_foreign_keys(
        data.table(a = NA_character_, b = NA_character_),
        data.table(a = "a", b = "b"),
        c("a", "b"),
        optional = c(TRUE, FALSE, TRUE)
      ),
      "optional must be length one or same length as keys"
    )
  })
  it("allows NA in addition to ref values if optional = TRUE", {
    expect_null(
      check_foreign_keys(
        data.table(a = NA_character_),
        data.table(a = "a"),
        "a",
        optional = TRUE
      )
    )
  })
  it("allows an optional flag for each key pair", {
    expect_exerr(
      check_foreign_keys(
        data.table(a = NA_character_, b = NA_character_),
        data.table(a = "a", b = "b"),
        c("a", "b"),
        optional = c(TRUE, FALSE)
      ),
      "foreign key values not found in reference columns:\nb: NA"
    )
  })
})

describe("check_no_required_values_missing", {
  it("returns error if NA values found", {
    expect_exerr(
      check_no_required_values_missing(data.table(a = NA)),
      "there are missing required values in the following rows:\na: 1"
    )
  })
  it("ignores optional columns, where normalization would hinder use", {
    expect_null(check_no_required_values_missing(data.table(a = NA), "a"))
    expect_exerr(
      check_no_required_values_missing(data.table(a = 1:2, b = c(1, NA), c = NA)),
      "there are missing required values in the following rows:\nb: 2\nc: 1, 2"
    )
  })
})

describe("check_column_types()", {
  it("expects to be given a type for each column, with no extras", {
    expect_exerr(
      check_column_types(
        data.table(a = integer(), b = character()),
        c(a = "integer")
      ),
      "missing column types: b"
    )
    expect_exerr(
      check_column_types(
        data.table(a = integer(), b = character()),
        c(a = "integer", b = "character", c = "numeric")
      ),
      "types given for absent columns: c"
    )
  })
  it("returns error if any primary column types are not as expected", {
    mult_inherit <- numeric()
    class(mult_inherit) <- c("test", "character")
    expect_exerr(
      check_column_types(
        data.table(a = integer(), b = mult_inherit),
        c(a = "integer", b = "character")
      ),
      "unexpected column types:\nb: expected character, observed test"
    )
  })
  it("can take types out of order", {
    expect_null(
      check_column_types(
        data.table(a = integer(), b = character()),
        c(b = "character", a = "integer")
      )
    )
  })
  it("can allow check on inheritance instead of direct type", {
    mult_inherit <- numeric()
    class(mult_inherit) <- c("test", "character")
    expect_exerr(
      check_column_types(
        data.table(a = integer(), b = mult_inherit, c = mult_inherit),
        c(a = "integer", b = "character", c = "numeric"),
        inherit = TRUE
      ),
      "unexpected column types:\nc: expected numeric, observed test, character"
    )
  })
  it("can allow inheritance separately for individual columns", {
    mult_inherit <- numeric()
    class(mult_inherit) <- c("test", "character")
    expect_exerr(
      check_column_types(
        data.table(a = integer(), b = mult_inherit, c = mult_inherit),
        c(a = "integer", b = "character", c = "numeric"),
        inherit = c(FALSE, FALSE, TRUE)
      ),
      "unexpected column types:\nb: expected character, observed test\nc: expected numeric, observed test, character"
    )
  })
  it("expects inherits to be length one or same length as types", {
    expect_exerr(
      check_column_types(
        data.table(a = integer()),
        c(a = "integer"),
        inherit = c(FALSE, TRUE)
      ),
      "inherit must be length one or same length as types"
    )
  })
  it("uses inherit in same order as types if given out of order", {
    mult_inherit <- numeric()
    class(mult_inherit) <- c("test", "character")
    expect_null(
      check_column_types(
        data.table(a = integer(), b = mult_inherit),
        types = c(b = "character", a = "integer"),
        inherit = c(TRUE, FALSE)
      )
    )
  })
})

describe("check_table_constraint()", {
  it("checks whether calling an evaluation inside the table returns TRUE", {
    expect_null(
      check_table_constraint(
        data.table(a = 1:6, b = 1:3),
        expression(b <= a)
      )
    )
    expect_exerr(
      check_table_constraint(
        data.table(a = 1:3, b = c(1L, 3L, NA_integer_)),
        expression(b <= a)
      ),
      "table has entries that violate constraint b <= a:\na: 2\nb: 3\n\na: 3\nb: NA"
    )
  })
  it("expects expression to return logical vector of same length as table", {
    expect_exerr(
      check_table_constraint(
        data.table(a = 1:3, b = 1:3),
        expression(b[1] <= a[1])
      ),
      "expression result is not logical with length equal to table entry count"
    )
    expect_exerr(
      check_table_constraint(
        data.table(a = 1:3, b = 1:3),
        expression(as.integer(b <= a))
      ),
      "expression result is not logical with length equal to table entry count"
    )
  })
})

describe("check_column_relation()", {
  it("checks first column values are function of second column", {
    expect_null(
      check_column_relation(
        data.table(a = 3L),
        data.table(b = 1:3),
        "a",
        "b",
        max
      )
    )
    expect_exerr(
      check_column_relation(
        data.table(a = 3),
        data.table(b = 1:3),
        "a",
        "b",
        max
      ),
      "first column not function of second column"
    )
  })
  it("can check relation over groups, using by", {
    expect_null(
      check_column_relation(
        data.table(grp = c("a", "b"), a = 3:4),
        data.table(grp = rep(c("a", "b"), each = 3), b = c(1:3, 2:4)),
        "a",
        "b",
        max,
        by = "grp"
      )
    )
  })
  it("can group over unordered columns", {
    expect_null(
      check_column_relation(
        data.table(grp = c("a", "b"), a = 3:4),
        data.table(grp = rep(c("b", "a"), each = 3), b = c(2:4, 1:3)),
        "a",
        "b",
        max,
        by = "grp"
      )
    )
  })
})
