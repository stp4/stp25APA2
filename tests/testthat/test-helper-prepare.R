context("test-helper-prepare.R")

test_that("prepare_data2 works", {
  r1 <- prepare_data2( ~ m1 + m2 + m3 + m4, varana)
  
  expect_that(is.list(r1),
              is_true())
  expect_equal(
    names(r1),
    c(
      "data",
      "measure.vars",
      "group.vars",
      "condition.vars",
      "formula",
      "by",
      "measure",
      "row_name",
      "col_name"  ,
      "measure.class",
      "group.class",
      "condition.class",
      "digits",
      "N"
    )
  )
})
