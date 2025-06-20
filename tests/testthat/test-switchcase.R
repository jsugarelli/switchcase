library(testthat)
library(switchcase)

test_that("numeric comparison works", {
  res <- switchCase(5,
                    alt(..expr < 5, NULL, "less"),
                    alt(..expr == 5, NULL, "equal"),
                    alt(..expr > 5, NULL, "greater"))
  expect_equal(res, "equal")
})

test_that("character comparison works", {
  res <- switchCase("b",
                    alt(..expr == "a", NULL, "A"),
                    alt(..expr == "b", NULL, "B"))
  expect_equal(res, "B")
})

test_that("default case works", {
  res <- switchCase(3,
                    alt(..expr == 1, NULL, "one"),
                    alt(..expr == 2, NULL, "two"),
                    alt(NULL, NULL, "default"))
  expect_equal(res, "default")
})
