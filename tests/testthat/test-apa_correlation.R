context("test-apa_correlation")

test_that("corr formula works", {
  set.seed(0815)
  n <- 2 * 20
  e <- rnorm(n)
  dat <- Label(
    data.frame(
      a = rnorm(n) + e / 2,
      b = rnorm(n) + e,
      c = rnorm(n),
      d = rnorm(n) + e * 10,
      g = gl(2, 20, labels = c("Control", "Treat"))
    ),
    a = "Alpha",
    b = "Beta",
    c = "Gamma"
  )
  
  
  expect_equal(APA_Correlation(~ a + b + c, dat)[1, 3], " .45**")
  expect_equal(APA_Correlation(a ~ c, dat)[1, 4], ".706")
  expect_equal(APA_Correlation(a + b + c ~ d, dat)[1, 3], " .49")
  expect_equal(APA_Correlation(a + b ~ c +  d, dat)[1, 6], ".49")
  expect_equal(APA_Correlation(a + b + c ~ d, dat, groups = ~ g)[2, 3], ".75")
  
})
