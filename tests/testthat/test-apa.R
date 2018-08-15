context("test-apa.R")

test_that("lm gml aov", {
  require(stpvers)
  
  expect_equal(
    APA(mpg ~ cyl, mtcars)  ,
              c( mpg="20.09 (6.03)"))
  
  expect_equal(APA(glm(vs ~ mpg, mtcars, family = binomial())),
               "LogLik=-21.93, X2<sub>(2)</sub>=18.33, p<.001")
  
  expect_equal(APA(lm(mpg ~ drat + wt + qsec, mtcars)),
               "R2=.84, ad.R2=.82, F<sub>(3, 28)</sub>=47.93, p<.001")
  
  expect_equal(APA(aov(mpg ~ drat + wt + qsec, mtcars)),
               "R2=.84, ad.R2=.82, F<sub>(3, 28)</sub>=47.93, p<.001")
})
