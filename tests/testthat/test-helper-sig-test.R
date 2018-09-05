context("test-helper-sig-test.R")

test_that("catTest chi-test", {
  expect_equal(
    stp25APA2:::catTest(~ tension + wool, warpbreaks),
    "X2<sub>(2)</sub>=0.00, p=1.000"
    
  )
  
  expect_equal(
    stp25APA2:::conTest(breaks ~ wool,
                        warpbreaks,
                        "h.test") ,
    "H<sub>(1)</sub>=1.33, p=.250"
  )
  
  
})
