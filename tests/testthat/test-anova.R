context("test-anova")

test_that("anova apa2 works", {

  fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
  
  #  ANOVA
  x <- APA2(fm1, caption = "ANOVA", output = FALSE, fix_format=TRUE)
  
  expect_equal(
    names(x),
    c(
      "term",
      "meansq",
      "df"  ,
      "statistic",
      "eta.sq"  ,
      "eta.sq.part",
      "p.value"
    )
  )
  
 
  expect_equal(x$meansq,
               c("450.67",  "1017.13", "134.96"))
  
  
})


test_that("anova error mit interaction", {
  expect_equal(
    APA2(npk.aov <-
           aov(yield ~ block + N + P + K, npk), output = FALSE)$statistic,
    c("4.29",  "11.82", "0.52" , "5.95",  "")
  )
  expect_equal(
    APA2(npk.aov <-
           aov(yield ~ block + N * P + K, npk), output = FALSE)$statistic,
    c("4.39" , "12.11", "0.54" , "6.09" , "1.36" , "")
  )
  
  
  npk.aov3 <- aov(yield ~ block + N * P * K, npk)
  expect_error(APA2(npk.aov3),
               "extract_param_aov mit etaSquared")
  
  
  
})



