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
