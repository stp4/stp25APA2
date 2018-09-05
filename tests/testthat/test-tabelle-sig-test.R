context("test-tabelle-sig-test.R")

test_that("auto sig test", {
 
  
  res <-
    Tabelle(
      warpbreaks,
      breaks,
      tension,
      by =  ~ wool,
      APA = TRUE,
      include.test = TRUE
    )
  res2 <-
    Tabelle(breaks + tension ~ wool,
            warpbreaks,
            APA = TRUE,
            include.test = TRUE)
  
  res3 <- APA2(breaks + tension ~ wool,
               warpbreaks,
               test = TRUE,
               output = FALSE)
  
  
  
  
  expect_that(is.list(res),
              is_true())
  
  expect_that(is.data.frame(res[[1]]),
              is_true())
  
  
  expect_equal(
    res[[1]]$statistics,
    c(
      "F<sub>(1, 52)</sub>=1.33, p=.253",
      "X2<sub>(2)</sub>=0.00, p=1.000",
      "",
      "",
      ""
    )
  )
  
  expect_equal(res[[1]]$statistics,
               res2[[1]]$statistics)
  
  expect_equal(res2[[1]]$statistics[1],
               res3[[1]]$sig.Test[1])
})





test_that("eigene sig test", {
 
 
  
  expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool,
      APA = TRUE,
      test = "t.test"
    )[[1]]$statistics,
    "T<sub>(42)</sub>=1.63, p=.110"
  )
  
  
  expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool,
      APA = TRUE,
      test = "anova"
    )[[1]]$statistics,
    "F<sub>(1, 52)</sub>=2.67, p=.108"
  )
  
  # https://stats.stackexchange.com/questions/231738/clarification-on-mann-whitney-wilcoxon-test
  # Finally, note that the "Warning" from wilcox.test was just that: 
  # a warning rather than an indication that your results are incorrect. 
  # The test was OK, reporting a p-value based on a normal approximation 
  # rather than an exact p-value based on the data values. If you had 50 
  # or more cases, your call to wilcox.test would not even have tried to 
  # calculate exact p-values. The coin package in R has a wilcox_test 
  # function that can calculate exact p-values in the presence of ties, 
  # but I see no need here for an exact p-value.
  
  
  expect_warning( expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool,
      APA = TRUE,
      test = "wilcox.test"
    )[[1]]$statistics,
    "U=431.00, p=.253"
  ))
  
  expect_warning( expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool,
      APA = TRUE,
      test = "u.test"
    )[[1]]$statistics,
    "U=431.00, p=.253"
  ))
  
  
  
  expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool,
      APA = TRUE,
      test = "kruskal.test"
    )[[1]]$statistics,
    "H<sub>(1)</sub>=1.33, p=.250"
  )
  
  
  
  
  expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool,
      APA = TRUE,
      test = "h.test"
    )[[1]]$statistics,
    "H<sub>(1)</sub>=1.33, p=.250"
  )
  
  
  
  
  
  # expect_equal(
  #   Tabelle(
  #     warpbreaks,
  #     breaks,
  #     by =  ~ wool,
  #     APA = TRUE,
  #     test = "chisq.test"
  #   )[[1]]$statistics,
  #   "chisq.test"
  # )
  
  
  expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool,
      APA = TRUE,
      test = "Hmisc"
    )[[1]]$statistics,
    "F<sub>(1, 52)</sub>=1.33, p=.253"
  )
  
  
  
  
  
  # expect_equal(
  #   Tabelle(
  #     warpbreaks,
  #     breaks,
  #     by =  ~ wool,
  #     APA = TRUE,
  #     test = "SPSS"
  #   )[[1]]$statistics,
  #   "H<sub>(1)</sub>=1.33, p=.250"
  # )
  # 
  # 
  
  
  
  
  
  expect_equal(
    Tabelle(warpbreaks,
            breaks ,
            APA = TRUE,
            test = "shapiro.test")$shapiro.test,
    "W=0.89, p<.001"
  )
  
  
  
  
  
  expect_warning(expect_equal(Tabelle(warpbreaks,
                       breaks ,
                       APA = TRUE,
                       test = "ks.test")$ks.test,
               "W=0.17, p=.100"))
  
  
})