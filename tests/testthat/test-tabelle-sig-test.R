context("test-tabelle-sig-test.R")

test_that("auto sig test", {
  require(stpvers)
  
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
 
   require(stpvers)
  
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
  
  expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool,
      APA = TRUE,
      test = "wilcox.test"
    )[[1]]$statistics,
    "U=431.00, p=.253"
  )
  
  expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool,
      APA = TRUE,
      test = "u.test"
    )[[1]]$statistics,
    "U=431.00, p=.253"
  )
  
  
  
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
  
  
  
  
  
  expect_equal(Tabelle(warpbreaks,
                       breaks ,
                       APA = TRUE,
                       test = "ks.test")$ks.test,
               "W=0.17, p=.100")
  
  
})