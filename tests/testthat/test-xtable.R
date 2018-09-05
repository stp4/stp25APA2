context("test-xtable.R")

test_that("korrekte Werte wie xtabl", {
  data(infert, package = "datasets")
  
  tt2 <- xtabs( ~ education + induced, data = infert)
  
  att2 <- APA_Xtabs(
    ~ education + induced,
    data = infert,
    include.total = FALSE,
    include.total.columns = FALSE,
    include.total.sub = FALSE,
    include.total.rows = FALSE,
    include.percent = FALSE,
    include.count = TRUE,
    include.margin = FALSE,
    output = FALSE
  )
  expect_equal(as.vector(unlist(att2$xtab[, -1])),   as.character(tt2))
})


test_that("default", {
  data(infert, package = "datasets")
  
  att2 <-
    APA_Xtabs( ~ education + induced, data = infert, output = FALSE)
  expect_equal(att2[[1]][, 2],
               c("2% (4)", "31% (78)", "25% (61)", "58% (143)"))
  
  #   education induced_0 induced_1 induced_2 induced_Sum
  # 1 0-5yrs       2% (4)    1% (2)    2% (6)     5% (12)
  # 2 6-11yrs    31% (78)  11% (27)   6% (15)   48% (120)
  # 3 12+ yrs    25% (61)  16% (39)   6% (16)   47% (116)
  # 4 Sum       58% (143)  27% (68)  15% (37)  100% (248)
  
  
  # 2+31+25 +1+11+16+2+6+6 =100
  
  
  
  att2 <- APA_Xtabs(
    ~ education + induced,
    data = infert,
    include.total = FALSE,
    include.total.columns = TRUE,
    include.total.sub = FALSE,
    include.total.rows = FALSE,
    output = FALSE
  )
  expect_equal(att2[[1]][, 5]  , c("100% (12)", "100% (120)", "100% (116)"))
  att2 <- APA_Xtabs(
    ~ education + induced,
    data = infert,
    include.total = FALSE,
    include.total.columns = TRUE,
    include.total.sub = FALSE,
    include.total.rows = TRUE,
    output = FALSE
  )
  expect_equal(att2[[1]][, 5] , c("5% (12)",  "48% (120)", "47% (116)", "100% (248)"))
  att2 <- APA_Xtabs(
    ~ education + induced,
    data = infert,
    include.total = FALSE,
    include.total.columns = FALSE,
    include.total.sub = FALSE,
    include.total.rows = TRUE,
    output = FALSE
  )
  expect_equal(att2[[1]][, 4], c("16% (6)", "41% (15)", "43% (16)", "100% (37)"))
  
  
  
  
  
})
