context("test-tabelle.R")

test_that("return value default", {
  require(stpvers)
  res <-  Tabelle(warpbreaks, breaks, wool, tension)
  
  expect_that(is.data.frame(res),
              is_true())
  
  expect_that(is.character(res[, 2]),
              is_true())
  expect_equal(res[, 2],
               c("28.15 (SD 13.20, range 10.00 to 70.00)",
                 "27/27" ,
                 "18/18/18"))
  
})



test_that("return value APA=TRUE", {
  require(stpvers)
  res <-  Tabelle(warpbreaks, breaks, wool, tension, APA = TRUE)
  expect_that(is.data.frame(res),
              is_true())
  
  expect_that(is.character(res[, 2]),
              is_true())
  
  expect_equal(dim(res), c(8, 3))
  
  expect_equal(res[1, 3] , "28.15 (13.20)")
})




test_that("default Type APA=FALSE", {
  require(stpvers)
  expect_equal(Tabelle(
    warpbreaks,
    breaks,
    by =  ~ wool + tension,
    type = "1"
  )[1, 4],
  "44.56 (18.10)")
  
  expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool + tension,
      type = "2"
    )[1, 4] ,
    "44.56 (SD 18.10, range 25.00 to 70.00)"
  )
  
  # Tabelle2(breaks, by=~wool + tension, type="3")
  expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool + tension,
      type = "median"
    )[1, 4],
    "51.00 (26.00, 54.00)"
  )
  
  expect_equal(Tabelle(
    warpbreaks,
    breaks,
    by =  ~ wool + tension,
    type = "mean"
  )[1, 4],
  "44.56 (18.10)")
  
  expect_equal(
    expect_warning(Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool + tension,
      type = "freq"
    ))[1, 4],
    "0/0/0/0/0/0/0/0/0/0/0/0/1/2/0/0/0/1/0/0/0/0/0/0/0/0/1/1/1/1/1"
  )
})
  
  
  test_that("default Type fun = eigene Funktion", {
    require(stpvers)  
  expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      wool,
      tension,
      fun = function(x)
        length(x),
      measure.name = "N"
    )$N,
    c(54, 54, 54)
  )
  
  expect_equal(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool + tension,
      fun = mean
    )$value,
    aggregate(breaks ~ wool + tension, warpbreaks, FUN = mean)$breaks
  )
  
  
  
  
  
  
  expect_equal(Tabelle(
    warpbreaks,
    breaks,
    by =  ~ wool + tension,
    fun = function(x)
      round(c(mean = mean(x), sd = sd(x)))
  )$mean[1],
  45)
  
  #  (aggregate(breaks ~wool + tension, warpbreaks, FUN=function(x) c(mean=mean(x), sd=sd(x))c ))
  
  expect_equal(names(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool + tension,
      fun = function(x)
        round(mean(x), 1),
      formula = wool ~ tension
    )
  ),
  c("wool", "L" ,   "M" ,   "H"))
  
  expect_equal(names(
    Tabelle(
      warpbreaks,
      breaks,
      by =  ~ wool + tension,
      fun = function(x)
        round(c(mean = mean(x), sd = sd(x)))
    )
  ),
  c(".id", "wool", "tension", "mean", "sd"))
  
  
})