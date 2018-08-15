context("test-describe-likert.R")

test_that("default works", {
  require(stpvers)
  set.seed(1)
  n <- 100
  lvs <- c("--", "-", "o", "+", "++")
  DF2 <- data.frame(
    Magazines = gl(length(lvs), 1, n, lvs),
    Comic.books = gl(length(lvs), 2, n, lvs),
    Fiction = gl(length(lvs), 3, n, lvs),
    Newspapers = gl(length(lvs), 5, n, lvs)
  )
  DF2$Comic.books[sample.int(n / 2)] <- lvs[length(lvs)]
  DF2$Newspapers[sample.int(n / 2)] <- lvs[1]
  DF2$Magazines[sample.int(n / 2)] <- lvs[2]
  
  #DF2<- transform(DF2, Geschlecht= cut( rnorm(n), 2, Cs(m, f)))
  Res1 <- Likert( ~ ., DF2)
  
  expect_equal(
    names(Res1),
    c(
      "results",
      "names",
      "freq",
      "freq.na",
      "N",
      "n",
      "m",
      "sd",
      "Mittelwert",
      "items",
      "grouping",
      "nlevels",
      "levels"
    )
  )
  
  
  expect_that(is.data.frame(Res1$results),
              is_true())
  
  expect_equal(Res1$results[, 2], c(10, 10, 21, 60))
  
  
})


test_that("APA2 works", {
  require(stpvers)
  set.seed(1)
  n <- 100
  lvs <- c("--", "-", "o", "+", "++")
  DF2 <- data.frame(
    Magazines = gl(length(lvs), 1, n, lvs),
    Comic.books = gl(length(lvs), 2, n, lvs),
    Fiction = gl(length(lvs), 3, n, lvs),
    Newspapers = gl(length(lvs), 5, n, lvs)
  )
  DF2$Comic.books[sample.int(n / 2)] <- lvs[length(lvs)]
  DF2$Newspapers[sample.int(n / 2)] <- lvs[1]
  DF2$Magazines[sample.int(n / 2)] <- lvs[2]
  
  #DF2<- transform(DF2, Geschlecht= cut( rnorm(n), 2, Cs(m, f)))
  Res1 <- Likert( ~ ., DF2)
  
  ans <- APA2(Res1, output=FALSE)
 
 
  
  expect_that(is.data.frame(ans),
              is_true())
  
  expect_equal(ans[, 2], c("10% (10)", "10% (10)", "21% (21)", "60% (60)"))
  
  
})