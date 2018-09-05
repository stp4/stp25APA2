context("test-describe-multi.R")

test_that("APA2 und Tabelle und APA2_multiresponse  works", {
  DF <- data.frame(
    Sex = gl(2, 10, labels = c("male", "femal")),
    Magazines =   c(0, 0, 0, 0, 0, 1, 0, 1, 1, 1,
                    1, 0, 0, 1, 1, 1, 0, 0, 1, 0),
    Comic.books = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Fiction =     c(1, 1, 1, 0, 1, 1, 1, 0, 1, 1,
                    1, 0, 0, 1, 1, 1, 0, 0, 1, 0),
    Newspapers =  c(0, 0, 0, 0, 0, 1, 1, 1, 1, 1,
                    1, 0, 0, 1, 1, 1, 0, 0, 0, 1)
  )
  
  
  
  ans1 <-
    APA2(
      ~ Magazines + Comic.books + Fiction + Newspapers,
      DF,
      type = "multi" ,
      output = FALSE
    )
  ans2 <-
    APA2_multiresponse(~ Magazines + Comic.books + Fiction + Newspapers, DF, output =
                         FALSE)
  
  ans3 <-
    Tabelle(~ Magazines + Comic.books + Fiction + Newspapers,
            DF,
            APA = TRUE,
            type = "multi")
  
  
  
  
  
  
  
  
  
  
  
  
  expect_equal(ans1[, 2],  ans2[, 2])
  expect_equal(ans1[, 2], ans3[, 3])
  
  
  
  ans4 <- Tabelle(
    DF,
    Magazines,
    Comic.books,
    Fiction,
    Newspapers,
    by =  ~ Sex,
    APA = TRUE,
    include.n = FALSE,
    include.total = TRUE,
    include.test = TRUE,
    type = "multi"
  )
  
  
  
  expect_equal(names(ans4[[1]]),
               c("Item", "Total", " male", " femal", "statistics"))
  
  
  expect_equal(ans4[[1]][, 4],
               c("50% (5)" , "<1% (0)", "50% (5)" , "50% (5)"))
  expect_equal(
    ans4[[1]][, 5],
    c(
      "F<sub>(1, 18)</sub>=0.18, p=.673" ,
      "F<sub>(1, 18)</sub>=Inf, p<.001",
      "F<sub>(1, 18)</sub>=1.98, p=.177" ,
      "F<sub>(1, 18)</sub>=0.00, p=1.000"
    )
  )
  
  
  
  
  
  
  DF$Magazines2 <- factor(DF$Magazines, 0:1, c("no", "yes"))
  DF$Magazines3 <- factor(DF$Magazines,  1:0, c("yes", "no"))
  
  ans5 <-  Tabelle(~ Magazines2 + Comic.books + Fiction + Newspapers,
                   DF,
                   APA = TRUE,
                   type = "multi")
  ans6 <-  Tabelle(~ Magazines3 + Comic.books + Fiction + Newspapers,
                   DF,
                   APA = TRUE,
                   type = "multi")
  
  
  expect_equal(ans5[, 3], c("55% (11)", "50% (10)", "65% (13)", "50% (10)"))
  expect_equal(ans6[, 3], c("45% (9)",  "50% (10)", "65% (13)", "50% (10)"))
  
  
})
