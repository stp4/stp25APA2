context("test-lm-regression")

test_that("extract_param estimate same as coef", {
  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)
  fit4 <- update(fit3, . ~ . + cut(ddpi, 3))
  
  
  expect_equivalent(extract_param(fit0)[2],
                    broom::tidy(fit0)[2])
  
  fit0 <- lm(sr ~ 1, data = LifeCycleSavings)
  fit1 <- update(fit0, . ~ . + pop15)
  fit2 <- update(fit1, . ~ . + pop75)
  fit3 <- update(fit2, . ~ . + dpi)
  
  
  expect_equal(as.vector(coef(fit2)),
               extract_param(fit2)$estimate)
  
  expect_equal(as.vector(coef(fit3)),
               extract_param(fit3)$estimate)
  
  expect_equal(as.vector(coef(fit4)),
               extract_param(fit4)$estimate)
  
  
})




test_that("extract_param include._", {
  fit3 <- lm(sr ~ pop15+ pop75+ dpi, data = LifeCycleSavings)
 
  res <- extract_param(
    fit3,
    include.b = FALSE,
    include.se = FALSE,
    include.beta = TRUE,
    include.ci = TRUE,
    include.odds = TRUE,
    include.odds.ci = TRUE,
    include.statistic = FALSE,
    include.p = FALSE,
    include.stars = TRUE
  )
  expect_equal(names(res),
               c("term",
                 "conf.high",
                 "conf.low",
                 "beta",
                 "stars"))
})




test_that("APA2 vs APA_Table vs Ordnen vs ectract_param", {
  fit4 <-
    lm(sr ~ pop15 + pop75 + dpi + cut(ddpi, 3), data = LifeCycleSavings)
  
  
  res <- extract_param(
    fit4,
    include.b = TRUE,
    include.se = FALSE,
    include.beta = FALSE,
    include.ci = FALSE,
    include.odds = TRUE,
    include.odds.ci = TRUE,
    include.statistic = FALSE,
    include.p = FALSE,
    include.stars = TRUE
  )
  
  x0 <- Ordnen(fit4)
  x1 <- APA2(fit4, output = FALSE)
  x2 <- APA_Table(fit4, output = FALSE)
  x3 <- APA_Table(fit4, output = FALSE, type = "long")
  
  
  expect_equal(res$estimate,
               x0$estimate)
  expect_equal(res$estimate,
               x2[[1]]$estimate)
  expect_equal(x1$estimate, c("29.4", "-0.456", "-1.51",  
                              "-0.000494", "1.62", "2.63")
               )
  
  
  expect_equal(gsub(" ", "", paste0(
    stp25rndr::Format2(res$estimate, 2), res$stars
  )),
  gsub(" ", "", x3[1:6, 2]))
  
})



test_that("APA2_list", {
  fit1 <- lm(chol0 ~ ak + med + rrs0 + g, hyper)
  fit2 <- lm(chol0 ~  g + ak+rrs0+ med  , hyper)
  fit3 <- lm(chol0 ~ rrs0 +ak +  g+ med , hyper)
  
  coefs <- APA2_list(
    list(fit1,
         fit2,
         fit3), include.se=FALSE, output = FALSE)
 
  expect_equal( coefs[,2], coefs[,2])
  
  expect_equal(coefs[13,3], "174" )
  
})