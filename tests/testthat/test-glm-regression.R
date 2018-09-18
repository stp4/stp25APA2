context("test-glm-regression")

test_that("extract_param estimate same as coef", {
  fit <-
    glm(I(sr < 10) ~ pop15 + pop75 + dpi,
        data = LifeCycleSavings,
        family = binomial)
  
  expect_equal(as.vector(coef(fit)),
               broom::tidy(fit)$estimate)
  
  expect_equal(extract_param(fit)$estimate,
               broom::tidy(fit)$estimate)
 
  fit5 <-
    glm(num_awards ~ prog + math, poisson_sim, family =  poisson())
  
  
  expect_equal(extract_param(fit5)$estimate,
               broom::tidy(fit5)$estimate)
  
  
})


test_that("extract_param include._", {
  fit <-
    glm(I(sr < 10) ~ pop15 + pop75 + dpi,
        data = LifeCycleSavings,
        family = binomial)
  fit5 <-
    glm(num_awards ~ prog + math, poisson_sim, family =  poisson())
  
  
  res <- extract_param(
    fit,
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
  expect_equal(
    names(res),
    c(
      "term",
      "conf.high",
      "conf.low",
      "odds",
      "odds.conf.low",
      "odds.conf.high",
      "stars"
    )
  )
  
  res5 <- extract_param(
    fit,
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
  expect_equal(
    names(res5),
    c(
      "term",
      "conf.high",
      "conf.low",
      "odds",
      "odds.conf.low",
      "odds.conf.high",
      "stars"
    )
  )
})


test_that("APA2 vs APA_Table vs Ordnen vs ectract_param", {
  fit <-
    glm(I(sr < 10) ~ pop15 + pop75 + dpi + cut(ddpi, c(-Inf, 3, 5, Inf)),
        data = LifeCycleSavings,
        family = binomial)
  
  fit_extract <- extract_param(
    fit,
    include.b = TRUE,
    include.se = FALSE,
    include.beta = FALSE,
    include.ci = FALSE,
    include.odds = TRUE,
    include.odds.ci = TRUE,
    include.statistic = FALSE,
    include.p = FALSE,
    include.stars = TRUE,
    fix_format = TRUE
  )
  
  fit_Ordnen <- Ordnen(fit)
  fit_APA2 <- APA2(fit, output = FALSE)
  fit_APA_Table <- APA_Table(fit, output = FALSE)
  fit_APA_Table_long <-
    APA_Table(fit, output = FALSE, type = "long")
  
  
  expect_that(is.character(fit_APA2$estimate),
              is_true())
  
  expect_equal(fit_extract$estimate,
               c("-10.1", "0.24","0.398", "0.000786", "-0.805", "-0.73"))
  
  expect_equal(fit_APA_Table[[1]][, 2],
               c("-10.09", "0.24", "0.40", "0.00", "-0.81", "-0.73"))
  
  fit_extract2 <- extract_param(
    fit,
    include.b = TRUE,
    include.se = FALSE,
    include.beta = FALSE,
    include.ci = FALSE,
    include.odds = FALSE,
    include.odds.ci = FALSE,
    include.statistic = FALSE,
    include.p = FALSE,
    include.stars = TRUE,
    fix_format = FALSE
  )
  
  expect_equal(gsub(
    " ",
    "",
    paste0(
      stp25rndr:::Format2.default(fit_extract2$estimate, 
                                  2, format = "f"),
      fit_extract$stars
    )
  ),
  gsub(" ", "", fit_APA_Table_long[1:6, 2]))
  
})