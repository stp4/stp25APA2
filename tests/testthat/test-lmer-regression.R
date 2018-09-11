context("test-lmer-regression")

test_that("error in broom", {
  fit <- lmerTest::lmer(chol0 ~ rrs0 + med + ak + (1 | g), hyper)
  
  expect_error(broom::tidy(fit), "Column 6 must be named", fixed = TRUE)
})




test_that("lmerTest and lme4 equal", {
  fit1 <-
    lmerTest::lmer(chol0 ~ rrs0 + med + ak + (1 | g), hyper)
  fit2 <-  lme4::lmer(chol0 ~ rrs0 + med +ak  + (1 | g), hyper)
  expect_warning(expect_equal(extract_param(fit1),
                              extract_param(fit2)))
})


test_that("lmerTest and lme almost equal equal", {
  fit1 <-
    lmerTest::lmer(chol0 ~ rrs0 + med + ak + (1 | g), hyper)
  fit3 <-
    nlme::lme(chol0 ~ rrs0 + med + ak, random =  ~ 1 |
                g, data = hyper)
  expect_equal(round(extract_param(fit1)[1:6,-1] , 2),
               
               round(extract_param(fit3)[-1] , 2))
  
})


test_that("use of update", {
  fit1 <-
    lmerTest::lmer(chol0 ~ rrs0 + med + (1 | g), hyper)
  fit2 <- update(fit1, . ~ . + ak)
  fit3 <-
    lmerTest::lmer(chol0 ~ rrs0 + med + ak + (1 | g), hyper)
  
  
  
  expect_equal(extract_param(fit2),
               extract_param(fit3))
  
  
  fit1_lme4 <-
    lme4::lmer(chol0 ~ rrs0 + med   +  (1 | g), hyper)
  fit2_lme4 <- update(fit1_lme4, . ~ . + ak)
  fit3_lme4 <-
    lme4::lmer(chol0 ~ rrs0 + med +  ak  +  (1 | g), hyper)
  
  
  
  expect_warning(expect_equal(extract_param(fit2_lme4),
                              extract_param(fit3_lme4)))
  
  #
  # fit1_lme <-
  #   nlme::lme(chol0 ~ rrs0 + med, random =  ~ 1 | g, data = hyper)
  # fit2_lme <-   update(fit1_lme, . ~ . + ak)
  # fit3_lme <-
  #   nlme::lme(chol0 ~ rrs0 + med +  ak, random =  ~ 1 | g, data = hyper)
  #
  #
  #
  # expect_equal(extract_param(fit2_lme),
  #              extract_param(fit3_lme))
  
  
  
})






# fit0 <-   lm(chol0 ~ rrs0 + med +  ak  , hyper)
# broom::tidy(fit0, effects =  "fixed")
#   fit1 <-  lmerTest::lmer(chol0 ~ rrs0 + med +  ak  +  (1 | g) , hyper)
#   fit2 <-  lme4::lmer(chol0 ~ rrs0 + med +  ak  +  (1 | g) , hyper)
#
#
#
#
#


#   fit_sum$coefficients
# coef(fit)
# expect_error(broom::tidy(fit)$estimate, "Column 6 must be named", fixed=TRUE)
# expect_equal(as.vector(coef(fit)),
#              broom::tidy(fit)$estimate
#              )
#
# expect_equal(extract_param(fit)$estimate,
#              broom::tidy(fit)$estimate)
#
#


# fits <- list(fit1, fit2, fit3)
#
# x <- APA_Table(fit1, fit2, fit3,
#                test.my.fun = TRUE, output = FALSE)
#
# str(x)
# # x <-  APA_Table(fit1, fit2, fit3,
# #                 type = "long",
# #                 test.my.fun = TRUE, output = FALSE)
#
# summary(fit3)
# extract_param(fit3 ,  effects = "fixed" )