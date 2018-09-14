context("test-klassifikation")

test_that("fun works", {
  fit1 <- glm(gruppe ~ lai, hkarz, family = binomial)
  
  x <- Klassifikation(fit1)
  expect_equal(x$statistic[c(1, 7, 8), 2],
               c("0.82" , "0.81", "0.84"))
  
  rst <- pROC::auc(pROC::roc(x$response, x$predictor))
  expect_equal(round(rst, 3), 0.818)
})
