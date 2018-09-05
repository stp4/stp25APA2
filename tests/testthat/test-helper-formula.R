context("test-helper-formula.R")

test_that("clean_dots_formula", {
  data <- data.frame(x = NA, y = NA, z = NA)
  expect_equal(stp25APA2:::clean_dots_formula(x ~ y, data),
               x ~ y)
  
  expect_equal(stp25APA2:::clean_dots_formula(. ~ x + y, data),
               z ~ x + y)
  
  expect_equal(stp25APA2:::clean_dots_formula(x + y ~ ., data),
               x + y ~ z)
  
  expect_equal(stp25APA2:::clean_dots_formula(~ ., data),
               ~ x + y + z)
  
  expect_equal(
    stp25APA2:::formula_split(a + b ~ x | y),
    list(
      formula = a + b ~ x,
      condition =  ~ y,
      facet_type = "facet_wrap"
    )
  )
})




test_that("prepare_data2 formula simpel", {
  r1 <- prepare_data2( ~ m1 + m2 + m3 + m4, varana)
  
 
  
  expect_equal(r1$measure.vars,
               c("m1", "m2", "m3", "m4"))
  
  
  expect_equal(r1$group.vars,
               NULL)
  expect_equal(r1$condition.vars,
               NULL)
  
  expect_equal(r1$measure,
               c(
                 m1 = "numeric",
                 m2 = "numeric",
                 m3 = "numeric",
                 m4 = "numeric"
               ))
  
  expect_equal(r1$digits,
               c(
                 m1 = 2,
                 m2 = 2,
                 m3 = 2,
                 m4 = 2
               ))
  expect_equal(r1$by,
               "1")
  
  
  
  
  
  expect_that(is.data.frame(r1$data),
              is_true())
  expect_equal(r1$N,
               27)
  
})





test_that("prepare_data2 formula digits and measure", {
  r2 <- prepare_data2( ~ log(m1) + m2 + m3 + m4, varana)
  r3 <- prepare_data2( ~ m1[1] + m2 + m3 + m4, varana)
  r4 <- prepare_data2( ~ m1[1] + m2 + m3[2, median] + m4, varana)
  
  
  expect_equivalent(r2$data$m1,
                    log(varana$m1))
  
  
  expect_equal(r3$measure.vars,
               c("m1", "m2", "m3", "m4"))
  
  
  
  
  expect_equal(r4$measure,
               c(
                 m1 = "numeric",
                 m2 = "numeric",
                 m3 = "median",
                 m4 = "numeric"
               ))
  
  
  
  expect_equal(r3$digits,
               c(
                 m1 = 1,
                 m2 = 2,
                 m3 = 2,
                 m4 = 2
               ))
  
  expect_equal(r3$digits,
               r4$digits)
  
})

test_that("prepare_data2 formula and data.frame simpel", {
  r1 <- prepare_data2(varana, m1 , m2 , m3, m4)
  r2 <- prepare_data2(~ m1 + m2 + m3 + m4 , varana)
  
  
  expect_equal(r1$data,
               r2$data)
  expect_equivalent(r1$formula,
                    r2$formula)
  # str(r1$formula)
  # str(r2$formula)
  
  expect_equal(r1$measure.vars,
               r2$measure.vars)
  
  
  
  
  expect_equal(r1$measure,
               r2$measure)
  
  
  
  expect_equal(r1$digits,
               r2$digits)
  
  
})

test_that("prepare_data2 formula and data.frame digits", {
  r1 <- prepare_data2(varana, m1[3], m2[median], m3, m4[1, median])
  r2 <-
    prepare_data2(~ m1[3] + m2[median] + m3 + m4[1, median], varana)
  
  
  expect_equal(r1$data,
               r2$data)
  expect_equivalent(r1$formula,
                    r2$formula)
  # str(r1$formula)
  # str(r2$formula)
  
  expect_equal(r1$measure.vars,
               r2$measure.vars)
  
  
  
  
  expect_equal(r1$measure,
               r2$measure)
  
  
  
  expect_equal(r1$digits,
               r2$digits)
  
  
  
  
  r3 <- prepare_data2(varana, m1 , m2, m3 = median)
  r4 <- prepare_data2(varana, m1 , m2, m3[median])
  r5 <- prepare_data2(~ m1 + m2 + m3[median], varana)
  
  expect_equal(r3$measure,
               r4$measure)
  
  
  expect_equal(r3$measure,
               r5$measure)
  
  
  
  
  
})


test_that("prepare_data2  data.frame number and names", {
  r1 <- prepare_data2(varana, m1 , m2 , m3, m4)
  r6 <-  prepare_data2(varana, 4:7)
  
  expect_equal(r1$data,
               r6$data)
  expect_equivalent(r1$formula,
                    r6$formula)
  
  expect_equal(r1$measure.vars,
               r6$measure.vars)
  
  
  
  
  expect_equal(r1$measure,
               r6$measure)
  
  
  
  expect_equal(r1$digits,
               r6$digits)
  
  
})


test_that("prepare_data2 formula and data.frame groups", {
  r1 <- prepare_data2(varana, m1, m2, m3 , m4, by = ~ geschl)
  r2 <- prepare_data2(m1 + m2 + m3 + m4 ~ geschl, varana)
  r3 <- prepare_data2(varana, m1[4,median], m2, m3 , m4[5], by = ~ geschl)
  r4 <- prepare_data2(m1[4,median] + m2 + m3 + m4[5] ~ geschl, varana)
  
  
  expect_equal(r1$data,
               r2$data)
  expect_equal(r1$data,
               r3$data)
  expect_equal(r1$data,
               r4$data)
  
  expect_equivalent(r1$formula,
                    r2$formula)
  expect_equivalent(r1$formula,
                    r3$formula)
  expect_equivalent(r1$formula,
                    r4$formula)
  
  
  
  expect_equivalent(r1$group.vars,
                    r2$group.vars)
  expect_equivalent(r1$group.vars,
                    r3$group.vars)
  expect_equivalent(r1$group.vars,
                    r4$group.vars)
  
  
  expect_equal(r1$measure.vars,
               r2$measure.vars)
  
  
  expect_equal(r3$measure.vars,
               r4$measure.vars)
  
  
  
  
  
  expect_equal(r3$digits,
               r4$digits)
  
  
})
