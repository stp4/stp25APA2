context("test-t-test")

test_that("Tabelle t works", {
  expect_equal(APA(t.test(m1 ~ geschl, varana, var.equal = FALSE)), 
               "T<sub>(23)</sub>=0.32, p=.752")
  
  expect_equal(
    Tabelle(m1 + m2 ~ geschl, varana, test = "t.test", APA = TRUE)[[1]][1, 6],
    "T<sub>(23)</sub>=0.32, p=.752"
  )
  
  expect_equal(APA2(m1 + m2 ~ geschl, varana, test = "t.test")[[1]][1, 6],
               "T<sub>(23)</sub>=0.32, p=.752")
})
