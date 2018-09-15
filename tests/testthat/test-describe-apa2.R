context("test-describe-apa2")

test_that("Prop_Test2 works", {
  
 x<- factor(c("a", "b", "c", "b", "c", "b", "c","a", "b", "c", "b", "c", "a")) 
 
 expect_warning(x<- Prop_Test2(x))
expect_equal(as.character(x[1,3]), "23% CI=[ 6 - 54]")
})


test_that("Tabelle works", {
  
  expect_equal(Tabelle(hkarz, tzell="median", lai, gruppe)[1,2],"68.50 (IRQ 9.50, range 48.50 to 78.50)")
  set_my_options(median=list(style="IQR"))
  
  x<-Tabelle(tzell[1, median]+lai~gruppe, hkarz, 
             caption="Einfache Auswertung",
             test=TRUE, include.n = TRUE, APA=TRUE) 
  
  expect_equal(names(x), "gruppe")
  
  
  expect_equal(x[[1]][1,3], "63.2 (61.5, 67.0)")
})