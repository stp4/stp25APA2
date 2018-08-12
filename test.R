# usethis::use_test("Tabelle")


require(stpvers)
res <-  warpbreaks %>% Tabelle(breaks, wool, tension)

is.data.frame(res)
is.character(res[, 2])


res<-warpbreaks %>% Tabelle2(breaks, wool, tension, APA=TRUE)
is.data.frame(res)
is.character(res[, 2])



warpbreaks %>% Tabelle2(breaks, by=~wool + tension, fun=mean)
