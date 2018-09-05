#
# 
#
usethis::use_test("APA_APA_Table")
# usethis::use_test("describe-likert")
# usethis::use_test("describe-multi")
# usethis::use_test("describe-rank")
# usethis::use_test("describe-Tabelle")
# usethis::use_test("helper-formula")
# usethis::use_test("helper-prepare")
# usethis::use_test("helper-sig-test")
#  
# ?esoph
# xtabs( ~ncases +agegp, data = esoph)
# xtabs(cbind(ncases, ncontrols) ~ agegp, data = esoph)


d.ergo <- data.frame(Type = paste0("T", rep(1:4, 9*4)),
                     Subj = gl(9, 4, 36*4))
xtabs(~ Type + Subj, data = d.ergo)

 
 #att3 = APA_Xtabs(~education + induced + spontaneous, data = infert)
 
 
 
 
 
 #att1 = APA_Xtabs(~education, data = infert)
 att2 = APA_Xtabs(~education + induced, data = infert)
 att3 = APA_Xtabs(~education + induced + spontaneous, data = infert)
#require(gmodels)
 require(stpvers)
data(infert, package = "datasets")
 tt1 = xtabs(~education, data = infert)
 tt2 = xtabs(~education + induced, data = infert)
 tt3 = xtabs(~education + induced + spontaneous, data = infert)
 dim(tt2)

 
 
 att2 <- APA_Xtabs(~education + induced, data = infert, 
                   include.total = FALSE,
                   include.total.columns = TRUE,
                   include.total.sub = FALSE,
                   include.total.rows = FALSE,
                   output=FALSE)
 att2[[1]][,5] 
 
  "100% (12)"  "100% (120)" "100% (116)"
 att2 <- APA_Xtabs(~education + induced, data = infert, 
                   include.total = FALSE,
                   include.total.columns = TRUE,
                   include.total.sub = FALSE,
                   include.total.rows = TRUE,
                   output=FALSE)
 att2[[1]][,5]
  "5% (12)"    "48% (120)"  "47% (116)"  "100% (248)"
 att2 <- APA_Xtabs(~education + induced, data = infert, 
                   include.total = FALSE,
                   include.total.columns = FALSE,
                   include.total.sub = FALSE,
                   include.total.rows = TRUE,
                   output=FALSE)
 att2[[1]][,4]
 
 "16% (6)"   "41% (15)"  "43% (16)"  "100% (37)"