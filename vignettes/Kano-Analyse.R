## ---- echo=FALSE, cache=TRUE, results='hide', message=FALSE--------------
require(stpvers)     ##  R version 3.1.2 (2014-10-31)
 
set.seed(2)
#-- Demo Data
DF<- data.frame(  f1 =  c(rep(2,32), rep(1,42), rep(1,15), rep(2,11)  ) 
                 ,f2 =  c(rep(5,32), rep(5,42), rep(2,15), rep(2,11) ) 
                 
                 ,f3 =  c(rep(2,34), rep(1,18), rep(1,19), rep(2,27)  , rep(2,1)  , rep(1,1)) 
                 ,f4 =  c(rep(5,34), rep(5,18), rep(2,19), rep(2,27)  , rep(1,1)  , rep(1,1)) 
                 
                 ,f5 =  c(rep(2,5), rep(1,8), rep(1,41), rep(2,42)  , rep(2,2), rep(1,2)) 
                 ,f6 =  c(rep(5,5), rep(5,8), rep(2,41), rep(2,42) , rep(1,2), rep(1,2)) 
                 
                 ,f7 =  c(rep(2,22), rep(1,29), rep(1,32), rep(2,17)  ) 
                 ,f8 =  c(rep(5,22), rep(5,29), rep(2,32), rep(2,17) ) 
                 
                 ,f9 =  c(rep(2,3), rep(1,6), rep(1,28), rep(2,60) , rep(2,2), rep(1,1) ) 
                 ,f10 = c(rep(5,3), rep(5,6), rep(2,28), rep(2,60) , rep(1,2), rep(1,1)) 
                 
                 )

set.seed(0815)
DF<- rbind(DF,DF,DF)[sample.int(300,296),]

lvl <- c("1. like",  "2. must be",  "3. neutral",  "4. live with",  "5. dislike")         

Labels<- c("Service app","Excellent service","Hotline","Loan machine","Ready-to-use return")


#Labels<- c("24h availability","Tag der offenen Tuer","Tracking","Hotline","Service App")


DF<- Hmisc::upData(DF, f1  = factor(f1 , 1:5, lvl), 
            f2  = factor(f2 , 1:5, lvl),
            f3  = factor(f3 , 1:5, lvl),
            f4  = factor(f4 , 1:5, lvl),
            f5  = factor(f5 , 1:5, lvl),
            f6  = factor(f6 , 1:5, lvl),
            f7  = factor(f7 , 1:5, lvl),
            f8  = factor(f8 , 1:5, lvl),
            f9  = factor(f9 , 1:5, lvl),
            f10  = factor(f10 , 1:5, lvl),
            labels=c(f1=Labels[1], f2=Labels[1]
                          ,f3=Labels[2],f4=Labels[2]
                          ,f5=Labels[3],f6=Labels[3]
                          ,f7=Labels[4],f8=Labels[4]
                          ,f9=Labels[5],f10=Labels[5])
            )


kano_res <- Kano( ~.  , DF)
res<-APA2(kano_res) 


## ---- echo=FALSE, results='asis', message=FALSE--------------------------
# knitr::kable(head(DF[, 1:4],3)
#              , format = "pandoc"
#              , caption = "Auszug aus der Datentabelle: f1 = funktionale-Excellent service,  f2 = dysfunktionale-Excellent service, f3= funktionale-Hotline f4= dysfunktionale-Hotline")

# # 
#   Output(head(DF ) , caption = "Auszug aus der Datentabelle: f1 = funktionale-Excellent service,  f2 = dysfunktionale-Excellent service, f3= funktionale-Hotline f4= dysfunktionale-Hotline")

1+1


## ---- echo=FALSE, results='asis'-----------------------------------------
 

tab1<-GetData("  'like'  'must be'	'neutral'	'live with'	'dislike'
'like'	Q	A	A	A	O
'must be'	R	I	I	I	M
'neutral'	R	I	I	I	M
'live with'	R	I	I	I	M
'dislike'	R	R	R	R	Q
")

knitr::kable(tab1
             , format = "pandoc"
             , align='c'
             , caption = "Evaluations Tabelle (Disfunktional = Spalten, Funktional = Zeilen)" )

## ---- echo=FALSE, results='asis'-----------------------------------------

#APA2(kano_res, caption = "Ueber Objekt")
 

# # knitr::kable(res[,c(1,4:7, 10:11)]
# #              , format = "pandoc"
# #              , align= c('l', 'c', 'c', 'c', 'c', 'c', 'c')
# #              , caption = "Absolute Haeufigkeit in Prozent")
# 
#res[,c(1,4:7, 10:11)]

1+1

## ----Kano_Bar, message=FALSE, echo=FALSE, cache=TRUE, cache.path = 'DocumentName_cache/',fig.path='figure/', fig.width = 7, fig.height = 4----
#windows(7,4)

# ##-- 
#  trellis.par.set(ggplot2like(n = 4, h.start = 180))
#  trellis.par.set(axis.text = list(cex = 0.8, lineheight = 0.9, col = "grey20"))
# 
#  
#  Kano_plot(value ~ variable , kano_res,
#  main = "",
#  ylab = "Prozent",
#  type = 2,# col = 2:7,
#  prop.table = TRUE)


plot(1)

## ---- echo=FALSE, results='asis'-----------------------------------------

# knitr::kable(res[,c(1,4:7,12:15 )]
#              , format = "pandoc"
#              , align=c('l', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c')
#              , caption = "Absolute Haeufigkeit und die Masszahlen")

1+1

## ----Importance_Grid ,  fig.cap = "Importance-Grid", message=FALSE, echo=FALSE, cache=TRUE, cache.path = 'DocumentName_cache/', fig.path='figure/', fig.width = 5, fig.height = 5----
#windows(8,8)
kano_plot(  kano_res )


