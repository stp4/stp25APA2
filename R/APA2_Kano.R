#' @name Kano
#' @rdname Kano
#' @title Analyze Kano type items.
#' @description Transformiert Kano-Fragebogen zu Kano-Kodierung
#' http://www.eric-klopp.de/texte/angewandte-psychologie/18-die-kano-methode
#'
#' M O A I R Q Hoeufigkeit
#'
#' max Category
#'
#' M>O>A>I  max Category mit Hirarchie M Wichtiger als O usw.
#' also wen der Unterschied zwischen den zwei am hoechsten gelisteten Atributen
#' zwei Kategorien gleich ist,  5% Schwelle, dann gilt die Regel M>O>A>I
#'
#' Total Strength als zweite Masszahl gibt an wie hoch der Anteil an bedeutenden
#' Produktmerkmalen ist.
#'
#' Category Strength ist eine Masszahl die die angibt ob eine Anforderung nur in
#' eine Kategorie gehoert
#'
#' CS plus	Indx Positiv  CS.plus=  (A+O)/(A+O+M+I)
#'
#' CS minus	Index Negativ CS.minus= (O+M)/(A+O+M+I)
#'
#' Chi-Test	Eigendlich unsinn Testet ob Verteilung von M, a, O und I gleich ist
#'
#' Fong-Test Vergleich der zwei Haeufigsten-Kategorien gegenueber der Gesammtzahl
#' Ergebnis ist entweder ein signifikente oder ein nicht signifikente Verteilung.
#'
#'
#'  \subsection{Basis-Faktoren (Mussfaktoren)}{
#'  Basis-Merkmale (M...Mustbe) werden vom Kunden Vorausgesetzt schaffen
#'  unzufriedenheit wenn sie nicht vorhanden sind.
#'  }
#'   \subsection{Leistungs- Faktoren}{
#'  Leistungs-Merkmale (O...One-dimensional) werden vom Kunden verlangt
#'  }
#'  \subsection{Begeisterung-Faktoren}{
#'  Begeisterungs-Merkmale (A...Attractive) Kunde rechnet nicht damit hebt das
#'  Produkt vom Konkurenten ab.
#'  }
#'  \subsection{Unerhebliche- Faktoren }{
#'  Unerhebliche-Merkmale (I...Indifferent) werden vom Kunden Ignoriert.
#'  }
#' @param X data.frame mit parweisen Kano Fragen
#' @param grouping Dataframe mit Gruppen
#' @param type Fragetype entwerer vollstaendig (5) oder gekuerzt (3)
#' @param umcodieren logical False
#' @param rm_Q Remove Q Kategorien Q enfernen Anzahl an erlaubten Qs
#' @param rm_I Remove I Kategorien I enfernen Anzahl an erlaubten Is
#' @param methode eie sind die Items geornet
#' @param vars_func Welche Items sind die Funktionalen
#' @param vars_dysfunc Welche Items sind die Dys-Funktionalen
#' @param ...
#' \itemize{
#'    \item \code{type=c(1,2)}  Streudiagram (1) oder Balken (2)
#'    \item \code{groups= NULL} wenn ein Dataframe uebergeben dan sind das die Gruppen
#'    \item \code{legend.position = list(x="right", y=NULL) } - Beim Streudiagram die
#'    Legende bei Barplot \code{scale=list()}
#'    \item \code{txt.bg=list(m="M",i= "I", o="O", a="A")} Beim Streudiagram der
#'    Hintergrundtext
#'    \item \code{jitter=TRUE} Beim Streudiagram Verrauschen der Items (wegen besserer
#'    lesbarkeit)
#'    \item \code{col.bg="gray95"}
#'    \item \code{col = NA}
#'    \item \code{cex=0.70}
#'    \item \code{cex.items=cex*1}
#'
#'    \item \code{cex.lab=cex*1.07}
#'    \item \code{cex.legend=cex*1.1}
#'    \item \code{ylab="Zufriedenheitsstiftung (CS+)"}
#'    \item \code{xlab="Unzufriedenheitsstiftung (CS-)"}
#'    \item \code{CS.minus = "CS minus"}
#'    \item \code{center.axis=FALSE}
#'    \item \code{use.labels=FALSE  Zahlen oder Text als Beschriftung bei den Streudiagrammen}
#'    \item \code{use.total.strength =TRUE  groese der Schrift als Indikator fuer Total Strenght}
#'    \item \code{use.categorie=TRUE  M, O, A, oder i mit in die Labels ausgeben }
#'
#' }
#' @return data.frame mit der Kano-Kodierung
#' @export
#' @examples
#' library(stp5)
#'  Start("")
#'  head(DF<-GetData("   Geschlecht f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12 f13 f14 f15 f16 f17 f18
#'  1           w  1  1  1  2  1  3  1  4  1   5   5   1   4   1   5   2   5   1
#'  2           w  2  1  2  2  2  3  2  4  2   5   2   5   1   3   2   5   2   5
#'  3           m  3  1  3  2  3  3  3  4  3   5   5   1   4   1   5   2   5   1
#'  4           m  4  1  4  2  4  3  4  4  4   5   5   1   4   1   5   2   5   1
#'  5           w  5  1  5  2  5  3  5  4  5   5   5   1   4   1   5   2   5   1
#'  6           w  NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#'  7           m  2  5  1  5  2  5  1  5  1   5   2   5   1   5   1   5   2   5
#'  8           w  2  4  2  5  1  3  1  3  2   5   3   3   1   3   1   4   1   3
#'  9           m  2  4  2  5  2  3  1  3  2   5   1   3   1   3   2   4   3   3
#'  10          m  2  5  1  5  1  4  1  5  2   5   1   4   1   5   2   5   1   3
#'  11          w  1  5  2  5  1  4  1  4  1   5   1   4   2   5   2   5   1   4
#'  12          m  2  5  2  5  2  5  2  4  1   5   1   3   1   4   2   5   3   3
#'  13          w  2  5  2  5  3  3  1  5  2   5   1   5   1   5   3   3   3   3
#'  14          m  2  5  1  5  1  5  2  5  2  NA   1   5   1   5   2   5   1   5
#'  15          w  1  4  2  5  1  3  2  5  1   5   1   3   1   4   2   5   1   3
#'  16          w  1  4  2  5  2  5  2  5  1   5   1   4   2   5   2   5   1   3
#'  17          w  1  5  2  5  1  5  1  5  2   5   1   4   1   5   2   5   1   4
#'  18          w  1  5  2  5  2  5  2  5  1   5   1   2   1   5   1   5   3   2
#'  19          m  1  5  2  5  2  5  2  5  2   5   1   4   2   5   2   5   1   3
#'  20          w  2  5  2  5  2  5  2  5  2   5   1   3   2   5   2   5   1   3
#'  21          w  2  5  2  5  1  5  2  5  2   5   1   5   2   5   2   5   1   3
#'  22          m  1  3  2  5  1  3  2  5  2   5   1   3   2   5   1   3   1   3
#'  23          w  1  5  2  5  3  3  2  5  2   5   1   4   2   5   2   5   1   4
#'  24          w  2  4  2  5  2  3  2  4  2   5   3   3   2   4   2   4   1   3
#'  25          m  2  4  1  5  1  4  2  4  1   5   1   3   3   3   2   4   1   3
#'  26          w  1  5  1  5  1  3  1  5  1   5   1   3   1   5   1   5   3   3
#'  27          w  1  5  2  5  3  3  1  4  2   4   1   3   1   3   3   3   5   1
#'  28          w  2  5  2  5  1  4  2  5  1   5   1   3   2   4   2   5   4   1
#'  29          w  2  5  2  5  2  4  2  4  2   5   1   4   2   4   1   5   1   4
#'  30          m  1  5  2  5  1  3  1  3  1   4   1   3   1   3   1   3   1   3
#'  31          m  1  3  2  5  1  4  1  4  3   3   5   2   2   4  NA  NA   1   3
#'  "))
#'
#'  DF<- upData(DF,  labels=c(f1="f1.motiviert"
#'                            ,f3="f2.anpasst"
#'                            ,f5="f3.programm"
#'                            ,f7="f4.info"
#'                            ,f9="f5.text"
#'                            ,f11="f6.reagieren"
#'                            ,f13="f7.text"
#'                            ,f15="f8.Arzt"
#'                            ,f17="f9.Telefonanruf"
#'  ))
#'
#'#  match(DF$f1, 1) & match(DF$f2, 5)
#'
#'
#'  kano_res <- KANO2(DF[, -1], grouping = DF[1],
#'        # type = 5,
#'        # umcodieren = FALSE,
#'        # rm_Q = 999,
#'        # rm_I = 999
#'  )
#'
#'
#'  kano_res <- KANO2( .~ Geschlecht, DF)
#'
#'
#'  names(kano_res)
#'  #"value"      "scors"      "data"       "molten"     "formula"  "removed"    "Attributes" "answers"
#'
#'  APA2(kano_res, caption = "Ueber Objekt")
#'  APA2(value ~ variable, kano_res, caption = "Ueber formula")
#'  APA2(value ~ variable + Geschlecht , kano_res, caption = "Ueber formula +Gechlecht")
#'
#' #Kano_Auswertung( kano_res$molten, kano_res$formula)
#'
#'
#' library(stp25plot)
#'  Kano_plot(value ~ variable + Geschlecht, kano_res,
#'      main = "kano-analysis",
#'      ylab = "Prozent",
#'      type = 2, col = 2:7,
#'      prop.table = TRUE)
#'
#'  Kano_plot(value ~ variable + Geschlecht, kano_res,
#'      main = "kano-analysis",
#'      center.axis = TRUE)
#'
#' #   windows(8, 8)
#  #Kano_plot(value ~ variable, kano_res, main = main, ylab = "CS+", xlab = "CS-")  #   center.axis = F,
#   #SaveData(paste0("txt ",plot_name))
#   #windows(8, 8)
#   #Kano_plot(value ~ variable, kano_res, main = main, use.labels = FALSE, ylab = "CS+", xlab = "CS-")
#   #SaveData(paste0("nr ",plot_name))
#'
#'  End()
#'
NULL

#' @rdname Kano
#' @export
KANO2<-function(X,
                grouping  =NULL,
                type = 5, # langversion oder Kurzversion
                umcodieren = FALSE,
                rm_Q = 10000,
                rm_I = 10000,
                methode = 1,
                vars_func = NULL,
                vars_dysfunc = NULL,
                data=grouping,
                subset=NULL, na.action=NULL,
                ...
){



  #library(plyr) is.formula
  # function (x)
  #   inherits(x, "formula")
  #-- Neu  23.08.2014 10:11:41 Formula funktionalitae
  if(inherits(X, "formula")){

    Data <- Formula_Data(X, data, subset, na.action)
    X <- Data$Y_data
    grouping <- Data$X_data
  }






  #-- Reihenfolge der Items default is func dfunk func dfunc func ....
  n <- ncol(X)
  if(n %% 2 != 0) return("Die Anzahl dan Functionalen und Dysfunc Items ist ungleich")
  if(!is.null(vars_func) & !is.null(vars_dysfunc)){X<- X[,c(rbind(vars_func, vars_dysfunc))]
  }else{if(methode == 2){X <- X[,c(rbind(1:(n/2), (n/2+1):n))]}}

  #-  altlast vorher gab es kein grouping
  if(!is.null(grouping) & is.numeric(grouping)){
    if(is.logical(type)) umcodieren <- type
    type <- grouping
    grouping<-NULL
  }

  # print(head(X))

  ANS<-NULL
  vars<-seq(1, n, by=2)
  vars_func <-seq(1, n ,by=2)
  vars_dysfunc <-seq(2, n ,by=2)

  nams <- Hmisc::label(X[, vars])
  nams <- ifelse(nams == ""
                 , gsub(" $","", gsub("[\\._]+", " ", names(nams)), perl=T )
                 , nams )

  if(is.factor(X[,1])) X<-sapply(X, as.numeric)
  Scors<-X # Scors sind eine Alternative Codierung zum zweck der transformierung zu einer metrischen Skala
  Scors[,vars_func] <- sapply(Scors[,vars_func]
                              , function(a)  as.numeric(as.character(factor(a, 1:5, c(1,.5,0,-.25,-.5)))))
  Scors[,vars_dysfunc] <- sapply(Scors[,vars_dysfunc]
                                 , function(a)  as.numeric(as.character(factor(a, 1:5, c(-.5,-.25,0,.5,1)))))
  Scors<- as.data.frame(Scors)
  names(Scors)<- paste0(names(Scors), ".s")

  for(i in vars){
    # cat(i)
    funk <- X[,i]
    dysf <- X[,i+1]
    if(type==3){ #-- Kurzversion
      myrow  <-  ifelse( X[,i]==1 & X[,i+1]==1, "A"
                         ,ifelse( X[,i]==1 & X[,i+1]==2, "A"
                                  ,ifelse( X[,i]==1 & X[,i+1]==3, "O"

                                           ,ifelse( X[,i]==2 & X[,i+1]==1, "I"
                                                    ,ifelse( X[,i]==2 & X[,i+1]==2, "I"
                                                             ,ifelse( X[,i]==2 & X[,i+1]==3, "M"

                                                                      ,ifelse( X[,i]==3 & X[,i+1]==1, "I"
                                                                               ,ifelse( X[,i]==3 & X[,i+1]==2, "I"
                                                                                        ,ifelse( X[,i]==3 & X[,i+1]==3, "M" ,NA)))))))))
    }else if(type==5){
      x1 <- X[,i]
      if (umcodieren) x2 <-  type + 1 - X[,i+1]
      else x2 <-  X[,i+1]

      myrow<-ifelse(           x1==1 & x2==1, "Q"
                               ,ifelse( x1==1 & x2==2, "A"
                                        ,ifelse( x1==1 & x2==3, "A"
                                                 ,ifelse( x1==1 & x2==4, "A"
                                                          ,ifelse( x1==1 & x2==5, "O"

                                                                   ,ifelse( x1==2 & x2==1, "R"
                                                                            ,ifelse( x1==2 & x2==2, "I"
                                                                                     ,ifelse( x1==2 & x2==3, "I"
                                                                                              ,ifelse( x1==2 & x2==4, "I"
                                                                                                       ,ifelse( x1==2 & x2==5, "M"

                                                                                                                ,ifelse( x1==3 & x2==1, "R"
                                                                                                                         ,ifelse( x1==3 & x2==2, "I"
                                                                                                                                  ,ifelse( x1==3 & x2==3, "I"
                                                                                                                                           ,ifelse( x1==3 & x2==4, "I"
                                                                                                                                                    ,ifelse( x1==3 & x2==5, "M"

                                                                                                                                                             ,ifelse( x1==4 & x2==1, "R"
                                                                                                                                                                      ,ifelse( x1==4 & x2==2, "I"
                                                                                                                                                                               ,ifelse( x1==4 & x2==3, "I"
                                                                                                                                                                                        ,ifelse( x1==4 & x2==4, "I"
                                                                                                                                                                                                 ,ifelse( x1==4 & x2==5, "M"

                                                                                                                                                                                                          ,ifelse( x1==5 & x2==1, "R"
                                                                                                                                                                                                                   ,ifelse( x1==5 & x2==2, "R"
                                                                                                                                                                                                                            ,ifelse( x1==5 & x2==3, "R"
                                                                                                                                                                                                                                     ,ifelse( x1==5 & x2==4, "R"
                                                                                                                                                                                                                                              ,ifelse( x1==5 & x2==5, "Q" ,NA)))))))))))))))))))))))))
    }
    ANS<-cbind(ANS,myrow)
  }

  ##  ifelse(any(  nams ==""  ) ,paste("F",1:(ncol(X)/2),sep=""), nams)
  # nzahlan<- ncol(ANS)*.60+1   # mehr als 60%
  Errorrs <- rep(FALSE, n)
  if( rm_Q < 10000 | rm_I < 10000){
    Errorrs<- apply(ANS, 1,
                    function(x) any(c(sum(x=="Q", na.rm = TRUE)>rm_Q ,sum(x=="I", na.rm = TRUE)>rm_I )))

    ANS[which(Errorrs),] <-  NA
    Scors[which(Errorrs),] <-NA

  }


  ANS<-as.data.frame(lapply( as.data.frame(ANS), function(x) factor(x, levels= Cs(M,O,A,I,R,Q )) ))
  colnames(ANS)<- nams
  # ANS<-as.data.frame(lapply( as.data.frame(ANS), function(x) factor(x, levels= Cs(M,O,A,I,R,Q )) ))





  #ANS ##   ANS[which(!Errorrs),]
  if(is.null(grouping)){
    data <-  ANS
    molten <- Melt2(data)
    fm<- value~variable
  }else{
    data <- cbind(grouping, ANS)
    molten <- Melt2(data, id.vars=1:ncol(grouping))
    fm<- formula(paste("value~", paste(names(molten)[-length(molten)], collapse ="+") ))
  }


 #print(nrow(data))
  res<-list(value=ANS,
            scors=Scors,
            data= data,
            molten=molten,
            formula= fm,
            removed=Errorrs,
            N=nrow(data),
            Attributes= c("Must-be","One-dimensional", "Attractive","Indifferent","Reverse", "Questionable"),
            answers= c( "I like it that way", "It must be that way","I am neutral","I can live with it that way","I dislike it that whay")[1:type]

  )
  class(res)<-"Kano"
  return(res)
}


#' @rdname Kano
#' @export
Kano <- KANO2


#' @rdname Kano
#' @export
Kano_Auswertung <- function(data,
                            formula,
                            digits= options()$stp4$apa.style$prozent$digits[1],
                            prop.table=TRUE,
                            ...,
                            var_names=c("N","Total",
                                        "M","O","A","I","R","Q",
                                        "max Category","M>O>A>I","Total Strength","Category Strength",
                                        "CS plus","CS minus","Chi-Test","Fong-Test" )


){


  Fong<-function(x){
    if(length(x)==0) {"ns"}
    else{
      x<-as.numeric(x)
      a<-sort(x)[4]
      b<-sort(x)[3]
      n<-sum(x)
      ifelse( abs(a-b)<1.65*sqrt(((a+b)*(2*n-a-b)) / (2*n) ), "sig.","ns") }
  }
  kano_aggregate <- function(x) {

    x   <-   factor(x, levels= Cs(M,O,A,I,R,Q))
    tab <-   table(x)

    chi <-   if(length(x)==0) list(statistic=0, p.value=1)  else chisq.test(tab[1:4])
    proptab<-prop.table(tab)
    prop<- ffprozent(proptab*100)
        #paste0(ff(proptab*100, digits, options()$stp4$apa.style$prozent$lead.zero), "%")
    names(prop)<- names(tab)

    Kat <-   names( sort( tab, decreasing = TRUE))
    max.Kat<-Kat[1]
    Cat <-   as.numeric(diff(sort(proptab[c("O", "A", "M", "I", "R")], decreasing = TRUE)[2:1]))    # Category.Strength
    Tot <-   as.numeric(proptab["A"]+proptab["O"]+proptab["M"] )        # Total.Strength
    # q.value <-  (tab["A"]*3+ tab["O"]*2+ tab["M"])/(tab["A"]+ tab["O"]+ tab["M"])
    Auswertregel <-  ifelse(Cat > 0.06, max.Kat
                            ,ifelse( any(Kat[1:2]=="M"), "M"
                                     ,ifelse(any(Kat[1:2]=="O"), "O"
                                             ,ifelse(any(Kat[1:2]=="A"), "A"
                                                     ,ifelse(any(Kat[1:2]=="I"), "I", NA))))  )

    #Auswertung nach (O + A + M) >< (I + Q + R)
    #Wenn M + A + O > I + Q + R, dann Max (M, A, O)
    #Wenn M + A + O < I + Q + R, dann Max (I, Q, R)


    #  print(proptab[c("M","O", "A",  "I")])
    # print(  sort(proptab[c("O", "A", "M", "I")], decreasing = TRUE)[2:1])
    #  print(Cat)
    c(N=length(x),
      n=length(na.omit(x)),
      if(prop.table) prop else tab
      ,max.Kat = max.Kat
      ,M.O.A.I = Auswertregel
      ,Total.Strength =   paste0(ff(Tot*100, digits, options()$stp4$apa.style$prozent$lead.zero), "%")
      ,Category.Strength=  paste0(ff(Cat*100, digits, options()$stp4$apa.style$prozent$lead.zero), "%")
      #,q.value=round(q.value,2)
      ,CS.plus=  round( as.numeric((tab["A"]+tab["O"])/(tab["A"]+tab["O"]+tab["M"]+tab["I"])),3)
      ,CS.minus= round( as.numeric((tab["O"]+tab["M"])/(tab["A"]+tab["O"]+tab["M"]+tab["I"])),3) *-1
      ,Chi=   paste0(round(chi$statistic, 2), ifelse(chi$p.value<0.05,"*",""))
      ,Fong=Fong(tab)
    )
  }

  ##-16-07-2016 Anderung die Formel mit . ist obsolet
  ## - workaround fue cast
  #--  cas ist ja aus dem packages reshape das nich mehr weiterentwicklt wird


  # y <- unlist(strsplit(gsub(" ", "", deparse(formula[[2L]])), split = "\\+"))
  # x <- unlist(strsplit(gsub(" ", "", deparse(formula[[3L]])), split = "\\+"))
  #
  # dotformula<- if (y[1] == "." | x[1] == ".") TRUE  else FALSE
  #
  # if(dotformula){
  #   if (!require("reshape")) {
  #     install.packages("reshape")
  #     require(reshape)
  #   }
  #   ans<- cast(data, formula, fun.aggregate = kano_aggregate)
  #   names(ans)[1]<- "Item"
  #   names(ans)[ (ncol(ans)-15): ncol(ans)] <- var_names
  #
  #
  # }else{
    ans<-  as.data.frame(aggregate(formula, data, FUN = kano_aggregate )    )
    #  print(str(ans))
    #  print(names(ans))
    n_names <-length(names(ans))
    ans_value <-  as.data.frame(ans[n_names]$value)
    names(ans_value) <- var_names
    ans <- cbind( ans[,-n_names], ans_value )
    names(ans)[1]<- "Item"

 # }

return(ans)
}







