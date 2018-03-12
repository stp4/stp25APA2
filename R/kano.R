#' Analyze Kano Type Items.
#' 
#' Die Funktion \code{Kano()} transformiert Kano-Fragebogen zur Kano-Kodierung
#' 
#' http://www.eric-klopp.de/texte/angewandte-psychologie/18-die-kano-methode
#' https://de.wikipedia.org/wiki/Kano-Modell
#' 
#'  \subsection{M Basis-Faktoren (Mussfaktoren)}{
#'  Basis-Merkmale (\strong{M}ustbe) werden vom Kunden Vorausgesetzt schaffen
#'  Unzufriedenheit wenn sie nicht vorhanden sind.
#'  }
#'   \subsection{O Leistungs- Faktoren}{
#'  Leistungs-Merkmale (\strong{O}ne-dimensional) werden vom Kunden verlangt
#'  }
#'  \subsection{A Begeisterung-Faktoren}{
#'  Begeisterungs-Merkmale (\strong{A}ttractive) Kunde rechnet nicht damit hebt das
#'  Produkt vom Konkurrenten  ab.
#'  }
#'  \subsection{I Unerhebliche- Faktoren }{
#'  Unerhebliche-Merkmale (\strong{I}ndifferent) werden vom Kunden ignoriert.
#'  }
#'  \subsection{R Rueckweisende- Faktoren }{
#'  Ablehnende-Merkmale (\strong{R}) werden vom Kunden abgelehnt. Fuehren bei Vorhandensein zu Unzufriedenheit, bei Fehlen jedoch nicht zu Zufriedenheit.
#'  }

#' 
#' 
#' \strong{Kodierung}
#' 
#' Das würde mich sehr freuen (1)
#' Das setze ich voraus (2)
#' Das ist mir egal (3)
#' Das könnte ich in Kauf nehmen (4)
#' Das würde mich sehr stören (5)
#'
#'
#'
#'
#' func=1 & dysf=1 = "Q"
#' 
#' func=1 & dysf=2 = "A"
#' 
#' func=1 & dysf=3 = "A"
#' 
#' func=1 & dysf=4 = "A"
#' 
#' func=1 & dysf=5 = "O"
#' 
#' func=2 & dysf=1 = "R"
#' 
#' func=2 & dysf=2 = "I"
#' 
#' func=2 & dysf=3 = "I"
#' 
#' func=2 & dysf=4 = "I"
#' 
#' func=2 & dysf=5 = "M"
#' 
#' func=3 & dysf=1 = "R"
#' 
#' func=3 & dysf=2 = "I"
#' 
#' func=3 & dysf=3 = "I"
#' 
#' func=3 & dysf=4 = "I"
#' 
#' func=3 & dysf=5 = "M"
#' 
#' func=4 & dysf=1 = "R"
#' 
#' func=4 & dysf=2 = "I"
#' 
#' func=4 & dysf=3 = "I"
#' 
#' func=4 & dysf=4 = "I"
#' 
#' func=4 & dysf=5 = "M"
#' 
#' func=5 & dysf=1 = "R"
#' 
#' func=5 & dysf=2 = "R"
#' 
#' func=5 & dysf=3 = "R"
#' 
#' func=5 & dysf=4 = "R"
#' 
#' func=5 & dysf=5 = "Q"
#' 
#' 
#' M O A I R Q Heufigkeit
#'
#' max Category
#'
#' M>O>A>I  max Category mit Hierarchie  M Wichtiger als O usw.
#' also wen der Unterschied zwischen den zwei am hoechsten gelisteten Attributen 
#' zwei Kategorien gleich ist,  5% Schwelle, dann gilt die Regel M>O>A>I
#'
#' Total Strength als zweite Masszahl gibt an wie hoch der Anteil an bedeutenden
#' Produktmerkmalen ist.
#'
#' Category Strength ist eine Masszahl die die angibt ob eine Anforderung nur in
#' eine Kategorie gehoert
#'
#' CS plus	Index Positiv  CS.plus=  (A+O)/(A+O+M+I)
#'
#' CS minus	Index Negativ CS.minus= (O+M)/(A+O+M+I)
#'
#' Chi-Test	Eigentlich  Unsinn Testet ob Verteilung von M, A, O und I gleich ist. wird aber in wissenschaftlichen Arebitengerne angegeben.
#'
#' Fong-Test Vergleich der zwei Haeufigsten-Kategorien gegenueber der Gesamtzahl Ergebnis ist entweder ein signifikante oder ein nicht signifikante Verteilung.
#'
#'
#' @param x,data Daten mit/oder formula
#'
#' @param type Fragetype entweder  vollstaendig (5) oder gekuerzt (3)
#' @param umcodieren logical False
#' @param rm_Q Remove Q Kategorien Q entfernen  Anzahl an erlaubten Qs
#' @param rm_I Remove I Kategorien I entfernen  Anzahl an erlaubten Is
#' @param methode weie sind die Items geornet default = 1  (func dfunk func dfunc func)
#' @param vars_func,vars_dysfunc Welche Items sind die Funktionalen/Dys-Funktionalen
#'  
#' @param X,grouping uebergabe der Daten paarweisen beides sind data.frames
#' @param ... nicht benutzte Argumente
#' @return Liste mit:
#' 
#' data: data mit der Kano-Kodierung.
#' 
#' molten: Daten-Lang
#' 
#' scors:  Scors sind eine Alternative Codierung zum Zweck der Transformierung zu einer metrischen Skala.
#' 
#' formula, removed=Errorrs, N, attributes, answers
#' @export
#' @examples 
#' 
#' #  require(stpvers)
#' #   Projekt("html")
#' 
#' kano_labels <- c( "I like it that way", 
#'                   "It must be that way",
#'                   "I am neutral",
#'                   "I can live with it that way",
#'                   "I dislike it that whay")
#'                   
#'  
#' DF<-GetData("   Geschlecht Edu f1 d1 f2 d2 f3 d3 f4 d4 f5  d5  f6  d6  f7  d7  f8  d8  f9  d9  f10 d10
#'                  1           w  med 1  5  1  2  1  3  1  5  1   5   5   1   3   3   5   2   5   1   5   2
#'                  2           w  med 1  5  2  5  2  3  1  5  1   5   2   5   3   3   2   5   2   5   5   2
#'                  3           m  med 1  5  3  5  1  5  3  4  1   5   5   1   3   3   5   2   5   1   5   2
#'                  4           m  med 1  5  4  2  1  5  4  4  1   5   5   1   3   3   5   2   5   1   5   2
#'                  5           w  med 1  5  5  5  5  3  1  5  1   5   5   1   3   3   5   2   5   1   5   2
#'                  6           w  med NA NA NA NA NA NA NA NA NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA  NA
#'                  7           m  med 1  5  1  5  2  5  1  5  1   5   2   5   3   3   1   5   2   5   5   2
#'                  8           w  med 1  5  2  5  1  3  1  5  1   5   3   3   3   3   1   4   1   3   5   2
#'                  9           m  med 1  5  2  5  2  3  1  3  1   5   1   3   3   3   2   4   3   3   5   2
#'                  10          m  med 1  5  1  5  1  5  1  5  1   5   1   4   3   3   2   5   1   3   5   2
#'                  11          w  med 1  5  2  5  1  4  1  5  1   5   1   4   3   3   2   5   1   4   5   2
#'                  12          m  med 1  5  2  5  1  5  2  4  1   5   1   3   3   4   2   5   3   3   5   2
#'                  13          w  med 1  5  2  5  3  3  1  5  2   5   1   5   3   3   3   3   3   3   5   2
#'                  14          m  med 1  5  1  5  1  5  2  5  2  NA   1   5   3   3   2   5   1   5   5   2
#'                  15          w  med 1  5  2  5  1  3  1  5  1   5   1   3   3   3   2   5   1   3   5   2
#'                  16          w  low 1  5  2  5  2  5  2  5  1   5   1   4   3   3   2   5   1   3   5   2
#'                  17          w  low 1  5  2  5  1  5  1  5  2   5   1   4   3   3   2   5   1   4   5   2
#'                  18          w  low 2  5  2  5  2  5  2  5  1   5   1   2   3   3   1   5   3   2   5   2
#'                  19          m  low 1  5  2  5  1  5  2  5  2   5   1   4   2   3   2   5   1   3   5   2
#'                  20          w  low 2  5  2  5  2  5  2  5  2   5   1   3   3   3   2   5   1   3   5   2
#'                  21          w  low 2  5  2  5  1  5  2  5  2   5   1   5   1   3   2   5   1   3   5   2
#'                  22          m  low 1  3  2  5  1  5  2  5  2   5   1   3   3   3   1   3   1   3   5   2
#'                  23          w  low 1  5  2  5  3  3  2  5  2   5   1   4   1   3   2   5   1   4   5   2
#'                  24          w  low 2  4  2  5  2  3  2  4  2   5   3   3   3   4   2   4   1   3   5   2
#'                  25          m  hig 2  4  1  5  1  5  2  4  1   5   1   3   3   5   2   4   1   3   5   2
#'                  26          w  hig 1  5  1  5  1  3  1  5  1   5   1   3   1   5   1   5   3   3   5   2
#'                  27          w  hig 1  5  2  5  3  3  1  4  2   4   1   3   3   5   3   3   5   1   5   2
#'                  28          w  hig 2  5  2  5  1  4  2  5  1   5   1   3   3   5   2   5   4   1   5   2
#'                  29          w  hig 2  5  2  5  2  4  2  4  2   5   1   4   1   5   1   5   1   4   5   2
#'                  30          m  hig 1  5  2  5  1  5  1  3  1   4   1   3   1   5   1   3   1   3   5   2
#'                  31          m  hig 1  1  2  1  1  5  1  4  3   3   5   2   3   5  NA  NA   1   3   5   2
#'                  ")
#' 
#' 
#' DF<- upData2(DF,  labels=c(f1="Fahreigenschaften"
#'                            ,f2="Sicherheit"
#'                            ,f3="Beschleunigung"
#'                            ,f4="Verbrauch"
#'                            ,f5="Lebensdauer"
#'                            ,f6="Sonderausstattung"
#'                            ,f7="Schiebedach"
#'                            ,f8="Rostschutz"
#'                            ,f9="Design"
#'                            , f10= "Rostflecken"
#' ))
#'  
#' #  match(DF$f1, 1) & match(DF$f2, 5)
#' 
#' DF %>% Tabelle(Geschlecht, Edu)
#' 
#' 
#' kano_res1 <-  Kano( ~ . , DF[-c(1,2)])
#' APA2(kano_res1, caption = "Einzeln")
#' stp25APA2:::Kano_Auswertung(kano_res1, rnd_output=FALSE)
#' # kano_plot(kano_res1)
#' 
#' # library(lattice)
#' # x<-data.frame (xtabs(~ value+variable, kano_res1$molten ))
#' #   barchart(Freq ~value|variable, x, origin=0)
#' # 
#' DF[-c(1,2)] <- dapply2(DF[-c(1,2)], function(x) factor( x, 1:5, kano_labels))
#' DF %>% Tabelle2(f1,d1)
#' 
#' kano_res1 <-  Kano( ~ . , DF[-c(1,2)])
#' APA2(kano_res1, caption = "Einzeln")
#' 
#' 
#' kano_res <- Kano( .~ Geschlecht, DF[-2])
#' APA2(kano_res, caption = "Gruppe")
#' APA2(kano_res, caption = "Gruppe", include.percent=FALSE)
#' # kano_plot(kano_res,
#' #           legend.position = list(x = .75, y = 1),
#' #           #legend.title= "HAllo",
#' #           cex.legend=1)
#' 
#' kano_res <- Kano( .~ Geschlecht + Edu, DF )
#' stp25APA2:::Kano_Auswertung( kano_res, rnd_output=FALSE)
#' 
#' 
#' 
#' ## End()

Kano <-   function(x, ...) {
  UseMethod("Kano")
}

#' @rdname Kano
#' @export
Kano.data.frame <- function(x, na.action=na.pass, ...){
  
  Data <- Formula_Data(~., x, subset, na.action)
  X <- Data$Y_data
  grouping <- Data$X_data
  
  Kano_default(X, grouping, ...)
  
}


#' @param subset  an Formula_Data
#' @param na.action NA's entfernen odere behalten default ist na.pass()
#'
#' @rdname Kano
#' @export
Kano.formula <- function(x ,data, subset, na.action=na.pass, ...){
    Data <- Formula_Data(x, data, subset, na.action)
    X <- Data$Y_data
    grouping <- Data$X_data
    Kano_default(X, grouping, ...)
    
}
Kano_default<-function(X,
                grouping  = NULL, 
                type = 5, # langversion oder Kurzversion
                umcodieren = FALSE,
                rm_Q = 10000,
                rm_I = 10000,
                methode = 1,
                vars_func = NULL,
                vars_dysfunc = NULL,
                ...
){
 
  n <- ncol(X)
  kano_levels <-  c("M", "O", "A", "I", "R", "Q")
  attributes <- c(
    "Must-be",    "One-dimensional",
    "Attractive",    "Indifferent",
    "Reverse",    "Questionable"
  )
  answers <- c(
    "I like it that way",    "It must be that way",
    "I am neutral",    "I can live with it that way",
    "I dislike it that whay"
  )
  
  if(n %% 2 != 0) return("Die Anzahl dan Funktionalen und Dysfunktionalen Items ist ungleich!")
  
  if (!is.null(vars_func) &
      !is.null(vars_dysfunc)) {
    X <- X[, c(rbind(vars_func, vars_dysfunc))]
  } else{
    if (methode == 2) {
      X <- X[, c(rbind(1:(n / 2), (n / 2 + 1):n))]
    }
  }
  
  ANS <- NULL
  vars <- seq(1, n, by = 2)
  vars_func <- seq(1, n , by = 2)
  vars_dysfunc <- seq(2, n , by = 2)
  
  
  nams <- Hmisc::label(X[, vars])
  nams <- ifelse(nams == ""
                 , gsub(" $", "", gsub("[\\._]+", " ", names(nams)), perl = T)
                 , nams)
  # nams <- GetLabelOrName(X[, vars])
  if (is.factor(X[, 1]))
  { 
    cat("\nto numeric")
    print(levels(X[,1]))
    X <- sapply(X, as.numeric)
  }
  
 
  if(any(sapply(X, function(x) min(x) <1 | max(x)>type) )){
    print(lapply(X, function(x) range(x, na.rm=TRUE)) )
    stop("Zu viele Levels!")
  } 
  
  Scors <-  X 
  Scors[, vars_func] <- sapply(Scors[, vars_func]
                               , function(a)
                                 as.numeric(as.character(factor(
                                   a, 1:5, c(1, .5, 0, -.25, -.5)
                                 ))))
  Scors[, vars_dysfunc] <- sapply(Scors[, vars_dysfunc]
                                  , function(a)
                                    as.numeric(as.character(factor(
                                      a, 1:5, c(-.5, -.25, 0, .5, 1)
                                    ))))
  Scors <- as.data.frame(Scors)
  names(Scors) <- paste0(names(Scors), ".s")
cat("\ntransform kano (type",type,")")
  for(i in vars){
    
    X_func <- X[,i]
    X_dysf <- X[,i+1]
     if (umcodieren) X_dysf <-  type + 1 - X_dysf
    if(type==3){ #-- Kurzversion
      myrow  <-  ifelse( X_func==1 & X_dysf==1, "A"
                ,ifelse( X_func==1 & X_dysf==2, "A"
                ,ifelse( X_func==1 & X_dysf==3, "O"
                ,ifelse( X_func==2 & X_dysf==1, "I"
                ,ifelse( X_func==2 & X_dysf==2, "I"
                ,ifelse( X_func==2 & X_dysf==3, "M"
                ,ifelse( X_func==3 & X_dysf==1, "I"
                ,ifelse( X_func==3 & X_dysf==2, "I"
                ,ifelse( X_func==3 & X_dysf==3, "M" 
                ,NA)))))))))
    }else if(type==5){
      
      myrow<-  ifelse(X_func==1 & X_dysf==1, "Q"
             ,ifelse( X_func==1 & X_dysf==2, "A"
             ,ifelse( X_func==1 & X_dysf==3, "A"
             ,ifelse( X_func==1 & X_dysf==4, "A"
             ,ifelse( X_func==1 & X_dysf==5, "O"
             ,ifelse( X_func==2 & X_dysf==1, "R"
             ,ifelse( X_func==2 & X_dysf==2, "I"
             ,ifelse( X_func==2 & X_dysf==3, "I"
             ,ifelse( X_func==2 & X_dysf==4, "I"
             ,ifelse( X_func==2 & X_dysf==5, "M"
             ,ifelse( X_func==3 & X_dysf==1, "R"
             ,ifelse( X_func==3 & X_dysf==2, "I"
             ,ifelse( X_func==3 & X_dysf==3, "I"
             ,ifelse( X_func==3 & X_dysf==4, "I"
             ,ifelse( X_func==3 & X_dysf==5, "M"
             ,ifelse( X_func==4 & X_dysf==1, "R"
             ,ifelse( X_func==4 & X_dysf==2, "I"
             ,ifelse( X_func==4 & X_dysf==3, "I"
             ,ifelse( X_func==4 & X_dysf==4, "I"
             ,ifelse( X_func==4 & X_dysf==5, "M"
             ,ifelse( X_func==5 & X_dysf==1, "R"
             ,ifelse( X_func==5 & X_dysf==2, "R"
             ,ifelse( X_func==5 & X_dysf==3, "R"
             ,ifelse( X_func==5 & X_dysf==4, "R"
             ,ifelse( X_func==5 & X_dysf==5, "Q" 
             ,NA)))))))))))))))))))))))))
    }
   n<- sample.int(length(X_func))[1]
    
    cat("\nnr:", n,  names(X[i]), "|", names(X[i+1]), X_func[n],"+", X_dysf[n], "=", myrow[n] )
    ANS<-cbind(ANS,myrow)
  }

  Errorrs <- rep(FALSE, n)
  if (rm_Q < 10000 | rm_I < 10000) {
    Errorrs <- apply(ANS, 1,
                     function(x)
                       any(c(
                         sum(x == "Q", na.rm = TRUE) > rm_Q , sum(x == "I", na.rm = TRUE) > rm_I
                       )))
    
    ANS[which(Errorrs), ] <-  NA
    Scors[which(Errorrs), ] <- NA
  }


  ANS <-
    as.data.frame(lapply(as.data.frame(ANS), function(x)
      factor(x, levels = kano_levels)))
  colnames(ANS) <- nams
  # ANS<-as.data.frame(lapply( as.data.frame(ANS), function(x) factor(x, levels= Cs(M,O,A,I,R,Q )) ))
  #ANS ##   ANS[which(!Errorrs),]
  if (is.null(grouping)) {
    data <-  cbind(nr = 1:nrow(ANS), ANS)
    molten <- Melt2(data, id.vars=1)
    fm <- value ~ variable
  } else{
    data <- cbind(nr = 1:nrow(ANS), grouping, ANS)
    molten <- Melt2(data, id.vars = 1:(ncol(grouping)+1))
    fm <-
      formula(paste("value~", paste(c(names(grouping), "variable"), collapse ="+")))
  }

 molten$value <- factor(molten$value, kano_levels)
 res<-list(data= data, 
            molten=molten,
            scors=Scors,
            formula= fm,
            removed=Errorrs,
            N=nrow(data),
            attributes=  attributes,
            answers= answers[1:type])
  class(res)<-"Kano"
  
 res 
}


 
#' @rdname Kano
#' 
#' @param include.n,include.percent Anzahl, Prozent
#' @param include.total N und Total
#' @param include.test Fong und Chie-Test
#' @param rnd_output Intern fuer Plot bei FALSE ausgabe als Zahl
#' @param caption,note Ueberschrift
#' @param formula,data intern daten aus Kano-Objekt
#' @param digits Nachkommastellen
#' @param ... 
#' 
Kano_Auswertung <- function(x,
                      caption = "",
                      note = "",
                      formula = x$formula,
                      data = x$molten[-1],  # id, Group, Kano_func, Kano_dys, ... und id brauch ich nicht
                      digits = options()$stp4$apa.style$prozent$digits[1],
                      include.n=TRUE,
                      include.percent=TRUE,
                      include.total=TRUE,
                      include.test=TRUE,
                      rnd_output=TRUE,
                      ...) {
  var_names = c(
    n = "Total",
    M = "M",    O = "O",    A = "A",    I = "I",    R = "R",    Q = "Q",
    max.Kat = "max Category",    M.O.A.I = "M>O>A>I",
    Total.Strength = "Total Strength",
    Category.Strength = "Category Strength",
    CS.plus = "CS plus",    CS.minus = "CS minus",
    Chi = "Chi-squared Test",    Fong = "Fong-Test"
    )
  
  kano_kategorien <- c("M", "O", "A", "I", "R", "Q")
  
  Fong <- function(x) {
    if (length(x) == 0) {
      "ns"
    }
    else{
      x <- as.numeric(x)
      a <- sort(x)[4]
      b <- sort(x)[3]
      n <- sum(x)
      ifelse(abs(a - b) < 1.65 * sqrt(((a + b) * (2 * n - a - b)) / (2 *
                                                                       n)), "sig.", "ns")
    }
  }
  kano_aggregate <- function(x) {
    x     <-   factor(x, levels = kano_kategorien)
    tab   <-   table(x)
    proptab <- prop.table(tab)
    
    if (include.percent) {
      if (include.n) {
            myTab <- rndr_percent(as.vector(proptab* 100), as.vector(tab))
      } else{
            myTab <- rndr_percent(as.vector(proptab* 100))
      }
    } else{ 
            myTab <- as.vector(tab) 
      }
    
    names(myTab) <- names(tab)   
    
    Kat <-   names(sort(tab, decreasing = TRUE))
    max.Kat <- Kat[1]
    Cat <-
      as.numeric(diff(sort(proptab[c("O", "A", "M", "I", "R")], decreasing = TRUE)[2:1]))    # Category.Strength
    Tot <-
      as.numeric(proptab["A"] + proptab["O"] + proptab["M"])        # Total.Strength
    # q.value <-  (tab["A"]*3+ tab["O"]*2+ tab["M"])/(tab["A"]+ tab["O"]+ tab["M"])
    Auswertregel <- ifelse(Cat > 0.06,           max.Kat
                  , ifelse(any(Kat[1:2] == "M"), "M"
                  , ifelse(any(Kat[1:2] == "O"), "O"
                  , ifelse(any(Kat[1:2] == "A"), "A"
                  , ifelse(any(Kat[1:2] == "I"), "I"
                                               , NA
                  )))))
    
    #Auswertung nach (O + A + M) >< (I + Q + R)
    #Wenn M + A + O > I + Q + R, dann Max (M, A, O)
    #Wenn M + A + O < I + Q + R, dann Max (I, Q, R)
    
 
    res<- c(
     # N = length(x),
      n = length(na.omit(x)),
      myTab,
      max.Kat = max.Kat,
      M.O.A.I = Auswertregel,
      Total.Strength =   rndr_percent(Tot * 100),
      Category.Strength =  rndr_percent(Cat * 100),
      #,q.value=round(q.value,2)
      
      CS.plus =  round(as.numeric((tab["A"] + tab["O"]) / (
                 tab["A"] + tab["O"] + tab["M"] + tab["I"] )), 3),
      CS.minus = round(as.numeric((tab["O"] + tab["M"]) / (
                 tab["A"] + tab["O"] + tab["M"] + tab["I"])), 3) * -1
    
    )
    if(include.test) {
      if (length(x) == 0 | sum(tab[1:4])<12){
        res<- c(res, Chi= "n.a.", Fong = "n.a.")
        }else{
        chi <- chisq.test(tab[1:4])  
        res<- c(res, Chi = rndr_Chisq_stars(chi$statistic, chi$p.value),
                      Fong = Fong(tab))
        }
    }
    
    if(include.total) res  else res[-1]
      
  }
  
  
  kano_aggregate_num <- function(x) {
    x   <-   factor(x, levels = kano_kategorien)
    tab <-   table(x)
    proptab <- prop.table(tab)
    myTab <- as.vector(tab) 
    names(myTab) <- names(tab)   
    Kat <-   names(sort(tab, decreasing = TRUE))
    max.Kat <- Kat[1]
    Cat <-
      as.numeric(diff(sort(proptab[c("O", "A", "M", "I", "R")], decreasing = TRUE)[2:1]))    # Category.Strength
    Tot <-
      as.numeric(proptab["A"] + proptab["O"] + proptab["M"])        # Total.Strength
    # q.value <-  (tab["A"]*3+ tab["O"]*2+ tab["M"])/(tab["A"]+ tab["O"]+ tab["M"])
    Auswertregel <- ifelse(Cat > 0.06,           max.Kat
                  , ifelse(any(Kat[1:2] == "M"), "M"
                  , ifelse(any(Kat[1:2] == "O"), "O"
                  , ifelse(any(Kat[1:2] == "A"), "A"
                  , ifelse(any(Kat[1:2] == "I"), "I"
                    , NA
                  )))))
 
    
   c(
      n = length(na.omit(x)),
      myTab,
    max.Kat =  as.numeric(factor(max.Kat, levels = kano_kategorien))  ,
     M.O.A.I = as.numeric(factor(Auswertregel, levels = kano_kategorien)) ,
      Total.Strength =    Tot ,
      Category.Strength = Cat,
      CS.plus =  as.numeric((tab["A"] + tab["O"]) / (
        tab["A"] + tab["O"] + tab["M"] + tab["I"] )),
      CS.minus = (as.numeric((tab["O"] + tab["M"]) / (
        tab["A"] + tab["O"] + tab["M"] + tab["I"]))) * -1
    )
   
    
  }
  
  if(rnd_output){#Formatierte Ausgabe
  ans <-
    as.data.frame(aggregate(formula, data, FUN = kano_aggregate))
  n_names <- length(names(ans))
  #result ist eine liste
  ans_value <-  as.data.frame(ans[n_names]$value)
 
  var_names <-var_names[ intersect(names(var_names), names(ans_value) ) ] 
  # 
  # cat("\nvar_names\n")
  # print(names(ans_value))
  # print(var_names)
  # cat("\n---------\n")
  
  names(ans_value) <- var_names
  ans <- cbind(ans[-n_names], ans_value)
  prepare_output(ans, caption = caption, note = note)
  }
  else{
    ans <-
      as.data.frame(aggregate(formula, data, FUN = kano_aggregate_num))
    
    n_names <- length(names(ans))
    ans_value <-  as.data.frame(ans[n_names]$value)
    ans <- cbind(ans[-n_names], ans_value) 
    
    ans$max.Kat <- as.character(factor(ans$max.Kat, 1:length(kano_kategorien),  kano_kategorien))
    ans$M.O.A.I <- as.character(factor(ans$M.O.A.I, 1:length(kano_kategorien),  kano_kategorien))
    
    ans
  }
}



#' @rdname APA2
#' @export
APA2.Kano <- function(x, caption="", note="", ...) {
  
 ans <- Kano_Auswertung(x, caption=caption, note=note, ...)
 Output(ans)
 invisible(ans) 
  
}

#' @rdname APA2
#' @export
print.Kano <- function(x, ...) {
  cat("\n", names(x), "\n")
  print(x$formula)
  print(head(x$molten))
}



#' @param x Kano-Objekt
#' @param main Header
#' @param mar,xlim,ylim Plot Limits
#' @param legend.position,legend.title legend.position = list(x="right", y=NULL) 
#'  Beim Streudiagram die Legende bei Barplot  scale=list 
#' @param my.lines Grafik Element Halbkreis fuer Indiferent
#'  
#' @param col Farbe der Punkte 
#' @param txt.bg,cex.bg,col.bg   Hintergrund mit den Grossen Buchstaben
#' 
#' @param jitter Rauschen bei Ueberlappung
#' @param cex,cex.items,cex.lab,cex.legend Schriftgroesse
#' 
#' @param ylab,xlab x-y Beschrifung
#' @param center.axis center.axis=FALSE 
#' @param use.labels use.labels=FALSE  Zahlen oder Text als 
#' Beschriftung bei den Streudiagrammen
#' @param use.total.strength use.total.strength=TRUE  groese der Schrift 
#' als Indikator fuer Total Strenght
#' @param use.categorie use.categorie=TRUE  M, O, A, 
#' oder I mit in die Labels ausgeben 
#' @param use.points use.points = FALSE Punkte statt Text
#'
#' @rdname Kano
#' @export
kano_plot <- function(x,
                      main = "",
                      xlim = c(0, 1),
                      ylim = c(0, 1),
                      mar = c(0, 1, 2, 1),
                      legend.position = list(x = "right", y = NULL),
                      legend.title=NULL,
                      my.lines = "circle",
                      col.bg = "gray95",
                      col = NA,
                      txt.bg = list(m = "M",
                                    i = "I",
                                    o = "O",
                                    a = "A"),
                      cex.bg = 12,
                      jitter = TRUE,
                      cex = 1,
                      cex.items = cex * 1,
                      cex.lab = cex * 1.07,
                      cex.legend = cex * 1.1,
                      ylab = "Zufriedenheitsstiftung (CS+)",
                      xlab = "Unzufriedenheitsstiftung (CS-)",
                    
                      center.axis = FALSE,
                      use.labels = TRUE,
                      use.total.strength = TRUE,
                      use.categorie = TRUE,
                      use.points = FALSE,
                      ...) {
 
  data <- Kano_Auswertung(x,
                          rnd_output = FALSE)
  
  #geht nur mit einer Gruppe
  groups <- all.vars(x$formula[[3L]])
  print(groups)
  if (length(groups) > 1)
    groups <- groups[1]
  else
    groups <- NULL
   
  
  circle_plot <-
    function()
      symbols(
        0,
        0,
        circles = 0.4,
        add = TRUE,
        inches = FALSE,
        lwd = 2,
        fg = "gray85"
      )
  
  par(mar = mar)
  if (ncol(data) == 0) {
    # Leeres Blatt
    plot(
      1,
      1,
      pch = "",
      xlim = xlim,
      ylim = ylim,
      ann = FALSE,
      axes = FALSE,
      frame.plot = FALSE
    )
    
    mtext(main, cex = 1.5)
    # -- Hintergrund
    text(0.75, 0.25, txt.bg$m, cex = cex.bg, col = "gray95")
    text(0.25, 0.25, txt.bg$i, cex = cex.bg, col = "gray95")
    text(0.75, 0.75, txt.bg$o, cex = cex.bg, col = "gray95")
    text(0.25, 0.75, txt.bg$a, cex = cex.bg, col = "gray95")
    
    # grafische Hilfslienien
    if (!is.na(my.lines)) {
      if (my.lines == "circle") {
        circle_plot()
      } else {
        x1 <- 1:10000
        xx <- abs(sin(x1) / 2)
        yy <- abs(cos(x1) / 2)
        points(
          xx,
          yy,
          pch = ".",
          type = "p",
          col = "gray90",
          cex = 4
        )
        xx <- x1 / 10000 + 0.5
        yy <- sqrt(x1) / 100
        
        points(
          xx,
          yy,
          pch = ".",
          type = "p",
          col = "gray90",
          cex = 4
        )
        points(
          yy,
          xx,
          pch = ".",
          type = "p",
          col = "gray90",
          cex = 4
        )
      }
    }
    
    if (center.axis) {
      arrows(0.5, 1, 0.5, 0.01)
      text(
        -0.01,
        0.5,
        ylab,
        adj = c(NA, 0),
        pos = NULL,
        offset = 0.5,
        vfont = NULL,
        cex = cex.lab,
        srt = 90
      )
      arrows(0.01, 0.5, 1, 0.5)  #arrows(0,0.5,  1,0.5)
      text(
        0.5,-0.01,
        xlab,
        adj = c(NA, 1),
        pos = NULL,
        offset = 0.5,
        vfont = NULL,
        cex = cex.lab
      )
    } else {
      arrows(0, 0.01, 0, 1)
      text(
        -0.01,
        0.5,
        ylab,
        adj = c(NA, 0),
        pos = NULL,
        offset = 0.5,
        vfont = NULL,
        cex = cex.lab,
        srt = 90
      )
      arrows(0.01, 0, 1, 0)  #arrows(0,0.5,  1,0.5)
      text(
        0.5,-0.01,
        xlab,
        adj = c(NA, 1),
        pos = NULL,
        offset = 0.5,
        vfont = NULL,
        cex = cex.lab
      )  #  text(0.5, 0.49, 'Unzufriedenheitsstiftung (CS-)',adj=c(NA,1),cex=.75 )
    }
  } else {
    sadisfaction <- data$CS.plus
    dissadisfaction <-  data$CS.minus  * -1
    
    if (jitter) {
      set.seed(815)
      dissadisfaction <- jitter(dissadisfaction)
      sadisfaction <- jitter(sadisfaction)
    }
    
    mylevels <- NULL
    mycolors <-  1
    
    if (!is.null(groups)) {
      gr <- factor(data[, groups])
      n <- nlevels(gr)
      mylevels <- levels(gr)
      
      if (is.na(col[1]))
        mycolors <-
        RColorBrewer::brewer.pal(ifelse(n < 4, 3, n), "Dark2")[1:n]
      else
        mycolors <- col
      #  gr <- as.character(factor(gr, mylevels, mycolors[1:n]))
      
      #  print(gr)
    }
    
    
    
    Labels <- (data$variable)
    # print(Labels)
    plot(
      dissadisfaction,
      sadisfaction,
      pch = "",
      xlim = xlim,
      ylim = ylim,
      ann = FALSE,
      axes = FALSE,
      frame.plot = FALSE
    )
    
    mtext(main, cex = 1.5)
    
    # -- Hintergrung
    text(0.75, 0.25, txt.bg$m, cex = cex.bg, col = "gray95")
    text(0.25, 0.25, txt.bg$i, cex = cex.bg, col = "gray95")
    text(0.75, 0.75, txt.bg$o, cex = cex.bg, col = "gray95")
    text(0.25, 0.75, txt.bg$a, cex = cex.bg, col = "gray95")
    # grafische Hilfslienien
    if (!is.na(my.lines)) {
      if (my.lines == "circle") {
        circle_plot()
      } else {
        x1 <- 1:10000
        xx <- abs(sin(x1) / 2)
        yy <- abs(cos(x1) / 2)
        points(
          xx,
          yy,
          pch = ".",
          type = "p",
          col = "gray90",
          cex = 4
        )
        xx <- x1 / 10000 + 0.5
        yy <- sqrt(x1) / 100
        
        points(
          xx,
          yy,
          pch = ".",
          type = "p",
          col = "gray90",
          cex = 4
        )
        points(
          yy,
          xx,
          pch = ".",
          type = "p",
          col = "gray90",
          cex = 4
        )
      }
    }
    
    if (!is.null(mylevels))
      legend(
        x = legend.position$x,
        y = legend.position$y,
        mylevels,
        col = mycolors,
        pch = 16,
        box.lty = 3,
        box.col = "gray50",
        cex = cex.legend,
        title = if (is.null(legend.title)) groups else legend.title
      )
    
    # x und y Achse mit Beschriftung
    if (center.axis) {
      arrows(0.5, 1, 0.5, 0.01)
      text(
        -0.01,
        0.5,
        ylab,
        adj = c(NA, 0),
        pos = NULL,
        offset = 0.5,
        vfont = NULL,
        cex = cex.lab,
        srt = 90
      )
      
      arrows(0.01, 0.5, 1, 0.5)  #arrows(0,0.5,  1,0.5)
      text(
        0.5,-0.01,
        xlab,
        adj = c(NA, 1),
        pos = NULL,
        offset = 0.5,
        vfont = NULL,
        cex = cex.lab
      )
    } else {
      arrows(0, 0.01, 0, 1)
      text(
        -0.01,
        0.5,
        ylab,
        adj = c(NA, 0),
        pos = NULL,
        offset = 0.5,
        vfont = NULL,
        cex = cex.lab,
        srt = 90
      )
      arrows(0.01, 0, 1, 0)  #arrows(0,0.5,  1,0.5)
      text(
        0.5,-0.01,
        xlab,
        adj = c(NA, 1),
        pos = NULL,
        offset = 0.5,
        vfont = NULL,
        cex = cex.lab
      )  #  text(0.5, 0.49, 'Unzufriedenheitsstiftung (CS-)',adj=c(NA,1),cex=.75 )
    }
    
    
    cexy <- data$Total.Strength
    cexy <- cexy - min(cexy)
    if (use.total.strength)
      cex.items <-
      (cexy / max(cexy) / 2 + 0.5) * cex.items  ## use.total.strength =TRUE,
    if (!use.labels) {
      saveLabels <- Labels
      
      Labels <- as.numeric(factor(Labels))
      legend(
        x = 1,
        y = 0,
        xjust = 1,
        yjust = 0,
        paste(Labels, factor(saveLabels)),
        pch = NULL,
        box.lty = 0,
        box.col = NULL,
        cex = cex.legend * 0.7,
        title = NULL
      )
    }
    
    if (use.points) {
      symbols(
        dissadisfaction,
        sadisfaction,
        circles = cex.items / 30,
        add = TRUE,
        inches = FALSE,
        lwd = 1,
        fg = "gray60",
        bg = "gray90"
      )
    }
    
    print(Labels)
    # color <- ifelse(data$Total.Strengt <60, 'gray40', 'black' )
    if (use.categorie)
      #das ist noch Falsch
      Labels <-
      paste0(Labels, " (", data$M.O.A.I, ")")  #  =TRUE,
    
    #print(Labels)
    
    text(dissadisfaction,
         sadisfaction,
         Labels,
         cex = cex.items,
         col = mycolors)
  }
}



