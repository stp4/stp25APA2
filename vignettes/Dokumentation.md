<!-- html_document: --> <!--  number_sections: yes  -->

<!--  Valid themes include "default",  -->
<!--  "united","cosmo","lumen","flatly","sandstone", -->
<!--  "cerulean", "spacelab", "readable","yeti" -->
<!--   "journal","paper" ,  "simplex" -->
<!-- output: rmarkdown::html_vignette -->
<!-- http://rmarkdown.rstudio.com/html_document_format.html -->
<!-- C:/Users/wpete/Documents/R/win-library/3.4 -->
Einleitung
==========

Die von mir verwendeten statistischen Methoden basieren vor allem auf
den Empfehlungen von Bortz \[4\] sowie Sachs \[7\]. Die Darstellung der
Ergebnisse entspricht wissenschaftlicher Vorgaben, insbesondere halte
ich mich bei den Tabellen und Grafiken sowie der Darstellung
statistischer Kennzahlen an die Vorgaben von APA-Style\[2\]. (APA-Style
ist im Kontext sozialwissenschaftlicher Forschung quasi der
Gold-Standard hinsichtlich des Berichtens von Ergebnissen.)

Methoden
========

Deskriptive Auswertungen
------------------------

Je nach Kontext wird der **Mittelwert**, der **Median** oder die
**Häufigkeiten** berechnet, dabei wird die Schreibweise M(SD) oder
Median\[unteres Quartil, oberes Quartil\], verwendet. Bei Faktoren
(Nominal- Skala) wird der Anteil in Prozent angegeben, dabei wird die
Schreibweise Prozent(Anzahl) verwendet. Wenn ein Signifikanz Test
angegeben ist handelt es sich um einen **nicht-parametrischen Test** der
abhängig vom Skalenniveau entweder ein Chi-Quadrat-test bei
Nominal-Skala oder Wilcoxon bzw. Kruskal-Wallis-Test bei ordinalen und
metrischen-Skalen ist. (Es werden auf Empfehlungen des APA-Styles die
F-Werte mit ausgegeben) Die nicht Parametrischen Tests sind mit Hilfe
von Hmisc::spearman2 bzw. stats::chisq.test erstellt.

> Hmisc::spearman2\[14\] Wilcoxon, or Kruskal-Wallis tests are
> approximated by using the t or F distributions. spearman2 computes the
> square of Spearman’s rho rank correlation and a generalization of it
> in which x can relate non-monotonically to y. This is done by
> computing the Spearman multiple rho-squared between (rank(x),
> rank(x)^2) and y. When x is categorical, a different kind of Spearman
> correlation used in the Kruskal-Wallis test is computed (and spearman2
> can do the Kruskal-Wallis test). This is done by computing the
> ordinary multiple R^2 between k-1 dummy variables and rank(y), where x
> has k categories. x can also be a formula, in which case each
> predictor is correlated separately with y, using non-missing
> observations for that predictor. biVar is used to do the looping and
> bookkeeping. By default the plot shows the adjusted rho^2, using the
> same formula used for the ordinary adjusted R^2. The F test uses the
> unadjusted R2.

#### Anmerkung zu einfachen Auswertungen

**Wilcoxon-Test vs. Mann-Whitney-U-Test**

Beide Tests sind die gleichen Testverfahren, die genaue Bezeichnung ist
Wilcoxon-Mann-Whitney-Test und auch der H-Test ist mathematisch dasselbe
Verfahren (Lienert \[5\] Seite 140 und 203). (Die Verwechslung rührt
wahrscheinlich daher, dass noch der Wilcoxon-Vorzeichenrangtest als
Verfahren existiert.)

**Testen auf Normalverteilung**

Die Normalverteilung ist keine zwingende Voraussetzung für den T-Test,
die ANOVA oder die Regressionsanalyse. Bortz schreibt hier - ab ca. 30
Untersuchungsobjekten erübrigt sich die Forderung nach einer
Normalverteilung (Bortz \[4\] Seite 218).  
Normal verteilt sollen die Residuen sein! Bei den Daten erwarten wir in
der Regel keine Normalverteilung also die Abweichungen (Messfehler)
sollen normalverteilt sein. Der **KS-Test** (Kolmogorow-Smirnow-Test)
ist kein Nachweis der Normalverteilung sondern testet ob die Daten einer
angenommenen Wahrscheinlichkeitsverteilung folgen.

### Häufigkeiten

Häufigkeiten werden primär mit der Funktion **APA2()** und **Tabelle()**
erstellt die Ausgabe der Konfidenzintervalle erfolgt die Funktion durch
**Prop\_Test2()**.

``` r
Prop_Test2(data$g)  
```

| Characteristics |  Freq| Statistics         |
|:----------------|-----:|:-------------------|
| Control         |    23| 29% CI=\[20 - 41\] |
| Treat A         |    24| 31% CI=\[21 - 42\] |
| Treat B         |    31| 40% CI=\[29 - 51\] |

``` r
#  library(BayesianFirstAid)
#  x<-as.data.frame(table(data$g))$Freq
#  bayes.prop.test(x, rep(nrow(data), nlevels(data$g)), p=1/3)
#  APA2(~g+g2, data, type="freq.ci")
#  APA2(g~g2, data, type="freq.ci")
```

### Mittelwerte

Mittelwert ist ein Überbegriff von Maße der zentralen Tendenz. Folgende
Mittelwerte können berechnet werden: Modus (Ausprägung mit höchster
Häufigkeit), Median (sortierten Werte), Arithmetisches Mittel (was meist
unter Mittelwert verstanden wird) Geometrisches Mittel (für
logarithmische Verteilungen)

``` r
hkarz %>% Tabelle2(tzell="median", Lai, gruppe) 
```

| .id                        | value                               |
|:---------------------------|:------------------------------------|
| T-Zelltypisierung (median) | 68.50 (IQR 9.50, range 48.50-78.50) |
| LAI-Test (negativ/positiv) | 26/19                               |
| Gruppe (krank/gesund)      | 24/21                               |

``` r
set_my_options(median=list(style="IQR"))

APA2(tzell[1]+Lai~gruppe, hkarz, 
     type=c("auto", "median"),
     caption="Einfache Auswertung",
     test=TRUE, include.n = TRUE)
```

in conTest

| Item              | Characteristics | N   | krank (n=24)   | gesund (n=21)  | sig.Test                             |
|:------------------|:----------------|:----|:---------------|:---------------|:-------------------------------------|
| T-Zelltypisierung |                 | 45  | 63.2 (IQR=5.5) | 72.0 (IQR=4.0) | F<sub>(1, 43)</sub>=24.72, p&lt;.001 |
| LAI-Test          | negativ         | 45  | 88% (21)       | 24% ( 5)       | X2<sub>(1)</sub>=18.62, p&lt;.001    |
|                   | positiv         |     | 12% ( 3)       | 76% (16)       |                                      |

### Kreuztabellen

Kontingenztafeln, Kontingenztabellen oder Kreuztabellen sind Tabellen,
die Kombinationen von Häufigkeiten darstellen. Hier gibt es wieder die
Funktionen **APA2()** und **Tabelle()** sowie die R-Funktion
**xtabs()**. Bei der Verwendung von xtabs kann die Ergebnisstabelle mit
APA2 formatiert Tabellen erstellen. 2x2 Tabellen werden mit Häufigkeit
und wahlweise mit Prozent (das Verhalten wird über margin = 2 gesteuert)
ausgebeben. Berechnet werden mittels **fisher.test()** die Odds-Ratio
mit 95%-CI (vcd:: oddsratio). Weiteres lässt sich mit der Option type =
2 eine Sensitivitätsanalyse erstellen. Bei NxM-Tabellen wird als
Test-Statistik Pearson und der Kontingentkoeffizient berechnet,
alternativ steht auch der Phi-Coefficient zur Verfügung (auch hier mit
type = 2). Die Berechnung erfolgt hier mit der Funktion **vcd
::assocstat**.

``` r
hkarz$LAI<- factor(hkarz$lai, 0:1, c("pos", "neg"))
hkarz$Tzell<- cut(hkarz$tzell, 3, c("low", "med", "hig"))


xtab <- xtabs(~ gruppe+LAI, hkarz)
APA2(xtab, caption="Harnblasenkarzinom", test=FALSE)
```

    ## include.percent:  TRUEFormat_xtabs percent:  TRUE
    ## 
    ## Table: Harnblasenkarzinom
    ## 
    ## gruppe   LAI_     pos   LAI_     neg 
    ## -------  -------------  -------------
    ## krank    47%(21)        7%(3)        
    ## gesund   11%(5)         36%(16)

``` r
APA2(xtab, type="sens", test=TRUE, caption = "type=sens")
```

    ## include.percent:  TRUEFormat_xtabs percent:  TRUE
    ## 
    ## Table: type=sens
    ## 
    ## gruppe   LAI_     pos   LAI_     neg 
    ## -------  -------------  -------------
    ## krank    47%(21)        7%(3)        
    ## gesund   11%(5)         36%(16)      
    ## 
    ## 
    ## Table: 
    ## 
    ## gruppe    LAI_+   LAI_-
    ## -------  ------  ------
    ## +            21       3
    ## -             5      16
    ## 
    ## 
    ## Table: 
    ## 
    ## Statistic                Value        
    ## -----------------------  -------------
    ## Accuracy                 0.82         
    ## 95% CI                   [0.68, 0.92] 
    ## No Information Rate      0.58         
    ## P-Value [Acc > NIR]      p<.001       
    ## Kappa                    0.64         
    ## Mcnemar's Test P-Value   p=.724       
    ## Sensitivity              0.81         
    ## Specificity              0.84         
    ## Pos Pred Value           0.87         
    ## Neg Pred Value           0.76         
    ## Precision                0.88         
    ## Recall                   0.81         
    ## F1                       0.84         
    ## Prevalence               0.58         
    ## Detection Rate           0.47         
    ## Detection Prevalence     0.53         
    ## Balanced Accuracy        0.82         
    ## Positive Class           krank/pos    
    ## 
    ## in Output.default
    ## [1] "NULL"
    ## 
    ##  NULL 
    ## 
    ## in Output.default
    ## [1] "NULL"
    ## 
    ##  NULL 
    ## 
    ## in Output.default
    ## [1] "NULL"
    ## 
    ##  NULL

``` r
APA2(xtab, type="sens", caption = "geht nur mit teat=TRUE +  type=sens")
```

    ## include.percent:  TRUEFormat_xtabs percent:  TRUE
    ## 
    ## Table: geht nur mit teat=TRUE +  type=sens
    ## 
    ## gruppe   LAI_     pos   LAI_     neg 
    ## -------  -------------  -------------
    ## krank    47%(21)        7%(3)        
    ## gesund   11%(5)         36%(16)

``` r
APA2(xtabs(~ gruppe+Tzell, hkarz), caption="APA_Xtabs: 2x3 Tabelle", test=FALSE)
```

    ## Format_xtabs percent:  TRUE
    ## 
    ## Table: APA_Xtabs: 2x3 Tabelle
    ## 
    ## gruppe   Tzell_     low   Tzell_     med   Tzell_     hig 
    ## -------  ---------------  ---------------  ---------------
    ## krank    9%(4)            33%(15)          11%(5)         
    ## gesund   0%(0)            9%(4)            38%(17)

``` r
APA2(xtabs(~ gruppe+LAI+Tzell, hkarz), caption="APA_Xtabs: 2x2x3 Tabelle", test=FALSE)
```

    ## Format_xtabs percent:  TRUE
    ## 
    ## Table: APA_Xtabs: 2x2x3 Tabelle
    ## 
    ## gruppe   LAI   Tzell_     low   Tzell_     med   Tzell_     hig 
    ## -------  ----  ---------------  ---------------  ---------------
    ## krank    pos   9%(4)            29%(13)          9%(4)          
    ##          neg   0%(0)            4%(2)            2%(1)          
    ## gesund   pos   0%(0)            4%(2)            7%(3)          
    ##          neg   0%(0)            4%(2)            31%(14)

``` r
APA2(xtab, include.total.columns=TRUE, caption = "include.total.columns")
```

    ## include.percent:  TRUEFormat_xtabs percent:  TRUE
    ## 
    ## Table: include.total.columns
    ## 
    ## gruppe   LAI_      pos   LAI_      neg   LAI_      Sum 
    ## -------  --------------  --------------  --------------
    ## krank    88%(21)         12%(3)          100%(24)      
    ## gesund   24%(5)          76%(16)         100%(21)

``` r
APA2(xtab, include.total.sub=TRUE, caption = "include.total.sub")
```

    ## include.percent:  TRUEFormat_xtabs percent:  TRUE
    ## 
    ## Table: include.total.sub
    ## 
    ## gruppe   LAI_     pos   LAI_     neg 
    ## -------  -------------  -------------
    ## krank    47%(21)        7%(3)        
    ## gesund   11%(5)         36%(16)

``` r
xtab <- xtabs(~ gruppe+Tzell, hkarz)
APA2(xtab, test=FALSE, caption="APA2: 2x3 Tabelle")
```

    ## Format_xtabs percent:  TRUE
    ## 
    ## Table: APA2: 2x3 Tabelle
    ## 
    ## gruppe   Tzell_     low   Tzell_     med   Tzell_     hig 
    ## -------  ---------------  ---------------  ---------------
    ## krank    9%(4)            33%(15)          11%(5)         
    ## gesund   0%(0)            9%(4)            38%(17)

``` r
APA_Xtabs(xtab, caption="APA_Xtabs: 2x3 Tabelle")
```

    ## Format_xtabs percent:  TRUE
    ## 
    ## Table: APA_Xtabs: 2x3 Tabelle
    ## 
    ## gruppe   Tzell_     low   Tzell_     med   Tzell_     hig 
    ## -------  ---------------  ---------------  ---------------
    ## krank    9%(4)            33%(15)          11%(5)         
    ## gesund   0%(0)            9%(4)            38%(17)

### Sensitivity und Specificity

Sind spezielle Masszahlen bei 2x2 Tabellen Klassifikation
(richtig/falsch) für xtabs und glm-Objekte können mit
**Klassifikation()** berechnet werden, nicht zu vergessen ist die
Kappa-Statistik die über **MethComp()** angefordert wird.

``` r
xtab <- xtabs(~ gruppe+LAI, hkarz)
fit1<- glm(gruppe~lai, hkarz, family = binomial)
 
Klassifikation(fit1)$statistic[c(1,7,8),]
```

    ##     Statistic Value
    ## 1    Accuracy  0.82
    ## 7 Sensitivity  0.81
    ## 8 Specificity  0.84

``` r
Klassifikation(xtab)$statistic[c(1,7,8),]
```

    ##     Statistic Value
    ## 1    Accuracy  0.82
    ## 7 Sensitivity  0.81
    ## 8 Specificity  0.84

Sensitivität: richtig positive Rate eines Tests

Spezifität: richtig-negative Rate eines Tests

$$\\begin{array}
{rrr}
A & B \\\\
C & D \\\\
\\end{array}
$$

$$Sensitivity = \\frac{A}{A+C}$$
$$Specificity = \\frac{D}{B+D}$$

### ROC Analyse

Dient der Auffindung des **cut off value** (Trennwert). Receiver
Operating Characteristic (ROC), ist eine statistische Methode die
ursprünglich in der Rundfunktechnik entwickelt wurde. Man versteht
darunter eine Grafik die die Werte aus Sensitivität und Spezifität
aufträgt, also Richtig-Positiv-Rate gegen die Falsch-Positiv-Rate.

Die optimale Klassifikations-Schwelle finden man, in dem man jenen
ROC-Wert sucht, der den größten Normalabstand zur Diagonale des
Diagramms aufweist.

J = max(Sensitivity + Specificity - 1)

Sachs 159 Blutzucker - Beispieldaten (simuliert)

``` r
# require(pROC)
# data %>% Tabelle(Blutzucker, by=~Gruppe) 

roc_curve <- roc(data$Gruppe, data$Blutzucker)
plot(roc_curve, print.thres = "best",
     print.auc=TRUE)
```

![ROC-Kurve zu den
Blutzuckerwerten](C:\Users\wpete\AppData\Local\Temp\RtmpoxDuEs\preview-2f943e3f58f3.dir\Dokumentation_files/figure-markdown_github/fig-roc1-1.png)

``` r
# das selbe aber mit Regression daher sind die cut off -Werte nur indirekt interpretierbar
# fit1  <- glm(Gruppe~Blutzucker, data, family = binomial)
# x1 <- Klassifikation(fit1)
# roc_curve   <- roc(x1$response, x1$predictor)
# windows(8,8)
# plot(roc_curve, print.thres = "best",
#      print.auc=TRUE)
# abline(v = 1, lty = 2)
# abline(h = 1, lty = 2)
```

Weitere Info zu ROC findet sich unter
<https://www.hranalytics101.com/how-to-assess-model-accuracy-the-basics/>

``` r
fit1<- glm(gruppe~lai, hkarz, family = binomial)
x1 <- Klassifikation(fit1)
x1$statistic[c(1,7,8),]
```

    ##     Statistic Value
    ## 1    Accuracy  0.82
    ## 7 Sensitivity  0.81
    ## 8 Specificity  0.84

``` r
roc_curve <- roc(x1$response, x1$predictor)
auc(roc_curve)
```

    ## Area under the curve: 0.8185

``` r
#plot(roc_curve, print.thres = "best", print.auc=TRUE)
#abline(v = 1, lty = 2)
#abline(h = 1, lty = 2)
#text(.90, .97, labels = "Ideal Model")
#points(1,1, pch = "O", cex = 0.5)
```

    # Compute confidence intervals at every point of the curve (looks nicer when plotting)
    ciobj <- ci.se(rocobj, # CI of sensitivity...
                   specificities = seq(0,100,5)) # ...at every 5% of Sensitivity from 0-100

``` r
fit1<- glm(gruppe~lai, hkarz, family = binomial)
fit2<- glm(gruppe~lai+tzell, hkarz, family = binomial)
#thkarz <- as.data.frame(xtabs(~gruppe+lai, hkarz))
#fit2<- glm(Freq ~ gruppe*lai, thkarz, family = poisson())
x1 <- Klassifikation(fit1)
x2 <- Klassifikation(fit2) 
#require(pROC)
roc1   <- roc(x1$response, x1$predictor)
roc2   <- roc(x2$response, x2$predictor)

plot(roc1, print.auc = TRUE, print.auc.y = 0.6) 
plot(roc2, lty = 2, col = "blue", print.auc.y = 0.7, print.auc = TRUE, add = TRUE)
legend("bottomright",  legend = c("Lai", "Lai + T-Zell"),
  col = c(par("fg"), "blue"), lty = 1:2, lwd = 2
)
```

![ROC-Kurve zu den
Harnblasenkarzinom](C:\Users\wpete\AppData\Local\Temp\RtmpoxDuEs\preview-2f943e3f58f3.dir\Dokumentation_files/figure-markdown_github/fig-roc2-1.png)

``` r
roc.test(roc1, roc2)
```

    ## 
    ##  DeLong's test for two correlated ROC curves
    ## 
    ## data:  roc1 and roc2
    ## Z = -2.0833, p-value = 0.03722
    ## alternative hypothesis: true difference in AUC is not equal to 0
    ## sample estimates:
    ## AUC of roc1 AUC of roc2 
    ##   0.8184524   0.8938492

``` r
### Not run: 
# The latter used Delong's test. To use bootstrap test:
#roc.test(roc1, roc2, method="bootstrap")
# Increase boot.n for a more precise p-value:
#roc.test(roc1, roc2, method="bootstrap", boot.n=10000)

#ciobj <- ci.se(roc2)
#plot(ciobj, type = "shape", col="#D3D3D3", alpha = .5) 
```

### Effektstärke

Mehr zum Thema Unstandardisierte Effektgrößen ist unter
Regressionsanalyse zu finden.

Effektstärke ist eine dimensionslose Zahl zur Verdeutlichung der
praktischen Relevanz von statistisch signifikanten Ergebnisse.
Pearson-Korrelation r Cohens d

$$r={\\frac  {d}{{\\sqrt  {d^{2}+{\\frac  {(n\_{1}+n\_{2})^{2}}{n\_{1}n\_{2}}}}}}}$$

Cohens f2
$$f^{2}={\\frac  {R\_{{included}}^{2}-R\_{{excluded}}^{2}}{1-R\_{{included}}^{2}}}$$

partielle Eta-Quadrat

$$\\eta ^{2}={\\frac  {QS\_{{{\\rm {Effekt}}}}}{QS\_{{{\\rm {Effekt}}}}+QS\_{{{{\\rm {Res}}}}}}}$$

``` r
 APA2(tzell~gruppe,hkarz, type="cohen.d")  # APA_Effsize ist das gleiche
```

| Item              | N   | krank (n=24) | gesund (n=21) | cohen.d                |
|:------------------|:----|:-------------|:--------------|:-----------------------|
| T-Zelltypisierung | 45  | 63.85 (5.61) | 71.27 (4.84)  | 14.48 \[12.25, 16.72\] |

### Korrelation

Der Korrelationskoeffizient ist ein dimensionsloses Maß für den Grad des
linearen Zusammenhangs. Korrelationstabelle Interkorrelationen

``` r
 APA_Correlation(~a+b+c, DF, caption="~a+b+c")
```

| Source | (1) | (2)   | (3)    |
|:-------|:----|:------|:-------|
| (1) a  | 1   | -0.03 | 0.51\* |
| (2) b  |     | 1     | -0.09  |
| (3) c  |     |       | 1      |

### Likert-Skalen

Analyse von Likertskalen

``` r
Res1 <- Likert(~., DF2 )
```

Error gemischtes Skalenniveau!! Magazines Comic.books Fiction Newspapers
Geschlecht 5 5 5 5 2 Magazines Comic.books Fiction Newspapers Geschlecht
1 - ++ – – f 2 - ++ – – m 3 - ++ – – m 4 - ++ - – m 5 - ++ - – m 6 - ++
- – m

``` r
APA2(Res1)
```

| Item        | 1        | 2        | 3        | 4        | 5        |    n| Mittelwert  |
|:------------|:---------|:---------|:---------|:---------|:---------|----:|:------------|
| Magazines   | 10% (10) | 60% (60) | 10% (10) | 10% (10) | 10% (10) |  100| 2.5 (1.1)   |
| Comic.books | 10% (10) | 10% (10) | 10% (10) | 10% (10) | 60% (60) |  100| 4 (1)       |
| Fiction     | 21% (21) | 21% (21) | 21% (21) | 19% (19) | 18% (18) |  100| 2.92 (1.40) |
| Newspapers  | 60% (60) | 10% (10) | 10% (10) | 10% (10) | 10% (10) |  100| 2 (1)       |
| Geschlecht  | 65% (65) | 35% (35) | 0% ( 0)  | 0% ( 0)  | 0% ( 0)  |  100| 1.35 (0.48) |

Gruppenvergleich

``` r
APA2(Res2 <- Likert(.~ Geschlecht, DF2 ))
```

| Geschlecht | Item        | –        | \-       | o        | \+       | ++       |    n| Mittelwert  |
|:-----------|:------------|:---------|:---------|:---------|:---------|:---------|----:|:------------|
| m          | Magazines   | 8% ( 5)  | 57% (37) | 11% ( 7) | 14% ( 9) | 11% ( 7) |   65| 2.63 (1.15) |
| m          | Comic.books | 9% ( 6)  | 15% (10) | 8% ( 5)  | 9% ( 6)  | 58% (38) |   65| 3.92 (1.46) |
| m          | Fiction     | 18% (12) | 20% (13) | 25% (16) | 20% (13) | 17% (11) |   65| 2.97 (1.36) |
| m          | Newspapers  | 58% (38) | 9% ( 6)  | 11% ( 7) | 11% ( 7) | 11% ( 7) |   65| 2.06 (1.46) |
| f          | Magazines   | 14% ( 5) | 66% (23) | 9% ( 3)  | 3% ( 1)  | 9% ( 3)  |   35| 2.26 (1.04) |
| f          | Comic.books | 11% ( 4) | 0% ( 0)  | 14% ( 5) | 11% ( 4) | 63% (22) |   35| 4.14 (1.35) |
| f          | Fiction     | 26% ( 9) | 23% ( 8) | 14% ( 5) | 17% ( 6) | 20% ( 7) |   35| 2.83 (1.50) |
| f          | Newspapers  | 63% (22) | 11% ( 4) | 9% ( 3)  | 9% ( 3)  | 9% ( 3)  |   35| 1.89 (1.37) |

``` r
APA2(Res2, ReferenceZero=3, na.exclude=TRUE, type="freq")
```

3

| Geschlecht | Item        |  low(1:2)|  neutral(3)|  high(4:5)|    n| Mittelwert  |
|:-----------|:------------|---------:|-----------:|----------:|----:|:------------|
| m          | Magazines   |        42|           7|         16|   65| 2.63 (1.15) |
| m          | Comic.books |        16|           5|         44|   65| 3.92 (1.46) |
| m          | Fiction     |        25|          16|         24|   65| 2.97 (1.36) |
| m          | Newspapers  |        44|           7|         14|   65| 2.06 (1.46) |
| f          | Magazines   |        28|           3|          4|   35| 2.26 (1.04) |
| f          | Comic.books |         4|           5|         26|   35| 4.14 (1.35) |
| f          | Fiction     |        17|           5|         13|   35| 2.83 (1.50) |
| f          | Newspapers  |        26|           3|          6|   35| 1.89 (1.37) |

Likert-Plots

``` r
#require(HH)
# ?likertplot
#
#windows(7,3)
#likertplot( Item   ~ .| Geschlecht , data=Res2$results,
#            main='',ylab="", sub="" ,
#            # col=brewer_pal_likert(5, "RdBu", "Geschlechtay80") ,
#            positive.order=TRUE,
#            as.percent = TRUE,
#            auto.key=list(space="right", columns=1)
#)
#data<- Res2$names
#data$mean<- Res2$m
#  barchart( Item~ mean |Geschlecht, Mymean2, xlim=c(1,5))
#windows(3,6)
#dotplot(Item ~ mean, data,
#        groups=Geschlecht, xlim=c(.085, 5.15),
#        type=c("p", "l"))  
```

### Rangreihen

Rangordnungen von Objekten können durch eine Transformation der
Rangreihen in Intervallskalierte Merkmale überführt werden. Die
Grundidee dieser Methode geht auf Thurstone (1927) nach dem “Law of
Categorical Judgement” zurück. Dabei werden die kumulierten Häufigkeiten
in Normalverteilte z-Werte übergeführt und aus diesen die
Intervallskalierten Merkmalsauspraegungen gebildet. Literatur: Bortz, J.
& Doering, N. (2006). Forschungsmethoden und Evaluation fuer Human-und
Sozialwissenschaftler (4. Auflage). Berlin: Springer. Seite 155

``` r
ans <- Rangreihe(~FavA+FavB+FavC+FavD, DF )
APA2(ans, caption="Alle") 
```

| Items                           | FavA     | FavB     | FavC     | FavD     | mittlerer.Rang | Skalenwert |
|:--------------------------------|:---------|:---------|:---------|:---------|:---------------|:-----------|
| Cubanischer Arabica Kaltextrakt | 31% (21) | 34% (23) | 19% (13) | 15% (10) | 2.18           | 0.31       |
| Cubanischer Arabica Filter      | 33% (22) | 30% (20) | 15% (10) | 22% (15) | 2.27           | 0.21       |
| Dallmayr Prodomo Kaltextrakt    | 15% (10) | 21% (14) | 45% (30) | 19% (13) | 2.69           | -0.18      |
| Dallmayr Prodomo Filter         | 21% (14) | 15% (10) | 21% (14) | 43% (29) | 2.87           | -0.34      |

Tests
-----

Quelle:
<https://cran.r-project.org/doc/contrib/Ricci-refcard-regression.pdf>

-   *ad.test*: Anderson-Darling test for normality (nortest)
-   *bartlett.test*: Performs Bartlett’s test of the null that the
    variances in each of the groups (samples) are the same (stats)
-   *bgtest*: Breusch-Godfrey Test (lmtest)
-   *bptest*: Breusch-Pagan Test (lmtest)
-   *cvm.test*: Cramer-von Mises test for normality (nortest)
-   *durbin.watson*: Durbin-Watson Test for Autocorrelated Errors (car)
-   *dwtest*: Durbin-Watson Test (lmtest)
-   *levene.test*: Levene’s Test (car)
-   *lillie.test*: Lilliefors (Kolmogorov-Smirnov) test for normality
    (nortest)
-   *ncv.test*: Score Test for Non-Constant Error Variance (car)
-   *pearson.test*: Pearson chi-square test for normality (nortest)
-   *sf.test*: Shapiro-Francia test for normality (nortest) Vito Ricci -
    R Functions For Regression Analysis
-   *shapiro.test*: Performs the Shapiro-Wilk test of normality (stats)
    Variables transformations
-   *box.cox*: Box-Cox Family of Transformations (car)
-   *boxcox*: Box-Cox Transformations for Linear Models (MASS)
-   *box.cox.powers*: Multivariate Unconditional Box-Cox Transformations
    (car)
-   *box.tidwell*: Box-Tidwell Transformations (car)
-   *box.cox.var*: Constructed Variable for Box-Cox Transformation (car)

Methodenvergleich
-----------------

Oft interessiert die Zuverlässigkeit und Reproduzierbarkeit einer
Diagnose. Die Beurteilung kann dabei durch einen Bewerter
(Messverfahren) in wiederholter Form erfolgen und wird dann als
Intra-Rater bezeichnet oder die Beurteilung eines Merkmals erfolgt durch
mehrere Bewerter (Messverfahren). und hier spricht man von Inter-Rater.
Die Methode der Beurteilung der Übereinstimmung hängt von den jeweiligen
Verteilungseigenschaften ab.

Bei Nominalen wird abgezählt und die Rate der Übereinstimmung bewertet
(Cohen-Koeffizient) Bei Ordinalen-Daten werden die gewichteten
Übereinstimmungen ausgezählt (gewichteter Cohen-Koeffizient). Bei
metrischen(stetigen) Daten werden die Differenzen beurteilt
(Bland-Altman-Methode oder auch Tukey Mean Difference).

Bland-Altman-Methode Bias (d) systematische Abweichung Messfehler (s)
Standardabweichung der Differenz Limits of agreement (LOA) Intervall von
95 (entspricht d+-1.96 -&gt; es wird eine Normalverteilung unterstellt).

### MetComp

Die generische Funktion MetComp() kann sowohl Kappa als auch Tukey-means
berechnen. Kappa kann aber auch über die xtab() und APA2 berechnet
werden. Wobei hier nur 2x2-Tabellen untersucht werden können, hingegen
sind bei Kappa() auch mehrere ordinale Kategorien erlaubt.

Ähnliche Methode ist ICC die aber diese zählt eher zur
Reliabilitätsanalyse.

Funktionen: MetComp, BAP, und Kappa

#### Beispiel Altman and Bland

Understanding Bland Altman analysis, Davide Giavarina, Biochemia medica
2015;25(2) 141-51 doi: 10.11613/BM.2015.015

``` r
MetComp2(~A+B, Giavarina, caption = "Giavarina")
```

| Parameter              | Unit   | Percent | SE    | CI.low  | CI.hig |
|:-----------------------|:-------|:--------|:------|:--------|:-------|
| df (n-1)               | 29     |         |       |         |        |
| difference mean (d)    | -27.50 | -15.7%  | 6.20  | -40.17  | -14.83 |
| standard deviation (s) | 33.94  | 39.8%   |       |         |        |
| critical.diff (1.96s)  | 66.52  | 78.0%   |       |         |        |
| d-1.96s                | -94.02 | -93.7%  | 10.73 | -115.97 | -72.07 |
| d+1.96s                | 39.02  | 117.8%  | 10.73 | 17.07   | 60.97  |

#### Beispiel Botulinum A

Sachs Angewandte Statistik Seite 627

``` r
# APA2(xtabs(~A+B, Botulinum), test=TRUE)

MetComp2(~A+B, Botulinum)
```

| source     | Kappa | SE   | z    | p.value  |
|:-----------|:------|:-----|:-----|:---------|
| Unweighted | 0.60  | 0.13 | 4.71 | &lt;.001 |
| Weighted   | 0.60  | 0.13 | 4.71 | &lt;.001 |

``` r
# x <- BlandAltman(~A+E, DF)
# x %>% Output( )
```

``` r
 xt <-xtabs(~A+B, Botulinum)
 Klassifikation(xt)$statistic[c(1,3,4), ]
```

            Statistic  Value

1 Accuracy 0.80 3 No Information Rate 0.52 4 P-Value \[Acc &gt; NIR\]
p&lt;.001

``` r
Giavarina <- transform(Giavarina, C = round( A + rnorm(30,0,20)),
                D = round( A + rnorm(30,0,10) + A/10 ),
                E = round( A + rnorm(30,5,10) + (100-A/10) ))


ICC2(~A+E, Giavarina, caption="ICC (Korrelationen)")
```

    ## 
    ## 
    ## Table: ICC (Korrelationen)
    ## 
    ## type    ICC    F       df1    df2    p      lower bound   upper bound 
    ## ------  -----  ------  -----  -----  -----  ------------  ------------
    ## ICC1    0.97   70.52   29.0   30.0   0.00   0.94          0.99        
    ## ICC1k   0.99   70.52   29.0   30.0   0.00   0.97          0.99

``` r
# A - Goldstandart

x <- BlandAltman(~A+B+E, Giavarina)
```

    ## Warning: Warning in bland.altman.stats:Mehr als 2 Methoden.

``` r
# x %>% Output("BA-Analyse der Messwertreihe")
plot(x)
```

![Bland
Altman](C:\Users\wpete\AppData\Local\Temp\RtmpoxDuEs\preview-2f943e3f58f3.dir\Dokumentation_files/figure-markdown_github/fig-BlandAltman3-1.png)

``` r
x <- BlandAltman(~A+E+B, Giavarina)
```

    ## Warning: Warning in bland.altman.stats:Mehr als 2 Methoden.

``` r
# x %>% Output("BA-Analyse der Messwertreihe")
plot(x)
```

![Bland
Altman](C:\Users\wpete\AppData\Local\Temp\RtmpoxDuEs\preview-2f943e3f58f3.dir\Dokumentation_files/figure-markdown_github/fig-BlandAltman4-1.png)

    x <- BlandAltman(A+E+B~group, DF)
    plot(x)

#### Verschiedene Situationen

``` r
set.seed(0815)

n<-100
DF<- data.frame(
  A=rnorm(n, 100,50),
  B=rnorm(n, 100,50),
  C=NA,  D=NA,  E=NA,
  group= sample(gl(2, n/2, labels = c("Control", "Treat")))
)

cutA<-mean(DF$A)
DF <- transform(DF, C = round( A + rnorm(n, -5, 20)),
                D = round( A + rnorm(n,0,10) + A/10 ),
                #E = round( A + rnorm(n,5,10) + (100-A/10) )
                E = A + ifelse(A<cutA, A/5, -A/5 )+ rnorm(n, 0, 10)
)
```

``` r
x<- BlandAltman(~A+C, DF)
plot(x)
```

![A und C Messen das gleiche mit
SD=20](C:\Users\wpete\AppData\Local\Temp\RtmpoxDuEs\preview-2f943e3f58f3.dir\Dokumentation_files/figure-markdown_github/fig-BAx1-1.png)

``` r
x<- BlandAltman(~A+B, DF)
plot(x)
```

![A und B Messen unterschiedliche
Parameter](C:\Users\wpete\AppData\Local\Temp\RtmpoxDuEs\preview-2f943e3f58f3.dir\Dokumentation_files/figure-markdown_github/fig-BAx2-1.png)

``` r
x<- BlandAltman(~A+D, DF)
plot(x)
```

![A und D Messen das unterschiedlich D hat im unteren Wertevereich
deutlich geringere
Werte](C:\Users\wpete\AppData\Local\Temp\RtmpoxDuEs\preview-2f943e3f58f3.dir\Dokumentation_files/figure-markdown_github/fig-BAx3-1.png)

``` r
x<- BlandAltman(~A+E, DF)
plot(x)
```

![A und E Messen das unterschiedlich es esistiert ein Knick im
Wertebereich
100](C:\Users\wpete\AppData\Local\Temp\RtmpoxDuEs\preview-2f943e3f58f3.dir\Dokumentation_files/figure-markdown_github/fig-BAx4-1.png)

Regressionsanalyse
------------------

Regressionsanalysen sind eine Gruppe von statistische Verfahren, die
Beziehungen zwischen Variablen modellieren. Darunter fallen alle
Methoden wie ANOVA, lineare-Regression, logistische-Regression usw. ein
anderer Überbegriff ist **Generalized Linear Model** (GLM).

T-Test und ANOVA unterscheiden sich nicht wenn *d* = 1 ist. In diesen
Fall sind F-Wert und T-Wert dieselben Zahlen *F* = *T*<sup>2</sup>. Die
Regressionsanalyse ist die Verallgemeinerung der (Mittelwert)-Analyse
und liefert exakt die gleichen Ergebnisse wie die ANOVA. Der Unterschied
ist, dass die Regression mehr Informationen liefert. Aber die die
statistische Hypothese ist verschieden.

ANOVA: *H*<sub>0</sub> : *μ*<sub>1</sub> = … = *μ*<sub>*n*</sub>

Regression *H*<sub>0</sub> : *β*<sub>1</sub> = *β*<sub>2</sub> = 0 wobei
*y*<sub>*i*</sub> = *β*<sub>0</sub> + *β*<sub>1</sub>*x*<sub>*i*</sub> + …

Bei SPSS wird die ANOVA mit vielen verschiedenen Bezeichnungen
ausgegeben: als Test-der-Zwischen-Subjekt-Effekte,
Test-der-Inner-Subjekt-Effekte, Multivariate-Test, usw.. Aber hinter den
Begriffen steht immer eine ANOVA oder noch allgemeiner ein GLM
(Generalisiertes-Lineares-Modell). Ich verwende immer nur die Begriffe
ANOVA und Regressionsanalyse.

**Effektstärke, Eta-Quadrat** Das Eta-Quadrat Eta² ist nur eine Variante
von Maßzahlen zur Effektstärke und nicht die Effektstärke und nicht
immer ist das Eta² die passende Zahl. Eta² hat zudem den prinzipiellen
Nachteil, dass es “unanschaulich” ist. Trotzdem verwende ich meist, aus
pragmatischen Gründen, auch wenn es falsch ist das Eta-Quadrat.
Anmerkung zur Effektstärke das APA-Manual schreibt:

> include some measure of effect size …, whenever possible, provide a
> confidence interval of each effect size. Effect size may be expressed
> in the original units (mean, … regression slope). It can be valuable
> to report Effect size, also in some standardized or unites-free unit,
> … however, it, to provide the reader enough information to assess the
> magnitude of the observed effect(APA \[1\] Seite 34).

Frei interpretiert heißt das; unstandardisierte Effektgrößen
(Regressionskoeffizienten mit 95%-CI) müssen angeben werden, andere
können angegeben werden. Die “magnitude of the observed effect” ist am
besten über die Effectplots darzustellen (Fox \[6\]). Es gibt zwei Arten
von (standardisierten) Effektstärken die d-Familie, die Unterschiede
zwischen Gruppen betrachtet, und die r-Familie, welche ein Maß für
Zusammenhänge zwischen Daten ist (Hemmerich \[12\]). **Unstandardisierte
Effektgrößen** sind Differenzen von Gruppenmittelwerten (raw mean
difference) und unstandardisierte Regressionskoeffizienten. Der p-Wert
ist keine Effektstärke. Kreuztabelle: Cohen’s w, φ, Cramer’s V, C
ANOVA/Regression: f, η², η²part und d, Δ, Hedge’s g (bei Vergleich der
Stichproben) sowie Odds Ratio, Risk Ratio, r, R²

### Lineare Regressionsanalyse (lm)

Die Bezeichnung Regression stammte historisch gesehen von Francis
Galton, er untersuchte den Zusammenhang der Körpergröße von Eltern und
Kindern (Regression to the Mean). Ziel der Regressionsanalyse ist eine
funktionale Beziehung zwischen zwei Größen zu finden.\[1\] Mathematisch
lässt sich das folgend formulieren (*Ŷ* = *c* + *b**X* + *e*), dabei ist
*X* die unabhängige und *Ŷ* die abhängige Variable und *e* der
statistische Fehler. Gesucht wird, die “Formel” der Gerade, die in der
graphischen Darstellung durch den Mittelwert verläuft. Die Regression
ist quasi die Erweiterung der Korrelationsanalyse die ja die Stärke des
Zusammenhangs ermittelt.

#### Linear model

-   *Anova*: Anova Tables for Linear and Generalized Linear Models (car)
-   *anova*: Compute an analysis of variance table for one or more
    linear model fits(stasts)
-   *coef*: is a generic function which extracts model coefficients from
    objects returned by modeling functions. coefficients is an alias for
    it (stasts)
-   *coeftest*: Testing Estimated Coefficients (lmtest)
-   *confint*: Computes confidence intervals for one or more parameters
    in a fitted model. Base has a method for objects inheriting from
    class “lm” (stasts)
-   *deviance*:Returns the deviance of a fitted model object (stats)
-   *effects*: Returns (orthogonal) effects from a fitted model, usually
    a linear model. This is a generic function, but currently only has a
    methods for objects inheriting from classes “lm” and “glm” (stasts)
-   *fitted*: is a generic function which extracts fitted values from
    objects returned by modeling functions fitted.values is an alias for
    it (stasts)
-   *formula*: provide a way of extracting formulae which have been
    included in other objects (stasts)
-   *linear.hypothesis*: Test Linear Hypothesis (car) stratum analysis
    of variance and analysis of covariance (stasts)
-   *model.matrix*: creates a design matrix (stasts) *predict*:
    Predicted values based on linear model object (stasts) *residuals*:
    is a generic function which extracts model residuals from objects
    returned by modeling functions (stasts) *summary.lm*: summary method
    for class “lm” (stats) *vcov*: Returns the variance-covariance
    matrix of the main parameters of a fitted model object (stasts)

``` r
fit1<- lm(tzell~gruppe, hkarz)
fit2<- lm(tzell~gruppe+Lai, hkarz) 
fit3<- lm(tzell~gruppe*Lai, hkarz)
 
APA_Table(fit1,fit2,fit3, type="long", caption="Regression")
```

**Kennzahlen der (linearen) Regressionsanalyse)**

-   **b** estimate Erwartungswert der Koeffizienten,
    Regressionskoeffizienten ist der Marginale Effekt also der Einfluss
    pro Einheit

-   **SE** std. error Standardfehler

-   **p-Wert** Wahrscheinlichkeit der Null-Hypothese entweder als Zahl
    oder über die Sternsymbolik. p &lt; 0.05 =&gt; statistisch
    signifikant p &lt; 0.10 =&gt; statistisch tendenziell signifikant

-   **R²** und adjustierte R² Wie viel Varianz kann durch das
    Regressionsmodell aufgeklärt werden? Die Einschätzung der Höhe des
    Bestimmtheitsmaß hängt oft vom Anwendungsfeld ab zB. bei
    sozialwissenschaftlichen Arbeiten ist R2&gt;0.20 gut

-   t-Wert oder z-Wert sind die Wert der Teststatistik H0

-   **RMSEA** Root-Mean-Square-Error of Approximation Der RMSE ist eine
    relative Grösse (in der Einheit der Zielvariablen), je kleiner umso
    besser. Whereas R-squared is a relative measure of fit, RMSE is an
    absolute measure of fit.
    <https://stats.stackexchange.com/questions/56302/what-are-good-rmse-values>

-   **Beta-Werte** in Regressionsanalysen sind vereinfacht gesagt die
    standardisierten Koeffizienten und nur in theoretischen
    Fragestellungen interpretierbar. Besser sind meist die Koeffizienten
    (b). Ich verwende nach Möglichkeit immer die Koeffizienten da diese
    einfacher zu interpretieren sind.

### Logistische Regression Logit-Modell

Das Logit-Modell ist eine Gruppe von Regressionsverfahren die als
Klassifikationsverfahren verwendet werden. Zähldaten.

### Binomiale Regressionsanalyse

Die binomiale logistische Regression wird angewendet, wenn geprüft
werden soll, ob ein Zusammenhang zwischen einer binären Ziel-Variable
und einer oder mehreren Einfluss-Variablen besteht. Die folgenden Punkte
stammen von Backhaus Seite 456.

Beispiel Harnblasenkarzinom

``` r
fit1 <- glm(gruppe~tzell, hkarz, family = binomial)
APA_Table(fit1, type="long")
```

Signifikanz der Regressionskoeffizienten; bei SPSS wird der Wald-Test
verwendet, ich verwende den LRT (ein Vergleich findet man unter
<http://thestatsgeek.com/2014/02/08/wald-vs-likelihood-ratio-test/>)

**Odds ratio**\* OR Exp(b) - Exponent des Regressionskoeffizienten Odds
Ratios ist nur bei der logistischen Regression sinnvoll. Logarithmierte
Odds (Logits, Effekt-Koeffizienten)

``` r
res <- broom::tidy(fit1) 
cbind(res[1:2], 
      confint(fit1),
      res[5]) %>% 
  fix_format %>% Output("Odds Ratios")
```

    ## Waiting for profiling to be done...

| term        | estimate | 2.5 %  | 97.5 % | p.value |
|:------------|:---------|:-------|:-------|:--------|
| (Intercept) | -19.00   | -31.59 | -9.32  | .001    |
| tzell       | 0.28     | 0.14   | 0.46   | .001    |

Signifikanz des Regressionsmodells: Gütekriterien auf Basis der
LogLikelihood-Funktion Devianz **-2LL-Wert** H0: Modell besitzt eine
perfekte Anpassung Gute Werte sind -2LL nahe 0 und p-Wert nahe 1
*deviance(fit1)*

**LR-Test** (Likeihood Ratio-Test) H0: Alle Regressionskoeffizienten
sind gleich Null. Chi-Quadret möglichst hoch p&lt;0.05

Der **Wald Test** sind eigentlich zwei Methoden die erste ist der Test
ob *irgendein* Koeffizient signifikant ist, die zweite Methode *welcher*
Koeffizient signifikant ist. Der erstere Wald Test (Model Coefficients)
ist in der Regel wenig informativ. Der zweite Wald-Test ist die ANOVA
(Type II) andere Bezeichnung ist Wald Chi-square Test oder Analysis of
Deviance Table (Die Anova testet die Hypothese ob der zugehörige
Koeffizient Null ist.)

Likelihood ratio test = Wald-Test Analysis of Deviance Table = Wald-Test
(Type II), Anova Log Likelihood die Zahl kann für sich nicht
interpretiert werden **AIC**, **BIC** (Informationskriterium) kleiner
Wert steht für eine höhere Informativität der Models. Akaikes
Informationskriterium (AIC), Bayesian Information Criterion (BIC)

``` r
#--Omnibus Test
 lmtest::lrtest(fit1) %>% fix_format() %>% Output("LR-Test")
```

| \#Df | LogLik | Df   | Chisq | Pr(&gt;Chisq) |
|:-----|:-------|:-----|:------|:--------------|
| 2.0  | -21.70 |      |       |               |
| 1.0  | -31.09 | -1.0 | 18.79 | &lt;.001      |

``` r
# Fuer F-Test :  lmtest::waldtest(fit1)
```

**Pseudo-R-Quadrat**  
+ Cox und Snell R2: \[ 0.2 = akzeptabel, 0.4 = gut \]

-   Nagelkerke R2: \[ 0.2 = akzeptabel, 0.4 = gut, 0.5 = sehr gut\]

-   McFaddens R2: \[ 0.2 = akzeptabel, 0.4 = gut \] (see pR2)

``` r
Goodness <- function(x) {
  cbind(round(broom::glance(x)[, c(6,  3, 4, 5)], 1),
  round(R2(x), 2),
  round(RMSE(x)[2], 2))
}
fit1 %>% Goodness %>% Output()
```

|  deviance|  logLik|   AIC|  BIC|  McFadden|  r2ML|  r2CU|  RMSE|
|---------:|-------:|-----:|----:|---------:|-----:|-----:|-----:|
|      43.4|   -21.7|  47.4|   51|       0.3|  0.34|  0.46|  2.45|

Weitere Betrachtung ist über Klassifikationstabelle und dazu gibt es die
Tests Hosmer Lemeschow-Test und Press Q-Test.

``` r
Klassifikation2(fit1)
```

| Response |  Predictor\_+|  Predictor\_-|
|:---------|-------------:|-------------:|
| \+       |            18|             6|
| \-       |             4|            17|

| Statistic                | Value          |
|:-------------------------|:---------------|
| Accuracy                 | 0.78           |
| 95% CI                   | \[0.63, 0.89\] |
| No Information Rate      | 0.51           |
| P-Value \[Acc &gt; NIR\] | p&lt;.001      |
| Kappa                    | 0.56           |
| Mcnemar’s Test P-Value   | p=.752         |
| Sensitivity              | 0.82           |
| Specificity              | 0.74           |
| Pos Pred Value           | 0.75           |
| Neg Pred Value           | 0.81           |
| Precision                | 0.75           |
| Recall                   | 0.82           |
| F1                       | 0.78           |
| Prevalence               | 0.49           |
| Detection Rate           | 0.40           |
| Detection Prevalence     | 0.53           |
| Balanced Accuracy        | 0.78           |
| Positive Class           | krank/0        |

### Poisson Regression

Poisson Regression für Zahldaten und Kreuztabellen

``` r
#-- SPSS kodiert die Gruppe 3 als Referenz
poisson_sim$prog <-
  factor(poisson_sim$prog, c("vocation", "general",  "academic"))
  fit5 <- glm(num_awards ~ prog + math, 
              poisson_sim, family =  poisson())
```

``` r
poisson_sim %>% Tabelle2(num_awards, prog, math)
```

| .id                              | value                              |
|:---------------------------------|:-----------------------------------|
| num\_awards (mean)               | 0.63 (SD 1.05, range 0.00-6.00)    |
| prog (vocation/general/academic) | 50/45/105                          |
| math score (mean)                | 52.65 (SD 9.37, range 33.00-75.00) |

``` r
APA_Table(fit5)
```

| term               | estimate | std.error | statistic | p.value  |
|:-------------------|:---------|:----------|:----------|:---------|
| (Intercept)        | -4.90    | 0.63      | -7.76     | &lt;.001 |
| prog\[T.general\]  | -0.37    | 0.44      | -0.84     | .402     |
| prog\[T.academic\] | 0.71     | 0.32      | 2.23      | .026     |
| math               | 0.07     | 0.01      | 6.62      | &lt;.001 |

| term | LR.Chisq | df  | p.value  |
|:-----|:---------|:----|:---------|
| prog | 14.57    | 2.0 | .001     |
| math | 45.01    | 1.0 | &lt;.001 |

> The output above indicates that the incident rate for
> \[prog=academic\] is 2.042 times the incident rate for the reference
> group, \[prog=vocation\]. Likewise, the incident rate for
> \[prog=general\] is 0.691 times the incident rate for the reference
> group holding the other variables at constant. The percent change in
> the incident rate of num\_awards is an increase of 7% for every unit
> increase in math.
> <http://www.ats.ucla.edu/stat/spss/dae/poissonreg.htm>

### Kreuztabellen

Intresantes Detail Kreuztabellen, logistische-Regression und
poisonn-Regression liefern identische Ergebnisse siehe Beispiel unten.

``` r
APA2(xtabs(~gruppe+lai, hkarz), test=TRUE, type="fischer")
```

include.percent: TRUEFormat\_xtabs percent: TRUE

| gruppe | lai\_ 0 | lai\_ 1 |
|:-------|:--------|:--------|
| krank  | 47%(21) | 7%(3)   |
| gesund | 11%(5)  | 36%(16) |

| OR    | 95% CI           | p-Value  |
|:------|:-----------------|:---------|
| 20.29 | \[3.88, 153.90\] | &lt;.001 |

``` r
fit1<- glm(gruppe~lai, hkarz, family = binomial)
thkarz <- as.data.frame(xtabs(~gruppe+lai, hkarz))
fit2<- glm(Freq ~ gruppe*lai, thkarz, family = poisson())

APA_Table(fit1, include.odds = TRUE)
```

    ## Waiting for profiling to be done...

| term        | estimate | std.error | statistic | p.value  | OR    | 2.5 % | 97.5 % |
|:------------|:---------|:----------|:----------|:---------|:------|:------|:-------|
| (Intercept) | -1.40    | 0.50      | -2.88     | .004     | 0.24  | 0.08  | 0.58   |
| lai         | 3.10     | 0.80      | 3.88      | &lt;.001 | 22.40 | 5.24  | 128.95 |

| term | LR.Chisq | df  | p.value  |
|:-----|:---------|:----|:---------|
| lai  | 20.15    | 1.0 | &lt;.001 |

``` r
APA_Table(fit1, fit2, include.odds = TRUE, 
          include.b=FALSE,
          include.se = FALSE,
          type="long")
```

### Hierarchische Modelle

Mixed Designs, Blockdesign, Hierarchische Modelle vs. feste/zufällige
Effekte, Mehrebenenanalyse, Messwiederholung die Begriffe bedeuten alle
das gleich es gibt keinen Unterschied in der Berechnung.

``` r
lmer_fit1<-lmerTest::lmer(score ~ agegrp+trial + (1|id), MMvideo)

lmer_fit2<-lmerTest::lmer(score ~ agegrp*trial + (1|id), MMvideo)
APA_Table(lmer_fit1, lmer_fit2, type="long")


# MySet()
# library(gridExtra)
#   windows(8,4)
#   p1 <- plot(effect("trial",lmer_fit1), multiline=TRUE, main="")
#   p2 <- plot(effect("agegrp*trial",lmer_fit2), multiline=TRUE, main="")
# 
#   grid.arrange(p1,p2,ncol=2)
# 
#  library(coefplot)
#  windows(4,3)
#   coefplot(lmer_fit2, intercept=F, xlab="b (SE)")
# 
#    windows(4,3)
#   multiplot(lmer_fit1, lmer_fit2, intercept=F, xlab="b (SE)")
```

### Kaplan-Meier

Zum Vergleich von zwei Therapien werden häufig Überlebenszeiten
herangezogen. Diese sollten
<https://www.uni-kiel.de/medinfo/lehre/seminare/methodik/Dtsch%20Arztebl%2015%20%C3%9Cberlebenszeitanalyse.pdf>

#### Survival analysis

-   *anova.survreg*: ANOVA tables for survreg objects (survival)
-   *clogit*: Conditional logistic regression (survival)
-   *cox.zph*: Test the proportional hazards assumption of a Cox
    regression (survival)
-   *coxph*: Proportional Hazards Regression (survival)
-   *coxph.detail*: Details of a cox model fit (survival)
-   *coxph.rvar*: Robust variance for a Cox model (survival)
-   *ridge*: ridge regression (survival)
-   *survdiff*: Test Survival Curve Differences (survival)
-   *survexp*: Compute Expected Survival (survival)
-   *survfit*: Compute a survival Curve for Censored Data (survival)
-   *survreg*: Regression for a parametric survival model (survival)

#### Beispiel MagenkarzinomBuehl

Die Datei mkarz.sav Seite 553 ist ein Datensatz mit 106 Patientan mit
Magenkarzinom über einen Zeitraum von 5 Jahren

``` r
library(survival)
mkarz <- GetData("C:/Users/wpete/Dropbox/3_Forschung/1 Statistik/BspDaten/SPSS/_Buehl/MKARZ.SAV")
```

File: C:/Users/wpete/Dropbox/3\_Forschung/1
Statistik/BspDaten/SPSS/\_Buehl/MKARZ.SAV character

file\_info

ext C:/Users/wpete/Dropbox/3\_Forschung/1
Statistik/BspDaten/SPSS/\_Buehl/MKARZ.SAV (2871KB), 2018-01-26 13:46:22,
N=106, Var=9, Missing=0

``` r
mkarz$status<- ifelse(mkarz$status=="tot", 1, 0)
mkarz %>% Tabelle2(survive="median", status, lkb)
```

| .id                                           | value                               |
|:----------------------------------------------|:------------------------------------|
| Überlebenszeit in Monaten (median)            | 12.00 (IQR 21.75, range 1.00-60.00) |
| status (mean)                                 | 0.59 (SD 0.49, range 0.00-1.00)     |
| Lymphknotenbefall (kein Befall/maessig/stark) | 33/22/51                            |

``` r
# Kaplan-Meier estimator without grouping

m0 <- Surv(survive, status) ~ 1
res0<- survfit(m0, mkarz)
APA2(res0)
```

records n.max n.start events *rmean *se(rmean) median 0.95LCL 0.95UCL

|  records|  events|  median|  0.95LCL|  0.95UCL| Mean       |
|--------:|-------:|-------:|--------:|--------:|:-----------|
|      106|      63|      17|       12|       28| 27.9 (2.5) |

``` r
APA2(summary(res0, times= c(5, 10, 20, 60)),
     percent=TRUE,
     #Statistik Anfordern und ander Schreibweise
     include=c( time ="time", n.risk ="n.risk", 
                n.event ="n.event", surv = "survival",
                lower = "lower 95% CI",upper ="upper 95% CI"),
     caption="Kaplan-Meier" )
```

|  time|  n.risk|  n.event| survival | lower 95% CI | upper 95% CI |
|-----:|-------:|--------:|:---------|:-------------|:-------------|
|     5|      87|       24| 77.29    | 69.70        | 85.71        |
|    10|      63|       16| 61.14    | 52.36        | 71.39        |
|    20|      36|       13| 45.62    | 36.50        | 57.02        |
|    60|      13|       10| 29.74    | 20.82        | 42.46        |

``` r
m1 <- Surv(survive, status) ~ lkb
res1<- survfit(m1, mkarz)
fit1<- coxph(m1, mkarz)
logrank1<- survdiff(m1, mkarz)
APA2(res1, caption="Kaplan-Meier")
```

Source records n.max n.start events *rmean *se(rmean) median 0.95LCL
0.95UCL

| Source          |  records|  events|  median|  0.95LCL|  0.95UCL| Mean       |
|:----------------|--------:|-------:|-------:|--------:|--------:|:-----------|
| lkb=kein Befall |       33|       7|      NA|       41|       NA| 49.6 (3.4) |
| lkb=maessig     |       22|      11|      18|       14|       NA| 32.7 (5.6) |
| lkb=stark       |       51|      45|       8|        5|       10| 11.9 (2.1) |

``` r
APA2(logrank1)
```

| Source          |    N|  Observed|  Expected|
|:----------------|----:|---------:|---------:|
| lkb=kein Befall |   33|         7|      28.7|
| lkb=maessig     |   22|        11|      14.8|
| lkb=stark       |   51|        45|      19.5|
| Overall         |  106|        63|        NA|

``` r
APA2(coxph(m1,mkarz))
```

| Source                | test           | df  | pvalue   |
|:----------------------|:---------------|:----|:---------|
| Wald test             | 42.31          | 2   | &lt;.001 |
| Score (logrank) test  | 58.00          | 2   | &lt;.001 |
| Likelihood ratio test | 55.90          | 2   | &lt;.001 |
| Concordance           | 0.76 (SE=0.76) | NA  | NA       |
| Rsquare               | 0.41           | NA  | NA       |
| AIC                   | 465.11         | NA  | NA       |
| BIC                   | 469.40         | NA  | NA       |

### MANOVA

MANOVA ist eine Methode der Varianzanalyse und prüft multivariate
Mittelwertunterschiede. Diskriminanzanalyse ist ein multivariates
Verfahren zur Analyse von Gruppenunterschieden.

Vorgehensweise

1.  Deskriptive Analyse mit Box-Plots

2.  Prüfung der Mittelwertunterschiede (MANOVA)

3.  Diskriminanzanalyse
    -   Formulierung und Schätzung der Diskriminanzfunktion
    -   Prüfung der Diskriminanzfunktion
    -   Prüfung der Merkmalsvarianblen
    -   Klassifikation neuer Elemente

Beispiel Manova von ucla.edu
<https://stats.idre.ucla.edu/spss/output/one-waymanova/>

``` r
fit1<- manova(z ~ m_data$GROUP)
APA2(fit1)
```

| Source    | F value | Df   | Pr(&gt;F) |
|:----------|:--------|:-----|:----------|
| GROUP     | 2.70    | 2.0  | .083      |
| Residuals |         | 30.0 |           |
| GROUP     | 0.47    | 2.0  | .628      |
| Residuals |         | 30.0 |           |
| GROUP     | 2.88    | 2.0  | .072      |
| Residuals |         | 30.0 |           |

| Source    | Df   | Wilks | approx F | num Df | den Df | Pr(&gt;F) |
|:----------|:-----|:------|:---------|:-------|:-------|:----------|
| GROUP     | 2.0  | 0.53  | 3.54     | 6.0    | 56.0   | .005      |
| Residuals | 30.0 |       |          |        |        |           |

``` r
#summary(fit1)$Eigenvalues
#library(MASS)
#fit2 <- MASS::lda(GROUP ~ ., data=m_data)
#APA2(fit2)
#plot(fit2)
```

### Voraussetzungen

Im Folgenden die wichtige Voraussetzungen der Regressionsanalyse

-   Das Modell ist linear in den Parametern.

-   Die Residuen sind normalverteilt und im Mittel sind die Residuen
    null.

-   Die Zahl der Beobachtungen muss größer sein als die Zahl der zu
    schätzenden Parameter: n &gt; k.

Assumptions of the regression model (nach Gellman Hill Seite 45)

We list the assumptions of the regression model in decreasing order of
importance. 1. Validity.  
2. Additivity and linearity. The most important mathematical assumption
3. Independence of errors 4. Equal variance of errors 5. Normality of
errors.

> *General principles*
>
> Our general principles for building regression models for prediction
> are as follows:
>
> 1.  Include all input variables that, for substantive reasons, might
>     be expected to be important in predicting the outcome.
> 2.  It is not always necessary to include these inputs as separate
>     predictors—for example, sometimes several inputs can be averaged
>     or summed to create a “total score” that can be used as a single
>     predictor in the model.
> 3.  For inputs that have large effects, consider including their
>     interactions as well.
> 4.  We suggest the following strategy for decisions regarding whether
>     to exclude a variable from a prediction model based on expected
>     sign and statistical significance (typically measured at the 5%
>     level; that is, a coefficient is “statistically significant” if
>     its estimate is more than 2 standard errors from zero):
>
> -   If a predictor is not statistically significant and has the
>     expected sign, it is generally fine to keep it in. It may not help
>     predictions dramatically but is also probably not hurting them.
> -   If a predictor is not statistically significant and does not have
>     the expected sign (for example, incumbency having a negative
>     effect on vote share), consider removing it from the model (that
>     is, setting its coefficient to zero).
> -   If a predictor is statistically significant and does not have the
>     expected sign, then think hard if it makes sense. (For example,
>     perhaps this is a country such as India in which incumbents are
>     generally unpopular; see Linden, 2006.) Try to gather data on
>     potential lurking variables and include them in the analysis.
> -   If a predictor is statistically significant and has the expected
>     sign, then by all means keep it in the model.

Multikollinearität, Autokorrelation, Heteroskedastizität Diese
speziellen Eigenschaften sind laut Gelman nicht besonders wichtig. Die
Prüfung erfolgt über die grafische Darstellung der Residuen. Die Tests
für Multikollinearität (VIF) und für **Autokorrelation**
(Durbin-Watson-Test) sind weniger aussagekräftig. Ich verwende statt der
sperrigen Begriffe Heteroskedastizität usw. den Begriff “Gleichheit der
Varianzen”.

Von **Multikollinearität**, bzw. Kollinearität, spricht man, wenn zwei
oder mehrere erklärende x Variablen hoch untereinander korreliert sind.
Anzeichen ist Hohes R2 und wenige signifikante Koeffizienten. Test über
Variance Inflation Factor VIF VIF&gt;10 Multikollinearitätsproblem

variance inflation factor, aka **VIF** values over 5 are troubling.
should probably investigate anything over 2.5. Quelle:
<https://hlplab.wordpress.com/2011/02/24/diagnosing-collinearity-in-lme4/>

``` r
# Kopie der \link{car::vif} funktion
library(car)
 fit<-lm(prestige ~ income + education, data=Duncan)
 car::vif(fit)
```

income education 2.1049 2.1049

``` r
 VIF2(fit)
```

| Source    |  VIF|
|:----------|----:|
| income    |  2.1|
| education |  2.1|

### Variablenauswahl

Ein Kriterium für die Aufnahme einer Variablen ist das Schrittweise
Verfahren (Step). Ein anderes ist der sachlogische Bezug also alle
Variablen die sachlogisch Begründet werden gehören ins Model. Ich gehe
immer folgenderweise vor: Ich verwende immer beide Varianten (forward
und backward). Anschließend überprüfe ich die Ergebnisse Visuell auf
einflussreiche Fälle. Das Heisst ich überprüfe ob die <Signifikanz> auf
einzelne Ausreiser beruht. Im nächsten Schritt passe ich das Modell an,
es werden also eventuell Variablen entfernt oder hinzugefügt.
Schrittweises Verfahren (Choose a model by AIC in a Stepwise Algorithm)

forward selection: die Einflussgröße mit dem kleinsten p-Wert wird als
Kandidat ins Modell aufgenommen und der Likelihood- Wert bestimmt.
backward elimination: prüft ob auf eine Variable verzichtet werden kann
Ich verwende immer beide Varianten (forward und backward)

### Regressionsdiagnostik

Überprüft die Linearität, Independence of error, Equal variance of
errors usw. Die Regressionsdiagnostik untersucht die Eigenschaften der
Residuen. Die Residuen sind dabei die Abweichungen vom Model. Die
Untersuchung erfolgt dabei über Grafik zu den Residuen.

Strukturprüfende Verfahren
--------------------------

Multivariate Methoden und Skalen-Analyse

Validität, Objektivität kann nicht mit Statistik bewertet werden.
Reliabilität, Interne Konsistenz (Cronbach’s Alpha), Trennschärfe (bei
SPSS Korrigierte-Item-Skala-Korrelation). Validität wird in interne (ist
durch die Randomisierung gegeben) und externe (Übertragbarkeit)
unterschieden. Die Reliabilität kann berechnet werden wenn genug Daten
n&gt;60 und mehr als 3 Items vorliegen. Ein alpha&gt;0.08 ist gut.

### Cronbach’s Alpha

``` r
Verarbeitung <- Reliability(~ F5+F16+F22+F9+F26+F6+F35+F33+F12+F34+F4, fkv, check.keys =TRUE)
Coping <- Reliability(~ F7+F8+F17+F14+F15+F18+F19+F1+F13+F20, fkv, check.keys =TRUE)
Vertrauen <- Reliability(~ F28+F27+F31+F29, fkv, check.keys =TRUE)
Religion <- Reliability(~F21+F25+F30+F23+F24, fkv, check.keys =TRUE)
Distanz <- Reliability(~F3+F2+F10+F11, fkv, check.keys =TRUE)
Verarbeitung %>% Output
```

| Items |    n| M    | SD   | Alpha.if.Item.Deleted |
|:------|----:|:-----|:-----|:----------------------|
| F5    |  160| 2.04 | 1.35 | 0.55                  |
| F16   |  160| 1.98 | 1.16 | 0.58                  |
| F22   |  160| 3.03 | 1.28 | 0.58                  |
| F9    |  160| 2.38 | 1.17 | 0.51                  |
| F26   |  160| 2.31 | 1.32 | 0.45                  |
| F6    |  160| 1.68 | 1.08 | 0.36                  |
| F35   |  160| 2.58 | 1.29 | 0.41                  |
| F33   |  160| 2.78 | 1.13 | 0.41                  |
| F12   |  160| 1.63 | 1.14 | 0.38                  |
| F34   |  160| 2.44 | 1.24 | 0.39                  |
| F4    |  160| 2.36 | 1.18 | 0.36                  |

|  Items|    n| M    | SD   | Alpha | Range      | Skew | Kurtosi | Shapiro.Test  |
|------:|----:|:-----|:-----|:------|:-----------|:-----|:--------|:--------------|
|     11|  160| 2.29 | 0.70 | 0.80  | 1.00; 5.00 | 0.59 | 0.00    | W=0.97 p=.001 |

``` r
Alpha(Verarbeitung, Coping, Vertrauen, Religion, Distanz) %>% Output()
```

| Source       |  Items|    n| M    | SD   | Alpha | Range      | Skew  | Kurtosi | Shapiro.Test      |
|:-------------|------:|----:|:-----|:-----|:------|:-----------|:------|:--------|:------------------|
| Verarbeitung |     11|  160| 2.29 | 0.70 | 0.80  | 1.00; 5.00 | 0.59  | 0.00    | W=0.97 p=.001     |
| Coping       |     10|  160| 3.40 | 0.74 | 0.82  | 1.00; 5.00 | -0.73 | 0.30    | W=0.96 p=&lt;.001 |
| Vertrauen    |      4|  160| 3.67 | 0.80 | 0.58  | 1.00; 5.00 | -0.48 | -0.32   | W=0.96 p=&lt;.001 |
| Religion     |      5|  160| 2.82 | 0.79 | 0.55  | 1.00; 5.00 | 0.21  | 0.03    | W=0.99 p=.161     |
| Distanz      |      4|  160| 2.87 | 0.78 | 0.48  | 1.00; 5.00 | 0.12  | -0.22   | W=0.98 p=.067     |

### ICC

ICC Intra-Klassen-Korrelation. Kopie von ICC aus dem Packet psych.

``` r
# #Quelle  http://www.personality-project.org/r/book/Chapter7.pdf
ICC2(sf)
```

| type  | ICC  | F    | df1 | df2  | p    | lower bound | upper bound |
|:------|:-----|:-----|:----|:-----|:-----|:------------|:------------|
| ICC1  | 0.32 | 3.84 | 5.0 | 30.0 | 0.01 | 0.04        | 0.79        |
| ICC1k | 0.74 | 3.84 | 5.0 | 30.0 | 0.01 | 0.21        | 0.96        |

### Explorative Faktorenanalyse (EFA)

Die EFA ist eine Gruppe von Verfahren für die Analyse unbekannten
korrelativen Strukturen. Zu diesen Methoden zählen die Faktorenanalyse
(FA) und die Hauptkomponenten-Analyse (PCA) . Ziel der
**Faktorenanalyse** ist aus einer Vielzahl von Variablen unabhängige
Beschreibung und Erklärungsvariablen zu extrahieren. Die Abgrenzung von
FA zu PCA ist je nach Lehrbuch und verwendeter Software fließend. So
spricht \[15\] dass die PCA ein Spezialfall der FA ist dagegen grenzt
\[16\] die FA stärker von der PCA ab. Verwirrung schafft zudem SPSS
welches keinen “Button” für PCA besitzt. Aus diesen Gründen verwende ich
hier die Begriffe Faktorenanalyse (FA) und PCA als synonym.

Der Unterschied ist, das bei der PAC die Anzahl der Faktoren sich aus
der Berechnung ergeben (Anwendungen sind sozialwissenschaftliche
Fragestellungen) und bei der (FA) die Faktoren vor der Berechnung
bekannt sein müssen (Anwendungen sind biologische Fragestellungen bei
denen bestimmte Gradienten wie Temperatur, pH Wert bekannte Komponenten
darstellen).

*Interpretation FA* Eine Auswertung die sich an den Tabellen Output von
SPSS orientiert, kann mit der library *library(psych)* und den
Funktionen *principal()* und *fa()* bewerkstelligt werden. Der Output
entspricht dem was SPSS unter Faktorenanalyse berechnet. Ein anderer
Zugang sind die Funktionen *princomp()* und *prcomp()* oder die library
*library(vegan)* mit der Funktion *rda()*.

Faktorladung = Korrelation zwischen Variable und Faktor Kommunalität h²
= Ausmaß an Varianz der Variable Eigenwert = Varianz des Faktors

Kaiser-Gutman-Kriterium = nur solche Faktoren sind zu interpretieren
deren Eigenwert größer 1 ist (Scree-Test.

``` r
  # APA2( ~., fkv, test=TRUE)
  # library(arm)
  # windows(5,5)
  # corrplot(fkv, abs=TRUE, n.col.legend=7)

 fit1<- Principal(fkv, 5, cut=.35)
```

    ## R was not square, finding R from data

``` r
 names(fit1$Loadings ) <- c("Item", "nr", 
                           "PC_1", "PC_2", "PC_3", "PC_4", "PC_5", "h2"  )
 fit1$Loadings %>% Output()
```

| Item | nr  | PC\_1 | PC\_2 | PC\_3 | PC\_4 | PC\_5 | h2   |
|:-----|:----|:------|:------|:------|:------|:------|:-----|
| F5   | 5   | 0.68  |       |       |       |       | 0.48 |
| F16  | 16  | 0.68  |       |       |       |       | 0.52 |
| F22  | 22  | 0.62  |       |       |       |       | 0.48 |
| F9   | 9   | 0.58  |       |       |       |       | 0.43 |
| F26  | 26  | 0.58  |       |       |       |       | 0.34 |
| F6   | 6   | 0.54  |       |       |       |       | 0.38 |
| F35  | 35  | 0.51  |       |       |       |       | 0.43 |
| F33  | 33  | 0.49  |       |       |       |       | 0.33 |
| F12  | 12  | 0.49  |       |       |       |       | 0.30 |
| F34  | 34  | 0.46  | -0.31 |       |       |       | 0.31 |
| F4   | 4   | 0.45  |       |       |       |       | 0.33 |
| F7   | 7   |       | 0.71  |       |       |       | 0.54 |
| F8   | 8   |       | 0.69  |       |       |       | 0.49 |
| F17  | 17  | -0.36 | 0.65  |       | 0.32  |       | 0.68 |
| F14  | 14  |       | 0.62  |       |       |       | 0.41 |
| F15  | 15  |       | 0.60  | 0.32  |       |       | 0.49 |
| F18  | 18  |       | 0.59  |       |       |       | 0.49 |
| F19  | 19  |       | 0.57  |       |       | 0.31  | 0.46 |
| F1   | 1   |       | 0.56  |       |       | -0.31 | 0.43 |
| F13  | 13  |       | 0.51  |       |       |       | 0.42 |
| F20  | 20  |       | 0.39  |       | 0.34  |       | 0.29 |
| F28  | 28  |       |       | 0.82  |       |       | 0.71 |
| F27  | 27  |       |       | 0.76  |       |       | 0.59 |
| F31  | 31  |       |       | -0.49 |       |       | 0.40 |
| F29  | 29  |       |       | -0.36 |       |       | 0.21 |
| F21  | 21  |       |       |       | 0.68  |       | 0.49 |
| F25  | 25  |       |       |       | 0.59  | 0.39  | 0.51 |
| F30  | 30  |       |       |       | 0.52  |       | 0.32 |
| F23  | 23  | 0.43  |       |       | 0.47  |       | 0.50 |
| F24  | 24  |       |       |       | 0.40  |       | 0.29 |
| F3   | 3   |       |       |       |       | 0.68  | 0.52 |
| F2   | 2   | 0.46  |       |       |       | 0.57  | 0.56 |
| F10  | 10  | 0.40  |       |       |       | -0.56 | 0.54 |
| F11  | 11  |       |       |       |       | 0.40  | 0.17 |
| F32  | 32  |       |       |       |       | -0.31 | 0.25 |

``` r
 fit1$Eigenvalue %>% Output()
```

| Source                |    PC1|    PC2|    PC3|    PC4|    PC5|
|:----------------------|------:|------:|------:|------:|------:|
| SS loadings           |  4.388|  3.972|  2.396|  2.256|  2.085|
| Proportion Var        |  0.125|  0.113|  0.068|  0.064|  0.060|
| Cumulative Var        |  0.125|  0.239|  0.307|  0.372|  0.431|
| Proportion Explained  |  0.291|  0.263|  0.159|  0.149|  0.138|
| Cumulative Proportion |  0.291|  0.554|  0.712|  0.862|  1.000|

``` r
 fit1$Test %>% Output()
```

| Measures             | Statistics           |
|:---------------------|:---------------------|
| n.obs                | 160                  |
| Mean item complexity | 1.8                  |
| RMSR                 | 0.07                 |
| empirical chi square | X2=815.59, p&lt;.001 |

Test der Eignung für die PCA Kaiser-Meyer-Olkin (KMO) **KMO&gt;0.80**
ist gut. (KMO ist die partiellen Korrelationen zwischen den Itempaaren)
Der **Bartlett Test** testet die Spherizität, überprüft die
Nullhypothese, ob die Korrelationsmatrix eine Identitätsmatrix ist(gut
ist wenn p &lt; .050).

``` r
 fit1$KMO %>% Output()
```

| Measures                      | Statistic                             |
|:------------------------------|:--------------------------------------|
| Kaiser-Meyer-Olkin Measure    | 0.71                                  |
| Bartlett’s test of sphericity | X2<sub>(595)</sub>=1708.23, p&lt;.001 |
| **Faustregel nach Bortz:**    |                                       |

unter n&lt;60 ist keine Faktorenanalyse durchführbar, ausreichend sind
n=100 die kann Faktorenstruktur interpretiert werden… - wenn auf einem
Faktor mindestens vier Variablen Ladungen **h&gt;0.60** aufweisen  
- oder wenn für zehn oder mehr Variablen Ladungen h&gt;0.40 vorliegen -
oder wenn weniger als zehn Variablen eine h&gt;0.40 aufweisen und die
Stichprobe n&gt;300 Probanden enthält

#### Beispiel FKV

Freiburger Fragebogen zur Krankheitsverarbeitung (Buehl Seite 473)

Quelle:<http://statistikguru.de/spss/hauptkomponentenanalyse/auswerten-und-berichten.html>
Es wurde eine Hauptkomponentenanalyse durchgeführt, um die wichtigsten,
unabhängigen Faktoren zu extrahieren. Das Kaiser-Meyer-Olkin-Kriterium
war 0.71 und der Bartlett-Test hoch signifikant (p &lt; .001), was eine
ausreichend hohe Korrelation zwischen Items darstellt, um eine
Hauptkomponentenanalyse durchzuführen. Nur Faktoren mit Eigenwerten ≥ 1
wurden in Betracht gezogen (Guttman, 1954; Kaiser, 1960). Eine
Überprüfung des Kaiser–Kriteriums und Scree-Plots rechtfertigte die
Extraktion von zwei Faktoren, jeweils mit Eigenwerten über 1, die eine
Gesamtvarianz von 43% aufklären. Unter den Lösungen lieferte die Varimax
rotierte zweifaktor-Lösung die Lösung, die am besten zu interpretieren
war, bei der die meisten Items nur auf einen der beiden Faktoren hohe
Ladungen zeigten. Die Faktoren konnten den theoretischen Begriffen
Verarbeitung, Coping, Vertrauen, Religion, Distanz und Verarbeitung
zugeordnet werden

### Mediation, Moderation, SEM, Sobel-Test

Der Sobel Test ist der indirekt Effekt also das Produkt aus Mediator und
Einfluss-variable. Das Verfahren ist alt (1986) moderneres Verfahren ist
**bootstrapping** oder auch Struktur-Gleichung-Modelle (SEM).

-   the mediation conventions (from Baron and Kenny, 1986)
    -   Path a = IV to MedV
    -   Path b = MedV to DV, controlling for the IV (when multiplied
        together these are the INDIRECT effect)
    -   + Path c = IV to DV (aka the TOTAL effect)

    -   Path c = IV to DV, controlling for the MedV (aka the DIRECT
        effect)
    -   Total effects = Indirect effect + Direct effect

### SEM

-   Chi-Quadrat-Wert Validität des Models H0: empirische Kovarianz
    entspricht modelltheoretischer Kovarianz Chi-Quadrat/df möglichst
    klein (Chi-Quadrat/df&lt;2.5 oder p&lt;0.100) Ist nur zuverlässig
    wenn eine Normalverteilung und eine ausreichend große Stichprobe
    gegeben ist.

-   Goodness-of-Fit-Index (GFI) Ist vergleichbar mit dem
    Bestimmtheitsmass in der Regressionsanalyse, also ein Mass für die
    erklärende Varianz GFI&gt;0.90

-   Adjusted-Goodness-of-Fit-Index (AGFI) Analog wie GFI nur korrigiert
    durch df und Anzahl an Variablen AGFI&gt;0.90

-   Normed-Fit-Index NFI Vergleicht das Modell mit einem Modell bei dem
    alle manifesten Variablen unkorreliert angenommen werden NFI&gt;0.90

-   Comparative-Fit-Index Wie NFI nur korrigiert durch df und Anzahl an
    Variablen CFI&gt;0.90

-   Root-Mean-Square-Error of Approximation (RMSEA) RMSEA&lt;0.05

Backhaus Multivariate Analysemethoden 11 AuflageSeite 383

Moosbrugger, Kelava 2012 Testtheorie 2. Auflage Seite 339

| Fit-Mass | Guter Fit | Akzeptabler Fit |
|----------|-----------|-----------------|
| Chi2/df  | .00-2.00  | 2.01-3.00       |
| RMSEA    | .000-.050 | 051.-.080       |
| CFI      | .970-1.00 | .950-.969       |
| NFI      | .950-1.00 | .900-.949       |

Anmerkungen zu Methoden
=======================

Hypothesen vs. Hypothesentest
-----------------------------

Hypothesen und Hypothesentest sind nicht das Gleiche. Wenn ein
Signifikanztest durchführt wird, ist das nicht automatisch ein
Hypothesentest sondern das ist meistens ein explorativer
Signifikanztest. Hypothesentests liegen nur vor, wenn man eine
A-priori-Hypothese aufstellt (Bortz \[3\] Seite 378).

Signifikanz, p-Wert
-------------------

Der p-Wert ist nicht die Signifikanz(-Zahl) und der p-Wert sagt nichts
über die Stärke der Unterschiede aus. Der p-Wert ist auch kein Maß oder
Beweis dafür, ob eine Unterschied besteht. Wenn p-Werte angegeben
werden, liegt nicht automatisch ein Hypothesentest vor. Trotzdem
verwende immer aus pragmatischen Gründen die Begriffe Signifikanz-Test
und Hypothesentest sowie p-Werte und Signifikanz (fälschlicherweise) als
gleichbedeutende Begriffe.

Literatur
=========

\[1\] Achim Bühl, (2014), SPSS 22 Einführung in die moderne
Datenanalyse, 14. aktualisierte Auflage, Pearson

\[2\] APA, 2009, Publication Manual of the American Psychological
Association

\[3\] Daniel Wollschläger (2012), Grundlagen der Datenanalyse mit R:
Eine anwendungsorientierte Einführung 2. Aufl., Heidelberg: Springer

\[4\] Jürgen Bortz, Nicola Döring, (2006), Forschungsmethoden und
Evaluation, Heidelberg: Springer

\[5\] Jürgen Bortz, Christof Schuster, (2010), Statistik für Human- und
Sozialwissenschaftler, 7. Aufl., Heidelberg: Springer \[6\] John Fox,
Sanford Weisberg, (2011), An R Companion to Applied Regression, Second
Edition, Sage

\[7\] Lothar Sachs, Jürgen Hedderich, (2006), Angewandte Statistik,
12.Aufl. Heidelberg: Springer

\[9\] Data Analysis Using Regression and Multilevel/Hierarchical Models;
Cambridge;2009; Gelman, Hill

\[10\] Torsten Hothorn, Brian S., Everitt, (2009), A Handbook of
Statistical Analyses Using R, Second Edition, Chapman and Hall/CRC

\[11\] Frank Harrell, PhD An Introduction to S and The Hmisc and Design
Libraries
<https://cran.r-project.org/doc/contrib/Alzola+Harrell-Hmisc-Design-Intro.pdf>
(18-02-2016)

\[12\] Hemmerich MatheGuru
<http://matheguru.com/stochastik/effektstaerke.html> (18-02-2016)

\[13\] R Core Team (2015). R: A language and environment for statistical
computing. R Foundation for Statistical Computing, Vienna, Austria. URL
<http://www.R-project.org/>.

\[14\] “Frank E Harrell Jr <f.harrell@vanderbilt.edu>, (2017), Hmisc:
Harrell Miscellaneous, URL <https://CRAN.R-project.org/package=Hmisc>”

Literatur \[15\] Stoyan, D., Stoyan, H., Jansen, U. Umweltstatistik.
Teubner, Stuttgart, Leipzig, 1997.

\[16\] Stier, W. Empirische Forschungsmethoden. 2. Au age, Springer,
Berlin, 1999.

\[17\] Backhaus, K., Erichson, B., Plinke, W. & Weiber, R. Multivariate
Analysemethoden. 12. Au age, Springer, Berlin, 2008.

More Examples
=============

Daten aufbereiten
-----------------

-   GetData: Liest Daten ein vor allem für Spezialfälle wie LimeSurvy
    oder auch Texte

-   CleanUp: Bereinigen CleanUp\_factor, Drop\_NA, Label

<!-- -->

    Steko<- GetData("Steko.sav") %>% 
               Drop_NA(key) %>% 
               mutate(jahr = factor(jahr)) %>%
               Label( BMI = "Body-Mass-Index",
                      WHtR =  "Waist-Height-Ratio",
                      WHtR_1 ="Waist-Height-Ratio",
                      bildprof = "Bildungsprofil",
                      jahr = "Jahr")

-   upData2: Kann labels und levels bearbeiten. Levels geht nur über ein
    DataDictionary wo die levels definiert werden.

<!-- -->

    data<- data.frame(
            g= gl(2, 8, labels = c("Control", "Treat")),
            a=rpois(16, 5),
            b=rep(1:2,8),
            c=rnorm(16))

    data$d<-rep(c("a", "b", "c"), 6)[1:16]

    upData2(data, labels = c(g="Gruppe", a="A", b="B"))

    datadir <- GetData("
              names  labels    levels
                g     Gruppe    NULL
                a     A         numeric
                b     Sex       male;female
                d     DNA       a;b
                c     C         NULL")

    upData2(data, datadir)



    # oder auch

    hkarz$Tzell <- cut(hkarz$tzell, 4)
     upData2(hkarz, data.frame(
       names=c("gruppe", "Tzell", "lai"),
       levels=c("gesund;krank", "Low;;Med;", "neg;pos")
    )) 

Mit **separate\_multiple\_choice()** können Strings mit einem
Trennzeichen zu einem Mehrfachantworten-Set aufgespalten werden. Erlaubt
ist auch ein String mit Zahlen dabei ist aber der Wertebereich von 1 bis
11 begrenzt.

``` r
x <- c(123, 23, 456,3)
separate_multiple_choice(x,
                          label = c("Allgemein", "Paro", 
                          "Endo", "Prothetik",
                           "Oral chirurgie",
                          "Kieferorthopedie"))
```

    ## Warning: Too few values at 4 locations: 1, 2, 3, 4

    ##    x_1  x_2  x_3  x_4  x_5  x_6
    ## 1   ja   ja   ja nein nein nein
    ## 2 nein   ja   ja nein nein nein
    ## 3 nein nein nein   ja   ja   ja
    ## 4 nein nein   ja nein nein nein

-   Daten Manipulation mit Drop\_ Select\_ transform2

<!-- -->

    Drop_NA(data)
    Drop_case(data, 1:2)
    Select_case(data, g=="Control")
    transform2(data, a=a+1)

-   Melt: Melt2, gather, spread

<!-- -->


    #library(tidyverse)
    #library(stp25aggregate)
    #library(stp25data)

    hyper1<-hyper[, c("g","chol0","chol1","chol6","chol12")]
    hyper_long<- Melt2(hyper1, id.vars=1)
    aggregate( value~variable, hyper_long, mean)

    hyper_long<-Melt2(chol0+chol1+chol6+chol12~g, 
                      hyper, "Zeit", "Cholesterin")
    #-- Spread + aggragate
    aggregate(Cholesterin~Zeit+g, hyper_long, mean) %>% 
      spread(g, Cholesterin) 

    #-- das gleiche wie aggragate
    hyper_long %>% group_by(Zeit, g) %>%
      summarise(Cholesterin=mean(Cholesterin)) %>%
      spread(g, Cholesterin)

    #-- Gather das gleiche wie oben aber ohne die Labels
     hyper  %>% 
       tidyr::gather("time", "chol", chol0:chol12) %>%
       dplyr::select(g, time, chol) 

-   ReCast: **Recast2()** kombination aus Melt2 + aggregate + spread

<!-- -->

     #-- Recast2 das gleiche wie oben
     Recast2(chol0+chol1+chol6+chol12~g, hyper, mean, 
             "Zeit", "Cholesterin") %>%
       spread(g, Cholesterin)
     
     #-- Recast2 das gleiche wie oben nur ohne  spread
    Recast2(chol0+chol1+chol6+chol12~g, hyper, mean, 
            formula=variable~g) 
     
     #-- und hier mit margins
    Recast2(chol0+chol1+chol6+chol12~g, hyper, 
            mean,formula=variable~g, margins=TRUE)

Messmethoden Weibull-Modul
--------------------------

Beim Weibull-Modul handelt es sich um einen statistischen Wert, der
unter anderem auch in Bezug auf die Materialermüdung von spröden
Materialien verwendet wird. Dabei gilt: Ist der Weibull-Modul hoch,
brechen Proben immer ab der gleichen Belastung. Ist der WeibullModul
niedrig, ergeben sich Schwankungen in den Belastungswerten. Anmerkung
bei Kunde 674 Nadine Wanda verwendet. Weitere Info unter:
<https://stats.stackexchange.com/questions/60511/weibull-distribution-parameters-k-and-c-for-wind-speed-data>

``` r
#Beispiel: Sachs Seite 337
#Scheuerfestigkeit eines Garns
garn  <- c( 550,  760,  830,  890, 1100, 
           1150, 1200, 1350, 1400, 1600, 
           1700, 1750, 1800, 1850, 1850, 
           2200, 2400, 2850, 3200)

Weibull<- function(x){# empirische Verteilungsfunktion
x <- sort(x); n <- length(x); i <- rank(x)
 if(n < 50) F_t <- (i - 0.3) / (n+0.4)  
 else       F_t <- i/(n+1)

 data.frame(data=x, x=log(x),
      y =log(log(1/(1-F_t))))
}

data<-Weibull(garn) 
z <- lm(y ~ x, data)
  res<-round(cbind(shape=coef(z)[2],             
                   scale=exp(-(coef(z)[1]/coef(z)[2]))), 2) 
res
```

    ##   shape   scale
    ## x  2.51 1807.45

``` r
m<-(-(coef(z)[1]/coef(z)[2]))
par(mfrow=c(1,2), lwd=2, font.axis=2, bty="n", ps=11,
    bg="white", las=1, cex=1.1)

plot(data$x, data$y, main="Weibull-Diagram",
     xlab="x=log(Garn)", ylab="y=log(log(1/(1-F)))", 
     xlim=c(5.9,8.5), ylim=c(-4, 2), axes = FALSE, pch=16, cex=1.0)
abline(z)
my.at<- signif(c(500, 1000, exp(m), 5000),4)
axis(1, at = log(my.at), labels = my.at)
axis(2)
i <- rank(data$x)
n <- length(data$x)
v.unten <- 1 / (((n-i+1)/i)*(qf(0.025, 2*(n-i+1), 2*i))+1)
v.oben  <- 1 - 1 / (1 + (i / (n-i+1)*qf(0.025, 2*i, 2*(n-i+1))))
lines(data$x, log(log(1/(1-v.oben))), lty=2)
lines(data$x, log(log(1/(1-v.unten))), lty=2)
abline(h=0, lty=3)
abline(v=m, lty=3)
points(m, 0, cex=5, col="red")


library(MASS)
(fit <- fitdistr(garn, densfun="weibull", lower = 0))
```

    ##       shape          scale    
    ##      2.5071425   1813.1921763 
    ##  (   0.4370025) ( 175.6967842)

``` r
library(car)
qqPlot(garn, distribution="weibull", main="qqPlot",
       scale=coef(fit)[1], shape=coef(fit)[2], las=1, pch=19)
```

![](C:\Users\wpete\AppData\Local\Temp\RtmpoxDuEs\preview-2f943e3f58f3.dir\Dokumentation_files/figure-markdown_github/unnamed-chunk-38-1.png)
