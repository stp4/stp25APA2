

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



#' @param data in kano_barchart molten data
#' @param groups in kano_barchart geht nicht
#' @param auto.key Kea auf der rechten Seite
#' @param prop.table Prozent oder Anzahl
#' @param ylab Beschriftung Prozent/Anzahl
#' @param col Farben RColorBrewer::brewer.pal(6, "Dark2")[c(4, 1, 2, 3, 5, 6)]
#' @param include.Q,include.R Q und R anzeigen
#' @param  main,include.n Ueberschrift mit N=
#' @param levels Levels haendisch anordnen
#' @param ... an lattice
#'
#' @return Tabelle als data.frame
#' @rdname Kano
#' @export
#'
#' @examples
#' #' 
#' graphics.off()
#' 
#' # res1 <-  Kano( ~ . , DF[-c(1,2)])
#' res1$dysfunc
#' windows(9, 7)
#' kano_plot_del_bar(kano_res)
kano_barchart  <- function(x,
                           # fm =  ~ variable + value,
                           data = x$molten,
                           groups = x$groups[1], # geht noch nicht
                           main="Kano-Analyse",
                           #    if(is.null(groups))print(p2)
                           #   else  print(useOuterStrips(p2))
                           
                           auto.key = list(space = "right"),
                           prop.table = TRUE,
                           ylab =  if (prop.table)
                             "Prozent"
                           else
                             "Anzahl",
                           
                           col = RColorBrewer::brewer.pal(6, "Dark2")[c(4, 1, 2, 3, 5, 6)],
                           par.settings = list(superpose.polygon = list(col = col)),
                           include.Q = TRUE,
                           include.R = TRUE,
                           include.n=TRUE,
                           levels = c("M", "O", "A", "I", "R", "Q"),
                           ...) {
  
  if(!is.null(groups)) warnings("Gruppen sind nicht Implementiert!")
  
  if (!include.Q & include.R)
    data$value <- factor(data$value, c("M", "O", "A", "I", "R"))
  else if (!include.Q &  !include.R)
    data$value <- factor(data$value, c("M", "O", "A", "I"))
  else if (include.Q & !include.R)
    data$value <- factor(data$value, c("M", "O", "A", "I", "Q"))
  else {data$value <- factor(data$value, levels)  }#
  
  
  data <- na.omit(data)
  
  fm1 <-  "~variable+value"
  # fm2<- Freq ~ value|variable
  # if(!is.null(groups)){ fm1 <-paste(fm1, " + " , groups)}
  
  
  datatab <- xtabs(formula(fm1), data)
  N<- addmargins(datatab,2)[,"Sum"]
  
  if(include.n) main<- paste0(main, " (N = ",  max(N, na.rm=TRUE), ")")
  
  if (prop.table)  
    dat  <- data.frame(prop.table(datatab , 1) * 100)
  else  dat  <- data.frame(datatab)
  #  print(datatab)
  
  dat$dummy <- ""
  
  
  
  
  p1 <- lattice::barchart(
    Freq ~ dummy | variable,
    data = dat,
    ylab = ylab,main=main,
    groups = value,
    #  horizontal=FALSE, stack = TRUE,
    origin = 0,
    auto.key = auto.key,
    par.settings = par.settings ,
    ...
  )
  
  print(p1)
  
  invisible(addmargins(datatab,2))
}
