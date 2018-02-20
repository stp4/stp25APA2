#- name auf Funktionsaufruf extrahieren
# Name <- Funktion(...)
# grap_call_name(Name)
grap_call_name <- function(x) {
  rsl <- deparse(substitute(x))
  unlist(strsplit(rsl, " <- "))[1]
}