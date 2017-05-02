###############################################################
##########DEFENSE

getdefvars<-function(tabletext){
  #trimming data
  x<-sub("\n                                    ", "", tabletext)
  x<-sub("\\d{2,3}(?=\\D)", "", x, perl=T)#match two or three digits before the first non-digit charact, sub by nothing
  x<-sub(" / Bloqueos", "", x)
  x<-gsub("\\. ", "", x)
  x<-gsub("\\: ", "", x)
  x<-gsub('([[:upper:]])', ' \\1', x)
  x<-gsub("Entradas Con éxito", "Entradasconéxito", x)
  x<-gsub("Sin éxito", "Sinéxito", x)
  x<-gsub("Último hombre", "Últimohombre", x)
  x<-gsub("Duelos Cuerpo a cuerpo", "DuelosCuerpoacuerpo", x)
  x<-gsub("Aéreos", "DuelosAéreos", x)
  x<-gsub("Sinéxito", "EntradasSinéxito", x)
  
  y<-as.data.frame(strsplit(x, " "))
  y<-as.character(y[3:12,])
  y<-y[-9]
  y<-sub("\\s+$", "", y)
  
  z<-gsub("[[:digit:]]", "", y)
  z<-gsub("Dispbloqueados", "DispBloqueados", z)
  z<-gsub("Despejes  ", "Despejes", z)
  z<-gsub("Entradasconéxito", "EntradasCéxito", z)
  z<-gsub("EntradasSinéxito", "EntradasSéxito", z)
  z<-gsub("Últimohombre  " , "Últimohombre" , z)
  z<-gsub("DuelosCuerpoacuerpo,", "DuelosCuerpoacuerpo%", z)
  z<-gsub("DuelosAéreos,", "DuelosAéreos%", z)
  return(z)
}


getdefstats<-function(tabletext){
  #trimming data
  x<-sub("\n                                    ", "", tabletext)
  x<-sub("\\d{2,3}(?=\\D)", "", x, perl=T)#match two or three digits before the first non-digit charact, sub by nothing
  x<-sub(" / Bloqueos", "", x)
  x<-gsub("\\. ", "", x)
  x<-gsub("\\: ", "", x)
  x<-gsub('([[:upper:]])', ' \\1', x)
  x<-gsub("Entradas Con éxito", "Entradasconéxito", x)
  x<-gsub("Sin éxito", "Sinéxito", x)
  x<-gsub("Último hombre", "Últimohombre", x)
  x<-gsub("Duelos Cuerpo a cuerpo", "DuelosCuerpoacuerpo", x)
  x<-gsub("Aéreos", "DuelosAéreos", x)
  x<-gsub("Sinéxito", "EntradasSinéxito", x)
  
  y<-as.data.frame(strsplit(x, " "))
  y<-as.character(y[3:12,])
  y<-y[-9]#es un %
  y<-sub("\\s+$", "", y)
  
  z<-gsub("[[:digit:]]", "", y)
  z<-gsub("Dispbloqueados", "DispBloqueados", z)
  z<-gsub("Despejes  ", "Despejes", z)
  z<-gsub("Entradasconéxito", "EntradasCéxito", z)
  z<-gsub("EntradasSinéxito", "EntradasSéxito", z)
  z<-gsub("Últimohombre  " , "Últimohombre" , z)
  z<-gsub("DuelosCuerpoacuerpo,", "DuelosCuerpoacuerpo%", z)
  z<-gsub("DuelosAéreos,", "DuelosAéreos%", z)
  
  w<-gsub("[[:alpha:]]", "", y)
  w[8:9]<-str_extract(w[8:9], "\\d{2}(?=\\,)")
  w<-as.numeric(w)
  return(w)}

#############################
#construcción

getconsvars<-function(tabletextcons){# puede que sea bueno que guarde tb los pases malos
  #trimming data
  x<-sub("\n                                    ", "", db$tabletextcons[4])
#  x<-sub("\\d{2,3}(?=\\D)", "", x, perl=T)#match two or three digits before the first non-digit charact, sub by nothing
#  x<-sub("[^Pases]", "", x)#no funciona
  x<-sub("Detalle", "", x)
  x<-gsub("\\. ", "", x)
  x<-gsub("\\: ", "", x)
  x<-gsub('([[:upper:]])', ' \\1', x)
  x<-gsub("Pases Largos", "PasesLargosP", x)
  x<-gsub("Al hueco", "AlHuecoP", x)
  x<-gsub("Total hacia delante", "HaciaDelante", x)
  x<-gsub("Total hacia detrás", "HaciaDetras", x)
  x<-gsub("Córners sacados", "CornersSacados", x)
  x<-gsub("Total", "TotalPasesP", x)
  x<-gsub("Cortos", "CortosP", x)
  x<-gsub("Centros", "CentrosP", x)
  
  x<-gsub("Pases clave", "PasesClave", x)
  
  y<-as.data.frame(strsplit(x, " "))
  y[,1]<-as.character(y[,1])
  y[2,1]<-paste(y[1,1], y[2,1], collapse="")
  y<-as.character(y[2:17,])
  y<-y[c(-3, -5, -7, -9, -13, -15)]
  y<-sub("\\s+$", "", y)
  
 z<-gsub("[[:digit:]]", "", y)
  z<-gsub(",", "", z)
  
#  z<-c("PLargosB", "PLargosP", "PCortosB", "PCortosP", "TPasesB", "TPasesP", "AlHuecoB",
#    "AlHuecoP", "HDelante", 
#"HAtras", "CentrosB", "CentrosP", "CornersB", "CornersP", "Clave")#al final tardaba menos si escribia esto
  
  return(z)
  
  
}


getconsstats<-function(tabletextcons){
  #trimming data
  x<-sub("\n                                    ", "", tabletextcons)
  #  x<-sub("\\d{2,3}(?=\\D)", "", x, perl=T)#match two or three digits before the first non-digit charact, sub by nothing
  #  x<-sub("[^Pases]", "", x)#no funciona
  x<-sub("Detalle", "", x)
  x<-gsub("\\. ", "", x)
  x<-gsub("\\: ", "", x)
  x<-gsub('([[:upper:]])', ' \\1', x)
  x<-gsub("Pases Largos", "PasesLargosP", x)
  x<-gsub("Al hueco", "AlHuecoP", x)
  x<-gsub("Total hacia delante", "HaciaDelante", x)
  x<-gsub("Total hacia detrás", "HaciaDetras", x)
  x<-gsub("Córners sacados", "CornersSacados", x)
  x<-gsub("Total", "TotalPasesP", x)
  x<-gsub("Cortos", "CortosP", x)
  x<-gsub("Centros", "CentrosP", x)
  
  x<-gsub("Pases clave", "PasesClave", x)
 
  y<-as.data.frame(strsplit(x, " "))
  y[,1]<-as.character(y[,1])
  y[2,1]<-paste(y[1,1], y[2,1], collapse="")
  y<-as.character(y[2:17,])
  y<-y[c(-3, -5, -7, -9, -13, -15)]
  y<-sub("\\s+$", "", y)

  
  w<-gsub("[[:alpha:]]", "", y)
  w<-gsub(",0", "", w)
  w<-gsub("\\.", "", w)
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))}
  
  w[c(2,3,4,5,8,9)]<-substrRight(w[c(2,3,4,5,8,9)], 2)
  w<-sub(" ", "", w)
  w<-as.numeric(w)
  return(w)
  }



###################################################
##################Tiros


for (i in 1:length(db$playersurl)){
  db$tabletexttiros[i]<-html(db$playersurl[i])%>% 
    html_node("#estadisticas-ataque") %>% #read .id or #class or arg
    html_text()}#c


gettirvars<-function(tabletextcons){# puede que sea bueno que guarde tb los pases malos
  #trimming data
  x<-sub("\n                                    ", "", db$tabletexttiros[4])
  #  x<-sub("\\d{2,3}(?=\\D)", "", x, perl=T)#match two or three digits before the first non-digit charact, sub by nothing
  #  x<-sub("[^Pases]", "", x)#no funciona
  #x<-sub("Detalle", "", x)
  #x<-gsub("\\. ", "", x)
  #x<-gsub("\\: ", "", x)
  x<-gsub('([[:upper:]])', ' \\1', x)
  x<-gsub("Tiros En Total", "PasesLargosP", x)
  x<-gsub("Al hueco", "AlHuecoP", x)
  x<-gsub("Total hacia delante", "HaciaDelante", x)
  x<-gsub("Total hacia detrás", "HaciaDetras", x)
  x<-gsub("Córners sacados", "CornersSacados", x)
  x<-gsub("Total", "TotalPasesP", x)
  x<-gsub("Cortos", "CortosP", x)
  x<-gsub("Centros", "CentrosP", x)
  
  x<-gsub("Pases clave", "PasesClave", x)
  
  y<-as.data.frame(strsplit(x, " "))
  y[,1]<-as.character(y[,1])
  y[2,1]<-paste(y[1,1], y[2,1], collapse="")
  y<-as.character(y[2:17,])
  y<-y[c(-3, -5, -7, -9, -13, -15)]
  y<-sub("\\s+$", "", y)
  
  z<-gsub("[[:digit:]]", "", y)
  z<-gsub(",", "", z)
  
  #  z<-c("PLargosB", "PLargosP", "PCortosB", "PCortosP", "TPasesB", "TPasesP", "AlHuecoB",
  #    "AlHuecoP", "HDelante", 
  #"HAtras", "CentrosB", "CentrosP", "CornersB", "CornersP", "Clave")#al final tardaba menos si escribia esto
  
  return(z)
  
  
}


a<-getconsvars(db$tabletext[1])



#this is another test

