#applythefunctionsforonetema 
#andmakethedb

require(rvest)
library(stringr)

###for full list of teams and urls
liga<-html("http://www.laliga.es/laliga-santander")

teamurl<-liga %>%
  html_nodes("#equipos")%>% 
  html_nodes("div")%>%   html_nodes("a")%>% 
  html_attr("href")

team<-liga %>%
  html_nodes("#equipos")%>% 
  html_nodes("div")%>%   html_nodes("a")%>% 
  html_nodes("span")%>% 
  html_nodes("span.nombre")%>% 
  html_text()


#db<-data.frame(team=rep(team, each=30), teamurl=rep(teamurl, each=30))#30 is not the best value here
#db$teamurl<-as.character(db$teamurl)


#############################################


  playersurl<-html(teamurl[2]) %>% html_nodes("#box-plantilla-equipo")%>%#truncated
  html_nodes(".box-jugador")%>%
  html_attr("href")

if(any(is.na(playersurl)) == TRUE){
  playersurl<-playersurl[-which(is.na(playersurl))]}

dorsal<-integer(length = length(playersurl))

  for(i in 1:length(playersurl)){
  dorsal[i]<- html(playersurl[i]) %>% html_nodes("#ficha-jugador") %>%
  html_node("#datos-jugador")  %>%
  html_node("#dorsal.info") %>% 
  html_text()
  }

if(any(is.na(dorsal)) == TRUE){
  dorsal<-dorsal[-which(is.na(dorsal))]}

db<-data.frame(dorsal, playersurl)

for(i in 1:length(playersurl)){
db$name[i]<- html(playersurl[i]) %>% html_nodes("#ficha-jugador") %>%
  html_node("#datos-jugador")  %>%
  html_node("#nickname") %>% 
  html_text()
}
timesrep<-length(db$playersurl)
db$team<-rep(team[2], 1)

db<-db[,c(4,3,1,2)]
db$playersurl<-as.character(db$playersurl)


###############################################################
##########apply functions (in functions... .R)

for (i in 1:length(db$playersurl)){
  db$tabletext[i]<-html(db$playersurl[i])%>% 
    html_node("#estadisticas-defensa") %>% #read .id or #class or arg
    html_text()#c
}#defense data $tableteXT

#db<-db[-which(is.na(db$tabletext)), ]#remove NAs. actually gk are NA. This must be cleared later

a<-getdefvars(db$tabletext[6])#fill the defensive data. 6 to bypass gk

#now get the values for everyone
b<-as.data.frame(matrix(ncol = length(a), nrow=length(db$playersurl)))

for (i in 1:length(db$tabletext)){
  b[i,]<-getdefstats(db$tabletext[i])
}

colnames(b)<-a
db<-cbind(db, b)#merge play-making data
db<-db[,-(which(names(db)=="tabletext"))]

####play-making

for (i in 1:length(db$playersurl)){
  db$tabletextcons[i]<-html(db$playersurl[i])%>% 
    html_node("#estadisticas-contruccion") %>% #read .id or #class or arg
    html_text()#c
}#play-making data $tabletextcons

a<-getconsvars(db$tabletextcons[which(!is.na(db$tabletextcons))])

b<-as.data.frame(matrix(ncol = length(a), nrow=length(db$playersurl)))

for (i in 1:length(db$name)){
  b[i,]<-getconsstats(db$tabletextcons[i])
}

colnames(b)<-a
db<-cbind(db, b)#merge play-making data
db<-db[,-(which(names(db)=="tabletextcons"))]


###tiros


for (i in 1:length(db$playersurl)){
  db$tabletexttiros[i]<-html(db$playersurl[i])%>% 
    html_node("#estadisticas-ataque") %>% #read .id or #class or arg
    html_text()}#c

a<-gettirvars(db$tabletexttiros[10])


b<-as.data.frame(matrix(ncol = length(a), nrow=length(db$playersurl)))

for (i in 1:length(db$name)){
  b[i,]<-gettirstats(db$tabletexttiros[i])
}

colnames(b)<-a
db<-cbind(db, b)#merge play-making data
db<-db[,-(which(names(db)=="tabletexttiros"))]


###########goles


for (i in 1:length(db$playersurl)){
  db$tabletextgoles[i]<-html(db$playersurl[i])%>% 
    html_node("#estadisticas-goles") %>% #read .id or #class or arg
    html_text()}#c

a<-getgolvars(db$tabletextgoles[10])

b<-as.data.frame(matrix(ncol = length(a), nrow=length(db$playersurl)))


for (i in 1:length(db$name)){
  b[i,]<-getgolstats(db$tabletextgoles[i])
}

colnames(b)<-a
db<-cbind(db, b)
db<-db[,-(which(names(db)=="tabletextgoles"))]

