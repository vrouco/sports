
require(rvest)
library(stringr)

futmondo<-html("http://www.futmondo.com/team?team=52038563b8d07d930b00008a") #alavés


points<-futmondo %>% html_node("#staticPlayers") %>%
  html_node("ul") %>% html_node("li:nth-child(2)") %>% html_node("article.data") %>%
  html_node("article.points") %>% html_text()
#nth-child(i) escoge al jugador
#selector #staticPlayers > ul > li:nth-child(1) > article.data > article.points

value<-futmondo %>% html_node("#staticPlayers") %>%
  html_node("ul") %>% html_node("li:nth-child(2)") %>% html_node("article.data") %>%
  html_node("article.value") %>% html_text()

#selector #staticPlayers > ul > li:nth-child(1) > article.data > article.value


nameandposition<-futmondo %>% html_node("#staticPlayers") %>%
  html_node("ul") %>% html_node("li:nth-child(2)") %>% html_node("a") %>%
  html_text()
