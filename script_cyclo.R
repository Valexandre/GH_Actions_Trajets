library(tidyverse)
library(jsonlite)
# On met un commentaire avec des accents é a à
tweetbot_token <- rtweet::rtweet_bot(
  api_key = Sys.getenv("T_API_KEY"),
  api_secret = Sys.getenv("T_API_SECRET"),
  access_token = Sys.getenv("T_ACCESS_TOKEN"),
  access_secret = Sys.getenv("T_ACCESS_SECRET")
)
rtweet::auth_as(tweetbot_token)

URLParis<-"http://www.infoclimat.fr/public-api/gfs/json?_ll=48.85341,2.3488&_auth=CRNeSVMtBiQDLlptBXNXfgBoAzYPeVN0An4AYwBlUy5VPlc2AWFWMFI8VCkBLlBmWHVSMVliVGQHbAF5CHoEZQljXjJTOAZhA2xaPwUqV3wALgNiDy9TdAJgAGMAalMuVTdXNQFjVipSPFQ%2BATVQelhqUjtZZFRzB3sBZwhjBGMJbF4%2FUzkGYANtWj8FPVd8ACwDZQ8yU2kCZABnAGVTOFU0VzcBMFZmUm5UMQE5UHpYaFI1WW9UbAdnAWYIZARlCXVeJVNJBhcDcVp4BXdXNgB1A34PZVM1AjU%3D&_c=8f5a43200f9e580178156325786c4183"
DonneesVille<-fromJSON(URLParis)
datefixee<-Sys.Date()

matin<-paste0(datefixee," 07:00:00")
am<-paste0(datefixee," 16:00:00")
DonneesVilleU<-DonneesVille %>% 
  enframe %>%
  filter(name%in%c(matin,am))%>%
  unnest
Donnees<-tibble(quand=c("matin","après-midi"),
                tempk=c(DonneesVilleU[[2]][[1]][["sol"]],DonneesVilleU[[2]][[13]][["sol"]]),
                pluie=c(DonneesVilleU[[2]][[3]],DonneesVilleU[[2]][[15]]),
                vent=c(DonneesVilleU[[2]][[6]][["10m"]], DonneesVilleU[[2]][[18]][["10m"]]))
Donnees<-Donnees%>%mutate(tempC=tempk-273.15)

rtweet::post_tweet(token = tweetbot_token, status = paste0("🚲 Bien le bonjour à toi cycliste de Paris. 🕖 Ce matin, température à 7h : ",round(Donnees$tempC[1],2),"°C, et ",round(Donnees$pluie[1],1),"mm de pluie sur 3h. Vent : ",round(Donnees$vent[1],2) ,"km/h. 🕓 Cet après-midi à 16h : ",round(Donnees$tempC[2],2),"°C et ",round(Donnees$pluie[2],1),"mm de pluie. Vent : ",round(Donnees$vent[2],2) ,"km/h. #MétéoCyclo"))
