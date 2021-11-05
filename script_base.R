#library(ragg)
library(tidyverse)
library(sf)
library(rtweet)

tweetbot_token <- rtweet::create_token(
  app = "TweetUnTrajet",
  consumer_key =    Sys.getenv("TWITTER_KEY"),
  consumer_secret = Sys.getenv("TWITTER_SECRET_KEY"),
  access_token =    Sys.getenv("TWITTER_TOKEN"),
  access_secret =   Sys.getenv("TWITTER_TOKEN_SECRET")
)

rtweet::rtweet_bot(
  api_key = Sys.getenv("TWITTER_KEY"),
  api_secret = "API secret",
  access_token =  Sys.getenv("TWITTER_TOKEN"),
  access_secret =  Sys.getenv("TWITTER_TOKEN_SECRET")
  )
# Example: post a tweet via the API
# The keys will are in your environment thanks to create_token()
rtweet::post_tweet(status = "This is a test tweet.")
Virg<-function(x){ as.character( gsub("\\.",",",as.character(x)))}

 # 
 # MOBPRO18<- read_delim("data/FD_MOBPRO_2018.csv", ";", escape_double = FALSE, col_types = cols(DCFLT = col_character()), trim_ws = TRUE)
 # MOBPRO18<-MOBPRO18%>%mutate(CODGEORES=case_when(ARM!="ZZZZZ"~ARM,
 #                                                 TRUE~COMMUNE))
 # 
 Varmod_MOBPRO_2018 <- read_delim("data/Varmod_MOBPRO_2018.csv", 
                                   ";", escape_double = FALSE, col_types = cols(LONG_VAR = col_skip()),trim_ws = TRUE)
 # colnames(MOBPRO18)
 # MOBPRO18_S<-MOBPRO18%>%select(CODGEORES,DCLT,AGEREVQ,CS1,DIPL,EMPL,IMMI,INATC,IPONDI,NA5,SEXE,STAT,STOCD,TYPL,TYPMR,TRANS)
 # #saveRDS(MOBPRO18_S,"MOBPRO18_S.Rdata")
 PhAT <- read_csv("data/Phrases a tweeter - Feuille 1.csv")
  JusteCommunesImportantes<-readRDS("data/VillesSelec.Rdata")
  
 
 # MOBPRO18_S_F<-MOBPRO18_S%>%filter(CODGEORES%in%JusteCommunesImportantes$INSEE_COM |
 #                                     DCLT %in% JusteCommunesImportantes$INSEE_COM)
 # 
 # MOBPRO18_S_F<-MOBPRO18_S_F%>%mutate(AGE=case_when(AGEREVQ=="015"~"15 à 19 ans",AGEREVQ=="020"~"20 à 29 ans",AGEREVQ=="025"~"20 à 29 ans",AGEREVQ=="030"~"30 à 39 ans",AGEREVQ=="035"~"30 à 39 ans",AGEREVQ=="040"~"40 à 49 ans",AGEREVQ=="045"~"40 à 49 ans",AGEREVQ=="050"~"50 à 59 ans",AGEREVQ=="055"~"50 à 59 ans",AGEREVQ=="060"~"60 à 69 ans",AGEREVQ=="065"~"60 à 69 ans",AGEREVQ=="070"~"70 et +",AGEREVQ=="075"~"70 et +",AGEREVQ=="080"~"70 et +",AGEREVQ=="085"~"70 et +",AGEREVQ=="090"~"70 et +",AGEREVQ=="095"~"70 et +",AGEREVQ=="100"~"70 et +",AGEREVQ=="105"~"70 et +",AGEREVQ=="110"~"70 et +",AGEREVQ=="115"~"70 et +",
 #                           TRUE~"PB"))%>%
 #   mutate(DIPLOME=case_when(DIPL%in%c("01","02","03","11","12","13","ZZ") ~"1.Parcours inférieur au bac",
 #                            DIPL%in%c("14","15") ~"2.Bac",
 #                            DIPL%in%c("16","17") ~"3.Du BTS à la licence (Bac +2 ou 3)",
 #                            DIPL%in%c("18","19") ~"4.Master ou plus",
 #                            TRUE~"Autre"))%>%
 #   group_by(CODGEORES,DCLT,CS1,DIPLOME,EMPL,IMMI,INATC,NA5,SEXE,STAT,STOCD,TYPL,TYPMR,TRANS,AGE)%>% 
 #   summarise(IPONDI=sum(IPONDI))
 # 
#saveRDS(MOBPRO18_S_F,"MOBPRO18_S.Rdata")
MOBPRO18_S<-readRDS("data/MOBPRO18_S_IDF.Rdata")
TouslesLibellesDesVilles <- read_csv("data/TouslesLibellesDesVilles.txt")

ComSelec<-sample(JusteCommunesImportantes$INSEE_COM[substr(JusteCommunesImportantes$INSEE_COM,1,2)%in%c(75,77,78,91:95)],1)
CarteArr<-st_read("https://raw.githubusercontent.com/Valexandre/france-geojson/master/arrondissements-millesimes0.geojson")%>%dplyr::select(code_insee,nom_com,geometry)%>%rename(code=1,nom=2)
CarteComm<-st_read("https://github.com/gregoiredavid/france-geojson/raw/master/communes-version-simplifiee.geojson")%>%dplyr::select(code,nom,geometry)
AllComm<-rbind(CarteComm,CarteArr)
Zone20Bornes<-AllComm%>%filter(code==ComSelec)%>%
  st_transform(crs=2154)%>%
  st_centroid()%>%
  st_buffer(20000)

CarteArr<-st_read("https://raw.githubusercontent.com/Valexandre/france-geojson/master/arrondissements-millesimes0.geojson")%>%dplyr::select(code_insee,nom_com,geometry)%>%rename(code=1,nom=2)
CarteComm<-st_read("https://github.com/gregoiredavid/france-geojson/raw/master/communes-version-simplifiee.geojson")%>%dplyr::select(code,nom,geometry)
AllComm<-rbind(CarteComm,CarteArr)
Zone20Bornes<-AllComm%>%filter(code==ComSelec)%>%
  st_transform(crs=2154)%>%
  st_centroid()%>%
  st_buffer(20000)

PhATL<-PhAT%>%filter(categorie=="ouva_carte")%>%
  slice_sample(.,n=1)%>%
  select(part1,part2)

OuVaTravaillerLaCommune<-MOBPRO18_S%>%filter(CODGEORES==ComSelec)%>%
    group_by(DCLT)%>%
    summarise(Act=round(sum(IPONDI),0))%>%
    ungroup()%>%
    mutate(Part=round(Act/sum(Act)*100,2))
 
    
    GraphOuVaTravaillerLaCommune<-OuVaTravaillerLaCommune%>%top_n(.,n=3,wt = Part)%>%
      left_join(Varmod_MOBPRO_2018%>%filter(COD_VAR=="COMMUNE")%>%select(COD_MOD,LIB_MOD),by=c("DCLT"= "COD_MOD"))%>%
      mutate(LIB_MOD=case_when(is.na(LIB_MOD) & substr(DCLT,1,2)==75~paste0("Paris ",substr(DCLT,4,5)),
                               is.na(LIB_MOD) & substr(DCLT,1,2)==69~paste0("Lyon ",substr(DCLT,4,5)),
                               is.na(LIB_MOD) & substr(DCLT,1,2)==13~paste0("Marseille ",substr(DCLT,4,5)),
                               TRUE~LIB_MOD))%>%arrange(desc(Part))
    
    Inter<-st_intersects(Zone20Bornes%>%st_transform(crs=4326),AllComm)
    Interdf<-as.data.frame(Inter)
    Inter_V<-AllComm[Interdf$col.id,]
    Inter_V_Donnees<-Inter_V%>%left_join(OuVaTravaillerLaCommune,by=c("code"="DCLT"))
    Inter_V_Donnees<-Inter_V_Donnees%>%mutate(Part=case_when(is.na(Part)~0,
                                                             TRUE~Part))
    Inter_V_Donnees<-Inter_V_Donnees%>%mutate(PartD=case_when(Part==0 ~"0%",
                                                              Part>0 & Part <= 1~"Entre 0 et 1%",
                                                              Part>1 & Part <= 3~"Entre 1 et 3%",
                                                              Part>3 & Part <= 5~"Entre 3 et 5%",
                                                              Part>5 & Part <= 15~"Entre 5 et 15%",
                                                              Part>15~"Plus de 15%"))
    couleurs<-c("0%"="#FFFFFF","Entre 0 et 1%"="#F4D4D4","Entre 1 et 3%"="#EAA9A9",
                "Entre 3 et 5%"="#E07E7E","Entre 5 et 15%"="#D65353","Plus de 15%"="#CC2828")
    nomcomm<-CarteComm%>%filter(code==ComSelec)%>%st_drop_geometry()%>%left_join(TouslesLibellesDesVilles,by=c("code"="com"))
    
 Phrase<-PhATL%>%mutate(TextePart1=str_replace(str_replace(str_replace(str_replace(
      part1,"VILLE1",paste0(GraphOuVaTravaillerLaCommune$LIB_MOD[1]," ",Virg(GraphOuVaTravaillerLaCommune$Part[1]),"%")),
      "VILLE2",paste0(GraphOuVaTravaillerLaCommune$LIB_MOD[2]," ",Virg(GraphOuVaTravaillerLaCommune$Part[2]),"%")),
      "VILLE3",paste0(GraphOuVaTravaillerLaCommune$LIB_MOD[3]," ",Virg(GraphOuVaTravaillerLaCommune$Part[3]),"%")),
      "DE_LA_VILLE",nomcomm$DeLaVille[1]))
    
    CreationHashTag<-paste0("#",str_remove_all(nomcomm$nom,"[^[:alpha:]]"))
  

#Poste Premier Tweet
    rtweet::post_tweet(status = paste0(Phrase$TextePart1," ",CreationHashTag))
    print("ok premier tweet")
    ###########
    #Et le second
      
UnionsCommunesDep<-Inter_V_Donnees%>%st_transform(crs=2154)%>%
      group_by(substr(code,1,2))%>%
      summarise()%>%ungroup()%>%rename(code=1)
    

Carte<-Inter_V_Donnees%>%st_transform(crs=2154)%>%ggplot()+
  geom_sf(aes(fill=PartD,colour=code==ComSelec))+
  geom_sf(data=Zone20Bornes%>%st_transform(crs=2154),colour="#CC2828",fill=NA,linetype="dashed")+
  geom_sf_text(data=CarteComm%>%filter(code%in%JusteCommunesImportantes$INSEE_COM)%>%
                 filter(code%in%Inter_V_Donnees$code),aes(label=nom),check_overlap = T,size=4,
                family = "Calibri")+theme_void()+
  geom_sf(data=UnionsCommunesDep,fill=NA,colour="#22222280")+
  geom_sf_label(data=UnionsCommunesDep,aes(label=code),colour="#22222280",size=6,alpha=0.8,family="Calibri")+
  scale_fill_manual("",values=couleurs)+
  scale_colour_manual("",values=c("#22222230","#222222"))+
  theme(legend.position="top",
        plot.title =element_text(hjust=0,family = "Garamond",size=14),
        plot.title.position =  "plot",
        plot.caption.position =   "plot",
        plot.subtitle = element_text(hjust=0,family = "Garamond",size=10),
        plot.caption = element_text(hjust=1,family = "Garamond",size=8),
        text=element_text(size=28,family = "Garamond"),
        legend.text = element_text(size=10),
        plot.margin = margin(0,0,0,0))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour=F)+
  labs(title=paste0("Où va travailler la population active ",nomcomm$DeLaVille[1],"?"),
       subtitle="Part des actifs se rendant au travail dans une commune à moins de 20 km",
       caption = "Données Insee, traitement Victor Alexandre @humeursdevictor")

agg_png(paste0("data/CarteActifs",nomcomm$nom[1],".jpg"), width=900, height = 900, res = 144)
Carte
invisible(dev.off())

Sys.sleep(3)
reply_id <- rtweet::get_timeline(user="humeursdevictor")$status_id[1]

rtweet::post_tweet(status=PhATL$part2[1],
               in_reply_to_status_id = reply_id,media = paste0("data/CarteActifs",nomcomm$nom[1],".jpg"))
 print("ok 2eme tweet")
