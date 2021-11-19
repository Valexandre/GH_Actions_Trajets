library(tidyverse)
library(sf)
library(rtweet)
library(sysfonts)
library(showtext)
font_add_google("Roboto Condensed","garamond")
showtext.auto()
tweetbot_token <- rtweet::rtweet_bot(
  api_key = Sys.getenv("T_API_KEY"),
  api_secret = Sys.getenv("T_API_SECRET"),
  access_token =  Sys.getenv("T_ACCESS_TOKEN"),
  access_secret =  Sys.getenv("T_ACCESS_SECRET")
 )
rtweet::auth_as(tweetbot_token)

Virg<-function(x){ as.character( gsub("\\.",",",as.character(x)))}

 Varmod_MOBPRO_2018 <- read_delim("data/Varmod_MOBPRO_2018.csv", 
                                   ";", escape_double = FALSE, col_types = cols(LONG_VAR = col_skip()),trim_ws = TRUE)

 PhAT <- read_csv("data/Phrases a tweeter - Feuille 1.csv")
  JusteCommunesImportantes<-readRDS("data/VillesSelec.Rdata")
 
font_files()
 #remet à our

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

PhATL<-PhAT%>%filter(categorie=="ouva_podium")%>%
  slice_sample(.,n=1)%>%
  select(part1,part2)

OuVaTravaillerLaCommune<-MOBPRO18_S%>%filter(CODGEORES==ComSelec)%>%
    group_by(DCLT)%>%
    summarise(Act=round(sum(IPONDI),0))%>%
    ungroup()%>%
    mutate(Part=round(Act/sum(Act)*100,2))
 
    
GraphOuVaTravaillerLaCommune<-OuVaTravaillerLaCommune%>%
  top_n(.,n=3,wt = Part)%>%
  left_join(Varmod_MOBPRO_2018%>%
              filter(COD_VAR=="COMMUNE")%>%
              select(COD_MOD,LIB_MOD),
            by=c("DCLT"= "COD_MOD"))%>%
    mutate(LIB_MOD=case_when(is.na(LIB_MOD) & substr(DCLT,1,2)==75~paste0("Paris ",substr(DCLT,4,5)),
                               is.na(LIB_MOD) & substr(DCLT,1,2)==69~paste0("Lyon ",substr(DCLT,4,5)),
                               is.na(LIB_MOD) & substr(DCLT,1,2)==13~paste0("Marseille ",substr(DCLT,4,5)),
                               TRUE~LIB_MOD))%>%
  arrange(desc(Part))
    
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

nomcomm<-AllComm%>%
  filter(code==ComSelec)%>%
  st_drop_geometry()%>%
  left_join(TouslesLibellesDesVilles,by=c("code"="com"))
    
Phrase<-PhATL%>%mutate(TextePart1=str_replace(str_replace(str_replace(str_replace(
  part1,"VILLE1",paste0(GraphOuVaTravaillerLaCommune$LIB_MOD[1]," ",Virg(GraphOuVaTravaillerLaCommune$Part[1]),"%")),
  "VILLE2",paste0(GraphOuVaTravaillerLaCommune$LIB_MOD[2]," ",Virg(GraphOuVaTravaillerLaCommune$Part[2]),"%")),
  "VILLE3",paste0(GraphOuVaTravaillerLaCommune$LIB_MOD[3]," ",Virg(GraphOuVaTravaillerLaCommune$Part[3]),"%")),
  "DE_LA_VILLE",nomcomm$DeLaVille[1]))
    
CreationHashTag<-paste0("#",str_remove_all(nomcomm$nom,"[^[:alpha:]]"))
  
#Poste Premier Tweet
#rtweet::post_tweet(status = paste0(Phrase$TextePart1," ",CreationHashTag," #DataTaff"))
#print("ok premier tweet")


UnionsCommunesDep<-Inter_V_Donnees%>%st_transform(crs=2154)%>%
      group_by(substr(code,1,2))%>%
      summarise()%>%ungroup()%>%rename(code=1)
    

Carte<-Inter_V_Donnees%>%st_transform(crs=2154)%>%ggplot()+
  geom_sf(aes(fill=PartD,colour=code==ComSelec))+
  geom_sf(data=Zone20Bornes%>%st_transform(crs=2154),colour="#CC2828",fill=NA,linetype="dashed")+
  geom_sf_text(data=CarteComm%>%filter(code%in%JusteCommunesImportantes$INSEE_COM)%>%
                 filter(code%in%Inter_V_Donnees$code)%>%
                 group_by(substr(code,1,2))%>%
                 slice_sample(.,n=1),aes(label=nom),check_overlap = T,size=3*3,
                family = "Calibri")+theme_void()+
  geom_sf(data=UnionsCommunesDep,fill=NA,colour="#22222280")+
  geom_sf_text(data=UnionsCommunesDep%>%filter(code!=75),aes(label=code),colour="#22222260",size=6*3,alpha=0.8,family="Calibri")+
  scale_fill_manual("",values=couleurs)+
  scale_colour_manual("",values=c("#22222230","#222222"))+
  theme(legend.position="top",
        plot.title =element_text(hjust=0,family = "garamond",size=30),
        plot.title.position =  "plot",
        plot.caption.position =   "plot",
        plot.subtitle = element_text(hjust=0,family = "garamond",size=22),
        plot.caption = element_text(hjust=1,family = "garamond",size=16),
        text=element_text(size=28,family = "garamond"),
        legend.text = element_text(size=20),
        plot.margin = margin(0,0,0,0))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour=F)+
 labs(title=enc2native(paste0("Où va travailler la population active ",nomcomm$DeLaVille[1],"?")),
       subtitle=enc2native("Part des actifs se rendant au travail dans une commune à moins de 20 km"),
       caption = enc2native("Données Insee, traitement Victor Alexandre @humeursdevictor"))


png(filename = paste0("data/CarteActifs",nomcomm$nom[1],".jpg"),width = 5,height=5,units="in",device = "png", type = "cairo", family = "garamond")
Carte
dev.off()

DOuVientTravailler<-MOBPRO18_S%>%filter(DCLT==ComSelec)%>%
  group_by(CODGEORES)%>%
  summarise(Act=round(sum(IPONDI),0))%>%
  ungroup()%>%
  mutate(Part=round(Act/sum(Act)*100,2))



GraphDOuVientTravailler<-DOuVientTravailler%>%
  top_n(.,n=3,wt = Part)%>%
  left_join(Varmod_MOBPRO_2018%>%
              filter(COD_VAR=="COMMUNE")%>%
              select(COD_MOD,LIB_MOD),by=c("CODGEORES"= "COD_MOD"))%>%
  mutate(LIB_MOD=case_when(is.na(LIB_MOD) & substr(CODGEORES,1,2)==75~paste0("Paris ",substr(CODGEORES,4,5)),
                           is.na(LIB_MOD) & substr(CODGEORES,1,2)==69~paste0("Lyon ",substr(CODGEORES,4,5)),
                           is.na(LIB_MOD) & substr(CODGEORES,1,2)==13~paste0("Marseille ",substr(CODGEORES,4,5)),
                           TRUE~LIB_MOD))%>%arrange(desc(Part))

PhATL2<-PhAT%>%filter(categorie=="douvient_podium")%>%
  slice_sample(.,n=1)%>%
  select(part1,part2)


Phrase2<-PhATL2%>%mutate(TextePart1=str_replace(str_replace(str_replace(str_replace(str_replace(
  part1,"VILLE1",paste0(GraphDOuVientTravailler$LIB_MOD[1]," ",Virg(GraphDOuVientTravailler$Part[1]),"%")),
  "VILLE2",paste0(GraphDOuVientTravailler$LIB_MOD[2]," ",Virg(GraphDOuVientTravailler$Part[2]),"%")),
  "VILLE3",paste0(GraphDOuVientTravailler$LIB_MOD[3]," ",Virg(GraphDOuVientTravailler$Part[3]),"%")),
  "A_LA_VILLE",nomcomm$ALaVille[1]),"DE_LA_VILLE",nomcomm$DeLaVille[1]))



reply_id <- rtweet::get_timeline("humeursdevictor",n=1)$id_str

rtweet::post_tweet(status=Phrase2$TextePart1[1],in_reply_to_status_id = reply_id)

Inter_V_DonneesDOuVient<-Inter_V%>%left_join(DOuVientTravailler,by=c("code"="CODGEORES"))
Inter_V_DonneesDOuVient<-Inter_V_DonneesDOuVient%>%mutate(Part=case_when(is.na(Part)~0,
                                                                         TRUE~Part))
Inter_V_DonneesDOuVient<-Inter_V_DonneesDOuVient%>%mutate(PartD=case_when(Part==0 ~"0%",
                                                                          Part>0 & Part <= 1~"Entre 0 et 1%",
                                                                          Part>1 & Part <= 3~"Entre 1 et 3%",
                                                                          Part>3 & Part <= 5~"Entre 3 et 5%",
                                                                          Part>5 & Part <= 15~"Entre 5 et 15%",
                                                                          Part>15~"Plus de 15%"))
UnionsCommunesDep<-Inter_V_DonneesDOuVient%>%st_transform(crs=2154)%>%
  group_by(substr(code,1,2))%>%
  summarise()%>%ungroup()%>%rename(code=1)

couleursdouvient<-c("0%"="#FFFFFF","Entre 0 et 1%"="#CFDDE5","Entre 1 et 3%"="#A0BBCC",
                    "Entre 3 et 5%"="#7199B2","Entre 5 et 15%"="#427799","Plus de 15%"="#135680")

Carte2<-Inter_V_DonneesDOuVient%>%st_transform(crs=2154)%>%ggplot()+
  geom_sf(aes(fill=PartD,colour=code==ComSelec))+
  geom_sf(data=Zone20Bornes%>%st_transform(crs=2154),colour="#135680",fill=NA,linetype="dashed")+
  geom_sf_text(data=CarteComm%>%filter(code%in%JusteCommunesImportantes$INSEE_COM)%>%
                 filter(code%in%Inter_V_DonneesDOuVient$code)%>%
                 group_by(substr(code,1,2))%>%
                 slice_sample(.,n=1),
               aes(label=nom),check_overlap = T,size=3*3,
               family = "Calibri")+theme_void()+
  geom_sf(data=UnionsCommunesDep,fill=NA,colour="#22222280")+
  geom_sf_text(data=UnionsCommunesDep%>%filter(code!=75),aes(label=code),colour="#22222260",size=6*3,alpha=0.8,family="Calibri")+
  scale_fill_manual("",values=couleursdouvient)+
  scale_colour_manual("",values=c("#22222230","#222222"))+
  theme(legend.position="top",
        plot.title =element_text(hjust=0,family = "garamond",size=30),
        plot.title.position =  "plot",
        plot.caption.position =   "plot",
        plot.subtitle = element_text(hjust=0,family = "garamond",size=22),
        plot.caption = element_text(hjust=1,family = "garamond",size=16),
        text=element_text(size=28,family = "garamond"),
        legend.text = element_text(size=20),
        plot.margin = margin(0,0,0,0))+
  guides(fill=guide_legend(nrow=2,byrow=TRUE),colour=F)+
labs(title=enc2native(paste0("D'où viennent les actifs qui travaillent ",nomcomm$ALaVille[1],"?")),
       subtitle=enc2native("Part des actifs se rendant au travail dans la commune depuis moins de 20 km"),
       caption = enc2native("Données Insee, traitement Victor Alexandre @humeursdevictor"))
 

ggsave(filename = paste0("data/CarteActifsProvenance",nomcomm$nom[1],".jpg"),width = 5,height=5,units="in", device="jpeg")
Carte2
dev.off()

reply_id2 <- rtweet::get_timeline("humeursdevictor",n=1)$id_str


rtweet::post_tweet(status=PhATL$part2[1],
               in_reply_to_status_id = reply_id2,
                   media = c(paste0("data/CarteActifs",nomcomm$nom[1],".jpg"),paste0("data/CarteActifsProvenance",nomcomm$nom[1],".jpg"))
                  )
 print("ok 2eme tweet")

 
#On regarde le csv des données si quelque chose a déjà été twitté à propos de cette commune pour le rajouter en suite.
dejaparus <- read_csv("data/dejaparus.csv", col_types = cols(codeinsee = col_character(), datetweets = col_character()))
dejaparus_communes<-dejaparus%>%filter(codeinsee==ComSelec)%>%summarise(TweetsPublies=paste0(lientweet,collapse = ", "))


reply_id3 <- rtweet::get_timeline("humeursdevictor",n=1)$id_str

if(nchar(dejaparus_communes$TweetsPublies)>4){
  rtweet::post_tweet(status=paste0("Pour rappel, nous avions déjà parlé ",nomcomm$DeLaVille, " ici : ",dejaparus_communes$TweetsPublies),in_reply_to_status_id = reply_id3)
}


dejaparus<-dejaparus%>%
  add_row(codeinsee=as.character(ComSelec),
          commune=nomcomm$nom[1],
          datetweets=as.character(Sys.time()),
          lientweet=paste0("https://twitter.com/humeursdevictor/status/",reply_id),
          categorietweet="Origine et provenance des actifs")

write_csv(dejaparus,paste0('data/dejaparus.csv'))    
