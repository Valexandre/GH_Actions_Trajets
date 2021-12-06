library(ragg)
library(tidyverse)
library(sf)
library(rtweet)
library(sysfonts)
library(usethis)
# On met un commentaire avec des accents é a à
tweetbot_token <- rtweet::rtweet_bot(
  api_key = Sys.getenv("T_API_KEY"),
  api_secret = Sys.getenv("T_API_SECRET"),
  access_token = Sys.getenv("T_ACCESS_TOKEN"),
  access_secret = Sys.getenv("T_ACCESS_SECRET")
)
rtweet::auth_as(tweetbot_token)

`%!in%` <- function(x, y) !(x %in% y)
Virg <- function(x) {  as.character(gsub("\\.", ",", as.character(x)))}


#Bases communes
Varmod_MOBPRO_2018 <- read_delim("data/Varmod_MOBPRO_2018.csv",";",escape_double = FALSE, col_types = cols(LONG_VAR = col_skip()), trim_ws = TRUE)
MOBPRO18_S <- readRDS("data/MOBPRO18_S_IDF.Rdata")
TouslesLibellesDesVilles <- read_csv("data/TouslesLibellesDesVilles.txt")
PhAT <- read_csv("data/Phrases a tweeter - Feuille 1.csv")
JusteCommunesImportantes <- readRDS("data/VillesSelec.Rdata")

#Quels sont les lieux déjà parus
dejaparus <- read_csv("data/dejaparus.csv", col_types = cols(codeinsee = col_character(), datetweets = col_character()))

#Geometries
CarteArr <- st_read("https://raw.githubusercontent.com/Valexandre/france-geojson/master/arrondissements-millesimes0.geojson") %>%
  dplyr::select(code_insee, nom_com, geometry) %>%
  rename(code = 1, nom = 2)
CarteComm <- st_read("https://github.com/gregoiredavid/france-geojson/raw/master/communes-version-simplifiee.geojson") %>% 
  dplyr::select(code, nom, geometry)

AllComm <- rbind(CarteComm, CarteArr)

couleurs <- c(
  "0%" = "#FFFFFF", "Entre 0 et 1%" = "#F4D4D4", "Entre 1 et 3%" = "#EAA9A9",
  "Entre 3 et 5%" = "#E07E7E", "Entre 5 et 15%" = "#D65353", "Plus de 15%" = "#CC2828")

couleursdouvient <- c(
  "0%" = "#FFFFFF", "Entre 0 et 1%" = "#CFDDE5", "Entre 1 et 3%" = "#A0BBCC",
  "Entre 3 et 5%" = "#7199B2", "Entre 5 et 15%" = "#427799", "Plus de 15%" = "#135680")

aujourdui <- Sys.Date()
jour <- lubridate::wday(aujourdui)

TrajetsOretPr<-function(x){
  ComSelec<-x

  Zone20Bornes <- AllComm %>%
    filter(code == ComSelec) %>%
    st_transform(crs = 2154) %>%
    st_centroid() %>%
    st_buffer(20000)
  
  
  nomcomm <- AllComm %>%
    filter(code == ComSelec) %>%
    st_drop_geometry() %>%
    left_join(TouslesLibellesDesVilles, by = c("code" = "com"))
  
  
  PhATL <- PhAT %>%
    filter(categorie == "ouva_podium") %>%
    slice_sample(., n = 1) %>%
    select(part1, part2)
  
  OuVaTravaillerLaCommune <- MOBPRO18_S %>%
    filter(CODGEORES == ComSelec) %>%
    group_by(DCLT) %>%
    summarise(Act = round(sum(IPONDI), 0)) %>%
    ungroup() %>%
    mutate(Part = round(Act / sum(Act) * 100, 2))
  
  
  GraphOuVaTravaillerLaCommune <- OuVaTravaillerLaCommune %>%
    top_n(., n = 3, wt = Part) %>%
    left_join(Varmod_MOBPRO_2018 %>%
                filter(COD_VAR == "COMMUNE") %>%
                select(COD_MOD, LIB_MOD),
              by = c("DCLT" = "COD_MOD")
    ) %>%
    mutate(LIB_MOD = case_when(
      is.na(LIB_MOD) & substr(DCLT, 1, 2) == 75 ~ paste0("Paris ", substr(DCLT, 4, 5)),
      is.na(LIB_MOD) & substr(DCLT, 1, 2) == 69 ~ paste0("Lyon ", substr(DCLT, 4, 5)),
      is.na(LIB_MOD) & substr(DCLT, 1, 2) == 13 ~ paste0("Marseille ", substr(DCLT, 4, 5)),
      TRUE ~ LIB_MOD
    )) %>%
    arrange(desc(Part))
  
  Inter <- st_intersects(Zone20Bornes %>% st_transform(crs = 4326), AllComm)
  Interdf <- as.data.frame(Inter)
  Inter_V <- AllComm[Interdf$col.id, ]
  Inter_V_Donnees <- Inter_V %>% left_join(OuVaTravaillerLaCommune, by = c("code" = "DCLT"))
  Inter_V_Donnees <- Inter_V_Donnees %>% mutate(Part = case_when(
    is.na(Part) ~ 0,
    TRUE ~ Part
  ))
  
  Inter_V_Donnees <- Inter_V_Donnees %>% mutate(PartD = case_when(
    Part == 0 ~ "0%",
    Part > 0 & Part <= 1 ~ "Entre 0 et 1%",
    Part > 1 & Part <= 3 ~ "Entre 1 et 3%",
    Part > 3 & Part <= 5 ~ "Entre 3 et 5%",
    Part > 5 & Part <= 15 ~ "Entre 5 et 15%",
    Part > 15 ~ "Plus de 15%"
  ))
  
  
  Phrase <- PhATL %>% mutate(TextePart1 = str_replace(
    str_replace(
      str_replace(
        str_replace(
          part1, "VILLE1", paste0(GraphOuVaTravaillerLaCommune$LIB_MOD[1], " ", Virg(GraphOuVaTravaillerLaCommune$Part[1]), "%")
        ),
        "VILLE2", paste0(GraphOuVaTravaillerLaCommune$LIB_MOD[2], " ", Virg(GraphOuVaTravaillerLaCommune$Part[2]), "%")
      ),
      "VILLE3", paste0(GraphOuVaTravaillerLaCommune$LIB_MOD[3], " ", Virg(GraphOuVaTravaillerLaCommune$Part[3]), "%")
    ),
    "DE_LA_VILLE", nomcomm$DeLaVille[1]
  ))
  
  CreationHashTag <- paste0("#", str_remove_all(nomcomm$nom, "[^[:alpha:]]"))
  
  # Poste Premier Tweet
  rtweet::post_tweet(status = paste0(Phrase$TextePart1, " ", CreationHashTag, " #DataTaff"))
  usethis::ui_done("ok premier tweet")
  
  
  UnionsCommunesDep <- Inter_V_Donnees %>%
    st_transform(crs = 2154) %>%
    group_by(substr(code, 1, 2)) %>%
    summarise() %>%
    ungroup() %>%
    rename(code = 1)
  
  
  Carte <- Inter_V_Donnees %>%
    st_transform(crs = 2154) %>%
    ggplot() +
    geom_sf(aes(fill = PartD, colour = code == ComSelec)) +
    geom_sf(data = Zone20Bornes %>% st_transform(crs = 2154), colour = "#CC2828", fill = NA, linetype = "dashed") +
    geom_sf_text(
      data = CarteComm %>% filter(code %in% JusteCommunesImportantes$INSEE_COM) %>%
        filter(code %in% Inter_V_Donnees$code) %>%
        group_by(substr(code, 1, 2)) %>%
        slice_sample(., n = 1), aes(label = nom), check_overlap = T, size = 3,
      family = "Bahnschrift"
    ) +
    theme_void() +
    geom_sf(data = UnionsCommunesDep, fill = NA, colour = "#22222280") +
    geom_sf_text(data = UnionsCommunesDep %>% filter(code != 75), aes(label = code), colour = "#22222260", size = 6, alpha = 0.8, family = "Bahnschrift") +
    scale_fill_manual("", values = couleurs) +
    scale_colour_manual("", values = c("#22222230", "#222222")) +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0, family = "Corbel", size = 14),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.subtitle = element_text(hjust = 0, family = "Corbel", size = 10),
      plot.caption = element_text(hjust = 1, family = "Corbel", size = 8),
      text = element_text(size = 14, family = "Corbel"),
      legend.text = element_text(size = 10),
      plot.margin = margin(0, 0, 0, 0)
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE), colour = F) +
    labs(
      title = paste0("Où va travailler la population active ", nomcomm$DeLaVille[1], "?"),
      subtitle = "Part des actifs se rendant au travail dans une commune à moins de 20 km",
      caption = "Données Insee, traitement Victor Alexandre @humeursdevictor"
    )
  
  agg_png(paste0("data/CarteActifs", nomcomm$nom[1], ".jpg"), width = 900, height = 900, res = 144)
  plot(Carte)
  invisible(dev.off())
  
  
  
  DOuVientTravailler <- MOBPRO18_S %>%
    filter(DCLT == ComSelec) %>%
    group_by(CODGEORES) %>%
    summarise(Act = round(sum(IPONDI), 0)) %>%
    ungroup() %>%
    mutate(Part = round(Act / sum(Act) * 100, 2))
  
  
  
  GraphDOuVientTravailler <- DOuVientTravailler %>%
    top_n(., n = 3, wt = Part) %>%
    left_join(Varmod_MOBPRO_2018 %>%
                filter(COD_VAR == "COMMUNE") %>%
                select(COD_MOD, LIB_MOD), by = c("CODGEORES" = "COD_MOD")) %>%
    mutate(LIB_MOD = case_when(
      is.na(LIB_MOD) & substr(CODGEORES, 1, 2) == 75 ~ paste0("Paris ", substr(CODGEORES, 4, 5)),
      is.na(LIB_MOD) & substr(CODGEORES, 1, 2) == 69 ~ paste0("Lyon ", substr(CODGEORES, 4, 5)),
      is.na(LIB_MOD) & substr(CODGEORES, 1, 2) == 13 ~ paste0("Marseille ", substr(CODGEORES, 4, 5)),
      TRUE ~ LIB_MOD
    )) %>%
    arrange(desc(Part))
  
  PhATL2 <- PhAT %>%
    filter(categorie == "douvient_podium") %>%
    slice_sample(., n = 1) %>%
    select(part1, part2)
  
  
  Phrase2 <- PhATL2 %>% mutate(TextePart1 = str_replace(str_replace(
    str_replace(
      str_replace(
        str_replace(
          part1, "VILLE1", paste0(GraphDOuVientTravailler$LIB_MOD[1], " ", Virg(GraphDOuVientTravailler$Part[1]), "%")
        ),
        "VILLE2", paste0(GraphDOuVientTravailler$LIB_MOD[2], " ", Virg(GraphDOuVientTravailler$Part[2]), "%")
      ),
      "VILLE3", paste0(GraphDOuVientTravailler$LIB_MOD[3], " ", Virg(GraphDOuVientTravailler$Part[3]), "%")
    ),
    "A_LA_VILLE", nomcomm$ALaVille[1]
  ), "DE_LA_VILLE", nomcomm$DeLaVille[1]))
  
  #deuxième tweet
  reply_id <- rtweet::get_timeline(user = "humeursdevictor", n = 1, token = tweetbot_token)$id_str
  rtweet::post_tweet(status = Phrase2$TextePart1[1], in_reply_to_status_id = reply_id)
  usethis::ui_done("OK 2e tweet")
  
  topdepart<-Sys.time()
  
  Inter_V_DonneesDOuVient <- Inter_V %>% left_join(DOuVientTravailler, by = c("code" = "CODGEORES"))
  Inter_V_DonneesDOuVient <- Inter_V_DonneesDOuVient %>% mutate(Part = case_when(
    is.na(Part) ~ 0,
    TRUE ~ Part
  ))
  Inter_V_DonneesDOuVient <- Inter_V_DonneesDOuVient %>% mutate(PartD = case_when(
    Part == 0 ~ "0%",
    Part > 0 & Part <= 1 ~ "Entre 0 et 1%",
    Part > 1 & Part <= 3 ~ "Entre 1 et 3%",
    Part > 3 & Part <= 5 ~ "Entre 3 et 5%",
    Part > 5 & Part <= 15 ~ "Entre 5 et 15%",
    Part > 15 ~ "Plus de 15%"
  ))
 
  
   
   UnionsCommunesDep <- Inter_V_DonneesDOuVient %>%
     st_transform(crs = 2154) %>%
     group_by(substr(code, 1, 2)) %>%
     summarise() %>%
     ungroup() %>%
     rename(code = 1)
   
   
   
   Carte2 <- Inter_V_DonneesDOuVient %>%
     st_transform(crs = 2154) %>%
     ggplot() +
     geom_sf(aes(fill = PartD, colour = code == ComSelec)) +
     geom_sf(data = Zone20Bornes %>% st_transform(crs = 2154), colour = "#135680", fill = NA, linetype = "dashed") +
     geom_sf_text(
       data = CarteComm %>% filter(code %in% JusteCommunesImportantes$INSEE_COM) %>%
         filter(code %in% Inter_V_DonneesDOuVient$code) %>%
         group_by(substr(code, 1, 2)) %>%
         slice_sample(., n = 1),
       aes(label = nom), check_overlap = T, size = 3,
       family = "Bahnschrift"
     ) +
     theme_void() +
     geom_sf(data = UnionsCommunesDep, fill = NA, colour = "#22222280") +
     geom_sf_text(data = UnionsCommunesDep %>% filter(code != 75), aes(label = code), colour = "#22222260", size = 6, alpha = 0.8, family = "Bahnschrift") +
     scale_fill_manual("", values = couleursdouvient) +
     scale_colour_manual("", values = c("#22222230", "#222222")) +
     theme(
       legend.position = "top",
       plot.title = element_text(hjust = 0, family = "Corbel", size = 14),
       plot.title.position = "plot",
       plot.caption.position = "plot",
       plot.subtitle = element_text(hjust = 0, family = "Corbel", size = 10),
       plot.caption = element_text(hjust = 1, family = "Corbel", size = 8),
       text = element_text(size = 14, family = "Corbel"),
       legend.text = element_text(size = 10),
       plot.margin = margin(0, 0, 0, 0)
     ) +
     guides(fill = guide_legend(nrow = 2, byrow = TRUE), colour = F) +
     labs(
       title = paste0("D'où viennent les actifs qui travaillent ", nomcomm$ALaVille[1], "?"),
       subtitle = "Part des actifs se rendant au travail dans la commune depuis moins de 20 km",
       caption = "Données Insee, traitement Victor Alexandre @humeursdevictor"
     )
   
   
   agg_png(paste0("data/CarteActifsProvenance", nomcomm$nom[1], ".jpg"), width = 900, height = 900, res = 144)
   plot(Carte2)
   invisible(dev.off())
   
   fin<-Sys.time()
   
   
   reply_id2 <- rtweet::get_timeline("humeursdevictor",n=1,token = tweetbot_token)$id_str
   rtweet::post_tweet(status=PhATL$part2[1],
                      in_reply_to_status_id = reply_id2,
                      media = c(paste0("data/CarteActifs",nomcomm$nom[1],".jpg"),
                                paste0("data/CarteActifsProvenance",nomcomm$nom[1],".jpg"))
                      )
   usethis::ui_done(paste0("ok tweet cartes, temps passé entre 2 derniers tweets : ",fin-topdepart))
   
   
   # On regarde le csv des données si quelque chose a déjà été twitté à propos de cette commune pour le rajouter en suite.
   dejaparus_communes <- dejaparus %>%
     filter(codeinsee == ComSelec) %>%
     summarise(TweetsPublies = paste0(lientweet, collapse = ", "))
   
   
   reply_id3 <- rtweet::get_timeline("humeursdevictor", n = 1, token = tweetbot_token)$id_str
   
   if (nchar(dejaparus_communes$TweetsPublies) > 4) {
     rtweet::post_tweet(
       status = paste0("Pour rappel, nous avions déjà parlé ", nomcomm$DeLaVille[1], " ici : ", dejaparus_communes$TweetsPublies), in_reply_to_status_id = reply_id3)
   }
   
   
   dejaparus <- dejaparus %>%
     add_row(
       codeinsee = as.character(ComSelec),
       commune = nomcomm$nom[1],
       datetweets = as.character(Sys.time()),
       lientweet = paste0("https://twitter.com/humeursdevictor/status/", reply_id),
       categorietweet = "Origine et provenance des actifs"
     )
   
   write_csv(dejaparus, paste0("data/dejaparus.csv"))
  
   usethis::ui_done("ok fin de fonction")
}


TrajetsEvitables<-function(x){
  ComSelec<-x
  
  # créer le code pour les textes dans les phrases.
  Phrase1 <- PhAT %>%
    filter(categorie == "trajet_evit_1") %>%
    slice_sample(., n = 1) %>%
    select(part1)
  
  GlobalTrav <- MOBPRO18_S %>%
    filter(DCLT == ComSelec) %>%
    group_by(TRANS) %>%
    summarise(Act = round(sum(IPONDI), 0)) %>%
    ungroup() %>%
    mutate(Part = round(Act / sum(Act) * 100, 2))
  
  NBSANSTRAJ <- GlobalTrav$Act[GlobalTrav$TRANS == 1] + GlobalTrav$Act[GlobalTrav$TRANS == 2]
  PARTSANSTRAJ <- GlobalTrav$Part[GlobalTrav$TRANS == 1] + GlobalTrav$Part[GlobalTrav$TRANS == 2]
  NBACTIFSTRAV <- sum(GlobalTrav$Act)
  
  nomcomm <- AllComm %>%
    filter(code == ComSelec) %>%
    st_drop_geometry() %>%
    left_join(TouslesLibellesDesVilles, by = c("code" = "com"))
  
  Phrase1$com <- ComSelec
  
  Phrase1$PremierTweet <- str_replace_all(
    str_replace_all(
      str_replace_all(Phrase1$part1, "NBSANSTRAJ", as.character(NBSANSTRAJ)),
    "PARTSANSTRAJ", Virg(PARTSANSTRAJ)),
    "NBACTIFSTRAV", as.character(NBACTIFSTRAV))
  
  Phrase1 <- Phrase1 %>%
    left_join(TouslesLibellesDesVilles, by = c("com" = "com")) %>%
    mutate(PremierTweet = str_replace_all(str_replace_all(PremierTweet, pattern = "A_LA_VILLE", ALaVille),
                                          "DE_LA_VILLE", nomcomm$DeLaVille[1]))
  PremierTweet <- Phrase1$PremierTweet[1]
  
  
  CreationHashTag <- paste0("#", str_remove_all(nomcomm$nom, "[^[:alpha:]]"))
  
  # Poste Premier Tweet
  rtweet::post_tweet(status = paste0(PremierTweet, " ", CreationHashTag, " #DataTaff"))
  usethis::ui_done(paste0("OK 1er tweet trajets évitables",Sys.time()))
  
  
  #second tweet
  Phrase2 <- PhAT %>%
    filter(categorie == "trajet_evit_2") %>%
    slice_sample(., n = 1) %>%
    select(part1)
  
  PARTACTIFSVOIT <- Virg(GlobalTrav$Part[GlobalTrav$TRANS == 5])
  PARTACTIFSTEC <- Virg(GlobalTrav$Part[GlobalTrav$TRANS == 6])
  PARTACTIFSVELO <- Virg(GlobalTrav$Part[GlobalTrav$TRANS == 3])
  
  Phrase2$com <- ComSelec
  Phrase2$SecondTweet <- str_replace_all(
    str_replace_all(
      str_replace_all(Phrase2$part1, "PARTACTIFSVOIT", as.character(PARTACTIFSVOIT)),
      "PARTACTIFSTEC", Virg(PARTACTIFSTEC)),
    "PARTACTIFSVELO", as.character(PARTACTIFSVELO))
  
  Phrase2 <- Phrase2 %>% left_join(TouslesLibellesDesVilles, by = c("com" = "com"))
  SecondTweet <- Phrase2$SecondTweet[1]
  
  reply_id <- rtweet::get_timeline("humeursdevictor", n = 1, token = tweetbot_token)$id_str
  rtweet::post_tweet(status = SecondTweet,
    in_reply_to_status_id = reply_id)
  
  usethis::ui_done(paste0("OK 2e Tweet",Sys.time()))
  
  
  
  ################
  # number3
  Phrase3 <- PhAT %>%
    filter(categorie == "trajet_evit_3") %>%
    slice_sample(., n = 1) %>%
    select(part1)
  
  NBTRAJETSVOITURE <- GlobalTrav$Act[GlobalTrav$TRANS == 5]
  
  DetailsParTrajetTrav <- MOBPRO18_S %>%
    filter(DCLT == ComSelec) %>%
    group_by(CODGEORES, TRANS) %>%
    summarise(Act = round(sum(IPONDI), 0)) %>%
    ungroup() %>%
    group_by(CODGEORES) %>%
    mutate(Part = round(Act / sum(Act) * 100, 2))
  
  TrajetsFaisablesEnTEC <- DetailsParTrajetTrav$CODGEORES[DetailsParTrajetTrav$TRANS == 6 & DetailsParTrajetTrav$Act > 5]
  TrajetsFaisablesEnVelo <- DetailsParTrajetTrav$CODGEORES[DetailsParTrajetTrav$TRANS == 3 & DetailsParTrajetTrav$Act > 5]
  TrajetsFaisablesAPied <- DetailsParTrajetTrav$CODGEORES[DetailsParTrajetTrav$TRANS == 2 & DetailsParTrajetTrav$Act > 5]
  
  TotalFaisableAutrement <- sum(DetailsParTrajetTrav$Act[DetailsParTrajetTrav$CODGEORES %!in% c(
    TrajetsFaisablesAPied,
    TrajetsFaisablesEnTEC,
    TrajetsFaisablesEnVelo
  )])
  
  PARTVOITUREPOSSIBLEAUTRE <- Virg(round(TotalFaisableAutrement / NBTRAJETSVOITURE * 100, 1))
  
  Phrase3$com <- ComSelec
  Phrase3$TroisiemeTweet <- str_replace_all(
    str_replace_all(Phrase3$part1, "PARTVOITUREPOSSIBLEAUTRE", as.character(PARTVOITUREPOSSIBLEAUTRE)),
    "NBTRAJETSVOITURE", Virg(NBTRAJETSVOITURE)
  )
  Phrase3 <- Phrase3 %>% left_join(TouslesLibellesDesVilles, by = c("com" = "com"))
  TroisiemeTweet <- Phrase3$TroisiemeTweet[1]
  
  reply_id2 <- rtweet::get_timeline("humeursdevictor", n = 1, token = tweetbot_token)$id_str
  rtweet::post_tweet(
    status = TroisiemeTweet,
    in_reply_to_status_id = reply_id2
  )
  usethis::ui_done(paste0("ok 3e tweet",Sys.time()))
  
  
  # On regarde le csv des données si quelque chose a déjà été twitté à propos de cette commune pour le rajouter en suite.
  dejaparus_communes <- dejaparus %>%
    filter(codeinsee == ComSelec) %>%
    summarise(TweetsPublies = paste0(lientweet, collapse = ", "))
  
  
  reply_id3 <- rtweet::get_timeline("humeursdevictor", n = 1, token = tweetbot_token)$id_str
  
  if (nchar(dejaparus_communes$TweetsPublies) > 4) {
    rtweet::post_tweet(status = paste0("Pour rappel, nous avions déjà parlé ", nomcomm$DeLaVille, " ici : ", dejaparus_communes$TweetsPublies), in_reply_to_status_id = reply_id3)
  }
  
  
  dejaparus <- dejaparus %>%
    add_row(
      codeinsee = as.character(ComSelec),
      commune = nomcomm$nom[1],
      datetweets = as.character(Sys.time()),
      lientweet = paste0("https://twitter.com/humeursdevictor/status/", reply_id),
      categorietweet = "Trajets auto évitables"
    )
  
  write_csv(dejaparus, paste0("data/dejaparus.csv"))
  finitrajetevit<-Sys.time()
  usethis::ui_done(paste0("fin de publi tweet  trajets évitable"))
}

jour<-1
if(jour %in%c(1,3,5,7)){
  CommunesPossibles<-sample(JusteCommunesImportantes$INSEE_COM[substr(JusteCommunesImportantes$INSEE_COM, 1, 2) %in% c(75, 77, 78, 91:95) & JusteCommunesImportantes$INSEE_COM %!in% c(dejaparus$codeinsee[dejaparus$categorietweet == "Origine et provenance des actifs
"],"75056")], 1)
  
  TrajetsOretPr(CommunesPossibles)
} else if (jour %in% c(2, 4, 6)) {
  CommunesPossibles <- sample(JusteCommunesImportantes$INSEE_COM[substr(JusteCommunesImportantes$INSEE_COM, 1, 2) %in% c(75, 77, 78, 91:95) & JusteCommunesImportantes$INSEE_COM %!in% c(dejaparus$codeinsee[dejaparus$categorietweet == "Trajets auto évitables"],"75056")], 1)
  TrajetsEvitables(CommunesPossibles)
  
}
