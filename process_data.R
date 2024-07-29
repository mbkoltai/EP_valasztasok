# URL
# debrecen
# https://vtr.valasztas.hu/ep2024/
# valasztopolgaroknak/varmegyek-telepulesek/varmegyek/09/telepulesek/018?tab=results
# BP
# https://vtr.valasztas.hu/ep2024/
# valasztopolgaroknak/varmegyek-telepulesek/varmegyek/01/telepulesek/007?tab=results
# JSON API
# https://vtr.valasztas.hu/ep2024/data/06152100/szavossz/09/TelepEredm-09-018.json

library(tidyverse)

  # 2024
  ep2024_szavazokori_eredmenyek <- lapply(
    list.files("2024_EP/szavazokori_eredmenyek/",pattern="szavaz"), 
      function(x) read_csv(file = paste0("2024_EP/szavazokori_eredmenyek/",x)) %>% 
    mutate(megye=strsplit(x,split = "_|.csv")[[1]][2]) ) %>%
    bind_rows()
  # SAVE
  write_csv(ep2024_szavazokori_eredmenyek,file = "2024_EP/ep2024_szavazokori_eredmenyek.csv")
  
  ep2024_telepules_eredmenyek <- ep2024_szavazokori_eredmenyek %>%
    group_by(megye,Település) %>%
    summarise(n_valpolg_nevjegyz=sum(AEP),
              n_valpolg_megjel=sum(FEP),
              n_erv_szav=sum(N),
              n_ervtelen_szav=sum(M),
              MEMO = sum(`1`),
              LMP = sum(`2`),
            `DK-MSZP-Párbeszéd` = sum(`3`),
            `2RK Párt` = sum(`4`),
            MMN= sum(`5`),
            Momentum= sum(`6`),
            `FIDESZ-KDNP`= sum(`7`),
            Jobbik= sum(`8`),
            TISZA= sum(`9`),
            MKKP= sum(`10`),
            `Mi Hazánk`=sum(`11`) ) %>%
    arrange(megye,Település)
  
  ep2024_telepules_eredmenyek <- ep2024_telepules_eredmenyek %>% 
    rename(MEGYE=megye,TELEPÜLÉS=Település) %>%
    pivot_longer(cols = !c(MEGYE,TELEPÜLÉS,n_valpolg_nevjegyz,
                         n_valpolg_megjel,n_ervtelen_szav,n_erv_szav),
               names_to="LISTA",values_to="SZAVAZAT")
  
  # SAVE
  write_csv(ep2024_telepules_eredmenyek,file = "2024_EP/ep2024_telepules_eredmenyek.csv")
  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 2019 EP
ep2019_telepules_eredmenyek <- lapply(
    list.files("2019_EP/telep_eredm_megyek/",pattern="telep"), 
    function(x) read_csv(file = paste0("2019_EP/telep_eredm_megyek/",x),skip = 4) %>% 
      mutate(megye=strsplit(x,split = "19_|.csv")[[1]][2]) ) %>%
    bind_rows() %>%
    rename(MEGYE=megye,
           TELEPÜLÉS=Település,
      n_valpolg_nevjegyz=`A`,
           n_valpolg_megjel=`E`,
           n_ervtelen_szav=`M`,
           n_erv_szav=`N`,
           `MSZP-Párbeszéd`=`01`,
           `MKKP`=`02`,
           `Jobbik`=`03`,
           `FIDESZ-KDNP`=`04`,
           `Momentum`=`05`,
           `DK`=`06`,
           `Mi Hazánk`=`07`,
           `MUNKÁSPÁRT`=`08`,
           `LMP`=`09`) %>%
    select(MEGYE,TELEPÜLÉS,`Település típusa`,n_valpolg_nevjegyz,
           n_valpolg_megjel,n_ervtelen_szav,n_erv_szav,
           `MSZP-Párbeszéd`, `MKKP`, `Jobbik`, `FIDESZ-KDNP`,
           `Momentum`, `DK`, `Mi Hazánk`, `MUNKÁSPÁRT`, `LMP`) %>% 
  pivot_longer(cols = !c(MEGYE,TELEPÜLÉS,`Település típusa`,n_valpolg_nevjegyz,
                         n_valpolg_megjel,n_ervtelen_szav,n_erv_szav),
               names_to="LISTA",values_to="SZAVAZAT")
  
# SAVE
write_csv(ep2019_telepules_eredmenyek,
          file="2019_EP/ep2019_telepules_eredmenyek.csv")
  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 2014 EP

ep2014_telepules_eredmenyek <- read_csv(file="2014_EP/2014_ep_listas_szavazas_jkv.csv",
                                        locale = locale(encoding = "ISO-8859-2")) %>% 
  select(MEGYE,TELEPÜLÉS,SZAVAZÓKÖR,VÁLASZTÓPOLGÁR,
        MEGJELENTEK,ÉRVÉNYTELEN,ÉRVÉNYES,LISTA,SZAVAZAT) %>%
  rename(n_valpolg_nevjegyz=`VÁLASZTÓPOLGÁR`,
         n_valpolg_megjel=`MEGJELENTEK`,
         n_ervtelen_szav=ÉRVÉNYTELEN,
         n_erv_szav=ÉRVÉNYES) %>%
  mutate(LISTA=case_when(LISTA %in% "JOBBIK" ~ "Jobbik",
                         LISTA %in% "DEMOKRATIKUS KOALÍCIÓ" ~ "DK",
                         .default = LISTA) ) %>% 
  fill(MEGYE,TELEPÜLÉS,SZAVAZÓKÖR,n_valpolg_nevjegyz,
       n_valpolg_megjel,n_ervtelen_szav,n_erv_szav,
       .direction = "downup") %>%
  filter(!is.na(LISTA))
  
# SAVE SZAVAZAKOR
write_csv(ep2014_telepules_eredmenyek,
          file="2014_EP/ep2014_szavazokor_eredmenyek.csv")

ep2014_telepules_eredmenyek <- ep2014_telepules_eredmenyek %>%
  group_by(MEGYE,TELEPÜLÉS,LISTA) %>%
  summarise(n_valpolg_nevjegyz = sum(n_valpolg_nevjegyz),
            n_valpolg_megjel = sum(n_valpolg_megjel),
            n_ervtelen_szav = sum(n_ervtelen_szav),
            n_erv_szav = sum(n_erv_szav),
            SZAVAZAT = sum(SZAVAZAT)) %>%
  relocate(c(LISTA,SZAVAZAT),.after = last_col())
# SAVE SZAVAZAKOR
write_csv(ep2014_telepules_eredmenyek,
          file="2014_EP/ep2014_telepules_eredmenyek.csv")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# EP 2009
ep2009_telepules_eredmenyek <- left_join(
  read_csv("2009_EP/ep2009_telepules_partlistak.csv"),
  read_csv("2009_EP/ep2009_telepules_reszveteli_adatok.csv") ) %>%
  # ,  locale=locale(encoding = "ISO-8859-1")
  # ,  locale=locale(encoding = "UTF-8")
  relocate(c(LISTA,SZAVAZAT),.after = last_col())

write_csv(ep2009_telepules_eredmenyek,file = "2009_EP/ep2009_telepules_eredmenyek.csv")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

# integrate all into one dataframe
ep_telepules_eredmenyek_2009_2014_2019_2024 <- bind_rows(
  ep2009_telepules_eredmenyek %>% mutate(EV=2009),
  ep2014_telepules_eredmenyek %>% mutate(EV=2014),
  ep2019_telepules_eredmenyek %>% mutate(EV=2019),
  ep2024_telepules_eredmenyek %>% mutate(EV=2024)) %>%
  relocate(c(EV),.before = MEGYE_ID) %>%
  select(!c(TELEPÜLÉS_ID,MEGYE_ID,`Település típusa`))

ep_telepules_eredmenyek_2009_2014_2019_2024 <- ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
  mutate(TELEPÜLÉS=gsub(". kerület|.ker.","",TELEPÜLÉS)) %>%
  rowwise() %>% mutate(TELEPÜLÉS=ifelse(grepl("Budapest",TELEPÜLÉS),
                          paste0("Budapest ",as.numeric(as.roman(gsub("Budapest ","",TELEPÜLÉS)))),
                          TELEPÜLÉS) )
# megye nevek
megye_dict= c(
  "bacs-kiskun" = "bács-kiskun",
  "baranya" = "baranya",
  "bekes" = "békés",
  "borsod-a-z" = "borsod-abaúj-zemplén",
  "budapest" = "budapest",
  "csongrad" = "csongrád",
  "fejer" = "fejér",
  "gyor-m-s" = "győr-moson-sopron",
  "hajdu-b" = "hajdú-bihar",
  "heves" = "heves",
  "jasz-n-sz" = "jász-nagykun-szolnok",
  "komarom-e" = "komárom-esztergom",
  "nograd" = "nógrád",
  "pest" = "pest",
  "somogy" = "somogy",
  "szabolcs-sz-b" = "szabolcs-szatmár-bereg",
  "tolna" = "tolna",
  "vas" = "vas",
  "veszprem" = "veszprém",
  "zala" = "zala",
  "jasz-nk-sz" = "jász-nagykun-szolnok",
  "szabolcs" = "szabolcs-szatmár-bereg" )

ep_telepules_eredmenyek_2009_2014_2019_2024 <- ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
  mutate(MEGYE=tolower(MEGYE)) %>%
  mutate(MEGYE=str_to_title(ifelse(MEGYE %in% names(megye_dict), 
                                   megye_dict[names(megye_dict) %in% MEGYE],MEGYE)))

unique(ep_telepules_eredmenyek_2009_2014_2019_2024$MEGYE)

write_csv(ep_telepules_eredmenyek_2009_2014_2019_2024,
          file = "ep_telepules_eredmenyek_2009_2014_2019_2024.csv")

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 

ogy_val_2022_egyeni <- read_csv("2022_ogy_val/Egyéni_szavazás_szkjkv.csv") %>%
  group_by(MEGYE,TELEPÜLÉS,OEVK) %>% summarise()

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# terkep
library(jsonlite); library(sf)

# source for geoJSON files: https://github.com/szabokrissz96/valterk

my_sf <- read_sf(c("../ogy_eredmenyek/oevk.geo.json",
                   "../ogy_eredmenyek/oevk.min.geo.json",
                   "../ogy_eredmenyek/counties.geojson",
                   "../ogy_eredmenyek/valasztokerulet_terkep_final3.geojson")[4])

plot(st_geometry(my_sf))
st_is_valid(my_sf,reason=T)
st_make_valid(my_sf)

#
library(geojsonR)
file_js = FROM_GeoJson(url_file_string ="../oevk.geo.json")
file_js