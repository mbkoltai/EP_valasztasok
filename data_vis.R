# packages
packs=c("tidyverse","ggrepel") # ,"RcppRoll","scales","lubridate","wpp2019","wesanderson"
missing_packs = setdiff(packs, as.data.frame(installed.packages()[,c(1,3:4)])$Package)
if (length(missing_packs)>0){ lapply(missing_packs,install.packages,character.only=TRUE) }
lapply(packs,library,character.only=TRUE); rm(list = c("packs","missing_packs"))
# GGPLOT SETTINGS
val_theme <- theme(plot.title=element_text(hjust=0.5,size=16),
  axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
  plot.caption=element_text(size=12),plot.caption.position="plot",
  axis.text.x=element_text(size=18),axis.text.y=element_text(size=18),
  strip.text = element_text(size=20),legend.title =element_text(size=22),
  panel.grid.major.y=element_blank(), 
  legend.text=element_text(size=15))

# dataframe with all elections 2009-2024
ep_telepules_eredmenyek_2009_2014_2019_2024 <- read_csv(
  "ep_telepules_eredmenyek_2009_2014_2019_2024.csv")

### 
# TELEPULES KATEGORIAK VAL szama szerint
l_telep_meretek <- list(nevek=c("<1000","1000-5000","5000-10000","10000-20000","20000-40000",
                                "40000-70000","70000+","Budapest"),
                        low_lims=c(0,1e3,5e3,1e4,2e4,4e4,7e4),
                        upp_lims=c(1e3,5e3,1e4,2e4,4e4,7e4))
telep_roviditesek <- data.frame(TELEPÜLÉS=unique(l_plot_fidesz_indiv_telep$eredmeny$TELEPÜLÉS)) %>%
  mutate(telep_rovid=case_when(grepl("Zalaegersz",TELEPÜLÉS) ~ "Zalaeg.",
    grepl("Kecskemét",TELEPÜLÉS) ~ "Kecskem.",
                               grepl("Kiskunfélegyháza",TELEPÜLÉS) ~ "Kiskunf.",
                               grepl("Mezőkövesd",TELEPÜLÉS) ~ "Mezőköv.",
                               grepl("Kazincbarc",TELEPÜLÉS) ~ "Kazincb.",
                               grepl("Törökszentmiklós",TELEPÜLÉS) ~ "Törökszentm.",
                               grepl("Hódmezővásárhely",TELEPÜLÉS) ~ "Hódmezőv.",
                               grepl("Tiszaújváros",TELEPÜLÉS) ~ "Tiszaújv.",
                               grepl("Hajdúböszörmény",TELEPÜLÉS) ~ "Hajdúb.",
                               grepl("Hajdúszoboszl",TELEPÜLÉS) ~ "Hajdúszob.",
                               grepl("Nyíregyháza",TELEPÜLÉS) ~ "Nyíregyh.",
                               grepl("Salgótarján",TELEPÜLÉS) ~ "Salgót.",
                               grepl("Mosonmagyaróvár",TELEPÜLÉS) ~ "Mosonnm.", #
                               grepl("Nagykanizsa",TELEPÜLÉS) ~ "Nagykan.", 
                               grepl("Székesfehérvár",TELEPÜLÉS) ~ "Szék.feh.", #
                               grepl("Dunaharaszti",TELEPÜLÉS) ~ "Dunahar.", #
                               grepl("Szigetszentmikl",TELEPÜLÉS) ~ "Szigetszentm.", # 
                               grepl("Jászberény",TELEPÜLÉS) ~ "Jászb.",
                               .default = TELEPÜLÉS) )


# ELLENZEKI LISTAK
ellenzek_listak_2019_2024 <- ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
  filter(EV %in% c(2019,2024) & !grepl("Mi H|FIDESZ|MUNK|MEMO",LISTA)) %>%
  group_by(EV,LISTA) %>%
  summarise(EV=unique(EV),
            LISTA=gsub("Párbeszéd","PM",unique(LISTA)) )
# gsub("LMP+","LMP+\n",gsub(" Párt","",gsub("Párbeszéd","PM",unique(LISTA)))) )

ellenzek_2019_24_caption <- paste0("ellenzék 2019=",
       paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2019]),collapse="+"),"\n",
       "ellenzék 2024=",
       paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2024]),collapse="+"))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# orszagos adatok

# levelszav
l_orszagos <- list()
l_orszagos$level_kulkepv <- bind_rows(read_csv("2024_EP/ep2024_level_kulkepv.csv") %>%
  group_by(LISTA) %>%
  summarise(SZAVAZAT=sum(SZAVAZAT)) %>%
  mutate(EV=2024),
read_csv("2019_EP/ep2019_level_kulkepv.csv") %>%
  group_by(LISTA) %>%
  summarise(SZAVAZAT=sum(SZAVAZAT)) %>%
  mutate(EV=2019) )

# belfoldi szavazatok

l_orszagos$eredmenyek <- bind_rows(
  ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
  filter(EV %in% c(2019,2024) & !grepl("összesen",TELEPÜLÉS)),
  l_orszagos$level_kulkepv %>% mutate(TELEPÜLÉS="kulfold") ) %>% 
  group_by(LISTA,EV) %>% 
  summarise(SZAVAZAT=sum(SZAVAZAT)) %>% 
  group_by(EV) %>% 
  mutate(százalék=100*SZAVAZAT/sum(SZAVAZAT),
         SZAVAZAT=SZAVAZAT/1e3) %>%
  rename(`szavazatok száma (ezer)`=SZAVAZAT) %>%
  filter(!LISTA %in% c("MUNKÁSPÁRT","MEMO")) %>%
  mutate(korm_ellenz=factor(case_when(grepl("FIDESZ",LISTA) ~ "kormány",
                               grepl("Hazánk",LISTA) ~ "Mi Hazánk",
                               .default="ellenzék")) ) %>%
  # szeljobb_ellenz=case_when(grepl("FIDESZ|Haz",LISTA) ~ "szélsőjobb",.default="ellenzék")
  pivot_longer(!c(LISTA,EV,korm_ellenz),names_to="valt_tipus") %>% # ,szeljobb_ellenz
  ungroup() %>% arrange(valt_tipus,korm_ellenz,EV) %>%
  mutate(EV_csoport=fct_rev(factor(paste0(korm_ellenz," (",EV, ")"))),
         LISTA=factor(LISTA),LISTA=reorder(LISTA,as.numeric(korm_ellenz)) )
# levels(l_orszagos$eredmenyek$EV_csoport) # unique(l_orszagos$eredmenyek$EV_csoport)

# plot tartozekok
l_orszagos$szinek = c("FIDESZ-KDNP"="darkorange2","TISZA"="#013E7F","Mi Hazánk"="#74AC64",
  "DK-MSZP-Párbeszéd"="red","MSZP-Párbeszéd"="red",
  "DK"="deepskyblue","Momentum"="darkviolet","LMP"="darkgreen",
  "MKKP"="white","Jobbik"="darkgrey","2RK"="darkgrey","MMN"="darkblue")
l_orszagos$text <- l_orszagos$eredmenyek %>%
  group_by(EV_csoport,EV,korm_ellenz,valt_tipus) %>%
  summarise(value=sum(value))
  
# BAR PLOT
l_orszagos$eredmenyek %>%
ggplot(aes(y=EV_csoport,x=value)) + 
  facet_wrap(~valt_tipus,scales = "free_x",nrow=2) +
  geom_bar(aes(fill=LISTA),stat="identity",position="stack",color="black",width=9/10) + # 
  labs(fill="Pártlisták",caption=ellenzek_2019_24_caption) +
  geom_text(data=l_orszagos$text,aes(label=signif(value,ifelse(grepl("szav",valt_tipus),4,3))),
            hjust=-0.1,size=6) +
  scale_x_continuous(expand = expansion(mult=c(0.01/2,0.08))) +
  scale_fill_manual(values=l_orszagos$szinek) +
  xlab("") + ylab("")  + theme_bw() + val_theme # + theme()
if (F) {
  "PLOTS/FIDESZ_ellenzek_Mihaz_EP_2019_2024_orsz.png" %>%
    ggsave(width=40,height=28,units="cm")
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 2019 -> 2024 telepulesi eredmenyek

# create dataframe
l_plot_fidesz_indiv_telep <- list()
l_plot_fidesz_indiv_telep$eredmeny <- ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
  filter(EV %in% c(2019,2024) & grepl("FIDESZ",LISTA) & !grepl("összesen",TELEPÜLÉS)) %>%
  mutate(szazalek=100*SZAVAZAT/n_erv_szav) %>%
  select(EV,MEGYE,TELEPÜLÉS,n_valpolg_nevjegyz,n_erv_szav,szazalek,SZAVAZAT) %>%
  group_by(MEGYE,TELEPÜLÉS) %>%
  pivot_wider(names_from = EV,values_from = c(szazalek,SZAVAZAT,n_valpolg_nevjegyz,n_erv_szav)) %>%
  select(!n_valpolg_nevjegyz_2019) %>% 
  rename(n_valpolg_nevjegyz=n_valpolg_nevjegyz_2024) %>%
  mutate(telep_meret=factor(case_when(
    n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[1] ~ l_telep_meretek$nevek[1],
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[2] & 
      n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[2] ~ l_telep_meretek$nevek[2],
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[3] & 
      n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[3] ~ l_telep_meretek$nevek[3], 
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[4] & n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[4] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[4], # 
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[5] & 
              n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[5] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[5], # 
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[6] & 
      n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[6] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[6],
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[7] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[7],
    grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[8]),
    levels=l_telep_meretek$nevek)) %>%
  mutate(delta_szazalek=szazalek_2024-szazalek_2019,
         valtozas_szazalek=ifelse(szazalek_2019>szazalek_2024,"csökkent","nőtt"),
         delta_szavazat=SZAVAZAT_2024-SZAVAZAT_2019,
         valtozas_szavazat=ifelse(SZAVAZAT_2019>SZAVAZAT_2024,"csökkent","nőtt")) %>% 
  group_by(telep_meret) %>%
  mutate(sorrend=rank(n_valpolg_nevjegyz,ties.method="first")) %>% # factor()
  mutate(telep_meret=as.character(telep_meret),
         telep_meret=ifelse(grepl("<1000",telep_meret),
                            paste0(telep_meret," (választópolgár)"),telep_meret),
         telep_meret=factor(telep_meret,levels=unique(telep_meret)))

# segment plot, panelek n_valpolg szerint, y-tengelyen n_valpolg szerint sorrendbe allitva
l_plot_fidesz_indiv_telep$dummy_df_xlims = l_plot_fidesz_indiv_telep$eredmeny %>% 
  group_by(telep_meret) %>%
  summarise(sorrend=median(sorrend)) %>%
  slice(rep(1:n(), each=2)) %>% 
  mutate(szazalek_2019=rep(c(30,60),length(unique(l_plot_fidesz_indiv_telep$eredmeny$telep_meret))),
         valtozas_szazalek=NA) %>% 
  mutate(szazalek_2019=case_when(
    grepl("20000-40000",telep_meret) & szazalek_2019==30 ~ 15,
    grepl("40000|70000|Budapest",telep_meret) & szazalek_2019==30 ~ 20,
    .default = szazalek_2019))
# labels
l_plot_fidesz_indiv_telep$df_text = left_join(
  l_plot_fidesz_indiv_telep$eredmeny %>% 
    filter(grepl("Budapest",telep_meret) | 
           n_valpolg_nevjegyz>20e3 | 
           (n_valpolg_nevjegyz>4e3 & delta_szazalek>0) |
           szazalek_2024>50 & n_valpolg_nevjegyz > 1e4) %>%
  group_by(TELEPÜLÉS) %>% 
  mutate(xadj=szazalek_2024,TELEPÜLÉS=gsub("Budapest ","",TELEPÜLÉS)),
  telep_roviditesek) %>% ungroup() %>%
  mutate(telep_rovid=ifelse(is.na(telep_rovid),TELEPÜLÉS,telep_rovid)) %>% 
  select(!TELEPÜLÉS) %>% rename(TELEPÜLÉS=telep_rovid)

dim(l_plot_fidesz_indiv_telep$df_text$TELEPÜLÉS)

# PLOT
l_plot_fidesz_indiv_telep$eredmeny %>% 
ggplot(aes(color=valtozas_szazalek)) + 
  facet_wrap(~telep_meret,scales="free",nrow=2) + # 
  geom_segment(aes(x=szazalek_2019,xend=szazalek_2024,
                   y=sorrend,yend=sorrend,alpha=valtozas_szazalek),
               arrow=arrow(length=unit(0.15,"cm")) ) + # 
  geom_point(aes(x=szazalek_2019,y=sorrend), # x axis limits
             data=l_plot_fidesz_indiv_telep$dummy_df_xlims,color=NA,show.legend=F) +
  geom_text(data=l_plot_fidesz_indiv_telep$df_text,aes(x=xadj,y=sorrend,label=TELEPÜLÉS,
            hjust=ifelse(szazalek_2024>szazalek_2019,-0.1,1.1)),
            size=4.3,show.legend=F,alpha=1) +
  geom_vline(xintercept = 50,linewidth=2/3,color="black") +
  geom_vline(xintercept = c(40,45),linewidth=1/3,color="black",linetype="dashed") +
  labs(color="Fidesz % \n2019→2024",alpha="Fidesz % \n2019→2024") + # Fidesz % \n2019→2024
  scale_color_manual(values = c("#013E7F","#B33C00")) + scale_alpha_manual(values = c(1/2,1)) +
  xlab("Fidesz % 2019 → 2024") + ylab("sorrend választópolgárok száma szerint (2024) →") + 
  theme_bw() + val_theme + theme(axis.text.y = element_blank())
# SAVE
if (F) {
  "PLOTS/FIDESZKDNP_szazalek_szint_2019_2024_indiv_telep.png" %>%
  ggsave(width=44,height=28,units="cm")
}

# szinkod: piros = >50%
l_plot_fidesz_indiv_telep$eredmeny %>% 
  mutate(valtozas_szazalek=ifelse(szazalek_2024>50,"50% felett","<50%")) %>%
  ggplot(aes(color=valtozas_szazalek)) + facet_wrap(~telep_meret,scales="free",nrow=2) + # 
  geom_segment(aes(x=szazalek_2019,xend=szazalek_2024,
                   y=sorrend,yend=sorrend,alpha=valtozas_szazalek),
               arrow=arrow(length=unit(0.15,"cm")) ) + # 
  geom_point(aes(x=szazalek_2019,y=sorrend), # x axis limits
             data=l_plot_fidesz_indiv_telep$dummy_df_xlims,color=NA,show.legend=F) +
  geom_text(data=l_plot_fidesz_indiv_telep$df_text %>% 
              mutate(valtozas_szazalek=ifelse(szazalek_2024>50,"50% felett","<50%")),
            aes(x=xadj,y=sorrend,label=TELEPÜLÉS,
                        hjust=ifelse(szazalek_2024>szazalek_2019,-0.1,1.1)),
            size=4.5,show.legend=F,alpha=1) +
  geom_vline(xintercept = 50,linewidth=2/3,color="black") +
  geom_vline(xintercept = c(40,45),linewidth=1/3,color="black",linetype="dashed") +
  labs(color="Fidesz % (2024)",alpha="Fidesz % (2024)") + # Fidesz % \n2019→2024
  scale_color_manual(values = c("#013E7F","#B33C00")) + scale_alpha_manual(values = c(1/2,1)) +
  xlab("Fidesz % 2019 → 2024") + ylab("sorrend választópolgárok száma szerint (2024) →") + 
  theme_bw() + val_theme + theme(axis.text.y = element_blank())
if (F) {
  "PLOTS/FIDESZKDNP_szazalek_szint_2019_2024_indiv_telep_colorcode_absztobbs.png" %>%
    ggsave(width=44,height=28,units="cm")
}


### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# +/- valtozas_szazalek

l_plot_fidesz_indiv_telep$valtozas$df_text <- left_join(
  l_plot_fidesz_indiv_telep$eredmeny %>% 
    filter(grepl("Budapest",telep_meret) | n_valpolg_nevjegyz>10e3 | 
             (n_valpolg_nevjegyz>5e3 & delta_szazalek>0) ), 
  telep_roviditesek) %>% ungroup() %>%
  mutate(telep_rovid=ifelse(is.na(telep_rovid),TELEPÜLÉS,telep_rovid)) %>% 
  select(!TELEPÜLÉS) %>% rename(TELEPÜLÉS=telep_rovid)
  
###  | (n_valpolg_nevjegyz_2024>3e3)
l_plot_fidesz_indiv_telep$valtozas$dummy_df_xlims <- l_plot_fidesz_indiv_telep$eredmeny %>% 
  group_by(telep_meret) %>% 
  summarise(n_valpolg_nevjegyz=median(n_valpolg_nevjegyz),
            sorrend=median(sorrend)) %>%
  slice(rep(1:n(),each=2)) %>% 
  mutate(delta_szazalek=rep(c(-15,15),
            length(unique(l_plot_fidesz_indiv_telep$eredmeny$telep_meret)) ))  
# PLOT
l_plot_fidesz_indiv_telep$eredmeny %>%
ggplot() + 
  facet_wrap(~telep_meret,scales="free",nrow=2) +
  geom_segment(aes(x=0, xend=delta_szazalek, y=sorrend,yend=sorrend, 
                   color=valtozas_szazalek),alpha=0.5,show.legend=F, # ,size=2/3
               arrow=arrow(length=unit(0.15,"cm"))) +
  geom_point(data=l_plot_fidesz_indiv_telep$valtozas$dummy_df_xlims,
             aes(x=delta_szazalek,y=sorrend),color=NA) +
  geom_vline(xintercept=0,color="black") + # linewidth=3/4,
  geom_vline(xintercept=c(-5,5,10,-10),linewidth=1/3,color="black",linetype="dashed") +
  geom_text(data=l_plot_fidesz_indiv_telep$valtozas$df_text %>%
              filter(n_valpolg_nevjegyz>20e3 | grepl("Budap",MEGYE)),
            aes(x=ifelse(delta_szazalek<0,1,delta_szazalek*1.1),y=sorrend,
                color=valtozas_szazalek,label=gsub("Budapest ","",TELEPÜLÉS)),
            hjust=0,size=4.5,show.legend=F) + #
  geom_text(data=l_plot_fidesz_indiv_telep$valtozas$df_text %>% 
              filter(n_valpolg_nevjegyz<=20e3 & 
                   n_valpolg_nevjegyz>5e3 & !grepl("Budap",MEGYE)),
            aes(x=ifelse(delta_szazalek<0,1/2,delta_szazalek*1.1),y=sorrend,
                color=valtozas_szazalek,label=gsub("Budapest ","",TELEPÜLÉS)),
            hjust=0,size=2.7,show.legend=F) + # scale_size(range=c(0,12)) +
  scale_y_continuous(expand=expansion(mult=0.03)) +
  scale_color_manual(values = c("#013E7F","#B33C00")) +
  xlab("Fidesz-KDNP % szavazatarány (%) változás 2019 -> 2024") + 
  ylab("sorrend választópolgárok száma szerint (2024) →") + 
  theme_bw() + val_theme + theme(axis.text.y = element_blank())
# SAVE
if (F) {
  ggsave("PLOTS/FIDESZKDNP_szazalek_valtozas_2019_2024_indiv_telep.png",
         width=44,height=30,units="cm") 
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# szavazatSZAM KULONBSEG valtozas

l_plot_swing_indiv_telep <- list()
l_plot_swing_indiv_telep$df_2019_24_swing_telep <- ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
  filter(EV %in% c(2019,2024) & !grepl("összesen",TELEPÜLÉS) & 
           !grepl("MEMO|MUNKÁSPÁRT|Hazánk",LISTA)) %>% # kiszurni: kamupartok/mihazank 
  mutate(lista_aggreg=case_when(!grepl("FIDESZ|Hazánk",LISTA) ~ "ellenzék",
                         .default = LISTA)) %>%
  filter(grepl("FIDESZ|ellenzék",lista_aggreg) ) %>%
  mutate(szazalek=100*SZAVAZAT/n_erv_szav) %>%
  select(EV,MEGYE,TELEPÜLÉS,lista_aggreg,n_valpolg_nevjegyz,
         n_erv_szav,szazalek,SZAVAZAT) %>%
  group_by(MEGYE,TELEPÜLÉS,EV,lista_aggreg) %>%
  summarise(n_valpolg_nevjegyz=unique(n_valpolg_nevjegyz),
            n_erv_szav=unique(n_erv_szav),
            szazalek=sum(szazalek),
            SZAVAZAT=sum(SZAVAZAT)) %>%
  group_by(MEGYE,TELEPÜLÉS,EV) %>% 
  summarise(szavszam_kulonbseg=SZAVAZAT[grepl("FIDESZ",lista_aggreg)]-
                           SZAVAZAT[grepl("ellenz",lista_aggreg)],
            szazalek_kulonbseg=szazalek[grepl("FIDESZ",lista_aggreg)]-
                           szazalek[grepl("ellenz",lista_aggreg)],
            n_valpolg_nevjegyz=unique(n_valpolg_nevjegyz)) %>%
  pivot_wider(names_from=EV,
              values_from=c(szavszam_kulonbseg,szazalek_kulonbseg,n_valpolg_nevjegyz)) %>%
  select(!n_valpolg_nevjegyz_2019) %>% rename(n_valpolg_nevjegyz=n_valpolg_nevjegyz_2024) %>%
  mutate(telep_meret=factor(case_when(
    n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[1] ~ l_telep_meretek$nevek[1],
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[2] & 
      n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[2] ~ l_telep_meretek$nevek[2],
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[3] & 
      n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[3] ~ l_telep_meretek$nevek[3], 
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[4] & n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[4] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[4], # 
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[5] & 
      n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[5] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[5], # 
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[6] & 
      n_valpolg_nevjegyz<=l_telep_meretek$upp_lims[6] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[6],
    n_valpolg_nevjegyz>l_telep_meretek$low_lims[7] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[7],
    grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[8]),
    levels=l_telep_meretek$nevek)) %>%
  mutate(delta_szazalek=szazalek_kulonbseg_2024-szazalek_kulonbseg_2019,
         valtozas_szazalek=ifelse(szazalek_kulonbseg_2019>szazalek_kulonbseg_2024, "ellenzéki erősödés",
                                  "FIDESZ erősödés"),
         delta_szavazat=szavszam_kulonbseg_2024-szavszam_kulonbseg_2019,
         valtozas_szavazat=ifelse(szavszam_kulonbseg_2019>szavszam_kulonbseg_2024, "ellenzéki erősödés",
                                  "FIDESZ erősödés")) %>% 
  group_by(telep_meret) %>%
  mutate(sorrend=rank(n_valpolg_nevjegyz,ties.method="first")) %>% # factor()
  mutate(telep_meret=as.character(telep_meret),
         telep_meret=ifelse(grepl("<1000",telep_meret),
                            paste0(telep_meret," (választópolgár)"),telep_meret),
         telep_meret=factor(telep_meret,levels=unique(telep_meret)))

# dummy/text-label DFs
l_plot_swing_indiv_telep$szavazatszam$dummy_df_xlims = l_plot_swing_indiv_telep$df_2019_24_swing_telep %>%
  group_by(telep_meret) %>%
  summarise(sorrend=median(sorrend)) %>%
  slice(rep(1:n(), each=2)) %>%
  mutate(szavszam_kulonbseg_2019=NA) %>%
  mutate(szavszam_kulonbseg_2019=case_when(
    grepl("20000-40000",telep_meret) ~ 9,
    grepl("40000-70000",telep_meret) ~ 9,
    grepl("70000+",telep_meret) ~ 17,
    grepl("Budapest",telep_meret) ~ 4,
    .default=szavszam_kulonbseg_2019))
# text labels
l_plot_swing_indiv_telep$szavazatszam$df_text = left_join( 
  l_plot_swing_indiv_telep$df_2019_24_swing_telep %>%
    filter(grepl("Budapest",telep_meret) |
             n_valpolg_nevjegyz>20e3 | 
             (n_valpolg_nevjegyz>5e3 & delta_szavazat>0) ) %>%
  group_by(TELEPÜLÉS) %>%
  mutate(xadj=szavszam_kulonbseg_2019/1e3,
         TELEPÜLÉS=gsub("Budapest ","",TELEPÜLÉS)),
  telep_roviditesek) %>% ungroup() %>%
  mutate(telep_rovid=ifelse(is.na(telep_rovid),TELEPÜLÉS,telep_rovid)) %>% 
  select(!TELEPÜLÉS) %>% rename(TELEPÜLÉS=telep_rovid)

# PLOT FIDESZ vs ellenzek szavazatszam-kulonbseg valtozas
l_plot_swing_indiv_telep$df_2019_24_swing_telep %>%
  filter(!(is.na(szavszam_kulonbseg_2019) | is.na(szavszam_kulonbseg_2024))) %>%
ggplot(aes(color=valtozas_szavazat)) + 
  facet_wrap(~telep_meret,scales="free",nrow=2) + # 
  geom_segment(aes(x=szavszam_kulonbseg_2019/1e3,xend=szavszam_kulonbseg_2024/1e3,
                   y=sorrend,yend=sorrend,alpha=valtozas_szavazat),
               arrow=arrow(length=unit(0.15,"cm")) ) + # 
  geom_point(data=l_plot_swing_indiv_telep$szavazatszam$dummy_df_xlims,
             aes(x=szavszam_kulonbseg_2019,y=sorrend), # x axis limits
             color=NA,show.legend=F) +
  geom_text(data=l_plot_swing_indiv_telep$szavazatszam$df_text,
            aes(x=xadj,y=sorrend,label=TELEPÜLÉS,
              hjust=ifelse(szavszam_kulonbseg_2019<szavszam_kulonbseg_2024,1.1,-0.01)),
            # hjust=ifelse(szazalek_kulonbseg_2019<szazalek_kulonbseg_2024,1.1,-0.03)),
            size=4.5,show.legend=F,alpha=1) +
  geom_vline(xintercept = 0,linewidth=2/3,color="black") +
  labs(color="szavazatszám-\nkülönbség (2024)",alpha="szavazatszám-\nkülönbség (2024)",
       caption=paste0("ellenzék 2019=",
                      paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2019]),collapse="+"),"\n",
                      "ellenzék 2024=",
                      paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2024]),collapse="+")) ) +
  scale_color_manual(values = c("#013E7F","#B33C00")) + 
  scale_alpha_manual(values = c(1/2,1)) +
  xlab("szavazatszám-különbség (ezer) 2019→2024") +
  ylab("sorrend választópolgárok száma szerint (2024) →") + 
  ggtitle("FIDESZ vs. ellenzék szavazatszám-különbség (ezer) 2019→2024") +
  theme_bw() + val_theme + theme(axis.text.y = element_blank())
# SAVE
if (F) {
  "PLOTS/fideszellenzek_2019_2024_valtozas_szavszamkulonb_indiv_telep.png" %>%
    ggsave(width=40,height=28,units="cm")
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# ugyanez de szinkod: Fidesz>ellenzek

l_plot_swing_indiv_telep$szavazatszam$dummy_df_xlims$szavszam_kulonbseg_2019[
  grepl("10000",l_plot_swing_indiv_telep$szavazatszam$dummy_df_xlims$telep_meret)] <- 3.5
# text labels
l_plot_swing_indiv_telep$szavazatszam$df_text_fideszelony <- left_join(
  l_plot_swing_indiv_telep$df_2019_24_swing_telep %>% 
  mutate(valtozas_szavazat=ifelse(szavszam_kulonbseg_2024>0,
                                  "FIDESZ>ellenzék","FIDESZ<ellenzék")) %>%
  filter(grepl("Budapest",telep_meret) |
           n_valpolg_nevjegyz>20e3 | 
           (n_valpolg_nevjegyz>10e3 & szavszam_kulonbseg_2024>0) ) %>%
  group_by(TELEPÜLÉS) %>%
  mutate(xadj=szavszam_kulonbseg_2019/1e3,
         TELEPÜLÉS=gsub("Budapest ","",TELEPÜLÉS)), telep_roviditesek) %>% 
  ungroup() %>%
  mutate(telep_rovid=ifelse(is.na(telep_rovid),TELEPÜLÉS,telep_rovid)) %>% 
  select(!TELEPÜLÉS) %>% rename(TELEPÜLÉS=telep_rovid)

# PLOT FIDESZ vs ellenzek szavazatszam-kulonbseg valtozas
l_plot_swing_indiv_telep$df_2019_24_swing_telep %>%
  mutate(valtozas_szavazat=ifelse(szavszam_kulonbseg_2024>0,
                                  "FIDESZ>ellenzék","FIDESZ<ellenzék")) %>%
ggplot(aes(color=valtozas_szavazat)) + 
  facet_wrap(~telep_meret,scales="free",nrow=2) + # 
  geom_segment(aes(x=szavszam_kulonbseg_2019/1e3,xend=szavszam_kulonbseg_2024/1e3,
                   y=sorrend,yend=sorrend,alpha=valtozas_szavazat),
               arrow=arrow(length=unit(0.15,"cm")) ) + # 
  geom_point(data=l_plot_swing_indiv_telep$szavazatszam$dummy_df_xlims,
             aes(x=szavszam_kulonbseg_2019,y=sorrend), # x axis limits
             color=NA,show.legend=F) +
  geom_text(data=l_plot_swing_indiv_telep$szavazatszam$df_text_fideszelony,
            aes(x=xadj,y=sorrend,label=TELEPÜLÉS,
                hjust=ifelse(szavszam_kulonbseg_2019<szavszam_kulonbseg_2024,1.1,-0.01)),
            size=4.5,show.legend=F,alpha=1) +
  geom_vline(xintercept = 0,linewidth=2/3,color="black") +
  labs(color="szavazatszám-\nkülönbség (2024)",alpha="szavazatszám-\nkülönbség (2024)",
       caption=paste0("ellenzék 2019=",
            paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2019]),collapse="+"),"\n",
                  "ellenzék 2024=",paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2024]),collapse="+")) ) + 
  scale_color_manual(values = c("#013E7F","#B33C00")) + 
  scale_alpha_manual(values = c(1/2,1)) +
  xlab("szavazatszám-különbség (ezer) 2019→2024") +
  ylab("sorrend választópolgárok száma szerint (2024) →") + 
  ggtitle("FIDESZ vs. ellenzék szavazatszám-különbség (ezer) 2019→2024") +
  theme_bw() + val_theme + theme(axis.text.y = element_blank())

# SAVE
if (F) {
  "PLOTS/fideszellenzek_2019_2024_valtozas_szavszamkulonb_indiv_telep_colorcode_szavsamkulonb.png" %>%
    ggsave(width=44,height=28,units="cm")
}
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# SZAZALEK-KULONBSEG valtozas 2019 -> 2024

# dummy/text-label DFs
l_plot_swing_indiv_telep$szazalek$dummy_df_xlims = l_plot_swing_indiv_telep$df_2019_24_swing_telep %>%
  group_by(telep_meret) %>%
  summarise(sorrend=median(sorrend)) %>%
  slice(rep(1:n(), each=2)) %>%
  mutate(szazalek_kulonbseg_2019=NA) %>%
  mutate(szazalek_kulonbseg_2019=case_when(
    grepl("20000-40000",telep_meret) ~ 44,
    grepl("40000-70000",telep_meret) ~ 35,
    grepl("70000+",telep_meret) ~ 36,
    # grepl("Budapest",telep_meret) ~ -48,
    .default=szazalek_kulonbseg_2019))
# text labels
l_plot_swing_indiv_telep$szazalek$df_text <- left_join( 
  l_plot_swing_indiv_telep$df_2019_24_swing_telep %>%
  filter(grepl("Budapest",telep_meret) |
           n_valpolg_nevjegyz>20e3 | 
           (n_valpolg_nevjegyz>5e3 & delta_szazalek>0) ) %>%
  group_by(TELEPÜLÉS) %>%
  mutate(xadj=szazalek_kulonbseg_2019,
         TELEPÜLÉS=gsub("Budapest ","",TELEPÜLÉS)) %>%
  select(sorrend,TELEPÜLÉS,telep_meret,n_valpolg_nevjegyz,valtozas_szazalek,
         szazalek_kulonbseg_2019,szazalek_kulonbseg_2024,xadj), 
  telep_roviditesek) %>% 
  ungroup() %>%
  mutate(telep_rovid=ifelse(is.na(telep_rovid),TELEPÜLÉS,telep_rovid)) %>% 
  select(!TELEPÜLÉS) %>% rename(TELEPÜLÉS=telep_rovid)

# PLOT FIDESZ vs ellenzek szazalek-kulonbseg valtozas
l_plot_swing_indiv_telep$df_2019_24_swing_telep %>%
ggplot(aes(color=valtozas_szazalek)) + 
  facet_wrap(~telep_meret,scales="free",nrow=2) + # 
  geom_segment(aes(x=szazalek_kulonbseg_2019,xend=szazalek_kulonbseg_2024,
                   y=sorrend,yend=sorrend,alpha=valtozas_szazalek),
               arrow=arrow(length=unit(0.15,"cm")) ) + # 
  geom_point(data=l_plot_swing_indiv_telep$szazalek$dummy_df_xlims,
             aes(x=szazalek_kulonbseg_2019,y=sorrend), # x axis limits
             color=NA,show.legend=F) +
  geom_text(data=l_plot_swing_indiv_telep$szazalek$df_text,
            aes(x=xadj,y=sorrend,label=TELEPÜLÉS,
                hjust=ifelse(szazalek_kulonbseg_2019<szazalek_kulonbseg_2024,1.1,-0.03)),
            size=4.5,show.legend=F,alpha=1) +
  geom_vline(xintercept = 0,linewidth=2/3,color="black") +
  labs(color="",alpha="",caption=paste0("ellenzék 2019=",
          paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2019]),collapse="+"),"\n",
          "ellenzék 2024=",paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2024]),collapse="+"))) + 
  scale_color_manual(values = c("#013E7F","#B33C00")) + # ,"#B33C00"
  scale_alpha_manual(values = c(1/2,1)) +
  xlab("százalék-különbség 2019→2024") +
  ylab("sorrend választópolgárok száma szerint (2024) →") + 
  ggtitle("FIDESZ vs. ellenzék százalék-különbség 2019→2024") +
  theme_bw() + val_theme + theme(axis.text.y = element_blank())
# SAVE
if (F) {
  paste0("PLOTS/","fideszellenzek_2019_2024_valtozas_szazalekkulonb_indiv_telep.png") %>%
    ggsave(width=44,height=28,units="cm")
}

# uez a plot, de a szinkod a FIDESZ elonyt jeloli
l_plot_swing_indiv_telep$szazalek$df_text_fideszelony <- left_join( 
  l_plot_swing_indiv_telep$df_2019_24_swing_telep %>% 
  mutate(valtozas_szazalek=ifelse(szazalek_kulonbseg_2024>0,
                      "FIDESZ>ellenzék","FIDESZ<ellenzék")) %>%
  filter(grepl("Budapest",telep_meret) |
           n_valpolg_nevjegyz>20e3 | 
           (n_valpolg_nevjegyz>10e3 & szazalek_kulonbseg_2024>0) ) %>%
  group_by(TELEPÜLÉS) %>%
  mutate(xadj=szazalek_kulonbseg_2019,
         TELEPÜLÉS=gsub("Budapest ","",TELEPÜLÉS)) %>%
  select(sorrend,TELEPÜLÉS,telep_meret,n_valpolg_nevjegyz,valtozas_szazalek,
         szazalek_kulonbseg_2019,szazalek_kulonbseg_2024,xadj), 
  telep_roviditesek) %>% 
  ungroup() %>%
  mutate(telep_rovid=ifelse(is.na(telep_rovid),TELEPÜLÉS,telep_rovid)) %>% 
  select(!TELEPÜLÉS) %>% rename(TELEPÜLÉS=telep_rovid)

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# PLOT
l_plot_swing_indiv_telep$df_2019_24_swing_telep %>% 
  mutate(valtozas_szazalek=ifelse(szazalek_kulonbseg_2024>0,
                                  "FIDESZ>ellenzék","FIDESZ<ellenzék")) %>%
ggplot(aes(color=valtozas_szazalek)) + 
  facet_wrap(~telep_meret,scales="free",nrow=2) + # 
  geom_segment(aes(x=szazalek_kulonbseg_2019,xend=szazalek_kulonbseg_2024,
                   y=sorrend,yend=sorrend,alpha=valtozas_szazalek),
               arrow=arrow(length=unit(0.15,"cm")) ) + # 
  geom_point(data=l_plot_swing_indiv_telep$szazalek$dummy_df_xlims,
             aes(x=szazalek_kulonbseg_2019,y=sorrend), # x axis limits
             color=NA,show.legend=F) +
  geom_text(data=l_plot_swing_indiv_telep$szazalek$df_text_fideszelony,
            aes(x=xadj,y=sorrend,label=TELEPÜLÉS,
                hjust=ifelse(szazalek_kulonbseg_2019<szazalek_kulonbseg_2024,1.1,-0.03) ),
            size=4.5,show.legend=F,alpha=1) +
  geom_vline(xintercept = 0,linewidth=2/3,color="black") +
  labs(color="",alpha="",caption=paste0("ellenzék 2019=",
                paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2019]),collapse="+"),"\n",
                "ellenzék 2024=",paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2024]),collapse="+"))) + 
  scale_color_manual(values=c("#013E7F","#B33C00")) +
  scale_alpha_manual(values=c(1/2,1)) +
  xlab("százalék-különbség 2019→2024") +
  ylab("sorrend választópolgárok száma szerint (2024) →") + 
  ggtitle("FIDESZ vs. ellenzék százalék-különbség 2019→2024") +
  theme_bw() + val_theme
# SAVE
if (F) {
  paste0("PLOTS/",
    "fideszellenzek_2019_2024_valtozas_szazalekkulonb_indiv_telep_colorcode_fideszelony.png") %>%
    ggsave(width=44,height=28,units="cm")
}

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# telepulesek szama kategoriankent ahol: 
# fidesz > ellenzek 2019 -> 2024
# fidesz > 50% 2019 -> 2024
# fidesz % nott/csokkent

l_fidesz_krit_telepszam <- ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
  filter(EV %in% c(2019,2024) & !grepl("összesen",TELEPÜLÉS) & 
           !grepl("MEMO|MUNKÁSPÁRT|Hazánk",LISTA)) %>% # kiszurni: kamupartok/mihazank 
  mutate(lista_aggreg=case_when(!grepl("FIDESZ|Hazánk",LISTA) ~ "ellenzék",
                                .default=LISTA)) %>%
  filter(grepl("FIDESZ|ellenzék",lista_aggreg) ) %>%
  mutate(szazalek=100*SZAVAZAT/n_erv_szav) %>%
  select(EV,MEGYE,TELEPÜLÉS,lista_aggreg,n_valpolg_nevjegyz,
         n_erv_szav,szazalek,SZAVAZAT) %>%
  group_by(MEGYE,TELEPÜLÉS,EV,lista_aggreg) %>%
  summarise(n_valpolg_nevjegyz=unique(n_valpolg_nevjegyz),
            n_erv_szav=unique(n_erv_szav),
            szazalek=sum(szazalek),
            SZAVAZAT=sum(SZAVAZAT)) %>%
  group_by(MEGYE,TELEPÜLÉS,EV) %>% 
  summarise(szavszam_kulonbseg=SZAVAZAT[grepl("FIDESZ",lista_aggreg)]-
              SZAVAZAT[grepl("ellenz",lista_aggreg)],
            szazalek_kulonbseg=szazalek[grepl("FIDESZ",lista_aggreg)]-
              szazalek[grepl("ellenz",lista_aggreg)],
            fidesz_szazalek=szazalek[grepl("FIDESZ",lista_aggreg)],
            ellenzek_szazalek=szazalek[grepl("ellenz",lista_aggreg)],
            n_valpolg_nevjegyz=unique(n_valpolg_nevjegyz)) %>%
  mutate(telep_meret=factor(case_when(
    n_valpolg_nevjegyz[EV==2024]<=l_telep_meretek$upp_lims[1] ~ l_telep_meretek$nevek[1],
    n_valpolg_nevjegyz[EV==2024]>l_telep_meretek$low_lims[2] & 
      n_valpolg_nevjegyz[EV==2024]<=l_telep_meretek$upp_lims[2] ~ l_telep_meretek$nevek[2],
    n_valpolg_nevjegyz[EV==2024]>l_telep_meretek$low_lims[3] & 
      n_valpolg_nevjegyz[EV==2024]<=l_telep_meretek$upp_lims[3] ~ l_telep_meretek$nevek[3], 
    n_valpolg_nevjegyz[EV==2024]>l_telep_meretek$low_lims[4] & 
      n_valpolg_nevjegyz[EV==2024]<=l_telep_meretek$upp_lims[4] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[4], # 
    n_valpolg_nevjegyz[EV==2024]>l_telep_meretek$low_lims[5] & 
      n_valpolg_nevjegyz[EV==2024]<=l_telep_meretek$upp_lims[5] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[5], # 
    n_valpolg_nevjegyz[EV==2024]>l_telep_meretek$low_lims[6] & 
      n_valpolg_nevjegyz[EV==2024]<=l_telep_meretek$upp_lims[6] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[6],
    n_valpolg_nevjegyz[EV==2024]>l_telep_meretek$low_lims[7] & 
      !grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[7],
    grepl("Budap",TELEPÜLÉS) ~ l_telep_meretek$nevek[8]),
    levels=l_telep_meretek$nevek)) %>% 
  group_by(TELEPÜLÉS,telep_meret) %>%
  summarise(
            # fidesz_szaz_nott=fidesz_szazalek[EV==2024]>=fidesz_szazalek[EV==2019],
            fidesz_szaz_csokk=fidesz_szazalek[EV==2024]<fidesz_szazalek[EV==2019],
            # fidesz_ellenz_kulonb_nott=(fidesz_szazalek[EV==2019]-ellenzek_szazalek[EV==2019]) < 
            #   (fidesz_szazalek[EV==2024]-ellenzek_szazalek[EV==2024]),
            fidesz_ellenz_kulonb_csokkent=(fidesz_szazalek[EV==2019]-ellenzek_szazalek[EV==2019]) >
              (fidesz_szazalek[EV==2024]-ellenzek_szazalek[EV==2024]),
            fidesz_absz_tobb_2019=fidesz_szazalek[EV==2019]>=50,
            fidesz_absz_tobb_2024=fidesz_szazalek[EV==2024]>=50,
            fidesz_gyoz_2019=fidesz_szazalek[EV==2019]>ellenzek_szazalek[EV==2019],
            fidesz_gyoz_2024=fidesz_szazalek[EV==2024]>ellenzek_szazalek[EV==2024]) %>%
  pivot_longer(!c(TELEPÜLÉS,telep_meret)) %>%
  group_by(name,telep_meret) %>%
  summarise(n_telep_krit=sum(value),n_telep_ossz=n()) %>%
  mutate(EV=ifelse(grepl("2019",name),2019,ifelse(grepl("2024",name),2024,NA)),
         name=gsub("_2019|_2024","",name), # ↑↓
         plot_name=case_when(# grepl("szaz_nott",name) ~ "FIDESZ % ↑",
                             grepl("szaz_csokk",name) ~ "FIDESZ % ↓",
                             grepl("gyoz",name) ~ "FIDESZ>ellenzék",
                             # grepl("kulonb_nott",name) ~ "FIDESZ − ellenzék ↑",
                             grepl("kulonb_csokk",name) ~ "FIDESZ − ellenzék ↓",
                             grepl("absz",name) ~ "FIDESZ>50%" ) ) %>%
  mutate(telep_meret_szam=factor(paste0(telep_meret," (n=",n_telep_ossz,")")),
         telep_meret_szam=reorder(telep_meret_szam,as.numeric(telep_meret)))

# PLOT
l_fidesz_krit_telepszam %>%
ggplot(aes(y=plot_name)) + facet_wrap(~telep_meret_szam,scales="free_x") +
  geom_point(data=l_fidesz_krit_telepszam %>% filter(is.na(EV)),
               aes(x=n_telep_krit),size=4,alpha=2/3) + # ,color=factor(EV)
  geom_segment(data = l_fidesz_krit_telepszam %>% filter(!is.na(EV)) %>%
                 select(!c(name,telep_meret,n_telep_ossz)) %>% 
                 pivot_wider(names_from=EV,values_from=n_telep_krit),
    aes(x=`2019`,xend=`2024`),size=1.1,arrow=arrow(length=unit(0.3,"cm"))) +
  # margins
  geom_point(data=l_fidesz_krit_telepszam %>% filter(is.na(EV)),
             aes(x=n_telep_ossz),color=NA) + # ,color=factor(EV)
  geom_point(data=l_fidesz_krit_telepszam %>% group_by(telep_meret_szam) %>% 
               summarise(n_telep_krit=min(c(0.87*min(n_telep_krit),min(n_telep_krit)-1))),
             aes(x=n_telep_krit,y=1),color=NA) + # ,color=factor(EV)
  # labels
  geom_text(data = l_fidesz_krit_telepszam %>% filter(!is.na(EV) & !(EV==2019 & n_telep_krit==0)),
    aes(x=n_telep_krit,hjust=ifelse(EV==2019,-0.1,1.2),label=n_telep_krit),size=5) +
  geom_text(data=l_fidesz_krit_telepszam %>% filter(is.na(EV)) %>%
              group_by(telep_meret_szam,plot_name) %>%
              summarise(x_pos=min(c(0.93*n_telep_krit,n_telep_krit-1/2)),
                        n_telep_krit=unique(n_telep_krit)),
            aes(x=x_pos,label=n_telep_krit),size=5) +
  geom_vline(aes(xintercept=n_telep_ossz) ) +
  labs(caption=paste0("ellenzék 2019=",
        paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2019]),collapse="+"),"\n",
         "ellenzék 2024=",
        paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2024]),collapse="+")) ) + 
  xlab("települések száma") + ylab("") + theme_bw() + val_theme
# SAVE
if (T) {
  "PLOTS/fidesz_ellenzek_aggreg_nvalpolg_kateg_krit_ntelep.png" %>%
    ggsave(width=33,height=25,units="cm")
}


# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 2014 -> telepules kategoriankent (nem egyesevel mutatva a telepuleseket)

ep_2019_2024_fidesz_ellenzek_nvalpolg_kateg <- ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
     filter(EV %in% c(2019,2024)  & 
           !grepl("MUNKÁSPÁRT|MEMO",LISTA) & 
           !grepl("összesen",TELEPÜLÉS)) %>% # 
  mutate(szazalek=100*SZAVAZAT/n_erv_szav) %>%
  select(EV,MEGYE,TELEPÜLÉS,n_valpolg_nevjegyz,n_erv_szav,szazalek,LISTA,SZAVAZAT) %>%
  ungroup() %>%
  mutate(telep_meret=factor(case_when(
    n_valpolg_nevjegyz<=1000 ~ "<1000",
    n_valpolg_nevjegyz>1000 & n_valpolg_nevjegyz<=5000 ~ "1000-5000",
    n_valpolg_nevjegyz>5000 & n_valpolg_nevjegyz<=10000 ~ "5000-10000", 
    n_valpolg_nevjegyz>10000 & n_valpolg_nevjegyz<=20000 & 
      !grepl("Budap",TELEPÜLÉS) ~ "10000-20000", # 
    n_valpolg_nevjegyz>20000 & n_valpolg_nevjegyz<=40000 & 
      !grepl("Budap",TELEPÜLÉS) ~ "20000-40000", # 
    n_valpolg_nevjegyz>40000 & n_valpolg_nevjegyz<=70000 & 
      !grepl("Budap",TELEPÜLÉS) ~ "40000-70000",
    n_valpolg_nevjegyz>70000 & !grepl("Budap",TELEPÜLÉS) ~ "70000+",
    grepl("Budap",TELEPÜLÉS) ~ "Budapest"),
    levels=c("<1000","1000-5000","5000-10000","10000-20000","20000-40000",
             "40000-70000","70000+","Budapest")),
    lista_aggreg=case_when(!grepl("FIDESZ|Hazánk",LISTA) ~ "ellenzék",
                           .default = LISTA) ) %>%
  group_by(MEGYE,TELEPÜLÉS,telep_meret,EV,lista_aggreg) %>%
  summarise(SZAVAZAT=sum(SZAVAZAT),
            n_valpolg_nevjegyz=unique(n_valpolg_nevjegyz),
            n_erv_szav=unique(n_erv_szav)) %>%
  pivot_longer(cols=c(SZAVAZAT,n_erv_szav),
               names_to="lista_osszes",values_to="n_szavazat") %>%
  mutate(lista_osszes=ifelse(lista_osszes %in% "n_erv_szav","összes",lista_aggreg)) %>%
  select(!lista_aggreg) %>% 
  mutate(százalék=n_szavazat/n_szavazat[lista_osszes %in% "összes"]) %>%
  unique()

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# abszolut szavazatszam valtozasa 2024 -> 2019
ep_2019_2024_fidesz_ellenzek_nvalpolg_kateg %>%
  filter(!grepl("Mi H",lista_osszes)) %>%
  group_by(telep_meret,EV,lista_osszes) %>%
  summarise(n_szavazat=sum(n_szavazat)) %>%
  mutate(százalék=100*n_szavazat/n_szavazat[lista_osszes %in% "összes"]) %>%
  pivot_longer(c(n_szavazat,százalék),names_to="tipus") %>%
  filter(!(lista_osszes %in% "összes" & value==100)) %>%
  pivot_wider(names_from="EV",names_prefix="value_") %>%
  mutate(y_base=as.numeric(telep_meret)-1,
         y_adj=as.numeric(factor(lista_osszes))*1/ifelse(grepl("szav",tipus),4,3),
         y_pos=y_base+y_adj,
         value_2019=value_2019/ifelse(grepl("szav",tipus),1e3,1),
         value_2024=value_2024/ifelse(grepl("szav",tipus),1e3,1),
         tipus=ifelse(grepl("szav",tipus),"szavazatok száma (ezer)",tipus)) %>%
ggplot(aes(y=y_pos,group=lista_osszes,color=lista_osszes)) +
  facet_wrap(~tipus,scales="free_x",nrow=2) +
  geom_segment(aes(x=value_2019,xend=value_2024),size=1.6,
               arrow=arrow(length=unit(0.45,"cm"))) +
  geom_point(aes(x=value_2019,fill=lista_osszes),size=2.5,shape=21,color="black") + 
  geom_hline(yintercept=(1:7),linetype="dashed",linewidth=2/3) +
  geom_vline(aes(xintercept = ifelse(grepl("szav",tipus),NA,50))) +
  scale_color_manual(values = c("#013E7F","darkorange2","darkgrey")) +
  scale_fill_manual(values = c("#013E7F","darkorange2","darkgrey")) + 
  scale_x_continuous(breaks=c(seq.default(30,65,by=10),35,45,seq.default(2,12,by=2)*100)) +
  scale_y_continuous(breaks=1/2+(0:7),
                     labels=with(ep_2019_2024_fidesz_ellenzek_nvalpolg_kateg,levels(telep_meret))) +
  labs(color="2019→2024 változás",fill="2019 eredmény",
       caption=ellenzek_2019_24_caption ) + 
  xlab("") + ylab("választópolgárok száma") +
  theme_bw() + val_theme + theme(plot.caption=element_text(size=12),
                                 plot.caption.position="plot")
if (T) {
  "PLOTS/fidesz_ellenzek_2019_2024_aggreg_nvalpolg_kateg.png" %>%
  ggsave(width=40,height=30,units="cm")
}

# (Fidesz-ellenzek) KULONBSEG valtozasa 2024 -> 2019
l_ep_2019_2024_fidesz_ellenzek_nvalpolg_kateg <- list()
l_ep_2019_2024_fidesz_ellenzek_nvalpolg_kateg$swing <- ep_2019_2024_fidesz_ellenzek_nvalpolg_kateg %>%
  filter(!grepl("Mi H",lista_osszes)) %>%
  group_by(telep_meret,EV,lista_osszes) %>%
  summarise(n_szavazat=sum(n_szavazat)) %>%
  mutate(százalék=100*n_szavazat/n_szavazat[lista_osszes %in% "összes"]) %>%
  pivot_longer(c(n_szavazat,százalék),names_to="tipus") %>%
  filter(!(lista_osszes %in% "összes" & value==100)) %>%
  pivot_wider(names_from="EV",names_prefix="value_") %>%
  mutate(y_base=as.numeric(telep_meret)-1,
         y_adj=as.numeric(factor(lista_osszes))*1/ifelse(grepl("szav",tipus),4,3),
         y_pos=y_base+y_adj,
         value_2019=value_2019/ifelse(grepl("szav",tipus),1e3,1),
         value_2024=value_2024/ifelse(grepl("szav",tipus),1e3,1),
         tipus=ifelse(grepl("szav",tipus),"szavazatok száma (ezer)",tipus)) %>%
  group_by(telep_meret,tipus) %>%
  summarise(fidesz_ellenzek_diff_2019=value_2019[grepl("FIDESZ",lista_osszes)]-
                                      value_2019[grepl("ellenz",lista_osszes)],
            fidesz_ellenzek_diff_2024=value_2024[grepl("FIDESZ",lista_osszes)]-
                                      value_2024[grepl("ellenz",lista_osszes)]) 

l_ep_2019_2024_fidesz_ellenzek_nvalpolg_kateg$swing %>%
ggplot(aes(y=telep_meret)) +
  facet_wrap(~tipus,scales="free_x",nrow=2) +
  geom_segment(aes(x=fidesz_ellenzek_diff_2019,xend=fidesz_ellenzek_diff_2024),size=1.9,
               arrow=arrow(length=unit(0.45,"cm")),color="black") +
  geom_point(aes(x=fidesz_ellenzek_diff_2019),size=4,shape=21,fill="grey") + # 
  geom_text(aes(x=fidesz_ellenzek_diff_2024,
                label=signif(fidesz_ellenzek_diff_2024,3),
                hjust=ifelse(fidesz_ellenzek_diff_2024>fidesz_ellenzek_diff_2019,-0.1,1.15)),size=5) +
  geom_hline(yintercept=(1:7)+1/2,linetype="dashed",linewidth=2/3) +
  geom_vline(aes(xintercept=0)) +
  labs(color="",fill="2019",caption=ellenzek_2019_24_caption) +
  ggtitle("FIDESZ-KDNP vs ellenzék különbség változás 2019→2024 \n (pozitív értékek: FIDESZ-KDNP előny)") +
  xlab("") + ylab("választópolgárok száma") +
  # scale_x_continuous(breaks=c(-150,-50,0,50,150,c(-20,-10,10,20)) ) +
  theme_bw() + val_theme # + theme()
if (T) {
  "PLOTS/fidesz_ellenzek_kulonbseg_2019_2024_aggreg_nvalpolg_kateg.png" %>%
    ggsave(width=30,height=25,units="cm")
}

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# valtozas merteke

df_plot_valtozas <- ep_2019_2024_fidesz_ellenzek_nvalpolg_kateg %>%
  # filter(!grepl("Mi H",lista_osszes)) %>%
  group_by(telep_meret,EV,lista_osszes) %>%
  summarise(n_szavazat=sum(n_szavazat)) %>%
  mutate(százalék=100*n_szavazat/n_szavazat[lista_osszes %in% "összes"]) %>%
  pivot_longer(c(n_szavazat,százalék),names_to="tipus") %>%
  filter(!(lista_osszes %in% "összes" & value==100)) %>%
  pivot_wider(names_from="EV",names_prefix="value_") %>%
  mutate(y_base=as.numeric(telep_meret)-1,
         y_adj=as.numeric(factor(lista_osszes))*1/(ifelse(grepl("szav",tipus),4,3) + 
                      ifelse(grepl("Mi H",lista_osszes),1,0)), #
         y_pos=y_base+y_adj,
         value_2019=value_2019/ifelse(grepl("szav",tipus),1e3,1),
         value_2024=value_2024/ifelse(grepl("szav",tipus),1e3,1),
         tipus=ifelse(grepl("szav",tipus),"szavazatok száma (ezer)",tipus),
         valtozas=value_2024-value_2019) 

df_plot_valtozas %>%
ggplot(aes(y=y_pos,group=lista_osszes,color=lista_osszes)) +
  facet_wrap(~tipus,scales="free_x",nrow=2) +
  geom_segment(aes(x=0,xend=valtozas),size=2,arrow=arrow(length=unit(0.4,"cm"))) +
  # geom_point(aes(x=value_2019,fill=lista_osszes),size=2.5,shape=21,color="black") + 
  geom_hline(yintercept=(1:7),linetype="dashed",linewidth=2/3) +
  geom_vline(aes(xintercept=0)) +
  scale_color_manual(values = c("#619CFF","darkorange","black","darkgrey")) + # 
  # scale_x_continuous(breaks=c(seq.default(30,65,by=10),35,45,seq.default(2,12,by=2)*100)) +
  scale_y_continuous(breaks=1/2+(0:7),
                     labels=with(ep_2019_2024_fidesz_ellenzek_nvalpolg_kateg,levels(telep_meret))) +
  labs(color="2019→2024 változás",fill="2019 eredmény", caption=paste0("ellenzék 2019=",
    paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2019]),collapse="+"),"\n",
      "ellenzék 2024=",paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2024]),collapse="+"))) + 
  xlab("") + ylab("választópolgárok száma") +
  theme_bw() + val_theme + theme(legend.title=element_text(size=20),
    legend.text=element_text(size=14),
    plot.caption=element_text(size=12),
    plot.caption.position="plot",legend.position="top") +
  guides(color=guide_legend(title.position="top",label.position="bottom"))
if (T) {
  paste0("PLOTS/fidesz_ellenzek_",
         ifelse(any(grepl("Mi Haz",df_plot_valtozas$lista_osszes)),"mihazank_",""),
         "2019_2024_valtozas_aggreg_nvalpolg_kateg.png") %>%
    ggsave(width=40,height=30,units="cm")
}

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# OEVK-re (valasztoker) vetitve

l_plot_fidesz_ellenz_indiv_telep <- list()
l_plot_fidesz_ellenz_indiv_telep$eredm_szavkor2024 <- left_join(
  read_csv("2024_EP/ep2024_telep_szavkor_sorszam_eredmeny.csv"),
  oevk_telep_szavkor_lista %>% rename(szavkor_sorszam=SZAVAZÓKÖR) %>%
   select(!MEGYE),
  by=c("TELEPÜLÉS","szavkor_sorszam")) %>%
  fill(OEVK, .direction = "down")  # Fill NAs with the previous non-NA value
  
l_plot_fidesz_ellenz_indiv_telep$eredm_oevk2024 <- l_plot_fidesz_ellenz_indiv_telep$eredm_szavkor2024 %>%
  group_by(TELEPÜLÉS) %>%
  mutate(n_valpolg_telep=sum(unique(n_valpolg_nevjegyz))) %>%
  group_by(MEGYE,OEVK) %>%
  mutate(KOZPONT=unique(TELEPÜLÉS[n_valpolg_telep==max(n_valpolg_telep)])) %>%
  group_by(MEGYE,OEVK,LISTA) %>% # telep_oevk,
  summarise(n_erv_szav=sum(unique(n_erv_szav)),
            SZAVAZAT=sum(SZAVAZAT),
            n_szavkor=n_distinct(szavkor_sorszam),
            n_telep=n_distinct(TELEPÜLÉS),
            KOZPONT=unique(KOZPONT)) %>% # )
  mutate(százalék=100*SZAVAZAT/n_erv_szav) %>%
  filter(!grepl("MUNKÁSPÁRT|MEMO",LISTA)) %>% # ungroup() %>%
  mutate(lista_aggreg=case_when(!grepl("FIDESZ|Hazánk",LISTA) ~ "ellenzék",
                           .default = LISTA) ) %>%
  group_by(MEGYE,OEVK,lista_aggreg) %>%
  summarise(SZAVAZAT=sum(SZAVAZAT),
            százalék=sum(százalék),
            n_szavkor=unique(n_szavkor),
            n_telep=unique(n_telep),
            KOZPONT=unique(KOZPONT))

show_every_nth = function(n) { return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]}) }

l_plot_fidesz_ellenz_indiv_telep$kul <- left_join(
  l_plot_fidesz_ellenz_indiv_telep$eredm_oevk2024 %>%
  group_by(MEGYE,OEVK) %>%
  summarise(KOZPONT=unique(KOZPONT),
            ellenzek_gyoz=SZAVAZAT[grepl("ellenz",lista_aggreg)] > 
              SZAVAZAT[grepl("FIDESZ",lista_aggreg)],
            FIDESZ_ellenzek_szav_kul=SZAVAZAT[grepl("FIDESZ",lista_aggreg)]-
              SZAVAZAT[grepl("ellenz",lista_aggreg)],
            FIDESZ_ellenzek_szazal_kul=százalék[grepl("FIDESZ",lista_aggreg)]-
              százalék[grepl("ellenz",lista_aggreg)]) %>%
  group_by(MEGYE) %>%
  mutate(n_oevk_megye=n_distinct(OEVK),
         point_size=case_when(n_oevk_megye>12 ~ 4, 
                              n_oevk_megye>8 ~ 6.5,
                              n_oevk_megye>5 ~ 7, .default=12)),
  telep_roviditesek %>% rename(KOZPONT=TELEPÜLÉS)) %>%
  ungroup() %>%
  mutate(telep_rovid=ifelse(is.na(telep_rovid),KOZPONT,telep_rovid)) %>% 
  select(!KOZPONT) %>% rename(KOZPONT=telep_rovid)
# telepulesnevek roviditve 


l_plot_fidesz_ellenz_indiv_telep$df_lim_szaz <- data.frame(OEVK=NA,
  MEGYE=unique(l_plot_fidesz_ellenz_indiv_telep$eredm_szavkor2024$MEGYE),
  min=c(-13,-22,-14,-21,-51,
        -53,-38,-40,-42,-9,
        -34,-17,-18,-26,-9,
        -41,-11,-10,-11,-19) ) %>% 
  mutate(max=case_when(grepl("Békés|Eszterg",MEGYE) ~ 10,
                       grepl("Kiskun",MEGYE) ~ 15,
                       grepl("Fejér",MEGYE) ~ 26,
                       grepl("Bihar",MEGYE) ~ 30,
                       grepl("Pest",MEGYE) ~ 27,
                       grepl("Csongr",MEGYE) ~ 47)) %>%
  pivot_longer(!c(MEGYE,OEVK),values_to=c("FIDESZ_ellenzek_szazal_kul")) 
  
# plot
l_plot_fidesz_ellenz_indiv_telep$kul %>%
ggplot(aes(x=FIDESZ_ellenzek_szazal_kul,y=as.numeric(OEVK))) + 
  facet_wrap(~MEGYE,scales="free") +
  geom_point(aes(color=ellenzek_gyoz,size=point_size),show.legend=F,alpha=2/3) + 
  geom_point(data=l_plot_fidesz_ellenz_indiv_telep$df_lim_szaz,color=NA) +
  geom_vline(xintercept=0,linewidth=1/2,linetype="dashed") +
  geom_text(aes(label=ifelse(!grepl("Budap",MEGYE),
                        paste0(KOZPONT, " (",ifelse(!ellenzek_gyoz,"+",""),
                               round(FIDESZ_ellenzek_szazal_kul,1),
                               ")" ),""), # as.numeric(OEVK)
                hjust=ifelse(ellenzek_gyoz,-0.1,1.1),
                size=ifelse(grepl("Pest",MEGYE),5,7.5)),show.legend = F) +
  scale_color_manual(values=c("darkorange","#013E7F")) +
  scale_x_continuous(expand = expansion(mult = c(0.11,0.1))) +
  scale_y_continuous(expand = expansion(mult = c(0.09,0.09))) + 
  scale_size_continuous(limits=c(1,12)) +
  ylab("OEVK") + xlab("ΔFIDESZ-ellenzék %") + labs(color="") +
  theme_bw() + val_theme + theme(axis.text.y=element_blank())
if (T) {
  "PLOTS/OEVK_fidesz_ellenzek_szazalek kulonbs.png" %>%
    ggsave(width=40,height=28,units="cm")
}

# what % does the opposition need to have in one block to win?

l_ellenz_oevk_scan <- list()
for (k_ell_int in 1:8) {
  # scan
l_ellenz_oevk_scan[[k_ell_int]] <- l_plot_fidesz_ellenz_indiv_telep$eredm_oevk2024 %>%
  group_by(MEGYE,OEVK) %>%
  summarise(ellenz_gyoz=SZAVAZAT[grepl("ellenz",lista_aggreg)]*(0.65+((k_ell_int-1)*5)/100) > 
              SZAVAZAT[grepl("FIDESZ",lista_aggreg)],
            fidesz_gyoz=SZAVAZAT[grepl("ellenz",lista_aggreg)]*(0.65+((k_ell_int-1)*5)/100) < 
              SZAVAZAT[grepl("FIDESZ",lista_aggreg)]) %>%
  ungroup() %>%
  summarise(n_ellenz_oevk=sum(ellenz_gyoz),
            n_fidesz_oevk=sum(fidesz_gyoz)) %>%
  mutate(ellenz_integr=(0.65+((k_ell_int-1)*5)/100)*100)
}

bind_rows(l_ellenz_oevk_scan) %>%
  pivot_longer(!ellenz_integr) %>%
  mutate(name=ifelse(grepl("fidesz",name),"FIDESZ-KDNP","ellenzék")) %>%
ggplot(aes(x=ellenz_integr,y=value,color=name)) + 
  geom_line() + geom_point(alpha=3/4,size=5) +
  geom_text(aes(label=value,y=value+ifelse(grepl("FIDESZ",name),-3,3)),
            show.legend=F,size=5.5) +
  scale_color_manual(values=c("#013E7F","darkorange")) +
  xlab("ellenzéki szavazatok egy blokkban (% összes)") + ylab("megnyert OEVK-k száma") + 
  labs(color="") + scale_y_continuous(limits = c(0,106)) +
  geom_hline(yintercept=53,linewidth=1/3,linetype="dashed") +
  theme_bw() + val_theme
if (T) {
  "PLOTS/OEVK_fidesz_ellenzek_scan.png" %>%
    ggsave(width=32,height=22,units="cm")
}
