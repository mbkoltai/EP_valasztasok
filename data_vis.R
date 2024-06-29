# packages
packs =c("tidyverse","ggrepel") # ,"RcppRoll","scales","lubridate","wpp2019","wesanderson"
missing_packs = setdiff(packs, as.data.frame(installed.packages()[,c(1,3:4)])$Package)
if (length(missing_packs)>0){ lapply(missing_packs,install.packages,character.only=TRUE) }
lapply(packs,library,character.only=TRUE)
# GGPLOT SETTINGS
# standard_theme=theme(plot.title=element_text(hjust=0.5,size=16), 
#                      axis.text.x=element_text(size=13,angle=90,vjust=1/2),
#                      axis.text.y=element_text(size=13),
#                      axis.title.x=element_text(size=15),axis.title.y=element_text(size=15),
#                      legend.title=element_text(size=16),
#                      legend.text=element_text(size=12)) # text=element_text(family="Calibri")
val_theme <- theme(plot.title=element_text(hjust=0.5,size=16),
  axis.text.x=element_text(size=15),axis.title.x=element_text(size=16),
# axis.ticks.y=element_blank(),axis.text.y=element_blank(),
  axis.text.y=element_text(size=15),axis.title.y=element_text(size=16),
  panel.grid.major.y=element_blank(), strip.text=element_text(size=16),
  legend.title=element_text(size=17),legend.text=element_text(size=15))

# dataframe with all elections 2009-2024
ep_telepules_eredmenyek_2009_2014_2019_2024 <- read_csv(
  "ep_telepules_eredmenyek_2009_2014_2019_2024.csv")

# orszagos adatok
ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
  filter(EV %in% c(2019,2024) & !grepl("összesen",TELEPÜLÉS)) %>% 
  group_by(LISTA,EV) %>% summarise(SZAVAZAT=sum(SZAVAZAT)) %>% 
  pivot_wider(names_from = LISTA,values_from = SZAVAZAT) %>%
  mutate(FIDESZ_MiHazank=rowSums(select(.,matches("FIDESZ|Hazánk"))),
         ellenzek=rowSums(select(.,
            matches("LMP|MKKP|Jobbik|Momentum|TISZA|MMN|2RK|MSZP|DK")),na.rm = T) ) %>%
  pivot_longer(!c(EV),names_to = "LISTA",values_to = "SZAVAZAT") %>% arrange(EV,LISTA) %>%
  group_by(EV) %>% # filter(!is.na(SZAVAZAT)) %>% 
  mutate(szazalek=SZAVAZAT/sum(SZAVAZAT[grepl("ellenzek|FIDESZ_MiHazank",LISTA)]))
  

# PLOT
# dataframe 2019 -> 2014 telepulesi eredmenyek
xx = ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
  filter(EV %in% c(2019,2024) & grepl("FIDESZ",LISTA) & !grepl("összesen",TELEPÜLÉS)) %>%
  mutate(szazalek=100*SZAVAZAT/n_erv_szav) %>%
  select(EV,MEGYE,TELEPÜLÉS,n_valpolg_nevjegyz,n_erv_szav,szazalek,SZAVAZAT) %>%
  group_by(MEGYE,TELEPÜLÉS) %>%
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
             "40000-70000","70000+","Budapest"))) %>%
  pivot_wider(names_from = EV,values_from = c(szazalek,SZAVAZAT,n_valpolg_nevjegyz,n_erv_szav)) %>%
  mutate(delta_szazalek=szazalek_2024-szazalek_2019,
         valtozas_szazalek=ifelse(szazalek_2019>szazalek_2024,"csökkent","nőtt"),
         delta_szavazat=SZAVAZAT_2024-SZAVAZAT_2019,
         valtozas_szavazat=ifelse(SZAVAZAT_2019>SZAVAZAT_2024,"csökkent","nőtt")) %>% 
  group_by(telep_meret) %>%
  mutate(sorrend=rank(n_valpolg_nevjegyz_2024,ties.method="first")) %>% # factor()
  mutate(telep_meret=as.character(telep_meret),
         telep_meret=ifelse(grepl("<1000",telep_meret),
                            paste0(telep_meret," (választópolgár)"),telep_meret),
         telep_meret=factor(telep_meret,levels=unique(telep_meret)))

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# segment plot, panelek n_valpolg szerint, y-tengelyen n_valpolg szerint sorrendbe allitva

dummy_df_xlims = xx %>% group_by(telep_meret) %>%
  summarise(sorrend=median(sorrend)) %>%
  slice(rep(1:n(), each=2)) %>% 
  mutate(szazalek_2019=rep(c(30,60),length(unique(xx$telep_meret))),valtozas_szazalek=NA) %>% 
  mutate(szazalek_2019=case_when(
    grepl("20000-40000",telep_meret) & szazalek_2019==30 ~ 15,
    grepl("40000|70000|Budapest",telep_meret) & szazalek_2019==30 ~ 20,
    .default = szazalek_2019))
df_text = xx %>% filter(grepl("Budapest",telep_meret) | 
           n_valpolg_nevjegyz_2024>20e3 | (n_valpolg_nevjegyz_2024>4e3 & delta_szazalek>0)) %>%
  group_by(TELEPÜLÉS) %>% 
  mutate(xadj=szazalek_2024,TELEPÜLÉS=gsub("Budapest ","",TELEPÜLÉS))
# PLOT
xx %>%
ggplot(aes(color=valtozas_szazalek)) + facet_wrap(~telep_meret,scales="free",nrow=2) + # 
  geom_segment(aes(x=szazalek_2019,xend=szazalek_2024,
                   y=sorrend,yend=sorrend,alpha=valtozas_szazalek),
               arrow=arrow(length=unit(0.15,"cm")) ) + # 
  geom_point(aes(x=szazalek_2019,y=sorrend), # x axis limits
             data=dummy_df_xlims,color=NA,show.legend=F) +
  geom_text(data=df_text,aes(x=xadj,y=sorrend,label=TELEPÜLÉS,
            hjust=ifelse(szazalek_2024>szazalek_2019,-0.1,1.1)),
            size=4,show.legend=F,alpha=1) +
  geom_vline(xintercept = 50,linewidth=2/3,color="black") +
  geom_vline(xintercept = c(40,45),linewidth=1/3,color="black",linetype="dashed") +
  labs(color="Fidesz % \n2019→2024",alpha="Fidesz % \n2019→2024") + # Fidesz % \n2019→2024
  scale_color_manual(values = c("darkgreen","red")) + scale_alpha_manual(values = c(1/2,1)) +
  xlab("Fidesz % 2019 → 2024") + ylab("sorrend választópolgárok száma szerint (2024) →") + 
  theme_bw() + val_theme
# SAVE
if (F) {
  "PLOTS/FIDESZKDNP_2019_2024_valtozas_szazalek_y_meretsorrend_segment_xaxis_2rows.png" %>%
  ggsave(width=40,height=28,units="cm")
  }

# geom_point(aes(x=szazalek_2024,y=sorrend,alpha=valtozas_szazalek)) + # ,size=2/3
# scale_y_discrete(expand = expansion(mult=0.03)) + # ,guide = guide_axis(n.dodge = 2

### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# +/- valtozas_szazalek

df_text <- xx %>% filter(grepl("Budapest",telep_meret) | 
                    n_valpolg_nevjegyz_2024>10e3 | (n_valpolg_nevjegyz_2024>5e3 & delta_szazalek>0) ) 
###  | (n_valpolg_nevjegyz_2024>3e3)
dummy_df_xlims <- xx %>% group_by(telep_meret) %>% 
  summarise(n_valpolg_nevjegyz_2024=median(n_valpolg_nevjegyz_2024),sorrend=median(sorrend)) %>%
  slice(rep(1:n(),each=2)) %>% mutate(
    delta_szazalek=rep(c(-15,15),length(unique(xx$telep_meret)) ))
# PLOT
xx %>%
ggplot() + 
  facet_wrap(~telep_meret,scales="free",nrow=2) +
  geom_segment(aes(x=0, xend=delta_szazalek, y=sorrend,yend=sorrend, 
                   color=valtozas_szazalek),alpha=0.5,show.legend=F, # ,size=2/3
               arrow=arrow(length=unit(0.15,"cm"))) +
  geom_point(data=dummy_df_xlims,aes(x=delta_szazalek,y=sorrend),color=NA) +
  geom_vline(xintercept=0,color="black") + # linewidth=3/4,
  geom_vline(xintercept=c(-5,5,10,-10),linewidth=1/3,color="black",linetype="dashed") +
  geom_text(data=df_text %>% filter(n_valpolg_nevjegyz_2024>20e3 | 
                                      delta_szazalek>0 | grepl("Budap",MEGYE)),
            aes(x=ifelse(delta_szazalek<0,1,delta_szazalek*1.1),y=sorrend,
                color=valtozas_szazalek,label=gsub("Budapest ","",TELEPÜLÉS)),
            hjust=0,size=4.5,show.legend=F) + #
  geom_text(data=df_text %>% filter(n_valpolg_nevjegyz_2024<=20e3 & 
                  n_valpolg_nevjegyz_2024>10e3 & !grepl("Budap",MEGYE)),
            aes(x=ifelse(delta_szazalek<0,1/2,delta_szazalek*1.1),y=sorrend,
                  color=valtozas_szazalek,label=gsub("Budapest ","",TELEPÜLÉS)),
            hjust=0,size=2.7,show.legend=F) + # scale_size(range=c(0,12)) +
  scale_y_continuous(expand=expansion(mult=0.03)) +
  scale_color_manual(values = c("darkgreen","red")) +
  xlab("Fidesz-KDNP % szavazatarány (%) változás 2019 -> 2024") + 
  ylab("sorrend választópolgárok száma szerint (2024) →") + 
  theme_bw() + val_theme # 
# SAVE
if (F) {
  ggsave("PLOTS/FIDESZKDNP_szazalek_2019_2024_valtozas_szazalek_2rows.png",
         width=40,height=30,units="cm") 
  }

# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# 2014 -> telepules kategoriankent (nem egyesevel mutatva a telepuleseket)

ep_2019_2024_nvalpolg_fidesz_ellenzek = ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
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
  
ellenzek_listak_2019_2024 = ep_telepules_eredmenyek_2009_2014_2019_2024 %>%
  filter(EV %in% c(2019,2024) & !grepl("Mi H|FIDESZ|MUNK|MEMO",LISTA)) %>%
  group_by(EV,LISTA) %>%
  summarise(EV=unique(EV),
            LISTA=gsub("Párbeszéd","PM",unique(LISTA)) )
# gsub("LMP+","LMP+\n",gsub(" Párt","",gsub("Párbeszéd","PM",unique(LISTA)))) )

# plot
ep_2019_2024_nvalpolg_fidesz_ellenzek %>%
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
  scale_color_manual(values = c("#619CFF","darkorange","darkgrey")) +
  scale_fill_manual(values = c("#619CFF","darkorange","darkgrey")) + 
  scale_x_continuous(breaks=c(seq.default(30,65,by=10),35,45,seq.default(2,12,by=2)*100)) +
  scale_y_continuous(breaks=1/2+(0:7),
                     labels=with(ep_2019_2024_nvalpolg_fidesz_ellenzek,levels(telep_meret))) +
  labs(color="2019→2024 változás",fill="2019 eredmény",
       caption=paste0("ellenzék 2019=",
        paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2019]),collapse="+"),"\n",
        "ellenzék 2024=",paste0(with(ellenzek_listak_2019_2024,LISTA[EV==2024]),collapse="+"))) + 
  xlab("") + ylab("választópolgárok száma") +
  theme_bw() + val_theme + theme(plot.caption=element_text(size=12),
                                 plot.caption.position="plot")
if (T) {
  "PLOTS/fidesz_ellenzek_2019_2024_aggreg_nvalpolg_kateg.png" %>%
  ggsave(width=40,height=30,units="cm")
}


# ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### 
# valtozas merteke

df_plot_valtozas <- ep_2019_2024_nvalpolg_fidesz_ellenzek %>%
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
                     labels=with(ep_2019_2024_nvalpolg_fidesz_ellenzek,levels(telep_meret))) +
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
# # segment plot, telepulesmeret az y tengelyen (vonalak vastagsaga ugyanaz)
