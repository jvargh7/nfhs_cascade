rm(list=ls()); gc(); source(".Rprofile")

source("paper/data_district development.R")
district_met <- bind_rows(read_csv(file = "analysis/nca08_district met need care cascade.csv") %>% 
                            dplyr::filter(is.na(stratification)) %>% 
                            mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()),
                          read_csv(file="analysis/nca04_district2018 level care cascade.csv") %>% 
                            dplyr::filter(is.na(stratification)) %>% 
                            mutate(variable = str_replace(variable,"dm_","") %>% str_to_title()) %>% 
                            dplyr::filter(variable == "Disease") %>% 
                            mutate(variable = "Diabetes")
) %>% 
  dplyr::filter(variable !="Screened") %>% 
  mutate(variable = factor(variable,levels=c("Diabetes","Diagnosed","Treated","Controlled"))) %>% 
  left_join(readxl::read_excel(paste0(path_cascade_repo,"/data/NFHS Cascade Variable List.xlsx"),sheet="map2020_v024") %>% 
              dplyr::select(v024,zone) %>% 
              distinct(v024,.keep_all=TRUE),
            by=c("v024"))

district_humandev = district_met %>% 
  left_join(factsheets %>% dplyr::select(sdist,pc1),
            by = c("REGCODE" = "sdist"))

district_humandev %>% 
  group_by(variable) %>% 
  summarize(r = cor(estimate,pc1)) %>% View()

source("functions/district_map.R")

figA_shp <- readRDS(paste0(path_india_shapefiles,"cleaned/dnfhs5_sp.RDS")) %>% 
  sp::merge(district_met %>%
              distinct(REGCODE,zone),
            by.x="REGCODE",by.y="REGCODE",all.x=TRUE)
state_shp <- readRDS(paste0(path_india_shapefiles,"cleaned/smapsmaster_sp.RDS"))

figA <- tm_shape(figA_shp,ext=1.2) + 
  tm_fill(title= "",
          col="zone",
          palette=c("red","blue","orange","darkgreen","purple","grey"),
          style = "fixed",
          # midpoint = NA,
          textNA="Data not available",
          colorNA = "white") + 
  tm_borders() + 
  tm_shape(state_shp) + tm_borders(col="black") +
  tm_text(text="ST_NM",col="black",size=0.5,remove.overlap = TRUE)+
  tm_legend(legend.position = c("right","top"),
            legend.outside=FALSE,
            legend.just=c("left","top"))+ 
  tm_xlab("") +
  tm_ylab("") +
  tm_layout("",title.size = 2,
            legend.text.size = 1,
            legend.title.size = 1)



figB <- factsheets %>% 
  mutate(variable = "pc1") %>% 
  rename(REGCODE = sdist,
         estimate = pc1) %>% 
  district_map(.,plot_variable = "pc1",plot_title = "",breaks = c(-5.2,-2.5,0,2.5,5),palette_chr = "RdYlGn")


tmap_arrange(
  figA,figB,
  ncol = 1,nrow=2) %>% 
  tmap_save(.,filename=paste0(path_cascade_folder,"/figures/figure_district human development.png"),height=14,width=7)



figC <- district_humandev %>% 
  dplyr::filter(variable == "Diabetes") %>% 
  ggplot(data=.,aes(x=pc1,y=estimate)) +
  geom_point(aes(col=zone)) +
  geom_smooth(method="lm") +
  xlab("Human Development Composite (z-score)") +
  ylab("Diabetes (%)") +
  scale_color_manual(name="Zone",values=c("red","blue","orange","darkgreen","purple","grey")) +
  scale_y_continuous(limits=c(0,30),breaks=seq(0,30,by=5)) +
  theme_bw()

figD <- district_humandev %>% 
  dplyr::filter(variable == "Diagnosed") %>% 
  ggplot(data=.,aes(x=pc1,y=estimate)) +
  geom_point(aes(col=zone)) +
  geom_smooth(method="lm") +
  xlab("Human Development Composite (z-score)") +
  ylab("Diagnosed (%)") +
  scale_color_manual(name="Zone",values=c("red","blue","orange","darkgreen","purple","grey"))  +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20))  +
  theme_bw()


figE <- district_humandev %>% 
  dplyr::filter(variable == "Treated") %>% 
  ggplot(data=.,aes(x=pc1,y=estimate)) +
  geom_point(aes(col=zone)) +
  geom_smooth(method="lm") +
  xlab("Human Development Composite (z-score)") +
  ylab("Treated (%)") +
  scale_color_manual(name="Zone",values=c("red","blue","orange","darkgreen","purple","grey"))  +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20))  +
  theme_bw()


figF <- district_humandev %>% 
  dplyr::filter(variable == "Controlled") %>% 
  ggplot(data=.,aes(x=pc1,y=estimate)) +
  geom_point(aes(col=zone)) +
  geom_smooth(method="lm") +
  xlab("Human Development Composite (z-score)") +
  ylab("Controlled (%)") +
  scale_color_manual(name="Zone",values=c("red","blue","orange","darkgreen","purple","grey"))  +
  scale_y_continuous(limits=c(0,100),breaks=seq(0,100,by=20))  +
  theme_bw()


library(ggpubr)
ggarrange(figC,figE,figD,figF,
          nrow=2,ncol=2,
          common.legend=TRUE,
          legend="bottom") %>% 
  ggsave(.,filename=paste0(path_cascade_folder,"/figures/figure_scatterplot development x cascade.png"),width=14,height=14)
