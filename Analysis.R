#####Introduction#####

#The following code imports CoralReefWatch Puerto Rico 5 km Single-Pixel Virtual station data to map the coral reef stations, determine which years experienced ecologically significant and ecologically severe heat stress, and compares rates of warming between Southern, Western, Northern, and Eastern Regions for the years 1985 to 2023 with Version 3.4 Thermal History product suite. 

#https://coralreefwatch.noaa.gov/product/vs_single_pixel_exp/puerto_rico.php

#Data accessed 10 January 2024

#####Load Packages#####
library(tidyverse)
library(lubridate)
library(nlme)
library(emmeans)
library(ggmap)
library(RColorBrewer)
library(scales)
library(readxl)
library(patchwork)
library(tigris)
library(ggthemes)

#####Import Data#####
LaParguera=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_LaParguera.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
LaParguera$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_LaParguera.txt")[3,1])
LaParguera$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_LaParguera.txt")[5,1])
LaParguera$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_LaParguera.txt")[7,1])+1
LaParguera$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_LaParguera.txt")[1,1])
LaParguera$Region="Southern"

Guanica=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Guanica.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
Guanica$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Guanica.txt")[3,1])
Guanica$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Guanica.txt")[5,1])
Guanica$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Guanica.txt")[7,1])+1
Guanica$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Guanica.txt")[1,1])
Guanica$Region="Southern"

Guayanilla=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Guayanilla.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
Guayanilla$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Guayanilla.txt")[3,1])
Guayanilla$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Guayanilla.txt")[5,1])
Guayanilla$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Guayanilla.txt")[7,1])+1
Guayanilla$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Guayanilla.txt")[1,1])
Guayanilla$Region="Southern"

Salinas=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Salinas.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
Salinas$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Salinas.txt")[3,1])
Salinas$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Salinas.txt")[5,1])
Salinas$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Salinas.txt")[7,1])+1
Salinas$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Salinas.txt")[1,1])
Salinas$Region="Southern"

CaboRojo=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_CaboRojo.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
CaboRojo$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_CaboRojo.txt")[3,1])
CaboRojo$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_CaboRojo.txt")[5,1])
CaboRojo$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_CaboRojo.txt")[7,1])+1
CaboRojo$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_CaboRojo.txt")[1,1])
CaboRojo$Region="Western"

Rincon=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Rincon.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
Rincon$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Rincon.txt")[3,1])
Rincon$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Rincon.txt")[5,1])
Rincon$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Rincon.txt")[7,1])+1
Rincon$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Rincon.txt")[1,1])
Rincon$Region="Western"

Aguadilla=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Aguadilla.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
Aguadilla$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Aguadilla.txt")[3,1])
Aguadilla$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Aguadilla.txt")[5,1])
Aguadilla$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Aguadilla.txt")[7,1])+1
Aguadilla$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Aguadilla.txt")[1,1])
Aguadilla$Region="Western"

Isabela=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Isabela.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
Isabela$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Isabela.txt")[3,1])
Isabela$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Isabela.txt")[5,1])
Isabela$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Isabela.txt")[7,1])+1
Isabela$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Isabela.txt")[1,1])
Isabela$Region="Northern"

Arecibo=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Arecibo.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
Arecibo$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Arecibo.txt")[3,1])
Arecibo$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Arecibo.txt")[5,1])
Arecibo$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Arecibo.txt")[7,1])+1
Arecibo$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Arecibo.txt")[1,1])
Arecibo$Region="Northern"

VegaBaja=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_VegaBaja.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
VegaBaja$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_VegaBaja.txt")[3,1])
VegaBaja$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_VegaBaja.txt")[5,1])
VegaBaja$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_VegaBaja.txt")[7,1])+1
VegaBaja$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_VegaBaja.txt")[1,1])
VegaBaja$Region="Northern"

SanJuan=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_SanJuan.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
SanJuan$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_SanJuan.txt")[3,1])
SanJuan$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_SanJuan.txt")[5,1])
SanJuan$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_SanJuan.txt")[7,1])+1
SanJuan$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_SanJuan.txt")[1,1])
SanJuan$Region="Northern"

Fajardo=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Fajardo.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
Fajardo$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Fajardo.txt")[3,1])
Fajardo$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Fajardo.txt")[5,1])
Fajardo$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Fajardo.txt")[7,1])+1
Fajardo$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Fajardo.txt")[1,1])
Fajardo$Region="Eastern"

Culebra=read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Culebra.txt", skip = 14) %>% 
  filter(YYYY<2024) %>% 
  mutate(date=make_date(YYYY,MM,DD))
Culebra$Latitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Culebra.txt")[3,1])
Culebra$Longitude=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Culebra.txt")[5,1])
Culebra$BleachingThreshold=as.numeric(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Culebra.txt")[7,1])+1
Culebra$Site=as.character(read_csv("Data/AtlanticOcean_Caribbean_PuertoRico_Culebra.txt")[1,1])
Culebra$Region="Eastern"

CoralTempPR=rbind(LaParguera,Guanica,Guayanilla,Salinas,CaboRojo,Rincon,Aguadilla,Isabela,Arecibo,VegaBaja,SanJuan,Fajardo,Culebra)

######Map of Puerto Rico#####
Site_Coordinates=CoralTempPR %>% 
  group_by(Region,Site) %>% 
  summarize(Longitude=mean(Longitude),Latitude=mean(Latitude)) %>% 
  mutate(Region = fct_relevel(Region, "Northern", "Southern", "Eastern", "Western"))

options(tigris_class = "sf")

PRmap=counties(state = "PR", cb = TRUE)

Figure1=ggplot() +
  geom_sf (data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(data=Site_Coordinates %>% mutate(Region = fct_relevel(Region, "Northern", "Southern", "Eastern", "Western")),mapping=aes(x=Longitude,y=Latitude,color=Region),size=5)+
  annotate(geom="text", x=-65.3, y=18.3-0.06, label="Culebra",color="#252525",size=7)+
  annotate(geom="text", x=-65.6+0.1, y=18.3-0.02, label="Fajardo",color="#252525",size=7)+
  annotate(geom="text", x=-66.7, y=18.5+0.07, label="Arecibo",color="#252525",size=7)+
  annotate(geom="text", x=-67.1, y=18.5+0.07, label="Isabela",color="#252525",size=7)+
  annotate(geom="text", x=-66.0, y=18.5+0.02, label="San Juan",color="#252525",size=7)+
  annotate(geom="text", x=-66.4, y=18.5+0.07, label="Vega Baja",color="#252525",size=7)+
  annotate(geom="text", x=-66.9, y=17.9-0.05, label="Guanica",color="#252525",size=7)+
  annotate(geom="text", x=-66.7, y=17.9-0.01, label="Guayanilla",color="#252525",size=7)+
  annotate(geom="text", x=-67.1, y=17.9-0.01, label="La Parguera",color="#252525",size=7)+
  annotate(geom="text", x=-66.2, y=17.9-0.02, label="Salinas",color="#252525",size=7)+
  annotate(geom="text", x=-67.2-0.1, y=18.4+0.02, label="Aguadilla",color="#252525",size=7)+
  annotate(geom="text", x=-67.2-0.17, y=18.2-0.02, label="Cabo Rojo",color="#252525",size=7)+
  annotate(geom="text", x=-67.3-0.07, y=18.3+0.02, label="Rincon",color="#252525",size=7)+
  scale_color_brewer(palette = "RdYlBu")+
  scale_shape_manual(values=c(15,18,16,17))+
  scale_y_continuous(limits = c(17.7,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-68,-65.15), expand = c(0, 0)) +
  xlab("")+
  ylab("")+
  theme_tufte()+
  theme(text = element_text(size=24),
        plot.title = element_text(vjust = -8),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position="off",
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(2, "lines"))
ggsave('Figure1.tiff', Figure1, width = 16, height = 6, dpi = 300)


######Are there differences in the rate of warming between regions?#####
regionSSTlme=lme(SST~YYYY*Region,random=~YYYY|Site,data=CoralTempPR) #random slopes and intercepts varying by site
summary(regionSSTlme)  #yes rates of warming are different
regionSSTlme_region_slope = emtrends(regionSSTlme, pairwise ~ Region, var="YYYY",adjust="tukey")

Figure2=
  as.data.frame(regionSSTlme_region_slope$emtrends) %>% 
  mutate(Region = fct_relevel(Region, "Northern", "Southern", "Eastern", "Western")) %>%
  ggplot() +
  geom_hline(yintercept=0.0,colour="gray",size=1)+
  geom_hline(yintercept=0.01,colour="red",linetype="dashed",size=2)+
  geom_linerange(aes(x = Region, ymin = lower.CL, ymax = upper.CL,color=Region), linewidth = 3) + 
  geom_point(aes(x = Region, y = YYYY.trend,fill=Region), size=6, color="black", shape=22) + 
  scale_fill_brewer(palette = "RdYlBu")+
  scale_colour_brewer(palette = "RdYlBu")+
  scale_y_continuous(name = "Rate of Warming (°C/year)",sec.axis = sec_axis( trans=~.*10, name="Rate of Warming (°C/decade)")) +
  theme_classic()+
  theme(text = element_text(size=16),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x=element_blank(),
        legend.position="none",
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 6)),
        panel.background = element_blank(),
        axis.ticks = element_blank())
ggsave('Figure2.tiff', Figure2, width = 8, height = 6, dpi = 300)

siteSSTlm=lm(SST~YYYY*Site,data=CoralTempPR)
summary(siteSSTlm)
siteSSTlm_site_slope = emtrends(siteSSTlm,pairwise~Site,var="YYYY",adjust="tukey")

Appendix1=
  as.data.frame(merge(x=as.data.frame(siteSSTlm_site_slope$emtrends),y=unique(CoralTempPR[,c(14,15)]),by="Site",all.x=TRUE)) %>% 
  mutate(Region = fct_relevel(Region, "Northern", "Southern", "Eastern", "Western")) %>%
  ggplot() +
  geom_hline(yintercept=0.0,colour="gray",size=1)+
  geom_hline(yintercept=0.01,colour="red",linetype="dashed",size=2)+
  geom_linerange(aes(x = Site, ymin = lower.CL, ymax = upper.CL, color=Region), linewidth = 3) + 
  geom_point(aes(x = Site, y = YYYY.trend, fill=Region), size=6, color="black", shape=22) + 
  facet_wrap(.~Region,ncol=4, scales = "free")+
  scale_fill_brewer(palette = "RdYlBu")+
  scale_colour_brewer(palette = "RdYlBu")+
  scale_y_continuous(limits = c(0,0.025),name = "Rate of Warming (°C/year)",sec.axis = sec_axis( trans=~.*10, name="Rate of Warming (°C/decade)")) +
  theme_classic()+
  theme(text = element_text(size=16),
        plot.title = element_text(vjust = -8),
        legend.title=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(colour = "black",angle=30,hjust=1),
        axis.text.y = element_text(colour = "black"),
        #axis.text.y.right = element_blank(),
        #axis.title.y.right = element_blank(),
        legend.position="none",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(0, "lines"))

ggsave('Appendix1.tiff', Appendix1, width = 12, height = 4, dpi = 300)

######Which years experienced ecologically significant and ecologically severe heat stress?#####
Figure3=CoralTempPR %>% 
  mutate(Region = fct_relevel(Region, "Northern", "Southern", "Eastern", "Western")) %>%
  group_by(Region,YYYY) %>% 
  summarize(max_DHW=max(DHW)) %>% 
  ggplot()+
  geom_bar(stat='identity',position='dodge',aes(x=YYYY,y=max_DHW))+
  facet_grid(Region~.)+
  geom_hline(yintercept=4,linetype="dashed",size=1,color="#fdae61")+
  geom_hline(yintercept=8,linetype="dashed",size=1,color="#d73027")+
  geom_hline(yintercept=0)+
  scale_x_continuous(expand = c(0,1), limits=c(1984,2024),breaks = seq(1980,2024,2))+
  scale_y_continuous(expand = c(0,0), limits=c(0,20),breaks = seq(0,20,5))+
  xlab("Year")+
  ylab("Annual Max Degree Heating Weeks")+
  theme_classic()+
  theme(text = element_text(size=16),
        legend.title=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.spacing.y = unit(1, "lines"))
ggsave('Figure3.tiff', Figure3, width = 11, height = 9, dpi = 300)

Appendix2=CoralTempPR %>% 
  mutate(Site = paste(Region,Site, sep="-")) %>% 
  group_by(Site,YYYY) %>% 
  summarize(max_DHW=max(DHW)) %>% 
  ggplot()+
  geom_bar(stat='identity',position='dodge',aes(x=YYYY,y=max_DHW))+
  facet_wrap(.~Site,ncol=1)+
  geom_hline(yintercept=4,linetype="dashed",size=1,color="#fdae61")+
  geom_hline(yintercept=8,linetype="dashed",size=1,color="#d73027")+
  geom_hline(yintercept=0)+
  scale_x_continuous(expand = c(0,0.5), limits=c(1984,2024),breaks = seq(1980,2024,2))+
  scale_y_continuous(expand = c(0,0), limits=c(0,20),breaks = seq(0,20,5))+
  xlab("Year")+
  ylab("Annual Max Degree Heating Weeks")+
  theme_classic()+
  theme(text = element_text(size=16),
        plot.title = element_text(vjust = -8),
        legend.title=element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        legend.position="top",
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(0, "lines"))
ggsave('Appendix2.tiff', Appendix2, width = 13, height = 13, dpi = 300)

######Visualize total number of annual coral bleaching observations#####

#Coral Bleaching indices from NCRMP Surveys
#T=total bleaching
#PB=partial bleaching
#P=paling
#N=no bleaching

#Definitions of coral bleaching ReefBase/Donner et al (2017)
#No bleaching = no bleaching observed
#Mild Bleaching = 1-10% of coral cover bleached
#Moderate Bleaching = 11-50% of coral cover bleached
#Severe Bleaching = >50% of coral cover bleached

options(tigris_class = "sf")

PRmap=counties(state = "PR", cb = TRUE)

myPalette <- colorRampPalette(brewer.pal(11, "YlOrRd"))

# Plot coral bleaching literature obersvations
bleaching_database_all=read_excel("Data/CM_BleachingData_vanWoesik_Kratochwill_2022.xlsx")

bleaching_database=bleaching_database_all %>% 
  group_by(Date_Year) %>% 
  count() %>% 
  rename(Year=Date_Year)

all_observations=read_csv("Data/Puerto_Rico_Bleaching_Summary.csv") %>% 
  group_by(Year) %>% 
  count()

Figure4a=ggplot() +
  geom_sf (data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(data=bleaching_database_all,mapping=aes(x=Longitude_Degrees,y=Latitude_Degrees),size=3,color="#252525")+
  scale_y_continuous(limits = c(17.85,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-68,-65.15), expand = c(0, 0)) +
  xlab("")+
  ylab("")+
  ggtitle("(a) Site-level Coral Bleaching Observations")+
  theme_tufte()+
  theme(text = element_text(size=16, family="sans"),
        plot.title = element_text(size=20,vjust = -8),
        legend.title=element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position="off",
        legend.box = "vertical",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(2, "lines"))

Figure4b=ggplot()+
  geom_point(mapping=aes(x=seq(1965,2025,1),y="Bleaching"),color="gray",shape=3,size=3)+
  geom_point(data=all_observations,mapping=aes(x=Year,y="Bleaching"),size=3,color="#252525")+
  scale_x_continuous(expand = c(0,2), limits=c(1965,2025),breaks = seq(1960,2025,5))+
  xlab("")+
  ylab("")+
  ggtitle("(b) Timeline of Coral Bleaching Observations")+
  theme_classic()+
  theme(text = element_text(size=20),
        plot.title = element_text(size=20),
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_blank(),
        axis.title.x=element_blank(),
        legend.position="none",
        panel.border = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.line.y=element_blank(),
        panel.grid.major.y = element_line(colour = "grey"),
        panel.background = element_blank(),
        axis.ticks = element_blank())

Figure4ab=Figure4a/Figure4b+
  plot_layout(heights = unit(c(10, 3), c('cm', 'cm')))

#####NCRMP Coral Bleaching Surveys#### 
#Coral Bleaching indices from NCRMP Surveys
#T=total bleaching
#PB=partial bleaching
#P=paling
#N=no bleaching

#Definitions of coral bleaching following Donner et al (2017)
#No bleaching = no bleaching observed
#Mild Bleaching = 1-10% of coral cover bleached
#Moderate Bleaching = 11-50% of coral cover bleached
#Severe Bleaching = >50% of coral cover bleached

#####NCRMP 2014 Coral Demographics Bleaching####
NCRMP_2014_bleached_corals=read_csv("Data/NCRMP_PR2014_Benthic_Data02_CoralDemographics.csv")%>% 
  filter(BLEACH_CONDITION=="PB"|BLEACH_CONDITION=="T") %>%
  group_by(PRIMARY_SAMPLE_UNIT) %>% 
  summarize(BLEACHED_CORALS=sum(N))

NCRMP_2014_total_corals=read_csv("Data/NCRMP_PR2014_Benthic_Data02_CoralDemographics.csv")%>% 
  filter(BLEACH_CONDITION=="PB"|BLEACH_CONDITION=="P"|BLEACH_CONDITION=="T"|BLEACH_CONDITION=="N") %>%
  group_by(LAT_DEGREES,LON_DEGREES,PRIMARY_SAMPLE_UNIT) %>% 
  summarize(TOTAL_CORALS=sum(N))

NCRMP_2014_bleaching=merge(NCRMP_2014_bleached_corals,NCRMP_2014_total_corals,by="PRIMARY_SAMPLE_UNIT",all.y=TRUE) %>% 
  mutate(PERCENT_BLEACHED=(BLEACHED_CORALS/TOTAL_CORALS)*100) %>% 
  mutate(PERCENT_BLEACHED = ifelse(is.na(PERCENT_BLEACHED), 0, PERCENT_BLEACHED))

#Site-level moderate bleaching (>10% of colonies) observed in 0 sites
NCRMP_2014_bleaching %>% 
  summarize(sum(PERCENT_BLEACHED>10&PERCENT_BLEACHED<=50))

#Site-level severe bleaching (>50% of colonies) observed at 0 sites
NCRMP_2014_bleaching %>% 
  summarize(sum(PERCENT_BLEACHED>50))

NCRMP_2014_bleaching_plot=
  ggplot() +
  geom_sf (data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(data=NCRMP_2014_bleaching,mapping=aes(x=LON_DEGREES,y=LAT_DEGREES,fill=PERCENT_BLEACHED),color="black",size=3,shape=21)+
  scale_fill_gradientn(colours = myPalette(100), limits=c(1, 75))+
  scale_y_continuous(limits = c(17.80,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-68,-65.15), expand = c(0, 0)) +
  xlab("")+
  ylab("")+
  labs(fill='% Bleached')+
  ggtitle("(c) NCRMP 2014 Coral Bleaching")+
  theme_tufte()+
  theme(text = element_text(size=16, family="sans"),
        plot.title = element_text(size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position="none",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(2, "lines"))

#####NCRMP 2016 Coral Demographics Bleaching####
NCRMP_2016_bleached_corals=read_csv("Data/NCRMP_PR2016_Benthic_Data02_CoralDemographics.csv")%>% 
  filter(YEAR==2016) %>% 
  filter(BLEACH_CONDITION=="PB"|BLEACH_CONDITION=="T") %>%
  group_by(PRIMARY_SAMPLE_UNIT) %>% 
  summarize(BLEACHED_CORALS=sum(N))

NCRMP_2016_total_corals=read_csv("Data/NCRMP_PR2016_Benthic_Data02_CoralDemographics.csv")%>% 
  filter(YEAR==2016) %>% 
  filter(BLEACH_CONDITION=="PB"|BLEACH_CONDITION=="P"|BLEACH_CONDITION=="T"|BLEACH_CONDITION=="N") %>%
  group_by(LAT_DEGREES,LON_DEGREES,PRIMARY_SAMPLE_UNIT) %>% 
  summarize(TOTAL_CORALS=sum(N))

NCRMP_2016_bleaching=merge(NCRMP_2016_bleached_corals,NCRMP_2016_total_corals,by="PRIMARY_SAMPLE_UNIT",all.y=TRUE) %>% 
  mutate(PERCENT_BLEACHED=(BLEACHED_CORALS/TOTAL_CORALS)*100) %>% 
  mutate(PERCENT_BLEACHED = ifelse(is.na(PERCENT_BLEACHED), 0, PERCENT_BLEACHED))

#Site-level moderate bleaching (>10% of colonies) observed in 0 sites
NCRMP_2016_bleaching %>% 
  summarize(sum(PERCENT_BLEACHED>10&PERCENT_BLEACHED<=50))

#Site-level severe bleaching (>50% of colonies) observed at 0 sites
NCRMP_2016_bleaching %>% 
  summarize(sum(PERCENT_BLEACHED>50))

NCRMP_2016_bleaching_plot=
  ggplot() +
  geom_sf (data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(data=NCRMP_2016_bleaching,mapping=aes(x=LON_DEGREES,y=LAT_DEGREES,fill=PERCENT_BLEACHED),color="black",size=3,shape=21)+
  scale_fill_gradientn(colours = myPalette(100), limits=c(1, 75))+
  scale_y_continuous(limits = c(17.80,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-68,-65.15), expand = c(0, 0)) +
  xlab("")+
  ylab("")+
  labs(fill='% Bleached')+
  ggtitle("(d) NCRMP 2016 Coral Bleaching")+
  theme_tufte()+
  theme(text = element_text(size=16, family="sans"),
        plot.title = element_text(size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position="none",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(2, "lines"))

#####NCRMP 2017 Coral Demographics Bleaching####

NCRMP_2017_bleached_corals=read_csv("Data/NCRMP_PR2016_Benthic_Data02_CoralDemographics.csv")%>% 
  filter(YEAR==2017) %>% 
  filter(BLEACH_CONDITION=="PB"|BLEACH_CONDITION=="T") %>%
  group_by(PRIMARY_SAMPLE_UNIT) %>% 
  summarize(BLEACHED_CORALS=sum(N))

NCRMP_2017_total_corals=read_csv("Data/NCRMP_PR2016_Benthic_Data02_CoralDemographics.csv")%>% 
  filter(YEAR==2017) %>% 
  filter(BLEACH_CONDITION=="PB"|BLEACH_CONDITION=="P"|BLEACH_CONDITION=="T"|BLEACH_CONDITION=="N") %>%
  group_by(LAT_DEGREES,LON_DEGREES,PRIMARY_SAMPLE_UNIT) %>% 
  summarize(TOTAL_CORALS=sum(N))

NCRMP_2017_bleaching=merge(NCRMP_2017_bleached_corals,NCRMP_2017_total_corals,by="PRIMARY_SAMPLE_UNIT",all.y=TRUE) %>% 
  mutate(PERCENT_BLEACHED=(BLEACHED_CORALS/TOTAL_CORALS)*100) %>% 
  mutate(PERCENT_BLEACHED = ifelse(is.na(PERCENT_BLEACHED), 0, PERCENT_BLEACHED))

#Site-level moderate bleaching (>10% of colonies) observed in 0 sites
NCRMP_2017_bleaching %>% 
  summarize(sum(PERCENT_BLEACHED>10&PERCENT_BLEACHED<=50))

#Site-level severe bleaching (>50% of colonies) observed at 0 sites
NCRMP_2017_bleaching %>% 
  summarize(sum(PERCENT_BLEACHED>50))

NCRMP_2017_bleaching_plot=
  ggplot() +
  geom_sf (data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(data=NCRMP_2017_bleaching,mapping=aes(x=LON_DEGREES,y=LAT_DEGREES,fill=PERCENT_BLEACHED),color="black",size=3,shape=21)+
  scale_fill_gradientn(colours = myPalette(100), limits=c(1, 75))+
  scale_y_continuous(limits = c(17.80,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-68,-65.15), expand = c(0, 0)) +
  xlab("")+
  ylab("")+
  labs(fill='% Bleached')+
  ggtitle("(e) NCRMP 2017 Coral Bleaching")+
  theme_tufte()+
  theme(text = element_text(size=16, family="sans"),
        plot.title = element_text(size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position="none",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(2, "lines"))

#####NCRMP 2019 Coral Demographics Bleaching####
NCRMP_2019_bleached_corals=read_csv("Data/NCRMP_PR2019_Benthic_Data02_CoralDemographics.csv")%>% 
  filter(BLEACH_CONDITION=="PB"|BLEACH_CONDITION=="T") %>%
  group_by(PRIMARY_SAMPLE_UNIT) %>% 
  summarize(BLEACHED_CORALS=sum(N))

NCRMP_2019_total_corals=read_csv("Data/NCRMP_PR2019_Benthic_Data02_CoralDemographics.csv")%>% 
  filter(BLEACH_CONDITION=="PB"|BLEACH_CONDITION=="P"|BLEACH_CONDITION=="T"|BLEACH_CONDITION=="N") %>%
  group_by(LAT_DEGREES,LON_DEGREES,PRIMARY_SAMPLE_UNIT) %>% 
  summarize(TOTAL_CORALS=sum(N))

NCRMP_2019_bleaching=merge(NCRMP_2019_bleached_corals,NCRMP_2019_total_corals,by="PRIMARY_SAMPLE_UNIT",all.y=TRUE) %>% 
  mutate(PERCENT_BLEACHED=(BLEACHED_CORALS/TOTAL_CORALS)*100) %>% 
  mutate(PERCENT_BLEACHED = ifelse(is.na(PERCENT_BLEACHED), 0, PERCENT_BLEACHED))

#Site-level moderate bleaching (>10% of colonies) observed in 24 sites
NCRMP_2019_bleaching %>% 
  summarize(sum(PERCENT_BLEACHED>10&PERCENT_BLEACHED<=50))

#Site-level severe bleaching (>50% of colonies) observed at 3 sites
NCRMP_2019_bleaching %>% 
  summarize(sum(PERCENT_BLEACHED>50))

NCRMP_2019_bleaching_plot=
  ggplot() +
  geom_sf (data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(data=NCRMP_2019_bleaching,mapping=aes(x=LON_DEGREES,y=LAT_DEGREES,fill=PERCENT_BLEACHED),color="black",size=3,shape=21)+
  scale_fill_gradientn(colours = myPalette(100), limits=c(1, 75))+
  scale_y_continuous(limits = c(17.80,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-68,-65.15), expand = c(0, 0)) +
  xlab("")+
  ylab("")+
  labs(fill='% Bleached')+
  ggtitle("(f) NCRMP 2019 Coral Bleaching")+
  theme_tufte()+
  theme(text = element_text(size=16, family="sans"),
        plot.title = element_text(size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position="none",
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(2, "lines"))

#####NCRMP 2021 Coral Demographics Bleaching####
NCRMP_2021_bleached_corals=read_csv("Data/NCRMP_PR2021_Benthic_Data02_CoralDemographics.csv")%>% 
  filter(BLEACH_CONDITION=="PB"|BLEACH_CONDITION=="T") %>%
  group_by(PRIMARY_SAMPLE_UNIT) %>% 
  summarize(BLEACHED_CORALS=sum(N))

NCRMP_2021_total_corals=read_csv("Data/NCRMP_PR2021_Benthic_Data02_CoralDemographics.csv")%>% 
  filter(BLEACH_CONDITION=="PB"|BLEACH_CONDITION=="P"|BLEACH_CONDITION=="T"|BLEACH_CONDITION=="N") %>%
  group_by(LAT_DEGREES,LON_DEGREES,PRIMARY_SAMPLE_UNIT) %>% 
  summarize(TOTAL_CORALS=sum(N))

NCRMP_2021_bleaching=merge(NCRMP_2021_bleached_corals,NCRMP_2021_total_corals,by="PRIMARY_SAMPLE_UNIT",all.y=TRUE) %>% 
  mutate(PERCENT_BLEACHED=(BLEACHED_CORALS/TOTAL_CORALS)*100) %>% 
  mutate(PERCENT_BLEACHED = ifelse(is.na(PERCENT_BLEACHED), 0, PERCENT_BLEACHED))

#Site-level moderate bleaching (>10% of colonies) observed in 5 sites
NCRMP_2021_bleaching %>% 
  summarize(sum(PERCENT_BLEACHED>10&PERCENT_BLEACHED<=50))

#Site-level severe bleaching (>50% of colonies) observed at 0 sites
NCRMP_2021_bleaching %>% 
  summarize(sum(PERCENT_BLEACHED>50))

NCRMP_2021_bleaching_plot=
  ggplot() +
  geom_sf (data=PRmap,fill = '#A9A9A9', aes (geometry = geometry, fill='white'))+
  geom_point(data=NCRMP_2021_bleaching,mapping=aes(x=LON_DEGREES,y=LAT_DEGREES,fill=PERCENT_BLEACHED),color="black",size=3,shape=21)+
  scale_fill_gradientn(colours = myPalette(100), limits=c(1, 75))+
  scale_y_continuous(limits = c(17.80,18.6), expand = c(0, 0)) +
  scale_x_continuous(limits = c(-68,-65.15), expand = c(0, 0)) +
  xlab("")+
  ylab("")+
  labs(fill='% Bleached')+
  ggtitle("(g) NCRMP 2021 Coral Bleaching")+
  theme_tufte()+
  theme(text = element_text(size=16, family="sans"),
        plot.title = element_text(size=20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        legend.position="bottom",
        legend.key.height= unit(1, 'cm'),
        legend.key.width= unit(2, 'cm'),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.background = element_blank(),
        panel.spacing = unit(2, "lines"))

####Combine NCRMP Coral Bleaching Plots####

Figure4=Figure4a/Figure4b/
  (NCRMP_2014_bleaching_plot+NCRMP_2016_bleaching_plot)/
  (NCRMP_2017_bleaching_plot+NCRMP_2019_bleaching_plot)/
  (NCRMP_2021_bleaching_plot+guide_area() + plot_layout(guides = 'collect')) +
  plot_layout(heights = unit(c(10,3,5,5,5), c('cm', 'cm')))

ggsave("Figure4.tiff",Figure4,height=14,width=14)
