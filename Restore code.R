rm(list=ls())
install.packages("circlize")
library(geojsonsf)
library(sf)
library(ggplot2)
library(RColorBrewer)
library(ggspatial)
library(cowplot)
library(tidyverse)
library(scales)
library(ggsci)
library(ggforce)
library(reshape2)
library(dplyr)
library(readxl)
library(ggpubr)
library(vegan)
library(ggpmisc)
library(circlize)

#Fig. 1
#read map data
setwd("D:/Download/Reproduction data")
API_pre = "http://xzqh.mca.gov.cn/data/" # (The source data here is publicly available)
China = st_read(dsn = paste0(API_pre, "quanguo.json"), stringsAsFactors=FALSE) 
st_crs(China) = 4326
China_line = st_read(dsn = paste0(API_pre, "quanguo_Line.geojson"), stringsAsFactors=FALSE) 
st_crs(China_line) = 4326
gjx <- China_line[China_line$QUHUADAIMA == "guojiexian",]
province <- read.delim("province.txt", row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)
#read map colour
colour <- read.csv("colour.csv")
#read minimap data
nine_lines = read_sf('nanhai.geojson') 
#define colour
colour$new_colour <- rep(0,nrow(colour))
colour$new_colour[which(colour$shengfen=="Jiangsu")] <- 1
colour$new_colour[which(colour$shengfen=="Guangdong")] <- 1
colour$new_colour[which(colour$shengfen=="Guangxi")] <- 1
colour$new_colour[which(colour$shengfen=="Liaoning")] <- 1
colour$new_colour[which(colour$shengfen=="Zhejiang")] <- 1
colour$new_colour[which(colour$shengfen=="Fujian")] <- 1
colour$new_colour[which(colour$shengfen=="Shandong")] <- 1
colour$new_colour[which(colour$shengfen=="Hainan")] <- 1
colour$QUHUADAIMA <- as.character(colour$QUHUADAIMA)
China <- dplyr::left_join(China,colour,by= "QUHUADAIMA")

#China map
map <- ggplot() +
  geom_sf(data = China, aes(fill = factor(new_colour)), linewidth = 1, color = "black") +
  geom_sf(data = gjx, color = "black", linewidth = 1) +
  scale_fill_manual(values = c("white", "lightgray")) +
  theme_test() +
  theme(
    legend.position = "none",
    legend.key = element_blank(),
    axis.line = element_line(color = "white", linewidth = 0.3),
    aspect.ratio = 0.95,
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "white", linetype = 1, linewidth = 0.3)
  )

#minimap
nine_map <- ggplot() +
  geom_sf(data = China,fill='NA', linewidth=0.1) + 
  geom_sf(data = nine_lines,color='#3C3C3C',linewidth=0.1)+
  coord_sf(ylim = c(-4028017,-1877844),xlim = c(117131.4,2115095),crs="+proj=laea +lat_0=40 +lon_0=104")+
  theme(aspect.ratio = 1.25, 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill=NA,color="grey10",linetype=1,linewidth=0.3),
        plot.margin=unit(c(0,0,0,0),"mm"))

#combine two map
ggdraw()+draw_plot(map)+draw_plot(nine_map, x = 0.8, y = 0.17, width = 0.1, height = 0.15)

HN = read_sf('hainan.json') 
GD = read_sf('guangdong.json') 
SD = read_sf('shandong.json')
ZJ = read_sf('zhejiang.json') 
JS = read_sf('jiangsu.json') 
LN = read_sf('liaoning.json') 
GX = read_sf('guangxi.json') 
FJ = read_sf('fujian.json') 
mydata <- read.delim('Fig.1a.txt', row.names = 1, sep = '\t', stringsAsFactors = FALSE, check.names = FALSE)

prov_labels <- data.frame(
  label = c("HN", "GD", "GX", "FJ", "ZJ", "JS", "SD", "LN"),
  Longitude = c(110, 113, 108, 119, 120, 120, 119, 123),
  Latitude = c(19, 23, 23, 25, 29, 32, 37, 41)
)

ggplot() +
  geom_sf(data = HN,color='#363636',fill="#FFFFFF",linewidth=1)+
  geom_sf(data = JS,color='#363636',fill="#FFFFFF",linewidth=1)+
  geom_sf(data = ZJ,color='#363636',fill="#FFFFFF",linewidth=1)+
  geom_sf(data = LN,color='#363636',fill="#FFFFFF",linewidth=1)+
  geom_sf(data = SD,color='#363636',fill="#FFFFFF",linewidth=1)+
  geom_sf(data = GX,color='#363636',fill="#FFFFFF",linewidth=1)+
  geom_sf(data = GD,color='#363636',fill="#FFFFFF",linewidth=1)+
  geom_sf(data = FJ,color='#363636',fill="#FFFFFF",linewidth=1)+
  
  geom_text(data = prov_labels, aes(x = Longitude, y = Latitude, label = label), 
            size = 4, fontface = "bold", color = "black") +
  
  xlim(95,127)+ylim(18,45)+ 
  theme_bw()+
  labs(x="",y="")+
  annotation_scale(location = "bl",text_cex = 0.5,height = unit(0.12, "cm"),line_width = 0.5)+
  theme(
    aspect.ratio = 1.33,
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    panel.border = element_rect(fill = NA, color = "grey10", linetype = 1, linewidth = 0.4),
    plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
    axis.ticks = element_line(linewidth = 0.3),
    axis.ticks.length = unit(0.05, "cm"),
    axis.text.x = element_text(size = 7, colour = "black"),
    axis.text.y = element_text(size = 7, colour = "black"),
    plot.background = element_rect(fill = "white", color = "white", linewidth = 0.4)
  ) +
  geom_point(data = mydata, aes(x = Longitude, y = Latitude), 
             color = "#FB8B40", size = 0.7, shape = 15)


#Fig. 2
data<-read_excel("Fig.1d.xlsx")
data$tax=factor(data$tax,levels=c("Proteobacteria","Bacteroidetes","Chloroflexi","Acidobacteria","Actinobacteria",
                                  "Gemmatimonadetes","Planctomycetes","Verrucomicrobia","Nitrospirae","Cyanobacteria",
                                  "Other bacteria","Thaumarchaeota","Bathyarchaeota","Euryarchaeota","Thermoplasmatota",
                                  "Woesearchaeota","Altiarchaeota","Heimdallarchaeota","Kariarchaeota","Other archaea"))
data$class=factor(data$class, levels=c("Bacteria",'Archaea'))
ggplot(data, aes(x=class, y=proportion))+
  geom_bar(stat="identity", color="#696969",width=0.48,linewidth=0.13,position="stack", aes(fill=tax))+
  scale_fill_manual(breaks = c("Proteobacteria","Bacteroidetes","Chloroflexi","Acidobacteria","Actinobacteria",
                               "Gemmatimonadetes","Planctomycetes","Verrucomicrobia","Nitrospirae","Cyanobacteria",
                               "Other bacteria","Thaumarchaeota","Bathyarchaeota","Euryarchaeota","Thermoplasmatota",
                               "Woesearchaeota","Altiarchaeota","Heimdallarchaeota","Kariarchaeota","Other archaea"),
                    values = c('#518DBE','#D79D65',"#63C28D",'#A155E8','#F1BD3E',
                               '#F18282',"#EDD9B4","#ECCEED",'#C0D0E5','#85D7BB',
                               "#CBCBCB",'#7DBDF1','#F1AE6D','#96D796',"#BC9BFF",
                               "#FFDF33",'#FB998E','#FCEBD0',"#F9E8F9",'#DCDCDC'))+
  labs(x="",y="Relative proportion (%)")+
  theme(legend.background=element_rect(color="white"),legend.key.size = unit(15, "pt"))+
  theme(legend.key = element_blank())+
  theme(axis.line = element_line(color=))+
  theme_test()+  
  theme(panel.grid.major=element_blank(),panel.grid.minor = element_blank(),
        axis.title = element_text(size=6.5),
        axis.ticks = element_line(size = 0.23),
        axis.ticks.length = unit(0.04, "cm"),
        axis.text.x = element_text(hjust =0.5,size=6,colour = 'black'),
        axis.text.y=element_text(size=6,colour = 'black'),
        panel.border = element_rect(size=0.35))+
  #legend
  theme(legend.title = element_blank(),legend.key.height = unit(0.2, "cm"), 
        legend.key.width = unit(0.2, "cm"),
        legend.text = element_text(size = 5))

#Fig. 3
data<-read_excel("Fig.2.xlsx") 
data$score=factor(data$score, levels=c('Low confidence','High confidence'))
data$gene=factor(data$gene, levels=c('Anaerobic oxidation of methane','Aerobic oxidation of methane',
                                     'Nitrification','Organic degradation and synthesis','Sulphur oxidation',
                                     'Dissimilatory sulphate reduction','Assimilatory sulphate reduction',
                                     'Phosphate transport system','Oxidative phosphorylation',
                                     'Organic phosphoester hydrolysis','Pentose phosphate pathway',
                                     'Phosphate starvation induction',
                                     'Ester degradation','Polysaccharide degradation'))
ggplot(data)+
  geom_bar(stat="identity",width=0.75, position="dodge", aes(x=gene, y=value,fill=class),alpha=1)+
  scale_fill_manual(values = c("#95D1D7","#6BB5FF","#FF9966","#FFD966","#AAAAD5"))+
  theme(panel.border = element_blank())+
  labs(x="",y="# of viral genes",color="Type")+
  theme(axis.line = element_line(color="black"))+coord_flip()+
  theme_test()+theme( axis.title.x = element_text(size=5.5),
                      axis.title.y = element_text(size=5.5),
                      axis.text.x = element_text(hjust =0.5,size=5,colour = 'black'),
                      axis.text.y=element_text(size=5,colour = 'black'),
                      axis.ticks = element_line(size = 0.25),
                      axis.ticks.length = unit(0.04, "cm"),
                      strip.text=element_text(size=5.5, margin = margin(2, 2, 2, 2)),
                      strip.background = element_rect(color = "black", size = 0.35),
                      panel.border = element_rect(size=0.4))+
  theme(legend.position='none')+ylim(0,50)+facet_wrap(~score)       

#Fig. 4
df <- read.csv("Fig.4d.csv", header=TRUE,stringsAsFactors = FALSE,check.names = FALSE)
df_melt<-melt(df,id.vars = 'Region')
colnames(df_melt)<-c('from','to','value')
df_melt$to<-as.character(df_melt$to)

#define factor
df_sum<-apply(df[,2:ncol(df)],2,sum)+apply(df[,2:ncol(df)],1,sum)
order<-sort(df_sum,index.return=TRUE,decreasing =TRUE)
df_melt$from<-factor(df_melt$from,levels=df$Region[order$ix],order=TRUE)

df_melt<-dplyr:: arrange (df_melt, from)

#define color
mycolor = c(Intertidal_zone= "#02C874", Marine = "#C4A8FF", Soil = "#F7BF90",
            Human = "#FBEA50", Freshwater = "#4D94F0", RefSeq = "gray")
names(mycolor) <-df$Region

#circos plot
circos.clear()
circos.par(start.degree = 90, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(mar = rep(0, 4))

chordDiagram(
  x = df_melt,order = c("Intertidal_zone","Marine","Soil","Human","Freshwater","RefSeq"),
  grid.col = mycolor,
  transparency = 0.6,
  directional = 1,
  direction.type = c("arrows", "diffHeight"),
  diffHeight = -0.04,
  annotationTrack = "grid",
  annotationTrackHeight = c(0.05, 0.1),
  link.arr.type = "big.arrow",
  link.sort = TRUE,
  link.largest.ontop = TRUE)

#circos add labels
circos.trackPlotRegion(
  track.index = 1,
  bg.border = NA,
  panel.fun = function(x, y) {
    xlim = get.cell.meta.data("xlim")
    sector.index = get.cell.meta.data("sector.index")
    circos.text(
      x = mean(xlim),
      y = 3.5,
      labels = sector.index,
      facing = "bending",
      cex = 1.2
    )
    circos.axis(
      h = "top",
      major.at = c(0,5,10,15,20,25,30,35,40),
      minor.ticks = 1,
      major.tick.length = 0.5,
      labels.niceFacing = FALSE)
  }
)