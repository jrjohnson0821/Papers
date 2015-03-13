## maps.r
## 3/8/15 BCS
## Create maps of state COLR features using ggplot2

rm(list=ls())

require(maptools)
require(mapproj)
require(rgeos)
require(rgdal)
require(RColorBrewer)
require(ggplot2)
require(foreign)
require(plyr)
require(dplyr)
require(grid)   # unit function

# for theme_map
devtools::source_gist("33baa3a79c5cfef0f6df")

# setup map data
## read U.S. states shapefile
us <- readOGR(dsn="data//state_shapefiles", layer="cb_2013_us_state_20m")

## convert it to Albers equal area
us_aea <- spTransform(us, CRS("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"))
us_aea@data$id <- rownames(us_aea@data)

## extract, then rotate, shrink & move alaska (and reset projection)
### need to use state IDs via # https://www.census.gov/geo/reference/ansi_statetables.html
alaska <- us_aea[us_aea$STATEFP=="02",]
alaska <- elide(alaska, rotate=-50)
alaska <- elide(alaska, scale=max(apply(bbox(alaska), 1, diff)) / 2.3)
alaska <- elide(alaska, shift=c(-2100000, -2500000))
proj4string(alaska) <- proj4string(us_aea)

## extract, then rotate & shift hawaii
hawaii <- us_aea[us_aea$STATEFP=="15",]
hawaii <- elide(hawaii, rotate=-35)
hawaii <- elide(hawaii, shift=c(5400000, -1400000))
proj4string(hawaii) <- proj4string(us_aea)

## remove old states and put new ones back in; note the different order
## we're also removing puerto rico in this example but you can move it
## between texas and florida via similar methods to the ones we just used
us_aea <- us_aea[!us_aea$STATEFP %in% c("02", "15", "72"),]
us_aea <- rbind(us_aea, alaska, hawaii)

## get ready for ggplotting it... this takes a cpl seconds
map <- fortify(us_aea, region="GEOID")



# import state data
data <- read.dta("data//map_data.dta") %>%
  tbl_df() %>%
  mutate(colr_term=factor(colr_term, levels=c(6,7,8,10,12,14,20,21),
                          labels=c("6 years","7 years","8 years","10 years",
                                   "12 years","14 years","Until Age 70","Life Tenure")),
         iac=factor(iac, levels=rev(levels(iac))),
         sco_elect=ifelse(colr_retain=="Partisan election"|colr_retain=="Nonpartisan election"|
                            colr_retain=="Retention election","Elected","Appointed")) %>%
  filter(state!="Oklahoma (criminal)" & state!="Texas (criminal)") %>%
  mutate(state=ifelse(state=="Oklahoma (civil)","Oklahoma",state),
         state=ifelse(state=="Texas (civil)","Texas",state)) %>%
  mutate(statefips=formatC(statefips, width=2, flag=0)) %>%
  # generate simplified measure of colr_retain
  mutate(colr_retain_simp=as.character(colr_retain),
         colr_retain_simp=ifelse(colr_retain!="Partisan election" & colr_retain!="Nonpartisan election" &
                                   colr_retain!="Retention election","N/A",colr_retain_simp),
         colr_retain_simp=ifelse(state=="Connecticut"|state=="Delaware"|state=="Maine"|
                                   state=="New Jersey"|state=="New York","Appointment",colr_retain_simp),
         # exclude retention states not in the ACS data
         colr_retain_simp=ifelse(state=="Arizona"|state=="Colorado"|state=="Indiana"|state=="Iowa"|
                                   state=="Kansas"|state=="Maryland"|state=="Missouri"|
                                   state=="Nebraska"|state=="Oklahoma"|state=="South Dakota"|
                                   state==" Tennessee"|state=="Utah"|state=="Wyoming","N/A",colr_retain_simp),
         colr_retain_simp=factor(colr_retain_simp, levels=c("Partisan election","Nonpartisan election",
                                                            "Retention election","Appointment","N/A")))%>%
  mutate(state=tolower(state)) %>%
  rename(region=state)



# set colors for categories
col <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# plot colr_select
colr.select.p <- ggplot() +
  geom_map(data=map, map=map,
           aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_map(data=data, map=map, aes(map_id=statefips, fill=colr_select),
                    color="#0e0e0e", size=0.15) +
  scale_fill_manual(values=col, guide = guide_legend(nrow=2)) +
  coord_equal() +
  labs(title=NULL, fill="Initial Selection\nMethod") +
  theme_bw(base_size = 24) +
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        panel.margin=unit(0, "lines")) 
colr.select.p
ggsave("graphics//colr_select.pdf", colr.select.p, scale=1)


#plot colr_retain
colr.retain.p <- ggplot() +
  geom_map(data=map, map=map,
           aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_map(data=data, map=map, aes(map_id=statefips, fill=colr_retain),
           color="#0e0e0e", size=0.15) +
  scale_fill_manual(values=col, guide = guide_legend(nrow=2)) +
  coord_equal() +
  labs(title=NULL, fill="Retention\nMethod") +
  theme_bw(base_size = 24) +
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        panel.margin=unit(0, "lines")) 
colr.retain.p
ggsave("graphics//colr_retain.pdf", colr.retain.p, scale=1)

#plot colr_retain_simp
colr.sample.p <- ggplot() +
  geom_map(data=map, map=map,
           aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_map(data=data, map=map, aes(map_id=statefips, fill=colr_retain_simp),
           color="#0e0e0e", size=0.15) +
  scale_fill_manual(breaks=c("Partisan election","Nonpartisan election",
                             "Retention election","Appointment"),
                    values=c(col[1:4],"white"), guide = guide_legend(nrow=2)) +
  coord_equal() +
  labs(title=NULL, fill="Retention\nMethod") +
  theme_bw(base_size = 24) +
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        panel.margin=unit(0, "lines")) 
colr.sample.p
ggsave("graphics//colr_retain_sample.pdf", colr.sample.p, scale=1)


# plot sco_elect
sco.elect.p <- ggplot() +
  geom_map(data=map, map=map,
           aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_map(data=data, map=map, aes(map_id=statefips, fill=sco_elect),
           color="#0e0e0e", size=0.15) +
  scale_fill_manual(values=col) +
  coord_equal() +
  labs(title=NULL, fill="Selection Method") +
  theme_bw(base_size = 24) +
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        panel.margin=unit(0, "lines"),
        plot.margin=unit(c(0,0,0,0), "cm")) 
sco.elect.p
ggsave("graphics//sco_elect.pdf", sco.elect.p, scale=1)


#plot colr_term
colr.term.p <- ggplot() +
  geom_map(data=map, map=map,
           aes(x=long, y=lat, map_id=id, group=group),
           fill="#ffffff", color="#0e0e0e", size=0.15) +
  geom_map(data=data, map=map, aes(map_id=statefips, fill=colr_term),
           color="#0e0e0e", size=0.15) +
  coord_equal() +
  scale_fill_brewer(palette="YlOrRd", guide = guide_legend(nrow=2)) +
  labs(title=NULL, fill="Term\nLength") +
  theme_bw(base_size = 24) +
  theme(legend.position="bottom",
        axis.line=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid=element_blank(),
        panel.margin=unit(0, "lines")) 
colr.term.p
ggsave("graphics//colr_term.pdf", colr.term.p, scale=1)



