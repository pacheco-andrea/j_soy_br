# mapping soy exports in brazil


# libraries
library(dplyr)
library(tidyr)
library(sf)
library(ggplot2)
# install.packages("fuzzyjoin")
library(fuzzyjoin)
library(cowplot)

# dir
wdmain <- "/Users/sellare/Dropbox/j_soy_br"
setwd(wdmain)

# get data ----
trase <- read.csv("BRAZIL_SOY_2.5.1_pc/BRAZIL_SOY_2.5.1_pc.csv")
head(trase)

gadm <- readRDS("gadm36_BRA_2_sf.rds")
head(gadm)
plot(gadm$geometry)
nrow(gadm)

# clean/prep trase data ----
unique(trase$COUNTRY)
# subset of data that's brazil's exports to germany
br2ger <- trase[which(trase$COUNTRY == "CHINA (MAINLAND)"),] 
length(unique(br2ger$MUNICIPALITY)) 
br2ger <- tidyr::unite(br2ger, col=st_mun, c("STATE", "MUNICIPALITY"), sep="_", remove=F) # make sure to create this variable, or you double count munis
length(unique(br2ger$st_mun))

# subset data brazil's exports to EU
br2eu <- trase[which(trase$ECONOMIC.BLOC == "BRAZIL"),] 
length(unique(br2eu$MUNICIPALITY)) 
br2eu <- tidyr::unite(br2eu, col=st_mun, c("STATE", "MUNICIPALITY"), sep="_", remove=F)
length(unique(br2eu$st_mun))

# "collapse" year data
ger_soy_allyrs <- aggregate(br2ger["SOY_EQUIVALENT_TONNES"], by=br2ger["st_mun"], sum)
names(ger_soy_allyrs)[2] <- "g_soy"
eu_soy_allyrs <- aggregate(br2eu["SOY_EQUIVALENT_TONNES"], by=br2eu["st_mun"], sum)
names(eu_soy_allyrs)[2] <- "eu_soy"
# check that all original st_mun are the kept the same
summary(sort(unique(ger_soy_allyrs$st_mun)) == sort(unique(br2ger$st_mun)))
summary(sort(unique(eu_soy_allyrs$st_mun)) == sort(unique(br2eu$st_mun)))


# join spatial and soy data ----
# create st_mun variable in spatial data
# first need to coerce to upper and remove accents
br <- gadm[,c(4,7)]
colnames(br) <- c("state", "mun", "geometry")
br$mun <- iconv(toupper(br$mun), from="UTF-8", to="ASCII//TRANSLIT")
br$state <- iconv(toupper(br$state), from="UTF-8", to="ASCII//TRANSLIT")
# create st_mun variable
br <- tidyr::unite(br, col=st_mun, c("state", "mun"), sep="_", remove=F)
# i realized there were municipalities that were duplicated
# because they originally had different accents
plot(br[duplicated(br$st_mun),]$geometry)
# and i will need to combine these 
dupes <- br[duplicated(br$st_mun),]

for(i in 1:nrow(dupes))
{
  dupe_name <- as.character(dupes[i,1])[1]
  dupe_position <- grep(paste0(dupe_name), br$st_mun)
  plot(br[dupe_position,]$geometry)
  single_sf <- st_union(br[dupe_position,])
  plot(single_sf)
  # replace in sf data frame
  new_sf <- br[dupe_position,][1,]
  new_sf$geometry <- st_as_sf(single_sf)$x
  br <- br[-dupe_position,]
  br <- rbind(br, new_sf)
}

# check again for duplicates
br[duplicated(br$st_mun),] # no duplicates
length(unique(br$st_mun)) # == 5495 municipalities 

# use left join 
data <- left_join(br, ger_soy_allyrs, by="st_mun")
nrow(data)
data <- left_join(data, eu_soy_allyrs, by="st_mun")
nrow(data)
head(data)
summary(data)

# check missing amounts
sum(ger_soy_allyrs$g_soy, na.rm=T)
sum(data$g_soy, na.rm = T) # im able to identify 75% of total soy sent to germany

sum(eu_soy_allyrs$eu_soy, na.rm=T)
sum(data$eu_soy, na.rm = T) # im able to identify 78% of total soy sent to eu

# map 1: amount of exported soy exported to germany (per municipality)
plot(data["g_soy"], lwd = .01)

gmap <- ggplot(data)+
  geom_sf(aes(fill=log(g_soy)), col="transparent")+ #  add size if want municipality outlines (size=.01)
  scale_fill_viridis_c("ln (soy equivalent tonnes)",na.value = "gray80")+
  labs(title="Germany")+
  theme_map()
gmap
# map 2: amount of exported soy exported to eu (per municipality)

eumap <- ggplot(data)+
  geom_sf(aes(fill=log(eu_soy)), col="transparent")+
  scale_fill_viridis_c("ln (soy equivalent tonnes)", na.value = "gray80")+
  labs(title="European Union")+
  theme_map()
eumap

# map together
plot_grid(gmap, eumap)

# por alguna razón esto no me esta funcionando en mi compu. tal vez sirve en la tuya!
png(file="mysoymaps_ch_br.png", width = 1000, height = 500, units="px", res = 300)
plot_grid(gmap, eumap)
dev.off()


# de bug notes:



# # tried using match function to identify matches in st_mun variable across datasets
# br[is.na(match(ger_soy_allyrs$st_mun, br$st_mun)),]
# anti_join(ger_soy_allyrs, br, by= "st_mun")
# 
# m <- stringdist::amatch(ger_soy_allyrs$st_mun, br$st_mun, method="jw", maxDist=0.1)
# 
# br[is.na(br$st_mun[m]),]
# test <- br$st_mun[m] == ger_soy_allyrs$st_mun
# f <- grep("FALSE", test)
# length(f)
# br$st_mun[m][f] # munis that did not match
# ger_soy_allyrs$st_mun[f] 
# # i can see these are basically the same, so i could simply replace 
# ger_soy_allyrs$st_mun[f]  <- br$st_mun[m][f]
# br$soy <- NA
# br[m,]$soy <- ger_soy_allyrs$SOY_EQUIVALENT_TONNES

# # also tried using a fuzzy match within the join function but it didn't really work
# # use fuzzy join
# test3 <- stringdist_left_join(br, ger_soy_allyrs, by="st_mun", method="jw", max_dist=.01)
# nrow(test3)
# sum(test3$SOY_EQUIVALENT_TONNES, na.rm = T)
# length(unique(test3$st_mun.x)) 
# length(unique(test3$st_mun.y)) # NOTE: it's clear that the TRASE data has ~100 municipalities I couldn't find in the GADM data

