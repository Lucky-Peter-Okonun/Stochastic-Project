setwd("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets")



install.packages("tidyverse")
install.packages("haven")
install.packages("maptools")
install.packages("raster")
install.packages("ggplot2")
install.packages("rgdal")

library(tidyverse)
library(haven)    ##For reading STATA files into R
library(maptools)
library(raster)
library(ggplot2)
library(rgdal)


                      
##Question(1a)
kenya.children = read_dta("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets/childrenfinal.dta")
##Expung variables starting with s,v,m with some digits attached
kenya.children = kenya.children %>% 
dplyr::select(-matches("^[svm][0-9]"))

##Change data type
kenya.children = kenya.children %>%
  mutate_if(is.double, as.double)

##Question(1b)
##Create tibble of selected variables
kenya.children = kenya.children %>%
  dplyr::select(c(hypage, ruralfacto, female, zstunt, zweight, zwast, adm2))

#Graph of Zstunt Against Hypage
graph1 = ggplot(kenya.children)+
 geom_point( aes(x = hypage, y = zstunt)) +
  geom_smooth( (aes(x = hypage, y = zstunt)),se = FALSE, color = "green")+
 ggtitle("Graph of Zstunt Against Hypage")+
  xlab("Hypage") +
  ylab("Zstunt")
graph1

#Graph of Zstunt against Hypage For Males and Females
graph2 = ggplot(kenya.children, aes(x = hypage, y = zstunt,  colour = factor(female))) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE) +
  scale_colour_manual(labels = c("male", "female"), values = c("blue2", "green")) +
  theme_classic(base_size = 14) + 
  theme(legend.key = element_rect(fill = "black", colour = "purple")) + 
  guides(colour = guide_legend(name="Sex"))+
  ggtitle("Graph of Zstunt against Hypage For Males and Females") +
  xlab("Hypage") +
  ylab("Zstunt")
graph2

###Grapgh of Zstunt against Age For Urban and Rural Children
graph3 = ggplot(kenya.children , aes(x = hypage, y = zstunt,  colour = factor(ruralfacto))) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE) +
  scale_colour_manual(labels = c("urban", "rural"), values = c("yellow", "red")) +
  theme_classic(base_size = 14) + 
  theme(legend.key = element_rect(fill = "black", colour = "purple")) + 
  guides(colour = guide_legend(name="Range"))+
   ggtitle("Grapgh of Zstunt against Age For Urban and Rural Children") +
  xlab("Hypage") +
  ylab("Zstunt")
graph3

##Question(1c)
#Creating the Kenya Map
Kenya1 = getData("GADM", country="KE", level=1) 
Kenya1_UTM = spTransform(Kenya1, CRS("+init=epsg:32737")) # setting an appropriate projection 
colnames(kenya.children)[7] = "NAME_1"   #To rename the adm column to suit what the kenyan map looks 

##Arranging the County alphabetically
kenya.children = kenya.children[order(kenya.children$NAME_1),]
Kenya1_UTM@data = Kenya1_UTM@data[order(Kenya1_UTM@data$NAME_1),]

#Summarizing the dataset by mean of each county
##detach(package:plyr)
kenya.children = kenya.children %>%
  group_by(NAME_1) %>%
  summarize(mean = mean(zstunt), n = n())

#Adding missing county Isiolo
kenya.children[nrow(kenya.children) + 1,] = NA
kenya.children$NAME_1[47] = "Isiolo"

#We need a dataframe for ggplot 
Kenya1_UTM@data$id = rownames(Kenya1_UTM@data)
Kenya1_UTM@data = mutate(Kenya1_UTM@data, zstunt.mean= kenya.children$mean)
Kenya1_df = fortify(Kenya1_UTM)
Kenya1_df = full_join(Kenya1_df,Kenya1_UTM@data, by="id")

##In the order listed at Kenya1_UTM@data, we need the centroids of each county
centroids_df = as.data.frame(coordinates(Kenya1_UTM))
names(centroids_df) = c("long", "lat")
kenya.children = kenya.children[order(kenya.children$NAME_1),]
centroids_df$NAME_1 = Kenya1_UTM@data$NAME_1
centroids_df$zstunt.mean = kenya.children$mean

##Generating the map with all the counties and their Zstunt mean
ggplot(data = Kenya1_df, aes(x = long, y = lat, group = group, fill = zstunt.mean)) + 
  geom_polygon(color = "black", size = 0.25) +
  geom_text(data = centroids_df, aes(x = long, y = lat, label = NAME_1, group = NULL), size = 3) +
  scale_fill_distiller(name="Zstunt mean \nKenyan Counties", palette = "Spectral") +
  ggtitle("Generating the map with all the counties and their Zstunt mean")+
  theme(aspect.ratio = 1)

#Tibble to text file 
##(1d)write the tibble from (b) into a text file
write.table(kenya.children,"kenya.children.txt")
file = ("C:/Users/peter/Desktop/Stochasticlabcourse2Datasets\\kenya.children.csv")








