#Fry analysis of gold occurrences
#read in shapefile of gold occurrences, the x and y of the gold occurrence have been computed as a attribute in a GIS
library(rgdal)
library(sp)
library(sf)

au_locs<-  st_read("C:/Users/user/OneDrive/Documents/Git/Fry/Gold_occurrences.shp")


str(au_locs)

head(au_locs)


x<- au_locs$x
y <- au_locs$y


data <- data.frame(x,y)
head(data)


result_x <- c()
result_y <- c()


# Loop through each record in x and y to perform the subtraction using each location as the centre point
for (i in 1:nrow(data)) {
  for (j in 1:nrow(data)) {
    diff_x <- data$x[i] - data$x[j]
    diff_y <- data$y[i] - data$y[j]
      if (!(diff_x == 0 && diff_y == 0)) {
      result_x <- c(result_x, diff_x)
      result_y <- c(result_y, diff_y)
    }
 }
}



#centring the fry points on the original gold occurrences
fry_pts <- data.frame(x = result_x+mean(data$x), y = result_y+mean(data$y))


#convert fry points to a spatial feature
fry_pts_sp <- st_as_sf(fry_pts, coords = c("x", "y"))

#assign coordinate system
st_crs(fry_pts_sp) <- st_crs(au_locs)

str(fry_pts_sp)


#plot the original gold occurrences and the fry points
ggplot()+
  theme_bw()+
  theme(panel.grid.major = element_blank())+
  geom_sf (data=au_locs, aes(col="black"))+
  geom_sf(data=fry_pts_sp, aes(col="red"))+
  scale_color_identity(labels=c(black="Gold Occurrences", red="Fry Points"), guide="legend")+
   theme(plot.title = element_text(hjust= 0.5))+
  theme(axis.text.y = element_text(angle = 88))+
  ggtitle ("Fry Plot")+
  coord_sf()

