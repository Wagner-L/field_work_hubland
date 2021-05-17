
library(RStoolbox)
library(sp)
library(tidyverse)
library(rgdal)



sat <- raster("raster/20210509_planet_data_wue.tif", layer=4)
sat

samples <- readOGR(paste0(getwd(),"/field_samples_hubland_all.shp"))
samples$name

plot(sat)
plot(samples, add=T)
View(as.data.frame(samples))



unique(samples$name)


###all samples, all classes
classification_rf<- superClass(sat, trainData=samples,
                 responseCol = "name",
                 model = "rf",
                 trainPartition = 0.7,
                 mode="classification",
                 predict=TRUE,
                 filename=paste0(getwd(), "classification"),
                 overwrite=TRUE,
                 verbose=TRUE)



classification_rf$validation$performance$byClass[,11]
print(paste0("Overall Accuray: ",classification_rf$validation$performance$overall[1]))

a <- ggR(classification_rf$map, geom_raster=T)+
  labs(title = "All sampled points",
       subtitle = "all classes",
      caption = paste0("Overall Accuray: ",round(classification$validation$performance$overall[1], digits=2)))

a


###combined vegetation class (agri, tree, vine, grass)
samples_veg <- samples 
samples_veg@data$name[samples@data$name == "TREE" | 
                  samples@data$name == "VINE" | 
                  samples@data$name == "GRASS" |
                  samples@data$name == "AGRI"] <- "VEG"


classification_rf_veg <- superClass(sat, trainData=samples_veg,
                             responseCol = "name",
                             model = "rf",
                             trainPartition = 0.7,
                             mode="classification",
                             predict=TRUE,
                             filename=paste0(getwd(), "classification"),
                             overwrite=TRUE,
                             verbose=TRUE)
 
print(paste0("Overall Accuray: ",classification_rf_veg$validation$performance$overall[1]))
classification_rf_veg$validation$performance$byClass[,11]
b <- ggR(classification_rf_veg$map, geom_raster=T)+
  labs(title = "All sampled points",
       subtitle = "vegetation in combined class",
       caption = paste0("Overall Accuray: ",round(classification_rf_veg$validation$performance$overall[1], digits=2)))

b

####combined with 4th generation data
combined_samples <- readOGR(paste0(getwd(),"/field_samples_hubland_all_with_4gen.shp"))
combined_samples

plot(sat)
plot(combined_samples, add=T)

classification_rf_combined <- superClass(sat, trainData=combined_samples,
                                    responseCol = "name",
                                    model = "rf",
                                    trainPartition = 0.7,
                                    mode="classification",
                                    predict=TRUE,
                                    filename=paste0(getwd(), "classification"),
                                    overwrite=TRUE,
                                    verbose=TRUE)

print(paste0("Overall Accuray: ",classification_rf_combined$validation$performance$overall[1]))
classification_rf_combined$validation$performance$byClass[,11]

c <- ggR(classification_rf_combined$map, geom_raster=T)+
  labs(title = "Combined with 4th generation samples",
       subtitle = "all classes",
       caption = paste0("Overall Accuray: ",round(classification_rf_combined$validation$performance$overall[1], digits=2)))


c
### combined with 4 gen data and combined veg class

combined_samples_veg <-  combined_samples
combined_samples_veg@data$name[combined_samples@data$name == "TREE" | 
                             combined_samples@data$name == "VINE" | 
                             combined_samples@data$name == "GRASS" |
                             combined_samples@data$name == "AGRI"] <- "VEG"


classification_rf_combined_veg <- superClass(sat, trainData=combined_samples_veg,
                                         responseCol = "name",
                                         model = "rf",
                                         trainPartition = 0.7,
                                         mode="classification",
                                         predict=TRUE,
                                         filename=paste0(getwd(), "classification"),
                                         overwrite=TRUE,
                                         verbose=TRUE)

print(paste0("Overall Accuray: ",classification_rf_combined_veg$validation$performance$overall[1]))
classification_rf_combined_veg$validation$performance$byClass[,11]

d <- ggR(classification_rf_combined_veg$map, geom_raster=T)+
  labs(title = "Combined with 4th generation samples",
       subtitle = "vegetation in combined class",
       caption = paste0("Overall Accuray: ",round(classification_rf_combined_veg$validation$performance$overall[1], digits=2)))

d


### stack figures
figure <- ggarrange(a, b, c,d,
                    ncol = 2, nrow = 2)
figure

