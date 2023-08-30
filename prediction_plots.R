library(dplyr)
library(sf)
library(terra)
library(ggplot2)
library(tidyterra)
library(raster)
library(performance)
library(patchwork)
library(stringr)

# 1.a Read result csv----
DL_pred <- read.csv("./data/results2500imgs_p_spec_32batch_6epoch.csv", header =  T)
DL_pred$SPECIES.NAME <- str_sub(DL_pred$SPECIES.NAME, start = 3, end=-2)
DL_pred$prediction <- str_sub(DL_pred$prediction, start = 3, end=-2)

species <- unique(DL_pred$SPECIES.NAME)

# 1.b Read boundary data----
coastline <- st_read("./data/Europe_coastline/Europe_coastline_poly.shp")
st_transform(coastline, crs = 3035)
#plot(coastline, col = "red")
#st_geometry(coastline)
#st_area(coastline)
coastline$area <- st_area(coastline)

# simplifcation: drop african polygon and islands
coastline.rough <- coastline[-1471,] # drop 2nd largest Polygon (African continent)
coastline.rough <- coastline.rough[order(coastline.rough$area, decreasing = T),]
coastline.rough <- coastline.rough[1:10,] # only 10 largest Polygons
coastline.rough <- st_intersection(coastline.rough, species.5.sf)
coastline.rough$area <- units::set_units(coastline.rough$area, "km^2")
head(coastline.rough)
#plot(coastline.rough)  


# 2. Train and test datset: plots----
## training data----
DL_training <- DL_pred |> 
  dplyr::select(X, Y, SPECIES.NAME, training, prediction) |> 
  dplyr::filter(training == "True")
DL_training.sf <- st_as_sf(DL_training, coords = c("X", "Y"))
st_crs(DL_training.sf) <- 3035

# plot.training
## Bet_pub
ggplot() + geom_sf(data = coastline.rough) +
  geom_sf(color = "khaki1", 
          data = DL_training.sf[which(DL_training.sf$prediction == "Betula pubescens"),]) +
  geom_sf(color = "darkolivegreen3", 
          data = DL_training.sf[which(DL_training.sf$SPECIES.NAME == "Betula pubescens"),], 
          alpha = 0.2, pch = 3) +
  theme(text = element_text(size = 8)) +
  labs(title = "Betula pubescens  (training)") + 
  scale_color_manual(values = DL_training.sf) -> bet_pub.plot.training
#bet_pub.plot.training

# Fag_syl
ggplot() + geom_sf(data = coastline.rough) +
  geom_sf(color = "khaki1", 
          data = DL_training.sf[which(DL_training.sf$prediction == "Fagus sylvatica"),]) +
  geom_sf(color = "darkolivegreen3", 
          data = DL_training.sf[which(DL_training.sf$SPECIES.NAME == "Fagus sylvatica"),], 
          alpha = 0.2, pch = 3) +
  theme(text = element_text(size = 8)) +
  labs(title = "Fagus sylvatica  (training)") + 
  scale_color_manual(values = DL_training.sf) -> fag_syl.plot.training
#fag_syl.plot.training

#Que_rob
ggplot() + geom_sf(data = coastline.rough) +
  geom_sf(color = "khaki1", 
          data = DL_training.sf[which(DL_training.sf$prediction == "Quercus robur"),]) +
  geom_sf(color = "darkolivegreen3", 
          data = DL_training.sf[which(DL_training.sf$SPECIES.NAME == "Quercus robur"),], 
          alpha = 0.2, pch = 3) +
  theme(text = element_text(size = 8)) +
  labs(title = "Quercus robur  (training)") + 
  scale_color_manual(values = DL_training.sf) -> que_rob.plot.training
#que_rob.plot.training

# Pic_abi
ggplot() + geom_sf(data = coastline.rough) +
  geom_sf(color = "khaki1", 
          data = DL_training.sf[which(DL_training.sf$prediction == "Picea abies"),]) +
  geom_sf(color = "darkolivegreen3", 
          data = DL_training.sf[which(DL_training.sf$SPECIES.NAME == "Picea abies"),], 
          alpha = 0.2, pch = 3) +
  theme(text = element_text(size = 8)) +
  labs(title = "Picea abies  (training)") + 
  scale_color_manual(values = DL_training.sf) -> pic_abi.plot.training
#pic_abi.plot.training

# Pin_syl
ggplot() + geom_sf(data = coastline.rough) +
  geom_sf(color = "khaki1", 
          data = DL_training.sf[which(DL_training.sf$prediction == "Pinus sylvestris"),]) +
  geom_sf(color = "darkolivegreen3", 
          data = DL_training.sf[which(DL_training.sf$SPECIES.NAME == "Pinus sylvestris"),], 
          alpha = 0.2, pch = 3) +
  theme(text = element_text(size = 8)) +
  labs(title = "Pinus sylvestris  (training)") + 
  scale_color_manual(values = DL_training.sf) -> pin_syl.plot.training
#pin_syl.plot.training

### test data----
DL_test <- DL_pred |> 
  dplyr::select(X, Y, SPECIES.NAME, training, prediction) |> 
  dplyr::filter(training == "False")
DL_test.sf <- st_as_sf(DL_test, coords = c("X", "Y"))
st_crs(DL_test.sf) <- 3035

# plots test
## Bet_pub
ggplot() + geom_sf(data = coastline.rough) +
  geom_sf(color = "khaki1", 
          data = DL_test.sf[which(DL_test.sf$prediction == "Betula pubescens"),]) +
  geom_sf(color = "darkolivegreen3", 
          data = DL_test.sf[which(DL_test.sf$SPECIES.NAME == "Betula pubescens"),], 
          alpha = 0.2, pch = 3) +
  theme(text = element_text(size = 8)) +
  labs(title = "Betula pubescens  (test)") + 
  scale_color_manual(values = DL_test.sf) -> bet_pub.plot.test
#bet_pub.plot.test

# Fag_syl
ggplot() + geom_sf(data = coastline.rough) +
  geom_sf(color = "khaki1", 
          data = DL_test.sf[which(DL_test.sf$prediction == "Fagus sylvatica"),]) +
  geom_sf(color = "darkolivegreen3", 
          data = DL_test.sf[which(DL_test.sf$SPECIES.NAME == "Fagus sylvatica"),], 
          alpha = 0.2, pch = 3) +
  theme(text = element_text(size = 8)) +
  labs(title = "Fagus sylvatica  (test)") + 
  scale_color_manual(values = DL_test.sf) -> fag_syl.plot.test
#fag_syl.plot.test

#Que_rob
ggplot() + geom_sf(data = coastline.rough) +
  geom_sf(color = "khaki1", 
          data = DL_test.sf[which(DL_test.sf$prediction == "Quercus robur"),]) +
  geom_sf(color = "darkolivegreen3", 
          data = DL_test.sf[which(DL_test.sf$SPECIES.NAME == "Quercus robur"),], 
          alpha = 0.2, pch = 3) +
  theme(text = element_text(size = 8)) +
  labs(title = "Quercus robur (test)") + 
  scale_color_manual(values = DL_test.sf) -> que_rob.plot.test
#que_rob.plot.test

# Pic_abi
ggplot() + geom_sf(data = coastline.rough) +
  geom_sf(color = "khaki1", 
          data = DL_test.sf[which(DL_test.sf$prediction == "Picea abies"),]) +
  geom_sf(color = "darkolivegreen3", 
          data = DL_test.sf[which(DL_test.sf$SPECIES.NAME == "Picea abies"),], 
          alpha = 0.2, pch = 3) +
  theme(text = element_text(size = 8)) +
  labs(title = "Picea abies  (test)") + 
  scale_color_manual(values = DL_test.sf) -> pic_abi.plot.test
#pic_abi.plot.test

# Pin_syl
ggplot() + geom_sf(data = coastline.rough) +
  geom_sf(color = "khaki1", 
          data = DL_test.sf[which(DL_test.sf$prediction == "Pinus sylvestris"),]) +
  geom_sf(color = "darkolivegreen3", 
          data = DL_test.sf[which(DL_test.sf$SPECIES.NAME == "Pinus sylvestris"),], 
          alpha = 0.2, pch = 3) +
  theme(text = element_text(size = 8)) +
  labs(title = "Pinus sylvestris (test)") + 
  scale_color_manual(values = DL_test.sf) -> pin_syl.plot.test
#pin_syl.plot.test

bet_pub.plot.training + bet_pub.plot.test +
  fag_syl.plot.training + fag_syl.plot.test +
  pic_abi.plot.training + pic_abi.plot.test +
  pin_syl.plot.training + pin_syl.plot.test +
  que_rob.plot.training + que_rob.plot.test +
  plot_layout(nrow = 5) +
  plot_annotation(title = "Training and test set valuation",
                  subtitle = "Actual occurence (yellow) vs. predictions (green)") -> plot.DL.summary
plot.DL.summary
