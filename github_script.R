#### Code for Paper
#### Lex Comber
#### a.comber@leeds.ac.uk
library(sf)
library(tmap)
library(tidyverse)
library(repmis)
library(caret)

#### Load data
load("Ch7sds.RData")

#### Pre-processing
# 1. Calculate percentages
do.percs = function(df, denom){
  df %>% st_drop_geometry() %>% .[] /denom * 100
}
# age
oa[, 4:8] = do.percs(oa[, 4:8], oa$age_pop)
lsoa[, 4:8] = do.percs(lsoa[, 4:8], lsoa$age_pop)
# employment
oa[, 10:11] = do.percs(oa[, 10:11], oa$ecn_ctv)
lsoa[, 10:11] = do.percs(lsoa[, 10:11], lsoa$ecn_ctv)
# 2. Transform to OSGB and add Easting and Northing
properties = st_transform(properties, 27700)
properties$Easting = 
  properties %>% st_coordinates() %>% .[,1]
properties$Northing = 
  properties %>% st_coordinates() %>% .[,2]
# 3. Intersect with OA and LSOA data 
# and drop unwanted variables
oa <- sf::st_make_valid(oa)
props_oa = st_intersection(properties, oa) %>% 
  select(Price, Beds, gs_area,  u25, u45, u65, o65, unmplyd)
props_lsoa = st_intersection(properties, lsoa) %>% 
  select(Price, Beds, gs_area,  u25, u45, u65, o65, unmplyd)
# 4. Extract model inputs
x_oa = props_oa %>% select(-Price, -Beds) %>% 
  st_drop_geometry() %>% mutate_all(as.double) %>% data.frame()
y_oa = props_oa %>% select(Price) %>% st_drop_geometry() %>%
  mutate_all(as.double) %>% data.frame()
x_lsoa = props_lsoa %>% select(-Price, -Beds) %>% 
  st_drop_geometry() %>% mutate_all(as.double) %>% data.frame()
y_lsoa = props_lsoa %>% select(Price) %>% st_drop_geometry() %>%
  mutate_all(as.double) %>% data.frame()
ctrl = trainControl(method = "repeatedcv", number = 3, repeats = 10)
# 5. Make models
# OA model
oaFit=  train(Price~.,data=cbind(x_oa,y_oa),method="lm",trControl=ctrl)
# LSOA 
lsoaFit=  train(Price~.,data=cbind(x_lsoa,y_lsoa),method="lm",trControl=ctrl)
# 6. Make maps
tmap_mode("view")
p1 = tm_shape(oa)+tm_borders(col = "red")+
  tm_layout(title ="OA: n=1584", title.position = c("left", "bottom"))+
  tm_view(basemaps = c('OpenStreetMap'), bbox = st_bbox(lsoa), set.view = 11)
p2 =  tm_shape(lsoa)+tm_borders("black")+
  tm_layout(title ="LSOA: n=298", title.position = c("left", "bottom"))+
  tm_view(basemaps = c('OpenStreetMap'), bbox = st_bbox(lsoa), set.view = 11)
p3 = tm_shape(props_oa) + tm_dots(col='Price', palette = "GnBu", title = "Price (£1000s)", style = "kmeans")+
  tm_layout(title ="n=2211", title.position = c("left", "bottom"))+
  tm_view(basemaps = c('Esri.WorldGrayCanvas'), bbox = st_bbox(lsoa), set.view = 11)
tmap_arrange(p1,p2,p3, sync = TRUE)
tmap_mode('plot')
# 7. Tables of Coefficient estimates
OA = round(coef(oaFit$finalModel), 3)
LSOA = round(coef(lsoaFit$finalModel),3)
Covariate = names(OA)
df0 = data.frame(Covariate, OA = OA, LSOA = LSOA)
capture.output(knitr::kable(df0, format = "markdown"), file = "table2.md")
# 8. Tables of Variable importance
df1 = round(data.frame(OA = varImp(oaFit)$importance), 3)
Covariate = rownames(df1)
df2 = data.frame(Covariate, df1, LSOA = round(varImp(lsoaFit)$importance, 3))
names(df2)[2:3] = c("OA", "LSOA")
df2
capture.output(knitr::kable(df2, format = "markdown"), file = "table3.md")

### END ###
