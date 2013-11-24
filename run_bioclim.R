library (maptools)
library (raster)
library (SDMTools)
library (dismo)
library (rJava)
library (stringr)
library (sp)
library (rgeos)
library (rgdal)

# load functions 
sapply(list.files(pattern=".R", path="C:/Users/Alunos/Documents/BrunoVilela/Sara/Birds/functions", full.names=TRUE), source)

# read variables 
variables <- read.table ("C:\\Users\\Alunos\\Documents\\Sara\\climate_PMIP5\\bio_var_CCSM_0k_global.txt", header=T)
bio1<- rasterFromXYZ (data.frame (variables [,2:3], variables$bio.1))
bio4<- rasterFromXYZ (data.frame (variables [,2:3], variables$bio.4))
bio12<- rasterFromXYZ (data.frame (variables [,2:3], variables$bio.12))
bio15<- rasterFromXYZ (data.frame (variables [,2:3], variables$bio.15))
variables_ccsm0<- stack (bio1, bio4, bio12, bio15)
rm (bio1, bio4, bio12, bio15)

# read and select the bird files to be opened. 
species<- read_species()

# run models and save them
setwd("C:\\Users\\Alunos\\Documents\\BrunoVilela\\Sara\\results_models")

species1<-species [2] 

run_models (species= species1, variables_0= variables_ccsm0, name_GCM_="CCSM_0_")


run_models (variables_miroc0, "MIROC_0_")

## project, present, past, future. 

load("CCSM_0_res.RData", .GlobalEnv)

ranges_0_ccsm_bioclim<- proyect_bioclim (variables_ccsm0)
ranges_0_ccsm_maxent<-  proyect_maxent (variables_ccsm0)
ranges_0_ccsm_maha<-    proyect_maha (variables_ccsm0)

ranges_21_ccsm_bioclim<- proyect_bioclim (variables_ccsm21)
ranges_21_ccsm_maxent<-  proyect_maxent (variables_ccsm21)
ranges_21_ccsm_maha<-    proyect_maha (variables_ccsm21)

ranges_2080_ccsm_bioclim<- proyect_bioclim (variables_ccsm2080)
ranges_2080_ccsm_maxent<-  proyect_maxent (variables_ccsm2080)
ranges_2080_ccsm_maha<-    proyect_maha (variables_ccsm2080)

#### read maps, cut them (buffer, continents), run tests
map_bioclim<- predict (res[[i]][[1]][2]$bioclim, variables_0)
plot (map_bioclim)
str (map_bioclim)


kk<- raster ("C:/Users/Alunos/Documents/BrunoVilela/Sara/climatic layers/climatic layers/CCSM_21/bio1.bil")
str (kk)
plot(kk)
?raster
(variables_0[[1]])@data@values
ranges_0_ccsm_bioclim<-


map_bioclim_reclass<- reclassify (map_bioclim, c(-Inf, 0.05, 0, 0.05, 1, 1))


model_maha<- mahal (calibration_variables, calibration_occ)
map_maha<- predict (model_maha, calibration_variables)
t<- quantile (extract (map_maha, validation_occ), na.rm=T, 0.05)
map_maha_reclass<- reclassify (map_maha, c(-Inf, t, 0, t, 1, 1))
tss_maha<- validate(map_maha_reclass, validation_occ, validation_abs)  
maha_model<- list (tss_maha= tss_maha, maha= model_maha)

model_maxent<- maxent (calibration_variables, calibration_occ)
map_maxent<- predict (model_maxent, calibration_variables)
t<- summary (model_maxent@results) 
map_maxent_reclass<- reclassify (map_maxent, c(-Inf, t, 0, t, 1, 1))


x<- mean (calibration_occ[,1])
y<- mean (calibration_occ[,2])
sp_real<- as.vector (c(x=x, y=y, PatchStat (sp)))
    

