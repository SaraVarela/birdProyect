run_models<- function (species, variables_0, name_GCM_)
{
i<-0
exp<-1
res<- list ()
for (x in species)   
{  
  i<-i+1
  print(length(species)-i)
  shape_bird<- readShapePoly (x, delete_null_obj=TRUE) # this could be problematic! we have some shape files with null geometries
  bird<- NULL
  bird<- breeding_birds(shape_bird)
  if (!is.null(bird)) {
    buf<-gBuffer(bird, byid=F, width=5)# set arbitrary buffer, here I choose 5
    #plot (wrld_simpl)
    #plot (buf, add=T)
    #plot (bird, col= "red", add=T)
    
    calibration_variables<- crop (variables_0, buf)
    calibration_variables<- mask (calibration_variables, buf)
    
    #select extent for calibration and validation, select occ and abs
    spp<- rasterize (bird, calibration_variables [[1]])
    spp<-mask(spp, calibration_variables[[1]])
    sp<- reclassify (spp, c(-Inf, 0, 0, 1, +Inf, 1))
    absence_area<- mask (calibration_variables[[1]], sp, inverse=TRUE)
    
    # select points for calibrate and validate
    calibration_occ<- sampleRandom (sp, 250, na.rm=TRUE, xy=TRUE)[,1:2]
    validation_occ<- sampleRandom (sp, 250, na.rm=TRUE, xy=TRUE)[,1:2]
    validation_abs<- sampleRandom (absence_area, 250, na.rm=TRUE, xy=TRUE)[,1:2]
    cond<- as.data.frame (calibration_occ)
    
    if (dim (cond)[1]>50) {
      
      #run models
      #bioclim, threshold=percentil 5
      model_bioclim<- bioclim (calibration_variables, calibration_occ)
      map_bioclim<- predict (model_bioclim, calibration_variables)
      map_bioclim_reclass<- reclassify (map_bioclim, c(-Inf, 0.05, 0, 0.05, 1, 1))
      tss_bio<- validate(map_bioclim_reclass, validation_occ, validation_abs)
      
      bioclim_model<- list (tss_bio= tss_bio, bioclim= model_bioclim)
      
      # run mahalanobis, threshold percentil 5
      model_maha<- mahal (calibration_variables, calibration_occ)
      map_maha<- predict (model_maha, calibration_variables)
      t<- quantile (extract (map_maha, calibration_occ), na.rm=T, 0.05)
      map_maha_reclass<- reclassify (map_maha, c(-Inf, t, 0, t, 1, 1))
      tss_maha<- validate(map_maha_reclass, validation_occ, validation_abs)  
      maha_model<- list (tss_maha= tss_maha, maha= model_maha)
      
      model_maxent<- maxent (calibration_variables, calibration_occ)
      map_maxent<- predict (model_maxent, calibration_variables)
      t<- model_maxent@results [which (rownames (model_maxent@results)== "Maximum.training.sensitivity.plus.specificity.logistic.threshold")]
      map_maxent_reclass<- reclassify (map_maxent, c(-Inf, t, 0, t, 1, 1))
      tss_maxent<- validate(map_maxent_reclass, validation_occ, validation_abs)
      
      maxent_model<- list (tss_maxent= tss_maxent, maxent= model_maxent)
      
      res[[i]] <- list (bioclim=bioclim_model, mahalanobis= maha_model, maxent=maxent_model)
      names (res[[i]])<- c(as.character(shape_bird@data$SCINAME)[1],
                           as.character(shape_bird@data$SCINAME)[1], 
                           as.character(shape_bird@data$SCINAME)[1])  
    
      
      if (length (res)==10){  
        save (res, file = paste (name_GCM_, exp, "res.RData", sep=""))
        exp<- exp+1
        res<- list()
      }
      
    }
  }
}
}




