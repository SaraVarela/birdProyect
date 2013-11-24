bioclim_calibrate<- function (calibration_variables, calibration_occ, validation_occ, validation_abs)
{
  model_bioclim<- bioclim (calibration_variables, calibration_occ)
  map_bioclim<- predict (model_bioclim, calibration_variables)
  map_bioclim_reclass<- reclassify (map_bioclim, c(-Inf, 0.05, 0, 0.05, 1, 1))
  
  Data<- (xyFromCell(map_bioclim_reclass, which (map_bioclim_reclass@data@values==1)))
  centroid<- (apply(Data, 2, "max") + apply (Data, 2, "min"))/2
  bio<- as.vector (c(PatchStat (map_bioclim_reclass)[2,] , centroid[1], centroid[2]))
  
  #pasado
  past_bioclim<- predict (model_bioclim, past_variables)
  past_bioclim_reclass<- reclassify (past_bioclim, c(-Inf, 0.05, 0, 0.05, 1, 1))
  Data<- (xyFromCell(past_bioclim_reclass, which (past_bioclim_reclass@data@values==1)))
  centroid_past<-(apply(Data, 2, "max") + apply (Data, 2, "min"))/2
  bio_past<- as.vector (c( PatchStat (past_bioclim_reclass)[2,], centroid_past[1], centroid_past[2]))
  
  #futuro
  #future_bioclim<- predict (model_bioclim, future_variables)
  #future_bioclim_reclass<- reclassify (future_bioclim, c(-Inf, 0.05, 0, 0.05, 1, 1))
  #xcentroid_bio_future<- (future_bioclim_reclass@extent@xmax + future_bioclim_reclass@extent@xmin)/2 
  #ycentroid_bio_future<- (future_bioclim_reclass@extent@ymax + future_bioclim_reclass@extent@ymin)/2
  #bio_future<- PatchStat (future_bioclim_reclass)[2,]  
  
  #validate 
  tss_bio<- validate(map_bioclim_reclass, validation_occ, validation_abs)
  
  fill_bio<- length (which (map_bioclim_reclass@data@values * sp@data@values>0))/
    length (which (map_bioclim_reclass@data@values==1))
  stable_lgm_present<- length (which (map_bioclim_reclass@data@values * past_bioclim_reclass@data@values>0))/
    length (which (past_bioclim_reclass@data@values==1))
  
 # stable_pres_fut<- length (which (map_bioclim_reclass@data@values * future_bioclim_reclass@data@values>0))/
 #  length (which (map_bioclim_reclass@data@values==1)) 
 
  areas<- as.matrix (c(tss_bio, fill_bio,stable_lgm_present, as.vector (bio_past), as.vector (bio)) )
  row.names (areas)<- c("tss", "range.fill", "stable.pres", 
                 "past_patchID", "past_n.cell", 
                 "past_n.core.cell", "past_n.edges.perimeter", "past_n.edges.internal", "past_area", 
                 "past_core.area", "past_perimeter", "past_perim.area.ratio", "past_shape.index", 
                 "past_frac.dim.index", "past_core.area.index", "past_x", "past_y", 
                 "present_patchID", "present_n.cell", 
                 "present_n.core.cell", "present_n.edges.perimeter", "present_n.edges.internal", "present_area", 
                 "present_core.area", "present_perimeter", "present_perim.area.ratio", "present_shape.index", 
                 "present_frac.dim.index", "present_core.area.index", "present_x", "present_y" )

  areas
}
