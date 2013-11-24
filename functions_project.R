proyect_bioclim<- function (variables)  
{
  
  predicted<-big.matrix(length (map_bioclim@data@values), length (species)) 
  options (bigmemory.allow.dimnames=TRUE)
  
  for (x in 1:length (res)){
    
    map_bioclim<- predict (res[[i]][[1]][2]$bioclim, variables)
    result<- map_bioclim@data@values
    predicted[,i]<- result
    colnames (predicted[,i])<- names (res[[i]][1])
  }
  
  predicted
  
}



proyect_maha<- function (variables)  
{
  
  predicted<-big.matrix(length (map_bioclim@data@values), length (species)) 
  options (bigmemory.allow.dimnames=TRUE)
  
  for (x in 1:length (res)){
    
    map_bioclim<- predict (res[[i]][[2]][2]$maha, variables)
    result<- map_bioclim@data@values
    predicted[,i]<- result
    colnames (predicted[,i])<- names (res[[i]][1])
  }
  
  predicted
  
}


proyect_maxent<- function (variables)  
{
  
  predicted<-big.matrix(length (map_bioclim@data@values), length (species)) 
  options (bigmemory.allow.dimnames=TRUE)
  
  for (x in 1:length (res)){
    
    map_bioclim<- predict (res[[i]][[3]][2]$maxent, variables)
    result<- map_bioclim@data@values
    predicted[,i]<- result
    colnames (predicted[,i])<- names (res[[i]][1])
  }
  
  predicted
  
}

