validate<- function (map_prediction, validation_occ, validation_abs)
  {
pred_occ<- extract (map_prediction, validation_occ)
pred_abs<- extract (map_prediction, validation_abs)
sensitivity<- length (which (pred_occ==1))/length (pred_occ)
specificity<- length (which (pred_abs==0))/length (pred_abs)
tss<- sensitivity + specificity -1 
tss
}
