library (maptools)
data(wrld_simpl)
plot (wrld_simpl)
attributes (wrld_simpl)$data

writePolyShape (wrld_simpl, "wrld_simpl.shp")

?readShapePoly