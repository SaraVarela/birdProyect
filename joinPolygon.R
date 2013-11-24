library (stringr)
library (sp)
library (rgeos)
library (maptools)
library (rgdal)

# read the shapes in the folder, the species
bird_species<- list.files ("C:/Users/Alunos/Documents/BrunoVilela/Birds",
                           pattern=".shp", full.names=T, recursive=T)

lista_species<- substr (bird_species, nchar ("C:/Users/Alunos/Documents/BrunoVilela/Birds//"),  nchar (bird_species))
nomes<- str_extract_all (lista_species, "[a-z]+") 
duplicated_shapes<- which (duplicated (nomes)==T)
eliminar_duplicatos <- c(duplicated_shapes, (duplicated_shapes -1))
  
setwd ("C:\\Users\\Alunos\\Documents\\BrunoVilela\\Bird_novos_shapes_dos_repetidos")
for (i in duplicated_shapes){
shape_bird<- readShapePoly (bird_species [[i]], delete_null_obj=TRUE) 
shape_bird2<- readShapePoly (bird_species [[i-1]], delete_null_obj=TRUE) 
n_pol<- length (slot (shape_bird, "polygons"))
n_pol2<- length (slot (shape_bird2, "polygons"))
shape_new_id<- spChFIDs (shape_bird2, as.character (n_pol: (n_pol+(n_pol2-1))))
new_shape<- spRbind(shape_bird, shape_new_id)
name<- as.character(shape_bird@data$SCINAME)[1]
shapefile (paste (name, ".shp", sep=""), object= new_shape)
}

