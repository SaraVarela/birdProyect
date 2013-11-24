# read the shapes in the folder, the species, eliminate the duplicated files, join to the preprocessed duplicated files

read_species<- function ()
{
bird_species<- list.files ("C:/Users/Alunos/Documents/BrunoVilela/Birds",
                           pattern=".shp", full.names=T, recursive=T)

lista_species<- substr (bird_species, nchar ("C:/Users/Alunos/Documents/BrunoVilela/Birds//"),  nchar (bird_species))
nomes<- str_extract_all (lista_species, "[a-z]+") 
duplicated_shapes<- which (duplicated (nomes)==T)
erase_duplicated <- c(duplicated_shapes, (duplicated_shapes -1))

bird_species2<- list.files ("C:/Users/Alunos/Documents/BrunoVilela/Bird_novos_shapes_dos_repetidos",
                            pattern=".shp", full.names=T, recursive=T)
species<- c(bird_species [- erase_duplicated], bird_species2)
species
}