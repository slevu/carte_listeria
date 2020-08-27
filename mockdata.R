## Création de données factices pour la carte
## Deux valeurs par code région insee, avec des zéros
create_mockdata <- function(codereg = readRDS("extdata/code_region.rds")){
  set.seed(123)
  mockdata <- data.frame(id = codereg,
                         value1 = sample( c(rep(0, 50), 1:50), 
                                          size = length(codereg)) ,
                         value2 = sample( c(rep(0, 50), 1:50), 
                                          size = length(codereg)) )
  return(mockdata)
}
