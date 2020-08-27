## Carte avec deux labels colorés par région
carte <- function(FNSHP = "extdata/Region_France_DOM_simplifie_2016.shp",
                  dat = read.csv2("mockdata.csv"),
                  nozero = TRUE,
                  colvalue1 = "red", colvalue2 = "green4",
                  justvalue1 = "left", justvalue2 = "right",
                  ...){
  ##- libs
  require(rgdal)
  require(broom)
  require(ggplot2)

  ##- shape
  map0 <- rgdal::readOGR(dsn = FNSHP, verbose = FALSE)
  suppressWarnings( positions <- broom::tidy(map0, region="NEW_REG") )
  positions$id <- as.numeric(as.character(positions$id))

  ##- centroid
  x <- sf::st_as_sf(map0)
  suppressWarnings( cent <- sf::st_centroid(x) )
  levels(cent$NEW_REG) <- as.character(as.numeric( levels(cent$NEW_REG) ))
  dd0 <- data.frame(id = as.numeric(as.character(cent$NEW_REG)),
                   matrix(unlist(cent$geometry),
                          ncol = 2,
                          byrow = TRUE,
                          dimnames = list(NULL, c("long", "lat"))) ,
                   stringsAsFactors = FALSE)

  ##- data
  ## don't show zeros
  if (nozero) {
    dat[dat == 0] <- NA
  }

  dd <- merge(dd0, dat, by = "id", all.x = TRUE)
  positions.data <- merge(positions, dd[, -(2:3)], by = 'id') #, all.x = TRUE)
  
  ##- plot
  ## blank map
  m0 <- ggplot() + geom_path(data = positions.data,
                                aes( x = long, y = lat,
                                     group = group),
                                color = "grey", size = .1) +
    coord_fixed() + theme_void()
  ## labels
    m1 <- m0 +  
      ## value1
      geom_text(data = dd,
                    mapping = aes(x = long, y = lat,
                                  label = value1,
                                  size = value1,
                                  # justify only if two values are plotted
                                  hjust = ifelse(is.na(value2), 
                                                 "middle", 
                                                 justvalue1)), 
                    colour = colvalue1,
                # for DOM, shift labels
                    nudge_x = ifelse(dd$id %in% 1:6, -1e5, 0) ) +
      ## value2
      geom_text(data = dd,
                    mapping = aes(x = long, y = lat,
                                label = value2,
                                size = value2,
                                hjust = ifelse(is.na(value1), 
                                               "middle", 
                                               justvalue2)), 
                    colour = colvalue2,
                    nudge_x = ifelse(dd$id %in% 1:6, -1e5, 0) ) + 
      # rescale symbol size after transformation
      scale_radius(range = c(3, 6)) + 
      theme(legend.position="none") 
  return(m1)
}
