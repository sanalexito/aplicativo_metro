library(leaflet)
library(htmltools)
library(htmlwidgets)
library(tidyverse)


colorea <- list("#1E5945", "#572364", "#e4007c", "#2271B3", "#424632", "#00FFFF",
                "#FFFF00", "#FF0000", "#EC7C26", "#00FF00", "#3C280D", "#E1CC4F")

lineas <- list()
for(i in 1:12) eval(parse(
  text = paste0("
  lineas[[i]] <- openxlsx::read.xlsx(\"C:/Users/52552/Alexito/Dataton/coordenadas_metro/cords_metro_cdmx_lineas.xlsx\", ",i,")
")))
vars <- map(.x = 1:12, .f = ~names(lineas[[.x]]))

#---- hace iconos --------------------------------------------------------------
ico_est <- list(); ruta<- list(); iconos <- list()
for(i in  1:12)eval(parse(text = paste0("
ruta[[i]] <- paste0(\"C:/Users/52552/Alexito/Dataton/datatonicos_app/www/\",lineas[[",i,"]][,1],\".png\")
iconos[[i]] <- paste0(lineas[[",i,"]][,1],\".png\")
")))
ico_est <- map(.x = 1:12, .f = ~makeIcon(ruta[[.x]], iconos[[.x]], 22, 22))
#-------------------------------------------------------------------------------
aa <- list("LB", "LA" ,"L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9", "L12")
pas <- map(.x=1:12, .f=~ openxlsx::read.xlsx("C:/Users/52552/Alexito/Dataton/proyecto/afluencia_metro.xlsx", aa[[.x]]))
for(i in 1:12){
  pas[[i]]$sem1_2020 <- pas[[i]]$'1_2020' + pas[[i]]$'2_2020'
  pas[[i]]$sem1_2021 <- pas[[i]]$'1_2021' + pas[[i]]$'2_2021'
}


leyendas <- map(.x = 1:12, .f= ~lapply(paste0(
  "<b>Estación: </b>", lineas[[.x]]$ESTACION,"<br>",
  "<b>Tipo: </b>", lineas[[.x]]$ATRIBUTO_1, "<br>",
  "<b>Correspondencia: </b>", lineas[[.x]]$ATRIBUTO_3,"<br>",
  "<b>1er Sem 2020: </b>", pas[[.x]]$sem1_2020," pers.<br>",
  "<b>1er Sem 2021: </b>", pas[[.x]]$sem1_2021," pers.<br>"

), htmltools::HTML))

#-----------------------------------------------------------------------------

hace_mapa <-function(vector){
  leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
    addProviderTiles("OpenStreetMap",group = "Calles") %>%
    addProviderTiles("Esri.NatGeoWorldMap", group ="Colonias" ) %>%
        addProviderTiles("Stamen.Toner", group = "Cartografía") %>%
    addTiles() %>%
    addMarkers(lat = lineas[[vector]][, 2],
               lng = lineas[[vector]][, 3],
               popup = leyendas[[vector]],
               icon = ico_est[[ vector ]])%>%  
    addPolylines(lat = lineas[[vector]][, 2],
                 lng = lineas[[vector]][, 3],
                 group=NULL, weight = 6, 
                 color = colorea[[vector]],
                 stroke = TRUE, 
                 opacity = 1) %>% 
    addCircleMarkers(lat = lineas[[vector]][, 2],
                     lng = lineas[[vector]][, 3], 
                     color = "#FFFFFF",
                     opacity = 1,
                     radius = 2)  %>%
    addLayersControl(baseGroups = c( "Calles", "Colonias", "Cartografía"),
                     options = layersControlOptions(collapsed = T), 
                     position="topleft")
  
}

mapas <- map(.x = 1:12, .f = ~ hace_mapa(vector = .x))
