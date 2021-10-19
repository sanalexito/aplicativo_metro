# Gracica Afluencia

# Librerias ----
library(tidyverse)

# Datos ----

afluencia <- './afluencia.xlsx' %>% 
  readxl::read_xlsx() %>% 
  select(1,2,6) %>% 
  rename(pasajeros = 3) %>% 
  mutate(Periodo = factor(Periodo, levels = c('Enero','Febrero','Marzo','Abril','Mayo','Junio',
                                               'Julio','Agosto','Septiembre','Octubre','Noviembre',
                                               'Diciembre')))

# Grafico función ----

graf_afluencia <- function(yy){
g <- afluencia %>% 
  filter(anio == yy) %>% 
  ggplot() +
  aes(x = Periodo, y = pasajeros) +
  geom_bar(stat="identity", position="stack", color = 'white', fill = 'purple4') +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25, hjust=1)) +
  labs(x ='',y='')
return(g)
}

graf_afluencia <- map(.x = 2012:2021, .f=~graf_afluencia(.x))
