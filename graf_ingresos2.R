# DatatÃ³n grÃ¡ficos de ingresos ----
library(tidyverse)
library(lubridate)
ingresos <- read_csv('C:/Users/52552/Alexito/Dataton/proyecto/ingresos.csv')

grep("01/01/2017", ingresos$FECHA)
grep("31/07/2021", ingresos$FECHA)

ingresos <- ingresos[c(1828:3500, 5298:6970, 8738:10410),]


graf_ingreso <- function(jj, name){
ingresos %>% 
    mutate(
    FECHA = dmy(FECHA),
    year = as.factor(year(FECHA)),
    TIPO_INGRESO = as.factor(TIPO_INGRESO)
  ) %>%
  filter(TIPO_INGRESO != 'Tarjetas') %>% 
  group_by(year, TIPO_INGRESO) %>% 
  rename(lineas = jj+2) %>% 
  summarise(L1 = sum(lineas)/1e6) %>% 
  ggplot(aes(
    x = year, 
    y = L1, 
    color = TIPO_INGRESO, 
    group = TIPO_INGRESO)
  ) +
  geom_ribbon(aes(
    ymin = 0, 
    ymax = L1, 
    fill = TIPO_INGRESO), 
    alpha = 0.3) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = 'Ingreso',
    caption = 'Cifras en Miles de Millones (MXN)'
  ) +
  theme(legend.position = 'bottom')
}

graficos_ingresos <- map2(.x =1:12, .y = c('B', 'A', 1:9, 12), .f = ~graf_ingreso(.x, .y))
