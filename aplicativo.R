################################################################################
library(shiny)
library(shinyWidgets)
library(plotly)
library(tidyverse)
library(shinythemes)
library(DT)
library(htmltools)
library(htmlwidgets)
library(lubridate)
library(scales)
source("C:/Users/52552/Alexito/Dataton/datatonicos_app/hace_mapas2.R")
#-------------------------------------------------------------------------------
base <- read.csv("C:/Users/52552/Alexito/Dataton/Tabulado_metro_inegi.csv", encoding = "UTF-8")
a1 <- list(1:13, 14:26, 27:39, 40:52, 53:65, 66:78, 79:91, 92:104, 105:117)

bases <- map(.x = 1:9, .f = ~ base[a1[[.x]] , c(1,5)])

bases <- map(.x = 1:9, .f = ~bases[[.x]][2:13, ]) 
for(i in 1:9){ colnames(bases[[i]]) <- c("MES", "PASAJEROS")}


source("C:/Users/52552/Alexito/Dataton/datatonicos_app/graf_ingresos.R")
aa <- paste0("LINEA_", c("A","B",1:9, 12))
bb <- map(.x = 1:12, .f = ~c("FECHA", "TIPO_INGRESO", aa[[.x]]))
  

maxims <- map(.x = 1:12, .f = ~ ingresos[ingresos[,aa[[.x]] ]==max(ingresos[,aa[[.x]]]), bb[[.x]]])

#-------------------------------------------------------------------------------

u <- bootstrapPage(
      navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
               HTML('<a style="text-decoration:none; cursor:default; color:#FFFFFF;" class="active" href="#"> Dataton 2021 </a>'), id="nav",
               windowTitle = "Dataton 2021",
#---- tab panel 1 ----  

tabPanel("Acerca del sitio",
    tags$div(
    tags$h4("Introducción"),           
     "Este sitio surge en el marco de la convocatoria del ''Primer Dataton sobre la plataforma Tu Ciudad, Tu dinero'',
     organizado por la Agencia Digital de Innovación Pública ", 
     tags$a(href="https://dataton2021.tudinero.cdmx.gob.mx/", "(ADIP)"),
     "para el aprovechamiento y análisis de los datos públicos del ",
     tags$a(href="https://transparencia.cdmx.gob.mx/", "Portal de Transparencia de la CDMX."),
     "El objetivo es complementar estos recursos con varias características interactivas, 
    incluida la función de descarga de datos y la capacidad de superponer capas en los mapas para el uso,
    análisis o comparación de los datos de cada línea del Sistema de Transporte Colectivo-Metro.",
    #---- background
    tags$br(),tags$br(),tags$h4("Background"), 
    "Los datos empleados para generar las gráficas, así como la información contenida en las tablas, provienen 
    de las bases de datos que aparecen públicas en el sitio web del ",
    tags$a(href="https://www.metro.cdmx.gob.mx/operacion/mas-informacion", "Metro de la CDMX."),
    "En particular, para el desarrollo del sitio se usa la base de datos sobre la Información de Ingresos del Sistema de Transporte 
     Colectivo Metro, que forma parte del listado de ",
    tags$a(href="https://dataton2021.tudinero.cdmx.gob.mx/docs/Catalogo%20de%20bases%20de%20datos.pdf", "Datasets del Dataton."),
    "Además, se complementa la información con otras bases de datos que permiten un mejor entendimiento de
    la información disponible. En particular, se incluye la información sobre pasajeros transportados entre 2012 y 2020 que es
    publicada por el Instituto Nacional de Estadística y Geografía (INEGI), en su sitio web sobre",
    tags$a(href="https://www.inegi.org.mx/app/tabulados/?nc=100100042", "Transporte Urbano de Pasajeros"),
    "con datos que provienen del STC-Metro.",
    #----  codigo  
    tags$br(),tags$br(),tags$h4("Código"),
    "El código y los conjuntos de datos empleados para generar este aplicativo Shiny se encuentran disponibles en",
    tags$a(href="https://github.com/sanalexito", "Github."),
    #---- Recursos   
    tags$br(),tags$br(),tags$h4("Recursos"),
    tags$b("Ingresos del STC-Metro: "), tags$a(href="https://datos.cdmx.gob.mx/dataset/ingresos-del-sistema-de-transporte-colectivo-metro", "Portal de datos abiertos de la CDMX"),
    tags$br(),
    tags$b("Coordenadas de las estaciones del Metro: "), tags$a(href="https://datos.cdmx.gob.mx/dataset/estaciones-metro", "Portal de datos abiertos de la CDMX"),
    tags$br(),
    tags$b("Líneas y estaciones del Metro: "), tags$a(href="https://datos.cdmx.gob.mx/dataset/lineas-y-estaciones-del-metro", "Portal de datos abiertos de la CDMX"),
    tags$br(),
    tags$b("Iconografía del Metro: "), tags$a(href="https://www.metro.cdmx.gob.mx", "Sitio web del Metro de la CDMX.")," Estaciones de la red.",
    
        #----  autores
    tags$br(),tags$br(),tags$h4("Autores"),
    "María del Rosario Machuca Gutiérrez, INFONAVIT", tags$br(),
    "Alejandro Sánchez Peralta, INEGI", tags$br(),
    #---- contacto
    tags$br(),tags$br(),tags$h4("Contacto"),
    "rosario.machuca21@gmail.com", tags$br(),
    "sanchez.alexito@gmai.com",tags$br(),tags$br(),
    #---- imagen
    
    tags$img(src = "./RMG.png", height = "200px", width="160" ), tags$img(src = "./alex.png", height = "195px", width="200" ),
    
    tags$br(),tags$br(),
    
    tags$h4("Última actualización"), 
    h6(Sys.Date()),
         )
),


tabPanel("El Metro de la CDMX",
          sidebarLayout(
             sidebarPanel(titlePanel("Contexto Histórico"),
               
               "Con base en el Decreto de Creación del Sistema de Transporte Colectivo (STC) 
                -publicado el 26 abril de 1967-, el metro ha brindado servicio de manera prácticamente 
                de manera ininterrumpida a millones de personas desde su inicio de operaciones el 4 de
                septiembre de 1969. Su creación, pensada para hacer frente al rápido crecimiento de la ciudad 
                hacia prácticamente todas direcciones, fue por demás oportuna dado que los camiones, tranvías y 
                taxis de la época, no eran ya suficientes para las necesidades de movilidad de la creciente y 
                masiva ciudad",
                
                
                br(), br(),
                
                "Actualmente el STC-Metro cuenta con 12 líneas que conectan los cuatro extremos
                de la ciudad y contribuyen de manera significativa al traslado de personas de manera
                rápida, eficiente y económica.",
                
                br(), br(),
                
                "En este sitio encontrarás información relevante acerca de su operación e 
                infraestructura. Te invitamos a conocer algo más de este Nuestro Metro.",
                br(), br()
           ),
           
           
           #muestra la tabla
           mainPanel(
             
             tabsetPanel(type = "tabs",
                         tabPanel("Historia",  
                                fluidRow(
                                br(), 
                                "El Metro de la Ciudad de México, en otrora conocida como Departamento del Distrito Federal,
                                fue puesto en operació por el Presidente Gustavo Díaz Ordaz quien, en 
                                compañía del regente Alfonso Corona del Rosal y el escritor Agustín Yáñez,
                                realizaron el viaje inaugural de las estaciones Candelaria a San Lázaro de la 
                                Línea 1.", 
                                br(), br(),
                                    img(src = "./diaz_ordaz.png", height = "384px", width="903"),
                                br(), br(), br(),
                                "Al inicio de sus operaciones, a finales de la década de los 60's del siglo pasado, 
                                la Línea 1 del metro comprendía el tramo que va de Chapultepec a Zaragoza, y constaba de 
                                dieciséis estaciones únicamente. Posteriormente, un crecimiento sostenido logró 
                                consolidar otras once líneas entre las que se incluyen las líneas A y B, y la línea 
                                dorada, terminada de construir en el año 2012. Dando así un total de 195 estaciones. 
                                Es decir casi trece veces más que las iniciales de 1969.",
                                br(), br(),
                                img(src = "./linea1_inicial.png", width = "559px", height = "268"),
                                br(), br(),
                                
                                "Por otra parte el famoso logo naranja del Metro fue creado por el estadounidense Lance Wyman, diseñador reconocido
                                a nivel mundial y cuyo trabajo - quizás más emblemático - fue el diseño de la iconografía de los
                                Juegos Olímpicos de 1968, que se llevaron a cabo en México. Además fue él quien diseño varios de los iconos de las
                                estaciones que todos ubicamos. Por mencionar algunos están: 'la caja de manzanas' para la estación Merced,
                                'el patito' para Candelaría o el 'chapulin' para Chapultepec, todas en la Línea 1.",
                                br(),
                                tags$b("Fuente: "), tags$a(href="https://plumasatomicas.com/cultura/cultura-cultura/lance-wyman-diseno-metro/", "Plumas Atómicas"),
                                br(), br(),
                                
                                img(src = "./Lance_Wyman01.png", width="500px", height = "300px" ),
                                img(src = "./metro_logo.png", width="300px", height = "300px"),
                                br(), br(),br(), br(),
                                ),
),
                         tabPanel("Red del Metro",
                               fluidRow(
                                 
                                   br(), br(),
                                   "Actualmente el metro cuenta con 195 estaciones distribuidas en 12 líneas que 
                                   cubren una longitud de 200.88 km lineales, sin incluir las vías de los talleres, 
                                   con las que se hace un total de 226.48 km. Esto permite a mucha gente desplazarse 
                                   de un extremo a otro de la ciudad por un costo accesible y en un tiempo 
                                   considerablemente rápido. ", 
                                br(), br(),
                                  img(src = "./plano_red.png", width="781", height = "920px" )
                        )),

                         tabPanel("Parque vehícular",
                                  fluidRow(
                                    
                                    br(), 
                                    "El STC-Metro cuenta con un parque vehicular de 384 trenes, de los cuales:",
                                    br(), br(),
                                    "* 321 son de rodadura neumática. De estos, 292 son trenes de 9 vagones y 29 son de 6 vagones.",
                                    br(),
                                    "* 63 trenes de rodadura férrea, que se desagregan en 12 trenes de 6 vagones, 
                                         21 de 9 vagones y, 30 trenes de 7 vagones.",
                                    br(), br(),
                                    
                                    "Este parque vehicular está integrado por 4 modelos de trenes férreos, mientras que el resto son neumáticos. 
                                    Se distinguen 2 tipos de tecnología en su fabricación: el 5% cuenta con sistema de tracción-frenado del tipo electromecánico 
                                    JH (árbol de levas), con trabajos de fiabilización y rehabilitación llevado a cabo por técnicos mexicano.
                                    El resto de los trenes cuenta con un sistema de tracción-frenado haciendo uso semiconductores y controles
                                    electrónicos, lo que permite aumentar la fiabilidad, reducir los costos de operación y mantenimiento, y
                                    alcanzar una mayor eficiencia en la recuperación de energía durante la etapa del frenado eléctrico.",
                                    br(),
                                    tags$b("Fuente: "), tags$a(href="https://www.metro.cdmx.gob.mx/parque-vehicular", "Metro de la CDMX,"),
                                    br(), br(),
                                    
                                    img(src = "./nm02prin.png", width="500", height = "300px" , align ="left"),br(), br(),
                                    img(src = "./fm95prin.png", width="500", height = "300px", align ="right" ),br(), br(),
                                    img(src = "./nm73arprin.png", width="500", height = "300px" , align ="left"),br(), br(),
                                    br(), br(),
                        ))
                              
                  ) 
                )
              )
),
#---- tab panel 2 ----
tabPanel("Líneas del Metro",
        
  sidebarLayout(
    
    sidebarPanel( titlePanel("Información por línea"), 
      
         h5("Selecciona en la caja de abajo alguna de las líneas para conocer información relevante sobre ella. 
         Sobre el mapa, al dar click en los iconos del metro, se desplegará también información útil de cada estación.",
         br(), br(),
         
         selectInput('linea', label = NULL,
                     choices = c('Línea 1' = 'L1',
                                 'Línea 2' = 'L2',
                                 'Línea 3' = 'L3',
                                 'Línea 4' = 'L4',
                                 'Línea 5' = 'L5',
                                 'Línea 6' = 'L6',
                                 'Línea 7' = 'L7',
                                 'Línea 8' = 'L8',
                                 'Línea 9' = 'L9',
                                 'Línea 12' = 'L12',
                                 'Línea A' = 'LA',
                                 'Línea B' = 'LB')
         ), 
      h4(textOutput("maximo")), 
      h5(textOutput("maximo_f")) 
         ),
      
      plotOutput("gram", height = "300px", width = "100%")
         ),
      
    mainPanel(
          h5("El Sistema de Transporte Colectivo Metro está conformado por doce líneas que proporcionan movilidad
             a una gran cantidad de personas de uno a otro extremo de la ciudad. En los recuadros de cada estación
             pueden verse también los datos sobre la afluencia de pasajeros para el Primer Semestre de 2020 y 2021, 
             respectivamente."),
          
          br(),
          leafletOutput("mymap", height = 500),
          h6("Usa los controles del mapa para explorar lugares cercanos a cada una de las estaciones. El trazado de la línea es aproximado.")
          
        )
    )
 ),
      

#---- tab panel 3 ----
    tabPanel("Pasajeros",
             titlePanel("Pasajeros Transportados (Millones de Personas)"),
             
             #Selector
             sidebarLayout(
                 sidebarPanel("El metro transporta diariamente a millones de pasajeros de un extremo
                              a otro de la Ciudad. En las tablas puedes conocer cuántas personas son movilizadas 
                               mensualmente según datos del Instituto Nacional de Estadística 
                               y Geografía (INEGI).",
                              
                     br(), br(), br(),
                    
                     sliderInput("anio",
                                 "Mueve el deslizador para elegir un año.",
                                 2012,
                                 2020,
                                 "2012",
                                 animate = F, sep=""),
                     
                     br(), br(),
                     
                     h4(textOutput("tot_pas")),
                              ),
                      
                 
                     #muestra la tabla
                         mainPanel(
                             
                             tabsetPanel(type = "tabs",
                             tabPanel("Tabla",  dataTableOutput("tab_pas")),
                             tabPanel("Gráfica", plotOutput("gra_ing"))
                             
                             ) 
                         )
                     )
                 )
      )
    )

#----

# Define server logic required to draw a histogram
s <- function(input, output) {
#---- imagen1 presen ----
  
  output$red <- renderImage({
    img(src = "./alex.png", height = "195px", width="200" )
    })
  
#---- mapa ----
    output$mymap <- renderLeaflet({
    if(input$linea=='LB'){
        mymap = mapas[[1]]
    }else if(input$linea== 'LA'){
        mymap = mapas[[2]]
    }else if(input$linea == 'L1'){
        mymap = mapas[[3]]
    }else if(input$linea == 'L2'){
        mymap = mapas[[4]]
    }else if(input$linea == 'L3'){
        mymap = mapas[[5]]
    }else if(input$linea == 'L4'){
        mymap = mapas[[6]]
    }else if(input$linea == 'L5'){
        mymap = mapas[[7]]
    }else if(input$linea == 'L6'){
        mymap = mapas[[8]]
    }else if(input$linea == 'L7'){
        mymap = mapas[[9]]
    }else if(input$linea == 'L8'){
        mymap = mapas[[10]]
    }else if(input$linea == 'L9'){
        mymap = mapas[[11]]
    }else if(input$linea == 'L12'){
        mymap = mapas[[12]]
    }else{
        mymap = leaflet(options = leafletOptions(minZoom = 0, maxZoom = 18)) %>%
            addProviderTiles("OpenStreetMap",group = "Calles") %>%
            addProviderTiles("Esri.NatGeoWorldMap", group ="Colonias" ) %>%
            addProviderTiles("Stamen.Toner", group = "Cartografía") %>%
            addTiles() %>%
            addMarkers(lat = 19.432447422693613,
                       lng = -99.13239106670832,
                       popup = "Centro histórico",
                       icon = metro)
    } 
    })
    

#--- Tabla pasajeros ----   
    output$tab_pas <- renderDataTable({
        if(input$anio == "2012"){
            dat <- bases[[1]]
        }else if(input$anio == "2013"){
            dat <- bases[[2]]
        }else if(input$anio == "2014"){
            dat <- bases[[3]]
        }else if(input$anio == "2015"){
            dat <- bases[[4]]
        }else if(input$anio == "2016"){
            dat <- bases[[5]]
        }else if(input$anio == "2017"){
            dat <- bases[[6]]
        }else if(input$anio == "2018"){
            dat <- bases[[7]]
        }else if(input$anio == "2019"){
            dat <- bases[[8]]
        }else if(input$anio == "2020"){
            dat <- bases[[9]]
        }
        
        dat <- dat[order(dat[1:(dim(dat)[1]), 2], decreasing = TRUE), ] 
    },
    extensions = c('Scroller','Buttons'),
    class = 'compact cell-border stripe',  rownames = FALSE,
    filter = 'none',
    options = list(
        pageLength = 16,
        deferRender = FALSE,
        #scrollY=200,
        scroller = F,
        dom = 'Bfrtip',  #Bfrtip
        buttons = c('csv', 'excel')
        )
    )
    
#---- total de pasajeros ----
    output$tot_pas <- renderText({
      if(input$anio == "2012"){
        dat <- paste0("Total en ",input$anio,": ", 1e+06 *sum(bases[[1]][2]), " pasajeros")
      }else if(input$anio == "2013"){
        dat <- paste0("Total en ",input$anio,": ", 1e+06 *sum(bases[[2]][2]), " pasajeros")
      }else if(input$anio == "2014"){
        dat <- paste0("Total en ",input$anio,": ", 1e+06 *sum(bases[[3]][2]), " pasajeros")
      }else if(input$anio == "2015"){
        dat <- paste0("Total en ",input$anio,": ", 1e+06 *sum(bases[[4]][2]), " pasajeros")
      }else if(input$anio == "2016"){
        dat <- paste0("Total en ",input$anio,": ", 1e+06 *sum(bases[[5]][2]), " pasajeros")
      }else if(input$anio == "2017"){
        dat <- paste0("Total en ",input$anio,": ", 1e+06 *sum(bases[[6]][2]), " pasajeros")
      }else if(input$anio == "2018"){
        dat <- paste0("Total en ",input$anio,": ", 1e+06 *sum(bases[[7]][2]), " pasajeros")
      }else if(input$anio == "2019"){
        dat <- paste0("Total en ",input$anio,": ", 1e+06 *sum(bases[[8]][2]), " pasajeros")
      }else if(input$anio == "2020"){
        dat <- paste0("Total en ",input$anio,": ", 1e+06 *sum(bases[[9]][2]), " pasajeros")
      }
      
      })
#---- gra_ing ----
    output$gram <- renderPlot(
      if(input$linea == "LB"){
        graficos_ingresos[[1]]
      }else if(input$linea == "LA"){
        graficos_ingresos[[2]]
      }else if(input$linea == "L1"){
        graficos_ingresos[[3]]
      }else if(input$linea == "L2"){
        graficos_ingresos[[4]]
      }else if(input$linea == "L3"){
        graficos_ingresos[[5]]
      }else if(input$linea == "L4"){
        graficos_ingresos[[6]]
      }else if(input$linea == "L5"){
        graficos_ingresos[[7]]
      }else if(input$linea == "L6"){
        graficos_ingresos[[8]]
      }else if(input$linea == "L7"){
        graficos_ingresos[[9]]
      }else if(input$linea == "L8"){
        graficos_ingresos[[10]]
      }else if(input$linea == "L9"){
        graficos_ingresos[[11]]
      }else if(input$linea == "L12"){
        graficos_ingresos[[12]]
      }
     )

#---- maximos ---- 
    output$maximo <- renderText(
      if(input$linea == "LB"){
        paste0("Ingreso mayor: ", maxims[[1]][3],"$ por venta de ", maxims[[1]][2],".")
      }else if(input$linea == "LA"){
        paste0("Ingreso mayor: ", maxims[[2]][3],"$ por venta de ", maxims[[2]][2],".")
      }else if(input$linea == "L1"){
        paste0("Ingreso mayor: ", maxims[[3]][3],"$ por venta de ", maxims[[3]][2],".")
      }else if(input$linea == "L2"){
        paste0("Ingreso mayor: ", maxims[[4]][3],"$ por venta de ", maxims[[4]][2],".")
      }else if(input$linea == "L3"){
        paste0("Ingreso mayor: ", maxims[[5]][3],"$ por venta de ", maxims[[5]][2],".")
      }else if(input$linea == "L4"){
        paste0("Ingreso mayor: ", maxims[[6]][3],"$ por venta de ", maxims[[6]][2],".")
      }else if(input$linea == "L5"){
        paste0("Ingreso mayor: ", maxims[[7]][3],"$ por venta de ", maxims[[7]][2],".")
      }else if(input$linea == "L6"){
        paste0("Ingreso mayor: ", maxims[[8]][3],"$ por venta de ", maxims[[8]][2],".")
      }else if(input$linea == "L7"){
        paste0("Ingreso mayor: ", maxims[[9]][3],"$ por venta de ", maxims[[9]][2],".")
      }else if(input$linea == "L8"){
        paste0("Ingreso mayor: ", maxims[[10]][3],"$ por venta de ", maxims[[10]][2],".")
      }else if(input$linea == "L9"){
        paste0("Ingreso mayor: ", maxims[[11]][3],"$ por venta de ", maxims[[11]][2],".")
      }else if(input$linea == "L12"){
        paste0("Ingreso mayor: ", maxims[[12]][3],"$ por venta de ", maxims[[12]][2],".")
      }
    )
    
#---- fecha maximos ---- 
    output$maximo_f <- renderText(
      if(input$linea == "LB"){
        paste0("Fecha: ", maxims[[1]][1],".")
      }else if(input$linea == "LA"){
        paste0("Fecha: ", maxims[[2]][1],".")
      }else if(input$linea == "L1"){
        paste0("Fecha: ", maxims[[3]][1],".")
      }else if(input$linea == "L2"){
        paste0("Fecha: ", maxims[[4]][1],".")
      }else if(input$linea == "L3"){
        paste0("Fecha: ", maxims[[5]][1],".")
      }else if(input$linea == "L4"){
        paste0("Fecha: ", maxims[[6]][1],".")
      }else if(input$linea == "L5"){
        paste0("Fecha: ", maxims[[7]][1],".")
      }else if(input$linea == "L6"){
        paste0("Fecha: ", maxims[[8]][1],".")
      }else if(input$linea == "L7"){
        paste0("Fecha: ", maxims[[9]][1],".")
      }else if(input$linea == "L8"){
        paste0("Fecha: ", maxims[[10]][1],".")
      }else if(input$linea == "L9"){
        paste0("Fecha: ", maxims[[11]][1],".")
      }else if(input$linea == "L12"){
        paste0("Fecha: ", maxims[[12]][1],".")
      }
    )
    
    
      
  }



# Run the application 
shinyApp(ui = u, server = s)
