pacman::p_load(shiny,tidyverse,ggplot2,readxl,
sf,leaflet,htmlwidgets,shinydashboard)

Base <- read_sf("BaseMuertes.shp",options = "ENCODING=WINDOWS-1252")

Base <- Base[, c(1:4,6)]
Sexo <- c("Total","Hombre","Mujer","No especificado")

Tipo_Muerte <- c("Total","Accidente","Homicidio","Suicidio","Int. legales y guerra","Se ignora")
Base <- na.omit(Base)

ui <- dashboardPage(
  dashboardHeader(title = "Muertes en México"),
  dashboardSidebar(width = 300,
                   tags$a(href = "https://www.inegi.org.mx/programas/mortalidad/#Tabulados", "Página de INEGI",target = "_blank"),
                   h5("Mapa de mortalidad en México, Elaboración Propia con datos del INEGI; Defunciones registradas por tipo de defunción."),
                   selectInput("Sexo",label = "Selecciona el Sexo", choices = Sexo),
                   selectInput("Tipos",label = "Selecciona el Tipo de Muerte",choices = Tipo_Muerte)),
  dashboardBody(
    tags$head(tags$style(HTML('.small-box .icon-large {top: 5px;}'))),
    tabPanel("Map" ),
    fluidRow(
      column(width=9,  
             valueBoxOutput("valuebox",width = 4),
             valueBoxOutput("valuebox2",width = 6),
             fluidRow(box(title="Muertes Violentas en México 2021",leafletOutput("map", width = "100%", height = 500), width=12, height = 600, status="primary", solidHeader=TRUE)))
    )
  )
)








# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$secondSelection <- renderUI({
    choice_second <- Tipo_Muerte
    selectInput(inputId = "Tipos", choices = choice_second,
                label = "Elige el Tipo de Muerte")
  })
  
  filtro <- reactive({
    req(input$Tipos)
    a <- Base %>% filter(Sexo == input$Sexo)
    m <- a %>% filter(Tipo == input$Tipos)
    return(m)
    
    
    
  })
  
  filEnt <- reactive({
    req(input$Tipos)
    a <- Base %>% filter(Sexo == input$Sexo)
    y <- a %>% filter(Tipo == input$Tipos)
    return(filter(y, Personas == max(Personas)))
    
    
    
  })
  
  
  
  
  
  output$valuebox <- renderValueBox({valueBox(sum(filtro()$Personas),"Total de defunciones"
                                              ,color="red")})
  
  output$valuebox2 <- renderValueBox({valueBox(filEnt()$Entidad,"Entidad con mayor número de defunciones"
  )})  
  
  output$map <- renderLeaflet({
    
    pal <- colorBin(palette = "OrRd",6, domain = filtro()$Personas)
    
    etique <- sprintf(
      "<strong>%s</strong><br/>Numero de fallecimientos: %s",
      filtro()$Entidad, filtro()$Personas) %>%
      lapply(htmltools::HTML)
    
    
    leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lng = -103.12766, lat =23.42847 , zoom = 4.5) %>%
      addPolygons(data = filtro(),
                  stroke = TRUE,
                  weight= 0.5,
                  label = etique,
                  group = "Entidad",
                  fillOpacity = .7,
                  fillColor = ~pal(filtro()$Personas),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "Black",
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = Base$Personas,
                title = "Numero de observaciones",
                opacity = 0.7)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)