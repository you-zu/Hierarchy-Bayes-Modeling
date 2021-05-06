library(shiny)
library(tidyverse)
library(leaflet)
library(maps)
library(maptools)
library(RColorBrewer)
library(threejs)
library(DT)
library(classInt)
library(ggplot2)
library(rgdal)
library(coda)
library(ggrepel)
library(dplyr)
library(hrbrthemes)

options(digits=13)

dataset <- read.csv("E:/pubh 8472/final project/std.csv")
dataset[is.na(dataset)] = 0
dataset %>% 
  filter(Disease == "Chlamydia") %>%
  filter(County != "California") %>%
  select(County, Year, Sex, Cases, Population, Rate) %>% 
  mutate(County = tolower(County)) %>%
  mutate(Year = as.integer(Year)) %>%
  arrange(County, Year)-> dataset
dataset = as.data.frame(dataset)


ca.map2 = map_data(map="county", region="California")
dataset.gis = aggregate(cbind(long, lat)~subregion, data=ca.map2, FUN=function(x) mean(x))
names(dataset.gis) = c("county","lon","lat")
#setequal(dataset.gis$subregion, unique(dataset$County))

dataset2 = left_join(dataset, dataset.gis, by=c("County" = "county"))

ca.map = maps::map("county", regions ="California" , fill = TRUE, plot = FALSE)
ca.map$names = substring(ca.map$names, first=12)
ca.sp = map2SpatialPolygons(ca.map, IDs=ca.map$names)
Name = unique(dataset2$County)
dataset.sp = SpatialPolygonsDataFrame(ca.sp, data.frame(id=Name, row.names = Name))
california.shp = readShapePoly("E:/pubh 8472/final project/ca-county-boundaries/CA_Counties_TIGER2016.shp", proj4string=CRS("+proj=longlat"))
# california.shp.for = fortify(california.shp)
dataset2$County = as.factor(dataset2$County)
dataset2$Sex = as.factor(dataset2$Sex)
##all_disease = levels(dataset2$Disease)
all_year = levels(as.factor(dataset2$Year))
all_sex = levels(dataset2$Sex)
all_county = levels(dataset2$County)

dataset2 = arrange(dataset2, County)


c = 1
ID = as.numeric(nrow(dataset2))
ID[1] = c

for(i in 2:nrow(dataset2)){
  if(dataset2$County[i] != dataset2$County[i-1]) c = c+1
  ID[i] = c
}

dataset2 = cbind(dataset2, ID)

dataset2$ID = as.character(dataset2$ID)

######################### UI #########################
######################################################
ui <- function(){
  bootstrapPage("",
                
                navbarPage("Chlamydia Mapping",
                           tabPanel("Intro"),
                           tabPanel("Data", 
                                    fluidRow(
                                      ##column(3, selectInput(inputId="disease01", label="Disease", choices=all_disease)),
                                      column(3, selectInput(inputId="year01", label="Year", choices=all_year)),
                                      column(3, selectInput(inputId="sex01", label="Gender", choices=all_sex, selected="Total"))
                                    ),
                                    
                                    fluidRow(column(3, actionButton(inputId="button01", label="Show"))),
                                    
                                    hr(),
                                    DT::dataTableOutput(outputId="DataTable")
                           ),
                           
                           tabPanel("Plot",
                                   ## fluidRow(column(3, selectInput(inputId="disease02", label="Select a Disease", choices=all_disease))),
                                    
                                   fluidRow(column(3, selectInput(inputId="county02", label="Select a County", choices=all_county))),
                                   
                                   radioButtons(inputId="measure02", label="Select a Measure:", choices=c("Cases","Rate"), inline=T),
                                   
                                    fluidRow(column(3, actionButton(inputId="button02", label="Show"))),
                                    
                                    hr(),
                                    
                                    plotOutput("plot1")
                           ),
                           
                           
                           tabPanel("Top 5 Counties",
                                    
                                    fluidRow(column(3, selectInput(inputId = "sex03", label = "Select a Gender", choices=all_sex))),
                                    
                                    radioButtons(inputId="measure03", label="Select a Measure:", choices=c("Cases","Rate"), inline=T),
                                    
                                    fluidRow(column(3, actionButton(inputId="button03", label="Show"))),
                                    
                                    hr(),
                                    
                                    plotOutput("plot2")
                           ),
                           
                           
                           tabPanel("2D Static Map",
                                  
                                    fluidRow(column(3, selectInput(inputId = "sex04", label = "Select a Gender", choices=all_sex))),
                                    
                                    fluidRow(column(3, selectInput(inputId = "year04", label = "Select a Year", choices=all_year))),
                                    
                                    radioButtons(inputId="measure04", label="Select a Measure:", choices=c("Cases","Rate"), inline=T),
                                    
                                    fluidRow(column(3, actionButton(inputId="button04", label="Show"))),
                                    
                                    hr(),
                                    
                                    plotOutput("plot3")
                           ),
                                    # absolutePanel(id="controls", fixed=T, draggable=T, 
                                    #               bottom=40, right="auto", left=30, top="auto", width=330, height="auto",
                                    #               
                                    #               h2("Disease explorer"),
                                    #               
                                    #               ## selectInput(inputId="disease03", label="Select a Disease:", choices=all_disease, selected="Amebiasis"),
                                    #               
                                    #               selectInput(inputId="sex03", label="Select a Gender:", choices=all_sex, selected="Total"),
                                    #               
                                    #               sliderInput(inputId="year03", label="Select a Year:", min=2001, max=2018, value=2001, sep=""),
                                    #           
                                    #               
                                    #               radioButtons(inputId="measure03", label="Select a Measure:", choices=c("Cases","Rate"), inline=T),
                                    #               
                                    #               actionButton(inputId="button03", label="Create"))),
                          
                           tabPanel("2D Dynamic Map",
                                    
                                    leafletOutput("map2d2", height="100vh"),
                                    
                                    absolutePanel(id="controls05",  fixed=T, draggable=T, 
                                                  bottom=20, left=35, right="auto", top="auto",width=250, height="auto",
                                                  
                                                  h2("Disease explorer"),
                                                  
                                                  ##selectInput(inputId="disease04", label="Select a Disease:", choices=all_disease, selected="Amebiasis"),
                                                  
                                                  selectInput(inputId="sex05", label="Select a Gender:", choices=all_sex, selected="Total"),
                                                  
                                                  radioButtons(inputId="measure05", label="Select a Measure:", choices=c("Cases","Rate"), inline=T),
                                                  
                                                  radioButtons(inputId="marker05", label="Select a Maker:", choices=c("Polygon","Circle"), selected="Polygon", inline=T),
                                                  
                                                  actionButton(inputId="button05", label="Create"),
                                                  
                                                  hr(),
                                                  
                                                  sliderInput(inputId="year05", label="Select a Year:", 
                                                              min=2001, max=2018, value=2001, step=0.25, sep="", 
                                                              animate = animationOptions(interval = 150, loop = FALSE, playButton = NULL,
                                                                                         pauseButton = NULL))))
                           
              
                ),
                
                tags$style(type = 'text/css', '.navbar { background-color: #c5b8a5;
                           font-family: Arial;
           font-size: 16px;
           color: #2F4F4F; }',
                           
                           '.navbar-dropdown { background-color: #DAA520;
           font-family: Arial;
           font-size: 13px;
           color: #FF0000; }',
                           
                           '.navbar-default .navbar-brand {
           font-size: 20px;
           color: #d8caaf;}'
                           
                ))
}


######################### Server ############################
############################################################
server <- function(input, output, session){
  
  ######################### Data Table #########################
  data.table = eventReactive(input$button01, {
    dataset2 %>%
      filter(Year==input$year01, Sex==input$sex01) %>%
      select(County, Cases, Population, Rate)
  })
  
  output$DataTable <- DT::renderDataTable({
    DT::datatable(data = data.table(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  
  
  ####################### ggplot #######################
  data.plot = eventReactive(input$button02, {
    dataset2 %>% filter(County == input$county02) %>% 
      select(Year, input$measure02, Sex)
  })
  
  measureBy = eventReactive(input$button02, {input$measure02})
  
  observeEvent(input$button02, {
  output$plot1 = renderPlot({
    measures = data.plot()[, measureBy()]
    Max = max(data.plot()[,"Year"])
    Min = min(data.plot()[,"Year"])
    
    ggplot(data=data.plot(), aes(x=Year, y=measures, group=Sex)) +
      geom_line(aes(color=Sex)) +
      geom_point(aes(color=Sex)) +
      scale_x_continuous(breaks=seq(Min, Max, by = 5)) + 
      theme_minimal(base_size=20)
    
    # ggplot(data=data.plot(), aes(x=Year, y=Rate, group=Sex)) +
    #   geom_line(aes(color=Sex)) +
    #   geom_point(aes(color=Sex)) +
    #   scale_x_continuous(breaks=seq(Min, Max, by = 1)) + 
    #   theme_minimal(base_size=20)
  })
  })
  
  
 ######################### Top 5 Counties #########################
 data.plot2 = eventReactive(input$button03, {
   dataset2 %>% filter(Sex==input$sex03) %>% 
           select(Year, Cases, Rate, County) 
 })
 
  measureBy2 = eventReactive(input$button03, {input$measure03})
  
  observeEvent(input$button03, {
   output$plot2 = renderPlot({
     
     d1 = data.plot2()
     d1$measure2 = data.plot2()[, measureBy2()]
     n =   d1 %>% 
       filter(Year == 2018) %>% 
       top_n(5, measure2) %>%
       arrange(desc(measure2))
     
   d = d1 %>% filter(County %in% n$County) 
   # d$measure2 = d[, measureBy2()]
   ggplot(data = d, aes(x=Year, y =measure2, color = County))+
     geom_line() + 
     geom_text_repel(aes(label=County), function(d) d[d$Year == 2018, ]) +
     theme_minimal(base_size=14) +
     theme(legend.position = "none")
   })
 })
 
  
 ######################### 2D Static Map #########################
 data.mapS = eventReactive(input$button04,{
   x = dataset2 %>%
     filter(Sex==input$sex04, Year==input$year04) %>%
     select(County, Cases, Population, Rate, lat, lon) %>% 
     rename(NAME = County) %>% 
     left_join(california.shp@data, by = "NAME")
 })
 
 measureBy3 = eventReactive(input$button04, {input$measure04})
 observeEvent(input$button04, {
   output$plot3 = renderPlot({
     measures3 = data.mapS()[,measureBy3()]
     california.shp@data = data.mapS()
     nclr = 7
     plotclr = brewer.pal(nclr,"Blues")
     class = classIntervals(measures3, nclr, style="equal", dataPrecision=0)
     colcode = findColours(class, plotclr)
     plot(california.shp)
     plot(california.shp, col=colcode, add=T)
     title(main="Chlamydia - Rate per 100")
     legend("bottomleft", legend=names(attr(colcode, "table")),
            fill=attr(colcode, "palette"), cex=0.6, bty="n")
   })
 })
 
 
  # measureBy = eventReactive(input$button03, {input$measure03})
  # 
  # observeEvent(input$button03, {
  #   
  #   output$map2d = renderLeaflet({
  #     
  #     measures = data.mapS()[,measureBy()]
  #     nc_pal = colorNumeric("YlOrRd", domain = log(measures+1)+100)
  #     
  #     palette2 <- rev(brewer.pal(5, "YlOrRd"))
  #     nc_pal2 = colorNumeric(palette = palette2, domain = log(measures+1)+100)
  #     
  #     
  #     if(measureBy() == "Rate"){r = (measures+1)^(4/5)+5}
  #     else{ r = (measures+1)^(1/2)+5}
  #     
  #     dataset.sp@data = data.mapS()
  #     dataset.sp@data$type = ifelse(measures > 0, "disease", "nondisease")
  #     dataset.sp@data$type = as.factor(dataset.sp@data$type)
  #     
  #     diseaseIcon = awesomeIconList(
  #       disease = makeAwesomeIcon(icon="sad", library = "ion", iconColor = 'black', markerColor="lightred"),
  #       nondisease = makeAwesomeIcon(icon="happy",  library = "ion", iconColor = 'black', markerColor="lightgreen")
  #     )
  #     
  #     dataset.sp %>% 
  #       leaflet(options=leafletOptions(zoomSnap=0.1,zoomDelta=0.1)) %>% 
  #       addTiles() %>%
  #       addProviderTiles("Wikimedia", group = "Wikimedia") %>%
  #       addProviderTiles("Esri.WorldTopoMap", group = "Esri.WorldTopoMap") %>%
  #       setView(lat=37, lng=-121, zoom=6.6) %>%
  #       addAwesomeMarkers(lng=~lon, lat=~lat, icon=~diseaseIcon[type],
  #                         #label=~paste0("by Icon", County, measures), 
  #                         group="icon",
  #                         popup= ~paste("<b>County:</b>", County, "<br>", "<b>Measure:</b>", measures)) %>%
  #       addPolygons(weight=1, color=~nc_pal(log(measures+1)+100), fillOpacity=0.5,
  #                   #label = ~paste0("by Polygon", County, measures), 
  #                   group = "polygon",opacity = 1,
  #                   highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE),
  #                   popup= ~paste("<b>County:</b>", County, "<br>", "<b>Measure:</b>", measures)) %>%
  #       addCircleMarkers(data = dataset.sp@data, radius=r, color = ~nc_pal2(log(measures+1)+100),
  #                        #label = ~paste("by Circle", County, measures), 
  #                        popup= ~paste("<b>County:</b>", County, "<br>", "<b>Measure:</b>", measures),
  #                        group = "circle", stroke=F, fillOpacity=1) %>%
  #       addLayersControl(baseGroups = c("OSM","Wikimedia", "Esri.WorldTopoMap"),
  #                        overlayGroups = c("polygon","circle", "icon"),
  #                        options = layersControlOptions(collapsed = FALSE))
  #   })
    
    

  
  
  ######################### 2D Dynamic Map #########################
  
  data.mapD = eventReactive(input$button05, {
    x = dataset2 %>%
      filter(Sex==input$sex05) %>%
      select(County, Year, Cases, Population, Rate, lat, lon)
  })
  
  measureBy05 = eventReactive(input$button05, {input$measure05})
  
  #layerBy3 = eventReactive(input$button3, {input$layer3})
  
  data.mapD1 = eventReactive(input$year05, {
    x = data.mapD () %>% filter(Year == floor(input$year05))
  })
  
  data.mapD1.1 = eventReactive(input$year05, {
    if(input$year05 < 2018){
      x = data.mapD () %>% filter(Year == floor(input$year05)+1)
    } else{
      x = NULL
    }
  })
  
  output$map2d2 <- renderLeaflet({
    leaflet(options = leafletOptions(zoomSnap = 0.1, zoomDelta = 0.1)) %>% 
      addProviderTiles("CartoDB") %>% 
      setView(lat=37, lng=-121, zoom=6.6)
  })
  
  observeEvent(input$button05, {
    data.mapD2 = data.mapD() %>% filter(Year == 2001)
    
    measures = data.mapD2[,measureBy05()]
    nc_pal = colorNumeric("YlOrRd", domain = log(measures+1)+100)
    
    palette2 <- rev(brewer.pal(5, "YlOrRd"))
    nc_pal2 = colorNumeric(palette = palette2, domain = log(measures+1)+100)
    
    if(measureBy05() == "Rate"){r = (measures+1)^(4/5)+5}
    else{ r = (measures+1)^(1/2)+5}
    
    dataset.sp@data = data.mapD2
    
    if(input$marker05 == "Polygon"){
      leafletProxy("map2d2", data = dataset.sp) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addPolygons(weight=1, color=~nc_pal(log(measures+1)+100), fillOpacity=1,
                    label = ~paste0("County: ", County, " Measure: ", measures),
                    opacity = 0.5,
                    #popup= ~paste("<b>County:</b>", County, "<br>", "<b>Measure:</b>", measures),
                    highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE))
    } else{
      leafletProxy("map2d2", data = dataset.sp) %>%
        clearShapes() %>%
        clearMarkers() %>%
        addCircleMarkers(data = dataset.sp@data, radius=r, color = ~nc_pal2(log(measures+1)+100),
                         label = ~paste0("County: ", County, " Measure: ", measures),
                         #popup= ~paste("<b>County:</b>", County, "<br>", "<b>Measure:</b>", measures),
                         group = "circle", stroke=F, fillOpacity=1)
    }
    
  })
  
  
  observeEvent(input$year05, {
    if(input$year05 < 2018){
      measures = data.mapD1()[,measureBy05()] + (data.mapD1.1()[,measureBy05()] - data.mapD1()[,measureBy05()]) * (input$year05 - floor(input$year05))
    } else{
      measures = data.mapD1()[,measureBy05()]
    }
    
    nc_pal = colorNumeric("YlOrRd", domain = log(measures+1)+100)
    
    palette2 <- rev(brewer.pal(5, "YlOrRd"))
    nc_pal2 = colorNumeric(palette = palette2, domain = log(measures+1)+100)
    
    if(measureBy05() == "Rate"){r = (measures+1)^(4/5)+5}
    else{ r = (measures+1)^(1/2)+5}
    
    dataset.sp@data = data.mapD1()
    
    if(input$marker05 == "Polygon"){
      leafletProxy("map2d2", data = dataset.sp) %>%
        #clearShapes() %>%
        #clearMarkers() %>%
        addPolygons(weight=1, color=~nc_pal(log(measures+1)+100), fillOpacity=1,
                    label = ~paste0("County: ", County, " Measure: ", measures),
                    #popup= ~paste("<b>County:</b>", County, "<br>", "<b>Measure:</b>", measures),
                    opacity = 0.1,
                    highlight = highlightOptions(weight = 5, color = "white", bringToFront = TRUE))
    }else{
      leafletProxy("map2d2", data = dataset.sp) %>%
        #clearShapes() %>%
        #clearMarkers() %>%
        addCircleMarkers(data = dataset.sp@data, radius=r, color = ~nc_pal2(log(measures+1)+100),
                         label = ~paste0("County: ", County, " Measure: ", measures),
                         #popup= ~paste("<b>County:</b>", County, "<br>" ,"<b>Measure:</b>", measures),
                         group = "circle", stroke=F, fillOpacity=1)
    }
    
  })
} # server



shinyApp(ui = ui, server = server)

