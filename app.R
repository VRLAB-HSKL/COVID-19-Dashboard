#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(rgdal)
library(shinydashboard)
library(tigris)
library(plotly)
library(tidyverse)
library(dplyr)
title <- tags$a(href='https://www.hs-kl.de/', target="_blank", style = "color: rgb(255,255,255); text-align: bottom",
                tags$img(src= "https://upload.wikimedia.org/wikipedia/commons/5/5e/Logo_of_Hochschule_Kaiserslautern.png",height= '40', width= '76.8',style ="vertical-align: top"),
                'CODA-19')

values <- rbind(c('Salaries', 'Office', 'Merchandise', 'Legal', '<b>TOTAL<br>EXPENSES</b>'), c("Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad", 
                                                                                               "Lorem ipsum dolor sit amet, tollit discere inermis pri ut. Eos ea iusto timeam, an prima laboramus vim. Id usu aeterno adversarium, summo mollis timeam vel ad"))

fig <- plot_ly(
  type = 'table',
  height = 800,
  columnorder = c(1,2),
  columnwidth = c(10,90),
  header = list(
    values = c('<b>Keyword</b>', '<b>DESCRIPTION</b>'),
    line = list(color = '#506784'),
    fill = list(color = '#119DFF'),
    align = c('left','center'),
    font = list(color = 'white', size = 12),
    height = 40
  ),
  cells = list(
    values = rbind(
      t(as.matrix(unname(glossar)))
    ),
    line = list(color = '#506784'),
    fill = list(color = c('#25FEFD', 'white')),
    align = c('left', 'left'),
    font = list(color = c('#506784'), size = 12),
    height = 30
  ))


# Define UI for application that draws a histogram
ui <- dashboardPage(
    title ="Hochschule Kaiserslautern Covid-19 Dashboard",
    skin = "blue",
    # Application title
    dashboardHeader(title=title),
    dashboardSidebar(
      sidebarMenu(
        menuItem("World Map", tabName = "Map", icon = icon("map")),
        menuItem(icon=icon("calendar"),timeslider<-sliderInput("Times",
                              "Time Series",
                              min = min(data_from_github$Date),
                              max = max(data_from_github$Date),
                              value = min(data_from_github$Date),
                              timeFormat = "%d %b %Y")),
        menuItem(icon=icon("globe"),country<-selectInput("country", "Choose a country:",
                             list(`Country` = unique(data_from_github$Country)))),
        htmlOutput("States"),
        menuItem("Statistic", tabName = "statistic", icon = icon("chart-line")),
        menuItem("Forecasts", tabName = "forecasts", icon = icon("chart-line")),
        
        menuItem("Glossar", icon = icon("list"), tabName = "glossar",
                  badgeColor = "green"),
        menuItem("Sources", icon = icon("book"), tabName = "sources",
                 badgeLabel = "new", badgeColor = "green"),
        menuItem("Imprint", icon = icon("info"), tabName = "imprint",
                 badgeLabel = "new", badgeColor = "green")
      )
    ),
    dashboardBody(
      tabItems(
      tabItem(tabName = "Map",
        
        fluidRow(box(title = "World Map",
                     width = 80,
                     height = '65vh',
                     
                     leafletOutput(outputId = "mymap",height = '60vh')),
                 align ="center"),
        fluidRow(box(title="Global Active Cases",
                     width = 3,
                     textOutput("GLA"),
                     tags$head(tags$style(
                       "#GLA{font-size: 32px;
                             font-weight: bold;
                             color: red;
                       }"
                     ))),
                 box(title="Global Infected",
                     width= 3,
                     textOutput("GLI"),
                     tags$head(tags$style(
                       "#GLI{font-size: 32px;
                             font-weight: bold;
                       }"))),
                 box(title="Global Recovered",
                     width = 3,
                     textOutput("GLR"),
                     tags$head(tags$style(
                       "#GLR{font-size: 32px;
                             font-weight: bold;
                       }"))),
                 box(title="Global Deceased",
                     width = 3,
                     textOutput("GLD"),
                     tags$head(tags$style(
                       "#GLD{font-size: 32px;
                             font-weight: bold;
                       }"))),
                 align="center")
      ),
      tabItem(tabName = "statistic",
              fluidRow(box(textOutput("StatsTitle"),
                           tags$head(tags$style("#StatsTitle{font-size: 2vw;
                                            font-weight: bold;}")),
                           width = 12,
                           height = '10vh'),
                       align = "center"),
              splitLayout(cellWidths = c("50%","50%"),
                          plotlyOutput("plot1", height = '40vh'),
                          tabBox(title = "Epidemiological measures",
                                 width = 12,
                                 height = '40vh',
                                 tabPanel("Prevelance",  plotlyOutput("prevelance")),
                                 tabPanel("All Case Mortality",plotlyOutput("allcasemort")),
                                 tabPanel("Case Fatality Rate",plotlyOutput("casefatalityrate")))),
              fluidRow(box(title = "Active Cases",
                           width = 3,
                           textOutput("LA"),
                           tags$head(tags$style(
                             "#LA{font-size: 32px;
                             font-weight: bold;
                             color: red}"))),
                       box(title="Infected",
                           width= 3,
                           textOutput("LI"),
                           tags$head(tags$style(
                             "#LI{font-size: 32px;
                             font-weight: bold;
                       }"))),
                       box(title="Recovered",
                           width = 3,
                           textOutput("LR"),tags$head(tags$style(
                             "#LR{font-size: 32px;
                             font-weight: bold;
                       }"))),
                       box(title="Deceased",
                           width = 3,
                           textOutput("LD"),tags$head(tags$style(
                             "#LD{font-size: 32px;
                             font-weight: bold;
                       }"))),
                       align = "center")
          
          
      ),
      tabItem(tabName="forecasts",
              fluidRow(box(width = 12,
                           textOutput("FO"),
                           tags$head(tags$style("#FO{font-size: 2vw;
                                            font-weight: bold;}"))),
                       align = "center"),
              fluidRow(
                column(width = 10,
                       tabBox(title="Forecasts",
                              width = 10,
                              height='60vh',
                              tabPanel("Forecast Infected Cases",plotlyOutput("forecast_confirmed", height = '60vh')),
                              tabPanel("Forecast Deceased Cases", plotlyOutput("forecast_deaths", height = '60vh')),
                              tabPanel("Forecast Recovered Cases", plotlyOutput("forecast_recovered",height = '60vh')))),
                column(width = 2,
                       tags$style(type="text/css", "#report {background-color:orange;color: black;font-family: Courier New;height:30px;width:200px;}"),
                       downloadButton("report","Save Forecast")))
                
              
      ),
      tabItem(tabName = "glossar",
              
              #textOutput("selected_country"),
              HTML('<center><h2>Glossar</h2></center>'),
             # tableOutput('glossartable')
            fig
      ),
      tabItem(tabName = "sources",
              
              HTML('<center><h2>Sources</h2></center>'),
              tags$br(),
              
              HTML('<center><h2>Data from </h2></center>'),
              HTML('<center><h3><a href="https://github.com/datasets/covid-19" target="_blank">Covid 19 Data Github Repository</a></h3></center>'),
              
              tags$br(),
              
              HTML('<center><h2>Glossarinformation from </h2></center>'),
              HTML('<center><h3><a href="https://www.mittelbayerische.de/region/kelheim-nachrichten/diese-corona-begriffe-sollte-man-kennen-21029-art1900824.html" target="_blank">Mittelbayerischer Verlag KG</a></h3></center>'),
              HTML('<center><h3><a href="https://www.quarks.de/gesundheit/was-die-daten-zu-corona-aussagen-und-was-nicht/" target="_blank">Westdeutscher Rundfunk Köln</a></h3></center>'),
      ),
      tabItem(tabName = "imprint",
              HTML('<center><img src="https://upload.wikimedia.org/wikipedia/commons/5/5e/Logo_of_Hochschule_Kaiserslautern.png" height= "100" width= "200"></center>'),
              
             HTML('<center><h2>Project Covid-19-Dashboard</h2></center>'),
             
             tags$br(),
             
             HTML('<center><h4>Jens Cedric Schug </h4></center>'),
             HTML('<center><h4>Department of Computer Science and Microsystems Technology</h4></center>'),
             HTML('<center><h4>University of Applied Sciences Kaiserslautern</h4></center>'),
             HTML('<center><h4>Campus Zweibruecken, Germany</h4></center>'),
             HTML('<center><h4><a href="mailto:jesc0030@stud.hs-kl.de">jesc0030@stud.hs-kl.de</a></h4></center>'),
             
             tags$br(),
             
             HTML('<center><h4>Julian Bernhart </h4></center>'),
             HTML('<center><h4>Department of Computer Science and Microsystems Technology</h4></center>'),
             HTML('<center><h4>University of Applied Sciences Kaiserslautern</h4></center>'),
             HTML('<center><h4>Campus Zweibruecken, Germany</h4></center>'),
             HTML('<center><h4><a href="mailto:jube0010@stud.hs-kl.de">jube0010@stud.hs-kl.de</a></h4></center>'),
             
             tags$br(),
             
             HTML('<center><h4>Marco Miles Noll </h4></center>'),
             HTML('<center><h4>Department of Computer Science and Microsystems Technology</h4></center>'),
             HTML('<center><h4>University of Applied Sciences Kaiserslautern</h4></center>'),
             HTML('<center><h4>Campus Zweibruecken, Germany</h4></center>'),
             HTML('<center><h4><a href="mailto:mano0010@stud.hs-kl.de">mano0010@stud.hs-kl.de</a></h4></center>'),
             
             tags$br(),
             
             HTML('<center><h4>Supervisor </h4></center>'),
             
             tags$br(),
             
             HTML('<center><h4>Prof. Dr. Manfred Brill </h4></center>'),
             HTML('<center><h4>Department of Computer Science and Microsystems Technology</h4></center>'),
             HTML('<center><h4>University of Applied Sciences Kaiserslautern</h4></center>'),
             HTML('<center><h4>Campus Zweibruecken, Germany</h4></center>'),
             HTML('<center><h4><a href="https://www.hs-kl.de/hochschule/profil/personenverzeichnis/detailanzeige-personen/person/manfred-brill" target="_blank">About Manfred Brill</a></h4></center>'),
             HTML('<center><h4><a href="mailto:manfred.brill@hs-kl.de">manfred.brill@hs-kl.de</a></h4></center>'),

      )
      
    )
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$States <- renderUI({
    if(length(unique(Splitted_Global_DF[[input$country]]$Province))>1){
      menuItem(province <- selectInput("state",
                                       "States:",
                                       list(`State`= unique(Splitted_Global_DF[[input$country]]$Province))))  
    }
  })
  
    output$mymap <- renderLeaflet({
        tmp_data <- filter(data_from_github, data_from_github$Date == input$Times, data_from_github$Confirmed>0,data_from_github$Lat > 0 && data_from_github$Long > 0)
        qpal <- colorBin(palette = c("#FFD82E","#E8A115","#FF8E24","#E84D15","#FF1408","#761408","#5A1408"),domain = tmp_data$logarithmic, n=12)
        label <- paste("<b>",
                       tmp_data$Country,
                       "<b/><br>Confirmed: ",
                       tmp_data$Confirmed,
                       "<br>Deaths: ",
                       tmp_data$Deaths,
                       "<br>Recovered: ",
                       tmp_data$Recovered,
                       sep = "")
        leaflet("mymap",data = tmp_data)%>%
            addTiles(urlTemplate = "https://{s}.tile.openstreetmap.de/tiles/osmde/{z}/{x}/{y}.png")%>%
            clearShapes()%>%
            fitBounds(~min(Long),~min(Lat),~max(Long),~max(Lat))%>%
            addCircleMarkers(data = tmp_data,
                       lng = ~Long,
                       lat = ~Lat,
                       weight = 2,
                       opacity = 1,
                       radius = tmp_data$logarithmic,
                       color = ~qpal(tmp_data$logarithmic),
                       popup = label)%>%
            addLegend("bottomright",pal = qpal,values = tmp_data$Confirmed,title = "Log Scale",opacity = 0.2)
    })
   
    observe({
        tmp_data <- filter(data_from_github, data_from_github$Date == input$Times, data_from_github$Confirmed>0, data_from_github$Lat > 0 && data_from_github$Long > 0)
        qpal <- colorBin(palette = c("#FFD82E","#E8A115","#FF8E24","#E84D15","#FF1408","#761408","#5A1408"),domain = tmp_data$logarithmic, n=12)
        label <- paste("<b>",
                       tmp_data$Country,
                       "<b/><br>Confirmed: ",
                       tmp_data$Confirmed,
                       "<br>Deaths: ",
                       tmp_data$Deaths,
                       "<br>Recovered: ",
                       tmp_data$Recovered,
                       sep = "")
        leafletProxy("mymap",data = tmp_data)%>%
            clearShapes()%>%
            addCircleMarkers(data = tmp_data,
                       lng = ~Long,
                       lat = ~Lat,
                       weight = 2,
                       opacity = 1,
                       radius = tmp_data$logarithmic,
                       color = ~qpal(tmp_data$logarithmic),
                       popup = label)%>%
            fitBounds(~min(Long),~min(Lat),~max(Long),~max(Lat))
        countrydata<-filter(Splitted_Global_DF[[input$country]],Splitted_Global_DF[[input$country]]$Date <= input$Times)
        plottitle <- paste("Covid-19 Cases in ", input$country, "(Population:",countrydata$Population,")",sep = "")
        forecasttitle <- paste("Forecasts for ", input$country,sep = "")
        if(!is_null(input$state)){
          if(input$state %in% Splitted_Global_DF[[input$country]]$Province){
            countrydata <-filter(Splitted_Global_DF_States[[input$country]][[input$state]], Splitted_Global_DF_States[[input$country]][[input$state]]$Date <= input$Times)
            plottitle <- paste("Covid-19 Cases in ", input$country," (",input$state,")", "(Population:",countrydata$Population,")",sep = "")
            forecasttitle <- paste("Forecasts for ", input$country," (",input$state,")",sep = "")
          }
        }
        Forecast_Confirmed <- Forecasts_Confirmed[[input$country]][[1]]
        Forecast_Deaths <- Forecasts_Deaths[[input$country]][[1]]
        Forecast_Recovered <- Forecasts_Recovered[[input$country]][[1]]
        countryselected <- Splitted_Global_DF_States[[input$country]][[1]]
        if(!is_null(input$state)){
          if(input$state %in% Splitted_Global_DF[[input$country]]$Province){
            Forecast_Confirmed <- Forecasts_Confirmed[[input$country]][[input$state]]
            Forecast_Deaths <- Forecasts_Deaths[[input$country]][[input$state]]
            Forecast_Recovered <- Forecasts_Recovered[[input$country]][[input$state]]
            countryselected <- Splitted_Global_DF_States[[input$country]][[input$state]]
          }
        }
        #Generating output for global statistics 
        global_stats <- filter(data_from_github, data_from_github$Date == input$Times)
        output$GLA <- renderText({
          paste("",format(sum(unique(global_stats$active_cases)),big.mark = ".",decimal.mark = ","),sep="")
        })
        output$GLI <- renderText({
          paste("",format(sum(unique(global_stats$Confirmed)),big.mark = ".",decimal.mark = ","), sep="")
        })
        output$GLR <- renderText({
          paste("",format(sum(unique(global_stats$Recovered)),big.mark = ".", decimal.mark = ","), sep="")
        })
        output$GLD <- renderText({
          paste("",format(sum(unique(global_stats$Deaths)),big.mark = ".", decimal.mark = ","), sep="")
        })
        
        #Generating output for local statistics
        locale_stats <- filter(countrydata, countrydata$Date == input$Times)
        output$LA <- renderText({
          paste("",format(locale_stats$active_cases, big.mark = ".",decimal.mark = ","),sep="")
        })
        output$LI <- renderText({
          paste("",format(locale_stats$Confirmed, big.mark = ".", decimal.mark = ","), sep="")
        })
        output$LR <- renderText({
          paste("",format(locale_stats$Recovered,big.mark = ".", decimal.mark = ","), sep="")
        })
        output$LD <- renderText({
          paste("",format(locale_stats$Deaths,big.mark = ".",decimal.mark = ","), sep="")
        })
        
        #Generate title for stats page
        output$StatsTitle <- renderText({
          unique(plottitle)
        })
        
        #Local Stats  
        output$plot1<- renderPlotly({
            plot_ly(data = countrydata, x = ~countrydata$Date)%>%
            add_trace(y = ~countrydata$Confirmed, color = I("red"), mode = 'lines+markers', name = 'Infected Cases',type="scatter")%>%
            add_trace(y = ~countrydata$Recovered, color = I("chartreuse"), mode = 'lines+markers', name ='Recovered Cases',type="scatter")%>%
            add_trace(y = ~countrydata$Deaths, color = I("black"), mode = 'lines+markers', name = 'Deceased Cases',type="scatter")%>%
            add_trace(y = ~countrydata$active_cases, color = I("cyan"), mode = 'lines+markers', name = 'Active Cases',type = "scatter")%>%
            layout(xaxis = list(title="Date"), yaxis = list(title="People"))
            
        })
        #Prevelance plotly graph
        output$prevelance <- renderPlotly({
          
          plot_ly(data = countrydata, x = ~countrydata$Date)%>%
            add_trace(y = ~countrydata$prevelance_100k, mode='lines+markers', name='Prevelance',type="scatter")%>%
            layout(title = "Prevelance", xaxis = list(title="Date"), yaxis = list(title="Prevelance (per 100.000 People)"))
        })
        # all case mortality plotly graph
        output$allcasemort <- renderPlotly({
          plot_ly(data = countrydata, x = ~countrydata$Date)%>%
            add_trace(y = ~countrydata$all_case_mortality_100k, mode='lines+markers', name='All Case Mortality',type="scatter")%>%
            layout(title = "All Case Mortality", xaxis = list(title="Date"), yaxis = list(title="All Case Mortality"))
        })
        # case fatality rate plotly graph
        output$casefatalityrate <- renderPlotly({
          plot_ly(data = countrydata, x = ~countrydata$Date)%>%
            add_trace(y = ~countrydata$case_fatality_rate, mode='lines+markers', name='Case Fatality Rate',type="scatter")%>%
            layout(title = "Case Fatality Rate", xaxis = list(title="Date"), yaxis = list(title="Case Fatality Rate"))
        })
        
        #Generate title for forecast page
        output$FO <- renderText({
          unique(forecasttitle)
        })
        
        # forecast for Confirmed Cases plotly graph
        output$forecast_confirmed <- renderPlotly({
          plot_ly(data = as.data.frame(Forecast_Confirmed))%>%
            add_lines(x = ~countryselected$Date, y = ~countryselected$Confirmed, color=I("red"), name="Confirmed Cases")%>%
            add_ribbons(x = ~Forecasts.date, ymin = ~round(Forecast_Confirmed$"Lo 95",digits=0), ymax = ~round(Forecast_Confirmed$"Hi 95", digits = 0), color=I("gray95"), name ="95% confidence")%>%
            add_ribbons(x = ~Forecasts.date, ymin = ~round(Forecast_Confirmed$"Lo 80",digits=0), ymax = ~round(Forecast_Confirmed$"Hi 80", digits = 0), color=I("gray80"), name ="80% confidence")%>%
            add_lines(x = ~Forecasts.date, y = ~round(Forecast_Confirmed$"Point Forecast", digits = 0), color=I("blue"), name="prediction")%>%
            layout(title="Forecast Infected Cases", xaxis = list(title="Date"), yaxis = list(title="Infected Cases"))
        })
        # forecast for death cases plotly graph
        output$forecast_deaths <- renderPlotly({
          plot_ly(data = as.data.frame(Forecast_Deaths))%>%
            add_lines(x = ~countryselected$Date, y = ~countryselected$Deaths, color=I("black"), name="Death Cases")%>%
            add_ribbons(x = ~Forecasts.date, ymin = ~round(Forecast_Deaths$"Lo 95", digits = 0), ymax = ~round(Forecast_Deaths$"Hi 95",digits=0), color=I("gray95"), name ="95% confidence")%>%
            add_ribbons(x = ~Forecasts.date, ymin = ~round(Forecast_Deaths$"Lo 80", digits = 0), ymax = ~round(Forecast_Deaths$"Hi 80",digits=0), color=I("gray80"), name ="80% confidence")%>%
            add_lines(x = ~Forecasts.date, y = ~round(Forecast_Deaths$"Point Forecast", digits=0), color=I("blue"), name="prediction")%>%
            layout(title="Deceased Death Cases", xaxis=list(title="Date"), yaxis=list(title="Deceased Cases"))
        })
        
        #forecast for resurrected cases plotly graph
        output$forecast_recovered <- renderPlotly({
          plot_ly(data = as.data.frame(Forecast_Recovered))%>%
            add_lines(x = ~countryselected$Date, y = ~round(countryselected$Recovered, digits = 0), color=I("green"), name="Recovered Cases")%>%
            add_ribbons(x = ~Forecasts.date, ymin = ~round(Forecast_Recovered$"Lo 95",digits = 0), ymax = ~round(Forecast_Recovered$"Hi 95", digits = 0), color=I("gray95"), name ="95% confidence")%>%
            add_ribbons(x = ~Forecasts.date, ymin = ~round(Forecast_Recovered$"Lo 80",digits = 0), ymax = ~round(Forecast_Recovered$"Hi 80", digits = 0), color=I("gray80"), name ="80% confidence")%>%
            add_lines(x = ~Forecasts.date, y = ~round(Forecast_Recovered$"Point Forecast", digits = 0), color=I("blue"), name="prediction")%>%
            layout(title="Forecast Recovered Cases", xaxis =list(title="Date"), yaxis=list(title="Recovered Cases"))
        })
        
        output$selected_country <- renderText({ 
          paste("You have selected", input$country)
        })
        output$report <- downloadHandler(
          filename =  "Forecast-Report.html",
          content <- function(file){
            tempReport <- file.path(tempdir(),"Forecast.rmd")
            file.copy("Forecast.rmd",tempReport,overwrite = TRUE)
            exportstate <- NA
            if(!is.null(input$state))
              if(input$state %in% Splitted_Global_DF[[input$country]]$Province)
                exportstate <- unique(input$state)
            params <- list(Infected = Forecast_Confirmed,
                           Recovered = Forecast_Recovered,
                           Deceased = Forecast_Deaths,
                           Country = unique(countrydata$Country),
                           State = exportstate,
                           DownloadDate = Sys.Date(),
                           ForecastDates = Forecasts.date)
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent=globalenv())
                              )
          }
        )
        
    })
  
    
  
    
    
 
    
  #  output$glossartable<-renderTable(glossar)
}

# Run the application 
shinyApp(ui = ui, server = server)

