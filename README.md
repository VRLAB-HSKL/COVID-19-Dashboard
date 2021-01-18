# COVID-19-Shiny-Dashboard

1. [Introduction](#indroduction)
1. [Components](#components)
    1. [app.R](#app)
    1. [libraries.R](#libraries)
    1. [prepareData.R](#data)
    1. [setWorkingDirectory.R](#wd)
    1. [startShinyServer.R](#server)
    1. [Run.bat](#run)
1. [Dashboard](#dashboard)
    1. [World Map](#world_map)
    1. [Statistics](#statistics)
    1. [Forecast](#forecast)
    1. [Glossar](#glossar)
1. [Sources](#sources)
1. [Imprint](#imprints)

<a name="introduction"></a>
## Indroduction
A student project in the summer term 2020 for the University of Applied Sciences of Kaiserslautern at campus Zweibrücken. 
The project provides a dashboard which visualizes the data for the actual global Covid-19-situation. The data is provided by the John Hopkins University.

The dashboard is implemented using R, Shiny and plotly.
<a name="components"></a>
## Components

<a name="app"></a>
### app.R
The main file of the Shiny application that builds the dashboard. This script contains the code for user interface and server. Open it in RStudio and use *Run App*.

<a name="data"></a>
### prepareData.R
This script contains the necessary steps to get the data in an appropriate form for plotting. Therefor data frames are created and their data types adjusted, in order to plot them. These data frames are then stored in a .RData file for further use.

<a name="wd"></a>
### setWorkingDirectory.R
Uses the path information provided by the Run.bat script, in order to set the working directory to the location this script remains.

<a name="server"></a>
### startShinyServer.R
Runs app.R by using the .RData file.

<a name="run"></a>
### Run.bat
The batch file that acts as a pipeline for creating the dashboard. This provides the information about their storage location and that of Rscript.exe via corresponding variables. Further this script downloads the data from a repository and uses the R scripts in order to start a server with the resulting dashboard.

<a name="dashboard"></a>
## Dashboard

<a name="world_map"></a>
### World Map
A bubble chart that shows on a world map the absolute number of reported cases of infection since the pandemic startet. Here, any point in time during the pandemic can be set, in order to display the corresponding global situation.

<a name="statistics"></a>
### Statistics
Histroy diagrams for important parameters of a pandemic.

<a name="forecast"></a>
### Forecast
Provides a possible forecast to predict the coming course.

<a name="glossar"></a>
### Glossar
Contains explanations of the most important epidemiological terms.
<a name="code-sources"></a>
### Code Sources
This is a list of all the R packages used and the source of code sections that were initially used:
1. R-Package shiny was used for the functionality of the dashboard: https://shiny.rstudio.com/
    1. In this project the team used the code segment "selectInput" for the country and state selection. The code was based on: https://shiny.rstudio.com/reference/shiny/latest/selectInput.html
1. R-Package shinydashboard was used for the Appearance of the dashboard: https://rstudio.github.io/shinydashboard/
    1. In this project the "Basic" Design was used: https://rstudio.github.io/shinydashboard/get_started.html
1. R-Package Leaflet was used for the World Map. As Code Base the team followed the instructions from the "Instruction", "Basemaps", "Markers" and "Colors" Segment from the original documentation: https://rstudio.github.io/leaflet/
1. R-Package plotly was used for the diagrams in the "Statistics", "Glossar" and "Forecast" Segment of the project: https://plotly.com/r/
    1. The table in the "Glossar" segment was developed with the code base from: https://plotly.com/r/table/
    1. The diagrams in the "Statistics" and "Forecast" segment was developed with the code base from: https://plotly.com/r/axes/; https://plotly.com/r/figure-labels/ and https://plotly.com/r/legend/
    
<a name="sources"></a>
## Sources
* https://www.mittelbayerische.de/region/kelheim-nachrichten/diese-corona-begriffe-sollte-man-kennen-21029-art1900824.html
* https://www.quarks.de/gesundheit/was-die-daten-zu-corona-aussagen-und-was-nicht/

<a name="imprints"></a>
## Imprints

Jens Cedric Schug   
Department of Computer Science and Microsystems Technology
University of Applied Sciences Kaiserslautern  
Zweibruecken, Germany  
<a href="mailto:jesc0030@stud.hs-kl.de">jesc0030@stud.hs-kl.de</a>  
             
  
               
Julian Bernhart  
Department of Computer Science and Microsystems Technology
University of Applied Sciences Kaiserslautern  
Zweibruecken, Germany  
<a href="mailto:jube0010@stud.hs-kl.de">jube0010@stud.hs-kl.de</a>  
  
  
  
Marco Miles Noll  
Department of Computer Science and Microsystems Technology
University of Applied Sciences Kaiserslautern  
Zweibruecken, Germany  
<a href="mailto:mano0010@stud.hs-kl.de">mano0010@stud.hs-kl.de</a>  
             

             
### Supervisor   
             
Prof. Dr. Manfred Brill   
Department of Computer Science and Microsystems Technology
University of Applied Sciences Kaiserslautern  
Zweibruecken, Germany  
<a href="https://www.hs-kl.de/hochschule/profil/personenverzeichnis/detailanzeige-personen/person/manfred-brill">About Manfred Brill</a>  
<a href="mailto:manfred.brill@hs-kl.de">manfred.brill@hs-kl.de</a>  
             
             


