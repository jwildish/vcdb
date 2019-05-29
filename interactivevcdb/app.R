#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(dplyr)
library(readxl)
library(DT)
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(lubridate)
#
library(shiny)
library(DT)
library(shinydashboard)
library(plotly)
library(dplyr)
library(RCurl)
library(leaflet)

#path <- "C:/Users/Jordan/Documents/vcdb"
#write.csv(CARdata, file.path(path, "CARdata.csv"))
#write.csv(ACRdata, file.path(path, "ACRdata.csv"))
#write.csv(VCSdata, file.path(path, "VCSdata.csv"))

getwd()
CARdata <- read.csv("./CARdata.csv")
#CARdata <- read.csv("C:/Users/Jordan/downloads/temp (7).csv")

#https://acr2.apx.com/myModule/rpt/myrpt.asp?r=111
ACRdata <- read.csv("C:/Users/Jordan/downloads/temp (9).csv")
ACRdata <- read.csv("./ACRdata.csv")

#https://www.vcsprojectdatabase.org/#/vcus

#VCSdata <- read.csv("C:/Users/Jordan/downloads/VCUs.csv")
#https://www.vcsprojectdatabase.org/#/ccb-verified
#CCBdata <- read.csv("C:/Users/Jordan/downloads/CCB Verified Projects.csv")

#CARdata <- subset(CARdata, ARB.Project.Status == "Not ARB Eligible")


VCSdata <- read.csv("./VCSdata.csv")


CARdata <- CARdata %>% select(Project.ID, Project.Developer, Project.Name,  Project.Site.State,
                              Project.Site.Country, Total.Number.of.Offset.Credits.Registered., Project.Type, Project.Listed.Date, Project.Website)

ACRdata <- subset(ACRdata, ARB.Status == "Not ARB Eligible")
ACRdata$Project.Listed.Date <- NA

ACRdata$Total.Number.of.Offset.Credits.Registered. <- ACRdata$Total.Number.of.Credits.Registered.

ACRdata <- ACRdata %>% select(Project.ID, Project.Developer, Project.Name, Project.Site.State, 
                              Project.Site.Country, Total.Number.of.Offset.Credits.Registered., Project.Type, Project.Listed.Date, Project.Website)

VCSdata <- VCSdata %>% rename("Project.ID" = "ID") %>%
    rename( "Project.Name" = "Name") %>%
    rename("Project.Site.Country" = "Country") %>%
    rename("Total.Number.of.Offset.Credits.Registered." = "VCU.Quantity.Issued") %>%
    rename("Project.Type" = "Sectoral.Scope") %>% rename("Project.Listed.Date" = "Issuance.Date")

VCSdata$Project.Developer <- NA
VCSdata$Project.Site.State <- NA
VCSdata$Project.Website <- NA
VCSdata <- VCSdata %>% select(Project.ID, Project.Developer, Project.Name, Project.Site.State, 
                              Project.Site.Country, Total.Number.of.Offset.Credits.Registered., Project.Type, Project.Listed.Date, Project.Website)

CARdata$Project.ID <- as.character(CARdata$Project.ID)
CARdata$Project.Developer <- as.character(CARdata$Project.Developer)
CARdata$Project.Name <- as.character(CARdata$Project.Name)
CARdata$Project.Site.State <- as.character(CARdata$Project.Site.State)
CARdata$Project.Site.Country <- as.character(CARdata$Project.Site.Country)
CARdata$Project.Type <- as.character(CARdata$Project.Type)
CARdata$Project.Listed.Date <- as.character(CARdata$Project.Listed.Date)
CARdata$Project.Website <- as.character(CARdata$Project.Website)
CARdata$Total.Number.of.Offset.Credits.Registered. <- as.numeric(CARdata$Total.Number.of.Offset.Credits.Registered.)

ACRdata$Project.ID <- as.character(ACRdata$Project.ID)
ACRdata$Project.Developer <- as.character(ACRdata$Project.Developer)
ACRdata$Project.Name <- as.character(ACRdata$Project.Name)
ACRdata$Project.Site.State <- as.character(ACRdata$Project.Site.State)
ACRdata$Project.Site.Country <- as.character(ACRdata$Project.Site.Country)
ACRdata$Project.Type <- as.character(ACRdata$Project.Type)
ACRdata$Project.Listed.Date <- as.character(ACRdata$Project.Listed.Date)
ACRdata$Project.Website <- as.character(ACRdata$Project.Website)
ACRdata$Total.Number.of.Offset.Credits.Registered. <- as.numeric(ACRdata$Total.Number.of.Offset.Credits.Registered.)

VCSdata$Project.ID <- as.character(VCSdata$Project.ID)
VCSdata$Project.Developer <- as.character(VCSdata$Project.Developer)
VCSdata$Project.Name <- as.character(VCSdata$Project.Name)
VCSdata$Project.Site.State <- as.character(VCSdata$Project.Site.State)
VCSdata$Project.Site.Country <- as.character(VCSdata$Project.Site.Country)
VCSdata$Project.Type <- as.character(VCSdata$Project.Type)
VCSdata$Project.Listed.Date <- as.character(VCSdata$Project.Listed.Date)
VCSdata$Project.Website <- as.character(VCSdata$Project.Website)
VCSdata$Total.Number.of.Offset.Credits.Registered. <- as.numeric(VCSdata$Total.Number.of.Offset.Credits.Registered.)

VCSdata$Registry <- "Verified Carbon Standard"
ACRdata$Registry <- "American Carbon Registry"
CARdata$Registry <- "Climate Action Reserve"

VCSdata$Project.Listed.Date <- dmy(VCSdata$Project.Listed.Date)
CARdata$Project.Listed.Date <- mdy(CARdata$Project.Listed.Date)
ACRdata$Project.Listed.Date <- mdy(ACRdata$Project.Listed.Date)

Combineddata <- rbind(ACRdata, CARdata, VCSdata)

devnames <- unique(mergedf6.6$`Offset.Project Operator`)
test <- c(devnames, "All")
protnames <- unique(mergedf6.6$Project.Type)

Combineddata$Project.Site.Country <- replace(Combineddata$Project.Site.Country , Combineddata$Project.Site.Country =="BO", "Bolivia")
Combineddata$Project.Site.Country <- replace(Combineddata$Project.Site.Country , Combineddata$Project.Site.Country =="US", "United States")
Combineddata$Project.Site.Country <- replace(Combineddata$Project.Site.Country , Combineddata$Project.Site.Country =="MX", "Mexico")
Combineddata$Project.Site.Country <- replace(Combineddata$Project.Site.Country , Combineddata$Project.Site.Country =="Congo", "Congo, the Democratic Republic of the")
Combineddata$Project.Site.Country <- replace(Combineddata$Project.Site.Country , Combineddata$Project.Site.Country =="BR", "Brazil")
Combineddata$Project.Site.Country <- replace(Combineddata$Project.Site.Country , Combineddata$Project.Site.Country =="RW", "Rwanda")
Combineddata$Project.Site.Country <- replace(Combineddata$Project.Site.Country , Combineddata$Project.Site.Country =="NI", "Nicaragua")
Combineddata$Project.Site.Country <- replace(Combineddata$Project.Site.Country , Combineddata$Project.Site.Country =="ML", "Mali")
Combineddata$Project.Site.Country <- replace(Combineddata$Project.Site.Country , Combineddata$Project.Site.Country =="ML", "Mali")


test <- Combineddata
table(test$Project.Type)
test$Project.Type <- gsub(" - ARB Compliance", "", test$Project.Type)
test$Project.Type <- gsub(" - MX", "", test$Project.Type)
test$Project.Type <- gsub(" - U.S.", "", test$Project.Type)
test$Project.Type <- gsub(" - Drainage", "", test$Project.Type)
test$Project.Type <- gsub(" - VAM", "", test$Project.Type)
test$Project.Type <- gsub("1. ", "", test$Project.Type)
test$Project.Type <- gsub("1", "", test$Project.Type)

test$Project.Type <- gsub("2. ", "", test$Project.Type)
test$Project.Type <- gsub("3. ", "", test$Project.Type)
test$Project.Type <- gsub("4. ", "", test$Project.Type)
test$Project.Type <- gsub("5. ", "", test$Project.Type)
test$Project.Type <- gsub("6. ", "", test$Project.Type)
test$Project.Type <- gsub("7. ", "", test$Project.Type)
test$Project.Type <- gsub("8. ", "", test$Project.Type)
test$Project.Type <- gsub("9. ", "", test$Project.Type)
test$Project.Type <- gsub("10. ", "", test$Project.Type)
test$Project.Type <- gsub("11. ", "", test$Project.Type)
test$Project.Type <- gsub("12. ", "", test$Project.Type)
test$Project.Type <- gsub("13. ", "", test$Project.Type)
test$Project.Type <- gsub("14. ", "", test$Project.Type)
test$Project.Type <- gsub("15. ", "", test$Project.Type)

Combineddata <- test
table(Combineddata$Project.Listed.Date)
Combineddata$year <- year(Combineddata$Project.Listed.Date)

table(Combineddata$year)

glimpse(Combineddata)
names(Combineddata)



Combineddata <- Combineddata %>% rename("Project ID" = "Project.ID")  %>% rename("Project Developer" = "Project.Developer") %>% 
    rename("Project Name" = "Project.Name") %>% rename("Project Site (State)" = "Project.Site.State") %>% rename("Project Site (Country)" = "Project.Site.Country") %>%
    rename("Number of Offsets Registered" = "Total.Number.of.Offset.Credits.Registered.") %>% 
    rename("Project Type" = "Project.Type") %>%  rename("Date Listed" = "Project.Listed.Date") %>% 
    rename("Website" = "Project.Website") %>% rename("Year" = "year")

table(Combineddata$`Project Site (Country)`)

Combineddata <- subset(Combineddata, !is.na(`Project ID`))
Combineddata <- subset(Combineddata, `Project Site (Country)` != "ML")
Combineddata <- subset(Combineddata, `Project Site (Country)` != "RW")
Combineddata <- subset(Combineddata, `Project Site (Country)` != "SV")
Combineddata <- subset(Combineddata, `Project Site (Country)` != "NI")


Combineddata$testlink <- ifelse(Combineddata$Website == "", paste0("None Available"), paste0('<a href="',Combineddata$Website,'">',"Link","</a>"))


Combineddata$Website <- Combineddata$testlink

Combineddata$link <- NULL
Combineddata$testlink <- NULL
Combineddata$`Number of Offsets Registered` <- as.numeric(Combineddata$`Number of Offsets Registered`)
Combineddata <- subset(Combineddata, `Number of Offsets Registered` >= 10)

Combineddata <- Combineddata %>% dplyr::arrange(desc(Year))

Combineddata <- subset(Combineddata, `Project ID` != "")

table(Combineddata$`Project Type`)

Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="0. Fugitive emissions from fuels", "Fugitive Emissions from Fuels")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Agricultural Land Management", "Agriculture, Forestry, Land Use")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Avoided Grassland Conversion", "Avoided Conversion")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Carbon Capture & Storage (CCS)", "Carbon Capture and Storage")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Chemical industry", "Chemical Industry")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Energy (renewable/non-renewable)", "Energy Production or Conservation")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Energy demand", "Energy Production or Conservation")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Energy distribution", "Energy Production or Conservation")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Energy Efficiency", "Energy Production or Conservation")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Fuel Switching", "Fugitive Emissions from Fuels")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Forestry", "Forestry (Undefined)")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Forest Carbon", "Forestry (Undefined)")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Conservation-Based Forest Management", "Improved Forest Management")

Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Industrial Gas Substitutions", "Industrial Process Emissions")

Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Industrial Gas Substitution", "Industrial Process Emissions")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Landfill", "Landfill Gas Capture")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Landfill Gas Capture & Combustion", "Landfill Gas Capture")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Landfill Gas Capture/Combustion", "Landfill Gas Capture")

Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Livestock", "Livestock Methane Capture")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Livestock and manure management", "Livestock Methane Capture")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Livestock Gas Capture/Combustion", "Livestock Methane Capture")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Chemical Industry", "Manufacturing industries")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Metal production", "Manufacturing industries")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Nitric Acid N2O- Secondary Catalyst", "Manufacturing industries")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Nitric Acid N2O- Tertiary Catalyst", "Manufacturing industries")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Organic Waste Composting", "Organic Waste Composting and Digestion")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Organic Waste Digestion", "Organic Waste Composting and Digestion")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Ozone Depleting Substances - Article 5 Imports", "Ozone Depleting Substances")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Renewable Energy", "Energy Production or Conservation")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Transport", "Transportation")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Fugitive Emissions from Fuels", "Transportation")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Transport / Fleet Efficiency", "Transportation")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Waste handling and disposal", "Waste Handling and Disposal")
Combineddata$`Project Type` <- replace(Combineddata$`Project Type` , Combineddata$`Project Type` =="Wastewater Treatment", "Waste Handling and Disposal")



library(choroplethr)
library(choroplethrMaps)


map_data <- Combineddata

map_data$value = map_data[, 2]

countr
names(map_data)
Countrymap <- map_data %>% select(`Project Site (Country)`, `Number of Offsets Registered` ) %>% 
    group_by(`Project Site (Country)`) %>% dplyr::summarise(value = sum(`Number of Offsets Registered`))

Countrymap <- rename(Countrymap, "region" = "Project Site (Country)")
Countrymap <- rename(Countrymap, "value" = "Number of Offsets Registered")


country_choropleth(Countrymap,
                   num_colors = 7)
state_choropleth(df = map_data,
                 title = colnames(map_data)[2], 
                 num_colors = 7)

# Download .shp file on the web:
download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
system("unzip world_shape_file.zip")
unzip("world_shape_file.zip")
world_spdf <- readOGR("TM_WORLD_BORDERS_SIMPL-0.3.shp")


# Read the file with the rgdal library in R
library(rgdal)
world_spdf =readOGR(dsn= getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")

world_spdf <- readOGR("TM_WORLD_BORDERS_SIMPL-0.3.shp")

# Look at the info provided with the geospatial object
head(world_spdf@data)
summary(world_spdf@data)

# Modify these info
world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 = as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)

library(leaflet)

mypalette = colorNumeric( palette="viridis", domain=world_spdf@data$POP2005, na.color="transparent")
mypalette(c(45,43))

# Basic choropleth with leaflet?
leaflet(world_spdf) %>% 
    addTiles()  %>% 
    setView( lat=10, lng=0 , zoom=2) %>%
    addPolygons( fillColor = ~mypalette(POP2005), stroke=FALSE )

Countrymap <- rename(Countrymap, "NAME" = "region")

world_spdf <- merge(world_spdf, Countrymap, by = "NAME")

# Final Map

mypalette = colorNumeric( palette="viridis", domain=world_spdf@data$value, na.color="transparent")

world_spdf@data$v

leaflet(world_spdf) %>% 
    addTiles()  %>% 
    setView( lat=10, lng=0 , zoom=2) %>%
    addPolygons( 
        fillColor = ~value, stroke=TRUE, fillOpacity = 0.9,
        highlight = highlightOptions( weight = 5, color = ~colorNumeric("Blues", value)(value), dashArray = "", fillOpacity = 0.3, bringToFront = TRUE),
        label = mytext,
        labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "13px", direction = "auto")
    ) %>%
    addLegend( pal=mypalette, values=~value, opacity=0.9, title = "Population (M)", position = "bottomleft" )


m=leaflet(world_spdf)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
    addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorNumeric("YlOrRd", value)(value) )
m

download.file("http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip" , destfile="world_shape_file.zip")
unzip("world_shape_file.zip")

world_spdf@data$NAME

table(Countrymap$NAME)

# Define UI for application that draws a histogram
ui <- dashboardPage(
        dashboardHeader(title = "Voluntary Offsets"),
        dashboardSidebar(selectizeInput('x', label = 'Project Type',
                                        choices = c("All", unique(Combineddata$`Project Type`))),
                         selectizeInput('y', label = 'Registry',  selected = "All",
                                        choices = c("All",unique(Combineddata$Registry))),
                         selectizeInput("color", label = "Year",
                                        choices = c("All", unique(Combineddata$Year)),
                                        selected = "All"),
                         selectizeInput("dev", label = "Country",  selected = "All",
                                        choices = c("All", unique(Combineddata$`Project Site (Country)`))),
                         downloadButton('downloadData', 'Download Full Dataset')
                         
        ),
        dashboardBody(
            tabsetPanel(type = "tabs",
                        tabPanel("Table", DT::dataTableOutput("mytable")),
                        tabPanel("Map", leafletOutput("mymap",height = 1000)),
                        tabPanel("Visualization", plotlyOutput("trendPlot"))))
    )
    



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    updateSelectizeInput(session, 'x',
                         choices = c("All", unique(Combineddata$`Project Type`)), 
                         server = TRUE
    )
    updateSelectizeInput(session, 'y',
                         choices = c("All", unique(Combineddata$Registry)), 
                         server = TRUE
    )
    updateSelectizeInput(session, 'color', 
                         choices = c("All", unique(Combineddata$Year)), 
                         server = TRUE
    )
    updateSelectizeInput(session, 'Avail',
                         choices = c("All", unique(Combineddata$`Project Site (Country)`)),
                         server = TRUE
    )
    
    output$mytable <- DT::renderDataTable(DT::datatable({
        data <- Combineddata
        if (input$y != "All") {
            data <- data[data$Registry == input$y,]
        }
        if (input$x != "All") {
            data <- data[data$`Project Type` == input$x,]
        }
        if (input$color != "All") {
            data <- data[data$Year == input$color,]
        }
        if (input$dev != "All") {
            data <- data[data$`Project Site (Country)` == input$dev,]
        }
        data
    }, escape = FALSE,  selection = 'multiple', filter ='top',extensions = 'Buttons', options = list(
        searching = TRUE,
        lengthMenu = c(20, 100, 500),
        pageLength = 100,
        ordering = TRUE,
        filter = 'top',
        selection ='multiple',
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
    ), class = 'display'
    ))
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(Combineddata, file)
        })
    output$trendPlot <- renderPlotly({
        data <- Combineddata
        if (input$y != "All") {
            data <- data[data$Registry == input$y,]
        }
        if (input$x != "All") {
            data <- data[data$`Project Type` == input$x,]
        }
        if (input$color != "All") {
            data <- data[data$Year == input$color,]
        }
        if (input$dev != "All") {
            data <- data[data$`Project Site (Country)` == input$dev,]
        }
        data
        p <- plot_ly(data, x = data$Year, y = data$`Number of Offsets Registered`, type = "bar", color =  ~data$Registry) %>% 
            layout(yaxis = list(title = 'Count'), barmode = 'stack')
        # style the xaxis
        
    })

    output$mymap <- renderLeaflet({
        data <- Combineddata
        if (input$y != "All") {
            data <- data[data$Registry == input$y,]
        }
        if (input$x != "All") {
            data <- data[data$`Project Type` == input$x,]
        }
        if (input$color != "All") {
            data <- data[data$Year == input$color,]
        }
        if (input$dev != "All") {
            data <- data[data$`Project Site (Country)` == input$dev,]
        }
        data <- rename(data,  "NAME" = "Project Site (Country)")
        data <- rename(data,  "value" = "Number of Offsets Registered")
        
        data <- data %>% group_by(NAME) %>% dplyr::summarise(value = sum(value))

        # Read the file with the rgdal library in R
        library(rgdal)
pal <- colorBin("YlOrRd", domain = data$value)
        world_spdf2 <- merge(world_spdf, data, by = "NAME")
        leaflet(world_spdf2)%>% addTiles()  %>% setView( lat=10, lng=0 , zoom=2) %>%
            addPolygons(data = world_spdf2, popup =~value, stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorNumeric("YlOrRd", value)(value)) %>%
            addLegend(pal = pal, values = ~value, opacity = 0.7, 
                      title = "Number of Offsets Issued ", position = "bottomright")
    }
        
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
