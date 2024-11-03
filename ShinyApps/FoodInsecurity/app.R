### THIS WORKS!!! 
## REACTIVE CLICKS!!

### TAKE 2

### THIS WORKS!!! 
## REACTIVE CLICKS!!

library(shiny)
library(leaflet)
library(tigris)
library(tidycensus)
library(tidyverse)

## INSTALL FROM R PACKAGE
#fc<-read.csv("data/fallFood.csv", 
#             head=TRUE, 
#             stringsAsFactors = FALSE)

## GITHUB FOOD INSEC INSTALL
fc<-read.csv("https://raw.githubusercontent.com/kitadasmalley/DATA502/refs/heads/main/FALL2022/Data/foodInsecure.csv", 
             head=TRUE, 
             stringsAsFactors = FALSE)


ui <- shinyUI(
    fluidPage(
        titlePanel("ASA Fall Data Challenge 2021: Food Insecurity"),
        
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
            
            # Sidebar panel for inputs ----
            sidebarPanel(
                
                # Input: Select State
                selectInput("select", h3("Select State"), 
                            choices = state.name, selected ="Oregon"),
                h5("Click a Census Tract on the map to learn more:"),
                verbatimTextOutput("Click_text"),
                verbatimTextOutput("Click_text6"),
                verbatimTextOutput("Click_text2"),
                verbatimTextOutput("Click_text3"),
                verbatimTextOutput("Click_text4"),
                verbatimTextOutput("Click_text5")
                
            ),
            
            # Main panel for displaying outputs ----
            mainPanel(
                radioButtons("radio2", h5("Select a metric for Low Access:"),
                             choices = list("Count", "Flag"),selected = "Count"),
                # Output: Leaflet
                leafletOutput("map"),
                radioButtons("radio", h5("Select distance to supermarket:"),
                             choices = list(1, 10),selected = 1),
                plotOutput("dem")#,
                #plotlyOutput("heatPlot")
                
            )
        )
    )
)


server <- function(input, output, session){
    
    
    
    #initial map output
    output$map <- renderLeaflet({
        
        or<-fc%>%
            filter(State==input$select)%>%
            mutate(GEOID=as.numeric(CensusTract))
        
        
        # MY KEY: fbf65ba295ce81cab0a2eccd52d62ca564bf896b
        
        #install.packages("tidycensus")
        
        
        census_api_key("fbf65ba295ce81cab0a2eccd52d62ca564bf896b")
        
        # Set a year of interest
        # It looks like 2019 is the most recent year of data
        this.year = 2019
        
        thisAbb<-state.abb[which(state.name==input$select)]
        
        
        
        # MEDIAN HOME VALUE with Geometry
        orMedvG <- get_acs(geography = "tract", year=this.year,
                           state = thisAbb, 
                           variables = "B25077_001E", 
                           geometry = TRUE)%>%
            mutate(GEOID=as.numeric(GEOID))
        
        
        ## USE GEO_JOIN TO COMBINE SPATIAL DATA AND OTHER DATA FRAMES
        joinOR<-geo_join(spatial_data=orMedvG , data_frame=or, 
                         by_sp='GEOID', by_df='GEOID')
        
        popup<-paste("Tract: ", as.character(substring(joinOR$GEOID, 6, 11)), "<br>",
                     "Population Low Access: ", as.character(joinOR$LAPOP1_10))
        
        if(input$radio2=="Count"){
            qpal<-colorQuantile("viridis", domain=joinOR$LAPOP1_10,
                                n=5,na.color="#FFFFFF")
            
            
            leaflet()%>%
                addProviderTiles("CartoDB.Positron")%>%
                addPolygons(data=joinOR,
                            fillColor= ~qpal(joinOR$LAPOP1_10),
                            fillOpacity = 0.7,
                            color="grey",
                            opacity=.5,
                            weight = 0.4,
                            smoothFactor = 0.2,
                            layerId =joinOR$GEOID,
                            popup = popup)%>%
                addLegend("bottomright", pal=qpal, values=joinOR$LAPOP1_10,
                          opacity = .7,
                          title="Percentiles")
        }
        
        else if(input$radio2=="Flag"){
            qpal2 <- colorFactor(palette = c("white", "blue"), 
                                 levels = c(0, 1))
            
            
            leaflet()%>%
                addProviderTiles("CartoDB.Positron")%>%
                addPolygons(data=joinOR,
                            fillColor= ~qpal2(joinOR$LA1and10),
                            fillOpacity = 0.7,
                            color="grey",
                            opacity=.5,
                            weight = 0.4,
                            smoothFactor = 0.2,
                            layerId =joinOR$GEOID,
                            popup = popup)%>%
                addLegend("bottomright", pal=qpal2, values=joinOR$LA1and10,
                          opacity = .7,
                          title="Rural")
        }
        
    }) #END RENDER LEAFLET
    
    observeEvent(input$map_shape_click, {
        
        #create object for clicked polygon
        click <- input$map_shape_click
        clickID<-click$id
        #if(is.null(click))
        #   return()
        
        
        
        text1<-paste("Census Tract:", clickID)
        output$Click_text<-renderText({text1})
        
        
        
        output$Click_text0<-renderText(clickID)
        
        #text1<-paste("Census Tract:", substring(clickID, 6, 11))
        
        
        tractT<-fc%>%
            filter(CensusTract==as.numeric(clickID))
        
        text2<-paste("County:", tractT$County)
        output$Click_text2<-renderText({text2})
        
        text3<-paste("Population:", tractT$Pop2010)
        output$Click_text3<-renderText({text3})
        
        text4<-paste("Median Income:", tractT$MedianFamilyIncome)
        output$Click_text4<-renderText({text4})
        
        text5<-paste("Poverty Rate:", tractT$PovertyRate)
        output$Click_text5<-renderText({text5})
        
        if(tractT$Urban==1){
            text6<-paste("Classification: Urban")
        }
        if(tractT$Urban==0){
            text6<-paste("Classification: Rural")
        }
        output$Click_text6<-renderText({text6})
        #print(click$id)
        
    }) #END OBSERVE EVENT
    
    output$dem <- renderPlot({
        #create object for clicked polygon
        click <- input$map_shape_click
        
        #if(is.null(click))
        #   return()
        
        clickID<-click$id
        
        if(!is.null(clickID)){
            tractT<-fc%>%
                filter(CensusTract==clickID)
            
            dem1<-data.frame(Demographics=c("White", 
                                            "Black", 
                                            "Asian", 
                                            "Native Hawaiian and \nOther Pacific Islander", 
                                            "American Indian and \nAlaska Native", 
                                            "Other/Multiple Race", 
                                            "Hispanic"),
                             Counts=c(tractT$lawhite1, 
                                      tractT$lablack1, 
                                      tractT$laasian1, 
                                      tractT$lanhopi1, 
                                      tractT$laaian1, 
                                      tractT$laomultir1, 
                                      tractT$lahisp1), 
                             Percents=c(tractT$lawhite1share, 
                                        tractT$lablack1share, 
                                        tractT$laasian1share, 
                                        tractT$lanhopi1share, 
                                        tractT$laaian1share, 
                                        tractT$laomultir1share, 
                                        tractT$lahisp1share))
            
            
            
            dem10<-data.frame(Demographics=c("White", 
                                             "Black", 
                                             "Asian", 
                                             "Native Hawaiian and \nOther Pacific Islander", 
                                             "American Indian and \nAlaska Native", 
                                             "Other/Multiple Race", 
                                             "Hispanic"),
                              Counts=c(tractT$lawhite10, 
                                       tractT$lablack10, 
                                       tractT$laasian10, 
                                       tractT$lanhopi10, 
                                       tractT$laaian10, 
                                       tractT$laomultir10, 
                                       tractT$lahisp10), 
                              Percents=c(tractT$lawhite10share, 
                                         tractT$lablack10share, 
                                         tractT$laasian10share, 
                                         tractT$lanhopi10share, 
                                         tractT$laaian10share, 
                                         tractT$laomultir10share, 
                                         tractT$lahisp10share))
            
            this.distance = input$radio
            
            if(this.distance==1){
                this.dem.df<-dem1
                title<-paste("Share of tract population by Race beyond ", 
                             this.distance,
                             " mile from supermarket", sep="")
            }
            if(this.distance==10){
                this.dem.df<-dem10
                title<-paste("Share of tract population by Race beyond ", 
                             this.distance,
                             " miles from supermarket", sep="")
                
            }
            
            this.dem.df$Demographics<-factor(this.dem.df$Demographics, levels = c("White", 
                                                                                  "Black", 
                                                                                  "Asian", 
                                                                                  "Native Hawaiian and \nOther Pacific Islander", 
                                                                                  "American Indian and \nAlaska Native", 
                                                                                  "Other/Multiple Race", 
                                                                                  "Hispanic"))
            
            
            
            ggplot(this.dem.df, aes(Percents, Demographics))+
                geom_col()+
                scale_y_discrete("",limits=rev, position = "right")+
                theme_bw()+
                xlim(c(0, 100))+
                geom_text(aes(label = Percents), hjust = -0.2)+
                ggtitle(title)
        }
    })
    
    
    
}
shinyApp(ui, server)