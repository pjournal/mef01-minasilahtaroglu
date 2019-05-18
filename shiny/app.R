#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)
library(ggplot2)
library(spotifyr)
library(ggjoy)
library(reshape)
library(treemap)
library(fmsb)
library(vcd)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(tm)
library(ECharts2Shiny)
library(DT)



# new.env(test=emptyenv())
# NS <- new.env()

# load("C:\\Users\\minas\\OneDrive\\Masaüstü\\shinydata.RData")

load(file="C:\\Users\\minas\\OneDrive\\Masaüstü\\shinydata1.RData")

# colnames(top20Artist_songFeatures)[22] <- "turkish_genres"

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Turkish Pop Most Popular Songs/Artists Visualization"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("bins", "Select the genre",choices = c("Choice required",top20Artist_songFeatures$genre)
      ),
      selectInput("bin", "Select the Artist",choices = c("Choice is optional",top20Artist_songFeatures$artist_name)
      )
    ),
    
    
    mainPanel(
      plotOutput("ggjoy"),
      plotOutput("chart"),
      DT::dataTableOutput("data_display")
      # plotOutput("wordcloud")
    )
  ))


# Define server logic required to draw a histogram
server <- function(input, output){
  
  output$ggjoy <- renderPlot({
    
    mydata <-top20Artist_songFeatures %>% filter(top20Artist_songFeatures$genre == input$bins)
    if(input$bins !="Choice required"){
      mydata<- mydata %>% filter(genre == input$bins)
    }
    ggplot(mydata, aes(x = valence, y = artist_name)) + 
      geom_joy() + 
      theme_joy() +
      ggtitle("Joyplot of Top Artists' Musical Positivity", subtitle = "Based on valence / measurement of positivity")
    
  })
  
  # output$wordcloud <- renderPlot({                  
  #   wordcloud(names, max.words=20 ,random.order=FALSE,rot.per=0.35,colors=brewer.pal(5, "Dark2"), main="Title")
  # })
  
  output$chart <- renderPlot({
    
    mydata1 <-top20Artist_songFeatures %>% filter(top20Artist_songFeatures$genre == input$bins & top20Artist_songFeatures$artist_name == input$bin)
    if(input$bins != "Choice required" & input$bin !="Choice is optional"){
      mydata1 <- mydata1 %>% filter(genre == input$bins & artist_name == input$bin)
      if(is.null(input$bin))
        return()
    }
    
    plotArtistRadar <- function(theArtist,songData){
      
      # theArtist <- "Sezen Aksu"
      
      prData <- 
        songData %>% 
        filter(artist_name==theArtist) %>%
        # arrange(desc(track_popularity)) %>%
        slice(1:5) %>%
        select(track_name,danceability, energy, speechiness, acousticness, liveness, valence)
      
      # colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9))
      # colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4))
      radarchart( prData %>% select(-track_name), axistype=1 , 
                  #custom polygon
                  # pcol=colors_border , pfcol=colors_in ,
                  plwd=4 , plty=1,
                  #custom the grid
                  cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,100,20), cglwd=0.5,
                  #custom labels
                  vlcex=1 , title=paste0(theArtist," Top Songs"), maxmin = FALSE
      )
      legend(x=1.3, y=1.0, legend = prData$track_name, bty = "n", pch=10 ,palette(), text.col = "black", cex=0.6, pt.cex=1.5)
    } 
    
    plotArtistRadar(theArtist = input$bin , top20Artist_songFeatures)
    
    table_data <- tibble()
    table_data <- top20Artist_songFeatures %>%
      filter(artist_name==input$bin) %>%
      select(artist_name, track_name, track_popularity, danceability:tempo)
    
    output$data_display <- DT::renderDataTable(                    
      # datatable(data(table_data),options = list(pageLength = 100),rownames = TRUE)
      table_data
    )
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)