# The file is used to create a shiny app about jjba stand ability

library(ggplot2)
library(ggradar)
library(shiny)
library(scales)
library(dplyr)

# add the group name for the stands
raw_data <- read.csv("jojostandstatsv2.csv")
raw_data$group <- "Stardust Crusaders"
raw_data$group[34:156] <- "Diamond Is Unbreakable"
raw_data$group[63:156] <- "Golden Wind"
raw_data$group[93:156] <- "Stone Ocean"
raw_data$group[117:156] <- "Steel Ball Run"
raw_data$group[141:156] <- "JoJolion"

# turn out the character variables into numeric
data_numeric <- raw_data
data_numeric[data_numeric == "None"] <- 0
data_numeric[data_numeric == "E"] <- 1
data_numeric[data_numeric == "D"] <- 2
data_numeric[data_numeric == "C"] <- 3
data_numeric[data_numeric == "B"] <- 4
data_numeric[data_numeric == "A"] <- 5
data_numeric[data_numeric == "Infi"] <- 6

data_numeric[,2:7] <- sapply(data_numeric[,2:7],as.numeric)

# code to draw radar plot for certain Stand
data_scale <- data_numeric
data_scale[,2:7] <- data_scale[,2:7] %>% mutate_each(funs(rescale))

# Define UI for app
ui <- navbarPage(titlePanel("JOJO save THE WORLD!"),
                 tabPanel("Stand length radar plot",
                          selectInput(inputId = "Standname",
                                      label = "PLEASE SELECT STAND NAME!!",
                                      choices = unique(data_numeric$Stand)),
                          fluidPage(tableOutput(outputId = "stand_length_table"),
                                    plotOutput(outputId = "radarplot",
                                               width = 600,height = 600))),
                 tabPanel("Works and their Stand",
                          selectInput(inputId = "Groupname",
                                      label = "PLEASE SELECT THE WOKR NAME!!",
                                      choices = unique(data_numeric$group)),
                          fluidPage(tableOutput(outputId = "stand_group_table"))))


# Define server logic
server <- function(input, output) {
  output$radarplot <- renderPlot({
    
    data_used <- data_scale[data_scale$Stand == input$Standname,]
    
    color_set <- list(c("#dd99ff", "#ffff80"), 
                      c("#a0e6ee","#ffb3bf"), #
                      c("#fff266","#ebccff"), # 
                      c("#8fceef","#acfc9c"), # 
                      c("#e580cf","#FACD91"), # 
                      c("#FF1FC0","#ffffff") # 
    )
    
    group_stand <- data_used$group
    
    if(length(group_stand) > 1){
      group_stand <- group_stand[1]
    }
    
    color_group <- switch(as.character(group_stand),"Stardust Crusaders" = 1,
                          "Diamond Is Unbreakable" = 2,
                          "Golden Wind" = 3,
                          "Stone Ocean" = 4,
                          "Steel Ball Run" = 5,
                          "JoJolion" = 6)
    
    ggradar::ggradar(data_used[,1:7],
                     group.point.size = 0,
                     group.line.width = 1,
                     group.colours = color_set[[color_group]][2],
                     font.radar = "",
                     label.gridline.min = FALSE,
                     label.gridline.mid = FALSE,
                     label.gridline.max = FALSE,
                     background.circle.colour = color_set[[color_group]][1],
                     background.circle.transparency = 1,
                     gridline.max.colour = color_set[[color_group]][1],
                     plot.title = data_used[1,1],
                     fill = TRUE,
                     fill.alpha = 1) + 
      theme(panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5,color = color_set[[color_group]][1]),
            legend.key = element_blank(),
            legend.background=element_blank(),
            strip.background = element_blank(),
            axis.title = element_text(color = "white", hjust = 0, face = "italic"))
    
  })
  
  output$stand_length_table <- renderTable(
    raw_data[raw_data$Stand == input$Standname,])
  
  output$stand_group_table <- renderTable(
    raw_data[raw_data$group == input$Groupname,]
  )
}
shinyApp(ui, server)


