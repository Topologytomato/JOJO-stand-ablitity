# The file is used to create a shiny app about jjba stand ability

library(ggplot2)
library(ggradar)
library(shiny)

# add the group name for the stands
raw_data <- read.csv("/Users/willowwu/Downloads/jojostandstatsv2.csv")
raw_data$group <- "Stardust Crusaders"
raw_data$group[34:156] <- "Diamond Is Unbreakable"
raw_data$group[63:156] <- "Golden Wind"
raw_data$group[93:156] <- "Stone Ocean"
raw_data$group[117:156] <- "Steel Ball Run"
raw_data$group[141:156] <- "JoJolion"

# turn out the character variables into numeric
data[,2:7] <- sapply(data[,2:7],as.factor)
raw_data[raw_data == "None"] <- 0
raw_data[raw_data == "E"] <- 1
raw_data[raw_data == "D"] <- 2
raw_data[raw_data == "C"] <- 3
raw_data[raw_data == "B"] <- 4
raw_data[raw_data == "A"] <- 5
raw_data[raw_data == "Infi"] <- 6

raw_data[,2:7] <- sapply(raw_data[,2:7],as.numeric)

# code to draw radar plot for certain Stand
data_scale <- raw_data
data_scale[,2:7] <- data_scale[,2:7] %>% mutate_each(funs(rescale))
data <- data_scale[1,1:7]
color_set <- list(c("#cc66ff", "#ffff80"), # 白金之星配色
              c("#a0e6ee","#ffb3bf"), # 疯狂钻石配色
              c("#fff266","#ebccff"), # 黄金体验配色
              c("#8fceef","#acfc9c"), # 石之自由配色
              c("#e580cf","#FACD91"), # 第七部配色
              c("#FF1FC0","#FF1FC0") # 第八部配色
)

ggradar::ggradar(data[,1:7],
                 group.point.size = 0,
                 group.line.width = 1,
                 group.colours = color_set[2],
                 font.radar = "",
                 label.gridline.min = FALSE,
                 label.gridline.mid = FALSE,
                 label.gridline.max = FALSE,
                 background.circle.colour = color_set[1],
                 background.circle.transparency = 0.6,
                 gridline.max.colour = color_set[1],
                 gridline.mid.colour = color_set[2],
                 plot.title = data[1,1],
                 fill = TRUE,
                 fill.alpha = 1) + 
  theme(panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5,color = color_set[1]),
        legend.key = element_blank(),
        legend.background=element_blank(),
        strip.background = element_blank(),
        axis.title = element_text(color = "white", hjust = 0, face = "italic")
  )


# Define UI for app
ui <- fluidPage(
  
  # App title ----
  titlePanel("JO等了!!"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs 
    sidebarPanel(
      
      # Input
      selectInput(inputId = "Standname",
                  label = "choose a Stand name!",
                  choices = unique(raw_data$Stand))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput(outputId = "radarplot"),
      textOutput(outputId = "groupname")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$radarplot <- renderPlot({
    
    data <- data_scale[data_scale$Stand == input$Standname,]
    
    color_set <- list(c("#dd99ff", "#ffff80"), # 白金之星配色
                      c("#a0e6ee","#ffb3bf"), # 疯狂钻石配色
                      c("#fff266","#ebccff"), # 黄金体验配色
                      c("#8fceef","#acfc9c"), # 石之自由配色
                      c("#e580cf","#FACD91"), # 第七部配色
                      c("#FF1FC0","#ffffff") # 第八部配色
    )
    
    group_stand <- data$group
    
    if(length(group_stand) > 1){
      group_stand <- group_stand[1]
    }
    
    color_group <- switch(as.character(group_stand),"Stardust Crusaders" = 1,
                          "Diamond Is Unbreakable" = 2,
                          "Golden Wind" = 3,
                          "Stone Ocean" = 4,
                          "Steel Ball Run" = 5,
                          "JoJolion" = 6)
    
    ggradar::ggradar(data[,1:7],
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
                     plot.title = data[1,1],
                     fill = TRUE,
                     fill.alpha = 1) + 
      theme(panel.background = element_blank(),
            plot.title = element_text(hjust = 0.5,color = color_set[[color_group]][1]),
            legend.key = element_blank(),
            legend.background=element_blank(),
            strip.background = element_blank(),
            axis.title = element_text(color = "white", hjust = 0, face = "italic"))
    
  })
  
  output$groupname <- renderPrint(
    paste("The Stand appears in ", as.character(data_scale[data_scale$Stand == input$Standname,8])),
    width = 38)
}
shinyApp(ui, server)
