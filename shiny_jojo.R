# The file is used to create a shiny app about jjba stand ability
library(ggradar)

# add the group name for the stands
raw_data <- read.csv("/Users/willowwu/Downloads/jojostandstatsv2.csv")
raw_data$group <- "Stardust Crusaders"
raw_data$group[34:156] <- "Diamond Is Unbreakable"
raw_data$group[63:156] <- "Golden Wind"
raw_data$group[93:156] <- "Stone Ocean"
raw_data$group[117:156] <- "Steel Ball Run"
raw_data$group[141:156] <- "JoJolion"


data[,2:7] <- sapply(data[,2:7],as.factor)
raw_data[raw_data == "None"] <- 0
raw_data[raw_data == "E"] <- 1
raw_data[raw_data == "D"] <- 2
raw_data[raw_data == "C"] <- 3
raw_data[raw_data == "B"] <- 4
raw_data[raw_data == "A"] <- 5
raw_data[raw_data == "Infi"] <- 6

raw_data[,2:7] <- sapply(raw_data[,2:7],as.numeric)

data_scale <- raw_data
data_scale[,2:7] <- data_scale[,2:7] %>% mutate_each(funs(rescale))
data <- data_scale[1,2:7]
ggradar(data,
        values.radar = c("None", "E", "D","B","C","A","INF"),
        group.colours = "#00cc99",
        group.line.width = 0,
        group.point.size = 0,
        fill = TRUE)
