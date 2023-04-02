library(tidyverse)
library(maps)
library(viridis)
library(fivethirtyeight)
library(cowplot)
library(plotly)
library(ggraph)
library(d3heatmap)
library(heatmaply)
library(htmlwidgets)
bdriver = as.data.frame(bad_drivers)
head(bdriver)

states <- map_data("state")
head(states)

# make a state map

p <- ggplot(data = states,
            mapping = aes(x = long, y = lat,
                          group = group, fill = region))

p + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
    guides(fill = FALSE)

bdriver$region <- tolower(bdriver$state)
states_driver <- left_join(states, bdriver)
head(states_driver)

# populate map with data

p0 <- ggplot(data = states_driver,
             aes(x = long, y = lat, group = group, fill = num_drivers, hoverinfo = state))
p1 <- p0 + geom_polygon(color = "gray90", size = 0.1) +
    coord_map(projection = "albers", lat0 = 39, lat1 = 45) 

p2 <- p1 + scale_fill_gradient(low = "red", high = "purple") +
    labs(title = "Number of Drivers Involved in Fatal Collisions per Billion Miles in each American State") 
p2 + theme_map()

statemap <- ggplotly (p2)

# save the widget

saveWidget(statemap, file= "C:/Users/Mandy/OneDrive/Documents/R/State Map.html")

#rename column names

colnames(bdriver)[2] = "Drivers in Collisions"
colnames(bdriver)[3] = "% Speeding"
colnames(bdriver)[4] = "% Alcohol-Impaired"
colnames(bdriver)[5] = "% Not Distracted"
colnames(bdriver)[6] = "% No Previous Accidents"
colnames(bdriver)[7] = "Insurance Premiums"
colnames(bdriver)[8] = "Losses"

#normalize data

matrix <- bdriver
rownames(matrix) <- matrix[,1]
matrix <- matrix %>% dplyr::select(-state)
matrix <- as.matrix(matrix)
head(matrix)

#make heatmap with dendrogram

heatmap <- heatmaply(matrix, 
        #dendrogram = "row",
        xlab = "", ylab = "", 
        main = "",
        scale = "column",
        col = turbo(256),
        margins =c(5,5),
        grid_color = "white",
        grid_width = 0.00001,
        titleX = FALSE,
        hide_colorbar = FALSE,
        branches_lwd = 0.1,
        label_names = c("State", "Feature", "Value"),
        fontsize_row = 6.5, fontsize_col = 7,
        labCol = colnames(matrix),
        labRow = rownames(matrix),
        heatmap_layers = theme(axis.line=element_blank())
        )
# save the widget

 saveWidget(heatmap, file= "C:/Users/Mandy/OneDrive/Documents/R/heatmap.html")