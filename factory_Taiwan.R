library(plyr)
library(shiny)
library(graphics)
library(sf)
library(leaflet)
library(magrittr)
library(htmlwidgets)
library(spdep)
library(psych)
library(colorRamps)

# Import data
data <- read.csv("data.csv")

# Shapefile data: Taiwan
Taiwan_C <- st_read("County.shp")
#Merge data
Taiwan_C <- merge(Taiwan_C, data, by.x="COUNTYNAME", by.y="COUNTYNAME")
# Adjacent area
nb <- poly2nb(Taiwan_C)

# Map----
# Choose a color palette and assign it to the values
color_01 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2511, bins = c(seq(0, 2000, by = 200)))
color_02 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2512, bins = c(seq(0, 500, by = 50)))
color_03 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2521, bins = c(seq(0, 50, by = 5)))
color_04 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2522, bins = c(seq(0, 1000, by = 100)))
color_05 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2531, bins = c(seq(0, 300, by = 30)))
color_06 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2539, bins = c(seq(0, 300, by = 30)))
color_07 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2541, bins = c(seq(0, 100, by = 10)))
color_08 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2542, bins = c(seq(0, 100, by = 10)))
color_09 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2543, bins = c(seq(0, 1000, by = 100)))
color_10 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2544, bins = c(seq(0, 60, by = 6)))
color_11 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2549, bins = c(seq(0, 30, by = 3)))
color_12 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2591, bins = c(seq(0, 30, by = 3)))
color_13 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2592, bins = c(seq(0, 700, by = 70)))
color_14 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$X2599, bins = c(seq(0, 30, by = 30)))
color_15 <- colorBin(heat.colors(10, alpha = 1), domain = Taiwan_C$Total, bins = c(seq(0, 6000, by = 600)))

# Create HTML labels for tooltip
tooltip_01 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2511) %>% lapply(htmltools::HTML)
tooltip_02 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2512) %>% lapply(htmltools::HTML)
tooltip_03 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2521) %>% lapply(htmltools::HTML)
tooltip_04 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2522) %>% lapply(htmltools::HTML)
tooltip_05 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2531) %>% lapply(htmltools::HTML)
tooltip_06 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2539) %>% lapply(htmltools::HTML)
tooltip_07 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2541) %>% lapply(htmltools::HTML)
tooltip_08 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2542) %>% lapply(htmltools::HTML)
tooltip_09 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2543) %>% lapply(htmltools::HTML)
tooltip_10 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2544) %>% lapply(htmltools::HTML)
tooltip_11 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2549) %>% lapply(htmltools::HTML)
tooltip_12 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2591) %>% lapply(htmltools::HTML)
tooltip_13 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2592) %>% lapply(htmltools::HTML)
tooltip_14 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$X2599) %>% lapply(htmltools::HTML)
tooltip_15 <- sprintf("%s: %d"
                      ,Taiwan_C$COUNTYNAME
                      ,Taiwan_C$"Total") %>% lapply(htmltools::HTML)
# Origin vote map
map_01 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_01(Taiwan_C$X2511)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_01, values = Taiwan_C$X2511, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_01(Taiwan_C$X2511),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_01)

map_02 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_02(Taiwan_C$X2512)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_02, values = Taiwan_C$X2512, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_02(Taiwan_C$X2512),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_02)

map_03 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_03(Taiwan_C$X2521)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_03, values = Taiwan_C$X2521, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_03(Taiwan_C$X2521),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_03)

map_04 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_04(Taiwan_C$X2522)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_04, values = Taiwan_C$X2522, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_04(Taiwan_C$X2522),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_04)

map_05 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_05(Taiwan_C$X2531)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_05, values = Taiwan_C$X2531, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_05(Taiwan_C$X2531),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_05)

map_06 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_06(Taiwan_C$X2539)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_06, values = Taiwan_C$X2539, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_06(Taiwan_C$X2539),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_06)

map_07 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_07(Taiwan_C$X2541)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_07, values = Taiwan_C$X2541, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_07(Taiwan_C$X2541),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_07)

map_08 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_08(Taiwan_C$X2542)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_08, values = Taiwan_C$X2542, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_08(Taiwan_C$X2542),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_08)

map_09 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_09(Taiwan_C$X2543)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_09, values = Taiwan_C$X2543, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_09(Taiwan_C$X2543),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_09)

map_10 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_10(Taiwan_C$X2544)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_10, values = Taiwan_C$X2544, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_10(Taiwan_C$X2544),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_10)

map_11 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_11(Taiwan_C$X2549)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_11, values = Taiwan_C$X2549, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_11(Taiwan_C$X2549),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_11)

map_12 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_12(Taiwan_C$X2591)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_12, values = Taiwan_C$X2591, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_12(Taiwan_C$X2591),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_12)

map_13 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_13(Taiwan_C$X2592)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_13, values = Taiwan_C$X2592, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_13(Taiwan_C$X2592),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_13)

map_14 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_14(Taiwan_C$X2599)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_14, values = Taiwan_C$X2599, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_14(Taiwan_C$X2599),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_14)

map_15 <- leaflet(Taiwan_C) %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3,
              fillOpacity = 0.7, fillColor = ~color_15(Taiwan_C$Total)) %>%
  setView(120.982024, 23.973875, zoom = 7) %>%
  addLegend(pal = color_15, values = Taiwan_C$Total, opacity = 0.7,
            title = "Numbers", position = "topright") %>%
  addPolygons(stroke = TRUE, color = "white", weight="1", smoothFactor = 0.3, fillOpacity = 0.7,
              fillColor = ~color_15(Taiwan_C$Total),
              highlight = highlightOptions(weight = 5, color = "grey", fillOpacity = 0.7, bringToFront = TRUE),
              label = tooltip_15)
