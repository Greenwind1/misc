# library ----
library(tidyverse)
library(sf)
library(jpndistrict)
library(leaflet)
library(shiny)
library(shinydashboard)
library(bslib)


# global ----
# load dataset
reg.codes.df <- read_csv("input/35region-codes.csv",
                         locale = locale(encoding = "cp932")) %>% 
    mutate(code = as.character(code)) %>%
    rename(REGION_CLASS = CLASS) %>%
    mutate(REGION_CLASS = as.factor(REGION_CLASS))

nutrition.df.h25 <- read_csv("input/H25_Nutrition Data of Shizuoka_v2.csv")
nutrition.df.h28 <- read_csv("input/Nutrition Data of Shizuoka_v2.csv")
nutrition.df.h28 <- nutrition.df.h28 %>% select(colnames(nutrition.df.h25))

data.df.h25 <- reg.codes.df %>% 
    left_join(nutrition.df.h25, by = "NUTRITION_REGION_CODE")
data.df.h28 <- reg.codes.df %>% 
    left_join(nutrition.df.h28, by = "NUTRITION_REGION_CODE")

rm(reg.codes.df, nutrition.df.h25, nutrition.df.h28)
gc()

city <- jpn_pref(22) %>% mutate(CODE = as.character(city))  # 22: Shizuoka
city[str_detect(city$city, "静岡市"), "CODE"] <- "静岡市"
city[str_detect(city$city, "浜松市"), "CODE"] <- "浜松市"
city[str_detect(city$city, "郡 "), "CODE"] <- 
    str_remove(city[str_detect(city$city, "郡 "), ]$city, pattern = " ")
city <- city[!str_detect(city$city, "浜松市"), ]

# unite Polygon, cast MULTIPOLYGON to GEOMETRY
mp.shizuoka <- sf::st_union(city[city$city == "静岡市 葵区", "geometry"],
                            city[city$city == "静岡市 駿河区", "geometry"])
mp.shizuoka <- sf::st_union(mp.shizuoka,
                            city[city$city == "静岡市 清水区", "geometry"])
# plot(mp.shizuoka)  # check Sizuoka-Shi polygon image

city["geometry2"] <- city["geometry"]
city[str_detect(city$city, "静岡市"), "geometry2"] <- 
    sf::st_cast(mp.shizuoka, "GEOMETRY")

# https://stackoverflow.com/questions/61001322/how-to-change-the-active-geometry-column-in-an-sf-table-to-a-different-geometr
st_geometry(city) <- "geometry2"

plot.sf.h25 <- city %>% left_join(data.df.h25, by = c("CODE" = "city_name"))
plot.sf.h25 <- plot.sf.h25 %>% slice(1:n())
plot.sf.h25 <- plot.sf.h25[!duplicated(plot.sf.h25$CODE), ]
plot.df.h25 <- plot.sf.h25 %>% as.data.frame

plot.sf.h28 <- city %>% left_join(data.df.h28, by = c("CODE" = "city_name"))
plot.sf.h28 <- plot.sf.h28 %>% slice(1:n())
plot.sf.h28 <- plot.sf.h28[!duplicated(plot.sf.h28$CODE), ]
plot.df.h28 <- plot.sf.h28 %>% as.data.frame

plot.df.all <- rbind(plot.df.h25, plot.df.h28)

nut.cols <- colnames(data.df.h28)[8:ncol(data.df.h28)]

rm(data.df.h25, data.df.h28)
gc()


# ui ----
ui <- fluidPage(
    theme = bs_theme(bootswatch = "minty"),
    dashboardPage(
        header = dashboardHeader(title = 'Nutrition Map'),
        sidebar = dashboardSidebar(disable = TRUE),
        body = dashboardBody(
            selectInput(inputId = "nut.col", 
                        label = "Type of Nutrition",
                        choices = nut.cols),
            br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
            leafletOutput("map.h28", width = 800, height = 600),
            br(), br(), 
            leafletOutput("map.h25", width = 800, height = 600),
        ),
        skin = "yellow",
    )
)


# server ----
server <- function(input, output) {
    # map.h28
    output$map.h28 <- renderLeaflet({
        journal.col1 <- "#495170"
        journal.col2 <- "#f7be16"
        journal.col3 <- "#fa8072"
        pal <- colorNumeric(
            palette = c(journal.col1, "#FFFFFF", journal.col3),
            domain = plot.df.all[, input$nut.col],
            reverse = F
        )
        
        leaflet(data = plot.sf.h28) %>%
            addProviderTiles("CartoDB.Positron",
                             options = providerTileOptions(opacity = 0.5)) %>%
            setView(lng = 138.3262, lat = 35.11148, zoom = 9) %>%
            addPolygons(
                fillColor = ~ pal(plot.df.h28[, input$nut.col]),
                fillOpacity = 0.7,
                stroke = TRUE, color = "dimgray",
                weight = 1, dashArray = 4, opacity = 0.2,
                popup = paste(plot.df.h28[, "city"], ": ", 
                              plot.df.h28[, input$nut.col])
            ) %>% 
            addLegend(
                position = "topleft",
                pal = pal, values = plot.df.h28[, input$nut.col], 
                # bins = 20, 
                opacity = 0.8,
                labFormat = labelFormat(
                    transform = function(x) sort(x, decreasing = F),
                    digits = 1
                ),
                title = "H28"
            )
    })
    
    # map.h25
    output$map.h25 <- renderLeaflet({
        journal.col1 <- "#495170"
        journal.col2 <- "#f7be16"
        journal.col3 <- "#fa8072"
        pal <- colorNumeric(
            palette = c(journal.col1, "#FFFFFF", journal.col3),
            domain = plot.df.all[, input$nut.col],
            reverse = F
        )
        
        leaflet(data = plot.sf.h25) %>%
            addProviderTiles("CartoDB.Positron",
                             options = providerTileOptions(opacity = 0.5)) %>%
            setView(lng = 138.3262, lat = 35.11148, zoom = 9) %>%
            addPolygons(
                fillColor = ~ pal(plot.df.h25[, input$nut.col]),
                fillOpacity = 0.7,
                stroke = TRUE, color = "dimgray",
                weight = 1, dashArray = 4, opacity = 0.2,
                popup = paste(plot.df.h25[, "city"], ": ", 
                              plot.df.h25[, input$nut.col])
            ) %>% 
            addLegend(
                position = "topleft",
                pal = pal, values = plot.df.h25[, input$nut.col], 
                # bins = 20, 
                opacity = 0.8,
                labFormat = labelFormat(
                    transform = function(x) sort(x, decreasing = F),
                    digits = 1
                ),
                title = "H25"
            )
    })
}


# shinyApp ----
shinyApp(ui = ui, server = server)


# library(rsconnect)
# rsconnect::deployApp("../Nutrition_map")