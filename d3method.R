#THIS WAS AN ATTEMPT TO USE TREEMAP AND D3TREER BUT DIFFICULT TO INSTALL PACKAGES SO SWITCHED https://r-graph-gallery.com/treemap.html
library(magrittr)

raw_inventoryData <- readxl::read_xlsx("Copy of 230511 AO10753PB_NetZeroInventoryMapping_2021Update_unamended_.xlsx",
                                        sheet = "IPCCT_UK", 
                                        skip = 6)

inventoryData <- raw_inventoryData[,1:12] %>% 
  janitor::clean_names(., "small_camel") %>% 
  dplyr::rename(ippcId = ipcc2006GLs, 
                gasType = shortPollName,
                emissionsSourceRef = emissionsSourceRef5,
                activitySourceRef = activitySourceRef6,
                x2017 = x2017_8, #should probably pivot these wider
                x2018 = x2018_9,
                x2019 = x2019_10,
                x2020 = x2020_11,
                x2021 = x2021_12) %>%
  dplyr::filter(!is.na(sourceName)) %>%
  dplyr::mutate(dplyr::across(x2017:x2021, ~tidyr::replace_na(.x, 0))) %>% #replace NAs with zero - check if this is right statistically speaking
  tidyr::pivot_longer(x2017:x2021, names_to = "year", values_to = "emissions") %>%
  dplyr::mutate(year = as.numeric(gsub("x","", year)))

plotData <- inventoryData %>%
  dplyr::filter(year == 2021) %>%
  dplyr::group_by(ippcId, activityName, gasType) %>% #removed sourcename for now as has many categories
  dplyr::summarise(emissionTotal = sum(emissions)) %>%
  dplyr::filter(emissionTotal >=0) #currently just remove negative emissions, could plot these in a second plot?

plot <- treemap::treemap(plotData,
                         index=c("ippcId", "activityName", "gasType"),
                         vSize="emissionTotal"
)    

#inter <- d3Tree::d3tree(plot) #couldnt get this to work

# ui <- shiny::fluidPage(
#   shiny::titlePanel("Net Zero Inventory Mapping", windowTitle = "Net Zero Inventory Mapping"),
#   
#   shiny::sidebarPanel(
#     shiny::selectInput(
#       "selectedYear",
#       "Choose year:",
#       c("2021")
#     ),
#
#   shiny::mainPanel(
#     plotly::plotlyOutput("sankeyFlows")
#   )
# 
# )

server <- function(input, output, session) {

  output$treemap <- d3Tree::renderD3tree()
  
}
# 
# shiny::shinyApp(ui, server)