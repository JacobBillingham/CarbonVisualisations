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
                x2017 = x2017_8,
                x2018 = x2018_9,
                x2019 = x2019_10,
                x2020 = x2020_11,
                x2021 = x2021_12) %>%
  dplyr::filter(!is.na(sourceName)) %>%
  dplyr::mutate(dplyr::across(x2017:x2021, ~tidyr::replace_na(.x, 0))) %>% #replace NAs with zero
  tidyr::pivot_longer(x2017:x2021, names_to = "year", values_to = "emissions") %>%
  dplyr::mutate(year = as.numeric(gsub("x","", year))) %>%
  dplyr::mutate(activityName = paste0(ippcId, ": ", activityName)) %>%
  dplyr::mutate(gasType = paste0(activityName, ": ", gasType))

years <- unique(inventoryData$year)

ui <- shiny::fluidPage(
  
  shiny::titlePanel("Net Zero Inventory Mapping", windowTitle = "Net Zero Inventory Mapping"),

  shiny::sidebarPanel(
    shiny::selectInput(
      "selectedYear",
      "Choose year:",
      selected = 2021,
      years)
  ),

  shiny::mainPanel(
    plotly::plotlyOutput("treemap")
  )
  
)

server <- function(input, output, session) {
  
  producePlot <- shiny::reactive({
  
    groupedInventoryData <- inventoryData %>%
      dplyr::filter(year == input$selectedYear) %>%
      dplyr::group_by(ippcId, activityName, gasType) %>% #removed sourcename for now as has many categories
      dplyr::summarise(emissionTotal = sum(emissions), .groups = "drop_last") %>%
      dplyr::filter(emissionTotal >0) #currently just remove negative emissions, could plot these in a second plot?
    
    groups <- groupedInventoryData %>% dplyr::group_by(ippcId) %>%
      dplyr::summarise(values = sum(emissionTotal), .groups = "drop_last") %>%
      dplyr::rename(labels = ippcId) %>%
      dplyr::mutate(parents = "")
    
    subgroups <- groupedInventoryData %>% dplyr::group_by(ippcId, activityName) %>%
      dplyr::summarise(values = sum(emissionTotal), .groups = "drop_last") %>%
      dplyr::rename(parents = ippcId, labels = activityName)
    
    plotData <- groupedInventoryData %>% 
      dplyr::select(-ippcId) %>%
      dplyr::rename(parents = activityName, labels = gasType, values = emissionTotal) %>%
      rbind(groups) %>%
      rbind(subgroups)
    
    plotly::plot_ly(
      type='treemap',
      labels=plotData$labels,
      parents=plotData$parents,
      values= plotData$values,
      branchvalues = "total",
      height=800
    )
    
    })
  
  output$treemap <- plotly::renderPlotly(producePlot())
  
}

shiny::shinyApp(ui, server)