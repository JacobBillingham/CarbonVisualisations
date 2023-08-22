#Improvements:
#Higher level categories? i.e. all the 1's from IPCC?
#Get rid of unnecessary categories i.e. if all of subcategory is one gas
#Make labels clearer, i.e. simplify colons and codes
#Tickboxes to allow selecting multiple years
#Check are units consistent across dataset? units column suggests otherwise
#Include source name col

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
  dplyr::mutate(sector = substr(ippcId, 1, 1), .before = ippcId) %>%
  dplyr::mutate(sector = dplyr::case_when(sector == 1 ~ "Energy",
                                          sector == 2 ~ "Industrial Processes",
                                          sector == 3 ~ "Agriculture",
                                          sector == 4 ~ "LULUCF",
                                          sector == 5 ~ "Waste",
                                          TRUE ~ NA)) 

 inventoryData %<>%
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
      years),
    
    shiny::htmlOutput("total")
  ),

  shiny::mainPanel(
    plotly::plotlyOutput("treemap")
  )
  
)

server <- function(input, output, session) {
  
  producePlot <- shiny::reactive({
  
    #this method of getting the child parent pairs is terrible - rethink
    
    plotData <- inventoryData %>%
      dplyr::filter(year == input$selectedYear) %>%
      dplyr::group_by(sector, ippcId, activityName, gasType) %>% #removed sourcename for now as has many categories
      dplyr::summarise(emissionTotal = sum(emissions), .groups = "drop_last") %>%
      dplyr::filter(emissionTotal >= 0) %>% #currently just remove negative emissions, could plot these in a second plot?
      tibble::rownames_to_column() %>%
      tidyr::pivot_longer(sector:gasType, names_to = "labels", values_to = "values") %>%
      dplyr::mutate(labels = dplyr::case_when(labels == "sector" ~ 1,
                                              labels == "ippcId" ~ 2,
                                              labels == "activityName" ~ 3,
                                              labels == "gasType" ~ 4,
                                              TRUE ~ NA)) 
    
    plotData$parents <- ""
    
    plotData1 <- dplyr::filter(plotData, labels == "1")
    plotData2 <- dplyr::filter(plotData, labels != "1")
    
    for(i in 1:nrow(plotData2)){
    
    plotData2$parents[i] <- plotData$values[plotData$rowname == plotData2$rowname[i] & plotData$labels == plotData2$labels[i]-1]
    
    }
    
    plotData <- rbind(plotData1, plotData2)
    
    plotData %<>% dplyr::select(-rowname, -labels) %>%
      dplyr::rename(labels = values, values = emissionTotal) %>%
      dplyr::group_by(labels, parents) %>%
      dplyr::summarise(values=sum(values), .groups = "drop_last") %>%
      dplyr::mutate(color = dplyr::case_when(labels == "Energy" ~ "red",
                                             labels == "Industrial Processes" ~ "yellow",
                                             labels == "Agriculture" ~ "green",
                                             labels == "LULUCF" ~ "blue",
                                             labels == "Waste" ~ "purple"))
    
    plotly::plot_ly(
      type='treemap',
      labels=plotData$labels,
      parents=plotData$parents,
      values= plotData$values,
      branchvalues = "total",
      height=800,
      marker=list(colors=plotData$color)
    )
    
    
    })
  
  calcTotal <- shiny::reactive({
  
      groupedInventoryData <- inventoryData %>%
        dplyr::filter(year == input$selectedYear) %>%
        dplyr::summarise(emissionTotal = sum(emissions), .groups = "drop_last")
        
  })
  
  output$treemap <- plotly::renderPlotly(producePlot())
  output$total <- shiny::renderText(shiny::HTML(paste0("<b>","Total emissions: ","</b>", round(calcTotal(),2))))
  
}

shiny::shinyApp(ui, server)
