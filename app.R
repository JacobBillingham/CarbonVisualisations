#'Improvements:
#'Make labels clearer, i.e. simplify appended colons and translate IPPC codes (using https://naei.beis.gov.uk/glossary?view=crf)
#'Tickboxes to allow selecting multiple years - plus historical visualisations
#'Check are units consistent across dataset? units column suggests otherwise
#'Different data cuts i.e. by gas at top level
#'Include source name column
#'Currently I just remove negative emissions, could plot these in a second plot?

#import the magrittr package to allow use of pipes (%>%)
library(magrittr)

#import the emissions data, skipping 6 rows at the top of the spreadsheet
raw_inventoryData <- readxl::read_xlsx("Copy of 230511 AO10753PB_NetZeroInventoryMapping_2021Update_unamended_.xlsx",
                                       sheet = "IPCCT_UK", 
                                       skip = 6)

#tidy up the emissions data
inventoryData <- raw_inventoryData[,1:12] %>% #select only columns 1-12
  janitor::clean_names(., "small_camel") %>% #standardise the names of the columns
  dplyr::rename(ippcId = ipcc2006GLs, 
                gasType = shortPollName,
                emissionsSourceRef = emissionsSourceRef5,
                activitySourceRef = activitySourceRef6,
                x2017 = x2017_8,
                x2018 = x2018_9,
                x2019 = x2019_10,
                x2020 = x2020_11,
                x2021 = x2021_12) %>% #modify some column names (years need an x in front as can't start a variable with a number in R)
  dplyr::filter(!is.na(sourceName)) %>% #filter out empty rows at bottom of spreadsheet
  dplyr::mutate(dplyr::across(x2017:x2021, ~tidyr::replace_na(.x, 0))) %>% #replace NAs with zero - needs checking this is right
  tidyr::pivot_longer(x2017:x2021, names_to = "year", values_to = "emissions") %>% #pivot the spreadsheet to long format i.e. a row for each source in each year
  dplyr::mutate(year = as.numeric(gsub("x","", year))) %>% #remove the x in front of years as now row entries rather than column names
  dplyr::mutate(sector = substr(ippcId, 1, 1), .before = ippcId) %>% #get first digit from ippc code to identify sector
  dplyr::mutate(sector = dplyr::case_when(sector == 1 ~ "Energy",
                                          sector == 2 ~ "Industrial Processes",
                                          sector == 3 ~ "Agriculture",
                                          sector == 4 ~ "LULUCF",
                                          sector == 5 ~ "Waste",
                                          TRUE ~ NA)) #recode ippc code first digit to correct sector

#'the tree map needs each box to have a unique name, so I append the different levels to achieve this 
#'e.g. all the different CO2 observations become IPPC:activity:CO2 (and ippc and activity are unique)
 inventoryData %<>%
  dplyr::mutate(activityName = paste0(ippcId, ": ", activityName)) %>%
  dplyr::mutate(gasType = paste0(activityName, ": ", gasType))

#get the years of data available for user to select from 
years <- unique(inventoryData$year)

#create user interface for R shiny
ui <- shiny::fluidPage(
  
  #app title
  shiny::titlePanel("Net Zero Inventory Mapping", windowTitle = "Net Zero Inventory Mapping"),

  shiny::sidebarPanel(
    
    #allow user to select a year, default set to 2021
    shiny::selectInput(
      "selectedYear",
      "Choose year:",
      selected = 2021,
      years),
    
    #display the total emissions for that year
    shiny::htmlOutput("total")
  ),

  shiny::mainPanel(
    #display the treemap
    plotly::plotlyOutput("treemap")
  )
  
)

#define the code that will create the app
server <- function(input, output, session) {
  
  #define a function for creating treemap
  producePlot <- shiny::reactive({
  
    
    #'convert raw data to format for plotting as treemap
    #'this requires getting the 'child-parent pairs' i.e. parent is the category that the child
    #'sits within. The way I am currently doing this is not very efficient so may need a rethink.
    plotData <- inventoryData %>%
      dplyr::filter(year == input$selectedYear) %>% #filter data to selected year
      dplyr::group_by(sector, ippcId, activityName, gasType) %>% #group by the treemap categories (currently sourcename not included as has too many categories)
      dplyr::summarise(emissionTotal = sum(emissions), .groups = "drop_last") %>% #sum up the emissions within each group
      dplyr::filter(emissionTotal >= 0) %>% #remove negative emissions as treemap cant deal with these, in future could plot these in a second plot?
      tibble::rownames_to_column() %>% #convert the row id to a column for later use
      tidyr::pivot_longer(sector:gasType, names_to = "labels", values_to = "values") %>% #pivot the chain of categories to long format
      dplyr::mutate(labels = dplyr::case_when(labels == "sector" ~ 1,
                                              labels == "ippcId" ~ 2,
                                              labels == "activityName" ~ 3,
                                              labels == "gasType" ~ 4,
                                              TRUE ~ NA)) #convert category string to a number
    
    plotData$parents <- "" #create empty column for parent categories
    
    plotData1 <- dplyr::filter(plotData, labels == "1") #get all top level entries as these wont have a parent
    plotData2 <- dplyr::filter(plotData, labels != "1") #get the rest
    
    #for loop over all the non-top level entries
    for(i in 1:nrow(plotData2)){
    
      #insert the parent category for each row by looking up using row Id and category number
      plotData2$parents[i] <- plotData$values[plotData$rowname == plotData2$rowname[i] & plotData$labels == plotData2$labels[i]-1]
    
    }
    
    #rejoin top level entries
    plotData <- rbind(plotData1, plotData2)
    
    #final processing of data for treemap
    plotData %<>% dplyr::select(-rowname, -labels) %>% #drop unneeded columns
      dplyr::rename(labels = values, values = emissionTotal) %>% #rename columns
      dplyr::group_by(labels, parents) %>% 
      dplyr::summarise(values=sum(values), .groups = "drop_last") %>% #group and summarise to combine duplicate rows
      dplyr::mutate(color = dplyr::case_when(labels == "Energy" ~ "red",
                                             labels == "Industrial Processes" ~ "yellow",
                                             labels == "Agriculture" ~ "green",
                                             labels == "LULUCF" ~ "blue",
                                             labels == "Waste" ~ "purple")) #code a colour for each sector
    
    #create treemap
    plotly::plot_ly(
      type='treemap',
      labels=plotData$labels, #specify categories
      parents=plotData$parents, #specify parents
      values= plotData$values, #specify values
      branchvalues = "total", #specify summing method (values for higher level categories are not in addition to categories below)
      height=800,
      marker=list(colors=plotData$color) #specify group colours
    )
    
  })
  
  #define function for calculating total
  calcTotal <- shiny::reactive({
  
      groupedInventoryData <- inventoryData %>%
        dplyr::filter(year == input$selectedYear) %>% #filter data to selected year
        dplyr::summarise(emissionTotal = sum(emissions), .groups = "drop_last") #sum
        
  })
  
  #send ouputs to shiny app
  output$treemap <- plotly::renderPlotly(producePlot())
  output$total <- shiny::renderText(shiny::HTML(paste0("<b>","Total emissions: ","</b>", round(calcTotal(),2)))) #do a bit of formatting on the total emissions to make it look nice
  
}

#run the app
shiny::shinyApp(ui, server)
