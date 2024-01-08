#'TO DO:
#'Add filter for gas and/or fuel type
#'Tickboxes to allow selecting multiple years - plus historical visualisations
#'Currently I just remove negative emissions, could plot these in a second plot?
#' implement as circles using anne's package

#'DONE:
#'Make labels clearer, i.e. simplify appended colons and translate IPPC codes (using https://naei.beis.gov.uk/glossary?view=crf)

#import the magrittr package to allow use of pipes (%>%)
library(magrittr)

#import the emissions data, skipping 6 rows at the top of the spreadsheet and specifying the data type
raw_inventoryData <-
  readxl::read_excel(
    "National Inventory Mapping 2021.xlsx",
    sheet = "Formatted Data",
    col_types = c(
      "text",
      "text",
      "text",
      "text",
      "numeric",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  )

#tidy up the raw data
inventoryData <- raw_inventoryData %>%
  janitor::clean_names() %>% #standardise the names of the columns
  dplyr::filter(!is.na(sector)) %>% #filter out totals rows at bottom of spreadsheet
  dplyr::select(level_1:level_9, fuel = activity_name, greenhouse_gas = short_poll_name, x2017:x2021) %>% #select and rename columns
  dplyr::group_by(across(level_1:greenhouse_gas)) %>%
  dplyr::summarise(dplyr::across(x2017:x2021, ~ sum(.x, na.rm = TRUE))) %>% #sum any identical rows
  tidyr::pivot_longer(x2017:x2021, names_to = "year", values_to = "emissions") %>% #pivot the spreadsheet to long format i.e. a row for each source in each year
  dplyr::mutate(year = as.numeric(gsub("x","", year))) %>% #remove the x in front of years as now row entries rather than column names
  dplyr::ungroup()

#sort naughty NAs in between categories - replace them with the a category called "Other [category below]"
fixed_inventoryData <- inventoryData %>%
  dplyr::mutate(level_7 = dplyr::case_when(
    is.na(level_7) & !is.na(level_8) ~ paste0("Other ", level_8),
    .default = level_7
  )) %>%
  dplyr::mutate(level_6 = dplyr::case_when(
    is.na(level_6) & !is.na(level_7) ~ paste0("Other ", level_7),
    .default = level_6
  )) %>%
  dplyr::mutate(level_5 = dplyr::case_when(
    is.na(level_5) & !is.na(level_6) ~ paste0("Other ", level_6),
    .default = level_5
  )) %>%
  dplyr::mutate(level_4 = dplyr::case_when(
    is.na(level_4) & !is.na(level_5) ~ paste0("Other ", level_5),
    .default = level_4
  ))

#'sort naughty repeated categories e.g. 'boar' flows into both 'pigs' and 'sheep'
#'use the category above in brackets after e.g. Boar (Pigs) and Boar (Sheep)
all_categories <- fixed_inventoryData %>%
  dplyr::select(level_1:level_9) %>%
  dplyr::mutate(level_0_1 = paste0("", ":", level_1)) %>%
  dplyr::mutate(level_1_2 = paste0(level_1, ":", level_2)) %>%
  dplyr::mutate(level_2_3 = paste0(level_2, ":", level_3)) %>%
  dplyr::mutate(level_3_4 = paste0(level_3, ":", level_4)) %>%
  dplyr::mutate(level_4_5 = paste0(level_4, ":", level_5)) %>%
  dplyr::mutate(level_5_6 = paste0(level_5, ":", level_6)) %>%
  dplyr::mutate(level_6_7 = paste0(level_6, ":", level_7)) %>%
  dplyr::mutate(level_7_8 = paste0(level_7, ":", level_8)) %>%
  dplyr::mutate(level_8_9 = paste0(level_8, ":", level_9)) %>%
  dplyr::select(level_0_1:level_8_9) %>%
  tidyr::pivot_longer(level_0_1:level_8_9, names_to = "level", values_to = "category") %>% 
  dplyr::mutate(label = sub(".*:","", category)) %>%
  dplyr::filter(label != "NA") %>% unique()
  
duplicated_categories <- all_categories %>%
  dplyr::group_by(label) %>%
  dplyr::mutate(category_count = dplyr::n()) %>%
  dplyr::filter(category_count > 1) %>%
  dplyr::select(level, category = label) %>%
  dplyr::mutate(level = paste0("level", sub("level\\_\\d", "", level))) %>%
  unique()

#bet theres a better way to do this where you need only one mutate and it just goes over the columns
fixed_inventoryData_2 <- fixed_inventoryData %>%
  dplyr::mutate(level_2 = dplyr::case_when(
    level_2 %in% dplyr::pull(dplyr::filter(duplicated_categories, level == "level_2"), category) ~ paste0(level_2, " (", level_1, ")"),
    .default = level_2
  )) %>%
  dplyr::mutate(level_3 = dplyr::case_when(
    level_3 %in% dplyr::pull(dplyr::filter(duplicated_categories, level == "level_3"), category) ~ paste0(level_3, " (", level_2, ")"),
    .default = level_3
  )) %>%
  dplyr::mutate(level_4 = dplyr::case_when(
    level_4 %in% dplyr::pull(dplyr::filter(duplicated_categories, level == "level_4"), category) ~ paste0(level_4, " (", level_3, ")"),
    .default = level_4
  )) %>%
  dplyr::mutate(level_5 = dplyr::case_when(
    level_5 %in% dplyr::pull(dplyr::filter(duplicated_categories, level == "level_5"), category) ~ paste0(level_5, " (", level_4, ")"),
    .default = level_5
  )) %>%
  dplyr::mutate(level_6 = dplyr::case_when(
    level_6 %in% dplyr::pull(dplyr::filter(duplicated_categories, level == "level_6"), category) ~ paste0(level_6, " (", level_5, ")"),
    .default = level_6
  )) %>%
  dplyr::mutate(level_7 = dplyr::case_when(
    level_7 %in% dplyr::pull(dplyr::filter(duplicated_categories, level == "level_7"), category) ~ paste0(level_7, " (", level_6, ")"),
    .default = level_7
  )) %>%
  dplyr::mutate(level_8 = dplyr::case_when(
    level_8 %in% dplyr::pull(dplyr::filter(duplicated_categories, level == "level_8"), category) ~ paste0(level_8, " (", level_7, ")"),
    .default = level_8
  )) %>%
  dplyr::mutate(level_9 = dplyr::case_when(
    level_9 %in% dplyr::pull(dplyr::filter(duplicated_categories, level == "level_9"), category) ~ paste0(level_9, " (", level_8, ")"),
    .default = level_9
  )) %>%
  dplyr::filter(emissions >= 0) #remove negative emissions as treemap cant deal with these, in future could plot these in a second plot?
  
#converting the data to parent-child pairs
layered_inventoryData <- fixed_inventoryData_2 %>%
  dplyr::mutate(level_0_1 = paste0("", ":", level_1)) %>%
  dplyr::mutate(level_1_2 = paste0(level_1, ":", level_2)) %>%
  dplyr::mutate(level_2_3 = paste0(level_2, ":", level_3)) %>%
  dplyr::mutate(level_3_4 = paste0(level_3, ":", level_4)) %>%
  dplyr::mutate(level_4_5 = paste0(level_4, ":", level_5)) %>%
  dplyr::mutate(level_5_6 = paste0(level_5, ":", level_6)) %>%
  dplyr::mutate(level_6_7 = paste0(level_6, ":", level_7)) %>%
  dplyr::mutate(level_7_8 = paste0(level_7, ":", level_8)) %>%
  dplyr::mutate(level_8_9 = paste0(level_8, ":", level_9)) %>%
  dplyr::select(level_0_1:level_8_9, greenhouse_gas, year, emissions) %>%
  tidyr::pivot_longer(level_0_1:level_8_9, names_to = "col_names", values_to = "levels") %>%
  dplyr::select(-col_names) %>%
  dplyr::group_by(year,levels,greenhouse_gas) %>%
  dplyr::summarise(emissions = sum(emissions)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!grepl(":NA$", levels))
 
split_cols <- stringr::str_split_fixed(layered_inventoryData$levels, ':', 2)
 
parent_child_inventoryData <- layered_inventoryData %>%
  cbind(split_cols) %>%
  dplyr::select(parent = "1", child = "2", greenhouse_gas, emissions, year)

#get the years and gases of data available for user to select from 
years <- unique(parent_child_inventoryData$year) 
gases <- unique(parent_child_inventoryData$greenhouse_gas)

#create user interface for R shiny
ui <- shiny::fluidPage(
  
  #app title
  shiny::titlePanel("Net Zero Inventory Mapping", windowTitle = "Net Zero Inventory Mapping"),

  shiny::sidebarPanel(
    
    #allow user to select a year, default set to most recent year
    shiny::selectInput(
      "selectedYear",
      "Choose year:",
      selected = max(years),
      choices = years),
    
    #allow user to select a gas, default set to most CO2
    shiny::checkboxGroupInput(
      "selectedGas",
      "Choose gases:",
      selected = gases,
      choices = gases),
    
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
    
    plotData <- parent_child_inventoryData %>%
      dplyr::filter(year == input$selectedYear & greenhouse_gas %in% input$selectedGas) %>%
      dplyr::group_by(parent, child) %>%
      dplyr::summarise(emissions = sum(emissions)) %>%
      dplyr::mutate(color = dplyr::case_when(child == "Energy" ~ "red",
                                             child == "Industrial Processes" ~ "yellow",
                                             child == "Agriculture" ~ "green",
                                             child == "LULUCF" ~ "blue",
                                             child == "Waste" ~ "purple")) #code a colour for each sector
    
    #create treemap
    plotly::plot_ly(
      type='treemap',
      labels=plotData$child, #specify categories
      parents=plotData$parent, #specify parents
      values= plotData$emissions, #specify values
      branchvalues = "total", #specify summing method (values for higher level categories are not in addition to categories below)
      height=800,
      marker=list(colors=plotData$color) #specify group colours
    )
    
  })
  
  #define function for calculating total
  calcTotal <- shiny::reactive({

    inventoryData %>%
      dplyr::filter(year == input$selectedYear) %>%
      dplyr::filter(greenhouse_gas %in% input$selectedGas) %>%
      dplyr::summarise(emissionTotal = sum(emissions), .groups = "drop_last") #sum

  })

  #send ouputs to shiny app
  output$treemap <- plotly::renderPlotly(producePlot())
  output$total <- shiny::renderText(shiny::HTML(paste0("<b>","Total emissions: ","</b>", round(calcTotal(),2)))) #do a bit of formatting on the total emissions to make it look nice
  
}

#run the app
shiny::shinyApp(ui, server)
