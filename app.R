#' TO DO:
#' Tickboxes to allow selecting multiple years
#' Currently just remove negative emissions, could plot these in a second plot
#' implement as circles using anne's package
#' Add filter for fuel type
#' Improve code quality - lots of inefficiency and repeated code

#' DONE:
#' Make labels clearer, i.e. simplify appended colons and translate IPPC
#' codes (using https://naei.beis.gov.uk/glossary?view=crf)
#' Add filter for gas
#' historical visualisations

# Set up ------------------------------------------------------------------

# import the magrittr package to allow use of pipes (%>%)
library(magrittr)

# Data import  ------------------------------------------------------------

#' import the emissions data, skipping 6 rows at the top of the spreadsheet and
#' specifying the data type
raw_inventory_data <-
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


# Data cleaning -----------------------------------------------------------

# tidy up the raw data
inventory_data <- raw_inventory_data %>%
  janitor::clean_names() %>% # standardise the names of the columns
  dplyr::filter(!is.na(sector)) %>%
  # filter out totals rows at bottom of spreadsheet
  dplyr::select(level_1:level_9,
    fuel = activity_name,
    greenhouse_gas = short_poll_name,
    x2017:x2021
  ) %>% # select and rename columns
  dplyr::group_by(across(level_1:greenhouse_gas)) %>%
  dplyr::summarise(dplyr::across(x2017:x2021, ~ sum(.x, na.rm = TRUE))) %>%
  # sum any identical rows
  tidyr::pivot_longer(x2017:x2021,
    names_to = "year",
    values_to = "emissions"
  ) %>%
  # pivot data to long format i.e. a row for each source in each year
  dplyr::mutate(year = as.numeric(gsub("x", "", year))) %>%
  # remove the x in front of years as now row entries rather than column names
  dplyr::ungroup()

#' sort naughty NAs in between categories - replace them with the a category
#' called "Other <category below>"
fixed_inventory_data <- inventory_data %>%
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

# pivot dataset to long format so we have a row for each category
all_categories <- fixed_inventory_data %>%
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
  tidyr::pivot_longer(level_0_1:level_8_9,
    names_to = "level",
    values_to = "category"
  ) %>%
  dplyr::mutate(label = sub(".*:", "", category)) %>%
  dplyr::filter(label != "NA") %>%
  unique()

# find labels that are used more than once in different areas of the inventory
duplicated_categories <- all_categories %>%
  dplyr::group_by(label) %>%
  dplyr::mutate(category_count = dplyr::n()) %>%
  dplyr::filter(category_count > 1) %>%
  dplyr::select(level, category = label) %>%
  dplyr::mutate(level = paste0("level", sub("level\\_\\d", "", level))) %>%
  # get the actual level the duplicate is at
  unique()

#' sort naughty repeated categories e.g. 'boar' flows into both 'pigs' and
#' 'sheep' use the category above in brackets after e.g. Boar (Pigs) and
#' Boar (Sheep)
#' bet there's a better way to do this where you need only one mutate and it
#' just goes over the columns
fixed_inventory_data_2 <- fixed_inventory_data %>%
  dplyr::mutate(level_2 = dplyr::case_when(
    level_2 %in% dplyr::pull(
      dplyr::filter(duplicated_categories, level == "level_2"),
      category
    ) ~ paste0(level_2, " (", level_1, ")"),
    .default = level_2
  )) %>%
  dplyr::mutate(level_3 = dplyr::case_when(
    level_3 %in% dplyr::pull(
      dplyr::filter(duplicated_categories, level == "level_3"),
      category
    ) ~ paste0(level_3, " (", level_2, ")"),
    .default = level_3
  )) %>%
  dplyr::mutate(level_4 = dplyr::case_when(
    level_4 %in% dplyr::pull(
      dplyr::filter(duplicated_categories, level == "level_4"),
      category
    ) ~ paste0(level_4, " (", level_3, ")"),
    .default = level_4
  )) %>%
  dplyr::mutate(level_5 = dplyr::case_when(
    level_5 %in% dplyr::pull(
      dplyr::filter(duplicated_categories, level == "level_5"),
      category
    ) ~ paste0(level_5, " (", level_4, ")"),
    .default = level_5
  )) %>%
  dplyr::mutate(level_6 = dplyr::case_when(
    level_6 %in% dplyr::pull(
      dplyr::filter(duplicated_categories, level == "level_6"),
      category
    ) ~ paste0(level_6, " (", level_5, ")"),
    .default = level_6
  )) %>%
  dplyr::mutate(level_7 = dplyr::case_when(
    level_7 %in% dplyr::pull(
      dplyr::filter(duplicated_categories, level == "level_7"),
      category
    ) ~ paste0(level_7, " (", level_6, ")"),
    .default = level_7
  )) %>%
  dplyr::mutate(level_8 = dplyr::case_when(
    level_8 %in% dplyr::pull(
      dplyr::filter(duplicated_categories, level == "level_8"),
      category
    ) ~ paste0(level_8, " (", level_7, ")"),
    .default = level_8
  )) %>%
  dplyr::mutate(level_9 = dplyr::case_when(
    level_9 %in% dplyr::pull(
      dplyr::filter(duplicated_categories, level == "level_9"),
      category
    ) ~ paste0(level_9, " (", level_8, ")"),
    .default = level_9
  ))

fixed_inventory_data_3 <- fixed_inventory_data_2 %>%
  dplyr::filter(emissions >= 0)
#' remove negative emissions as treemap cant deal with these, in future could
#' plot these in a second plot?

fixed_inventory_data_negative <- fixed_inventory_data_2 %>%
  dplyr::filter(emissions < 0)


# Reshape data for plotting -----------------------------------------------

#' pivot dataset to long format so we have a row for each category and sum any
#' identical rows
layered_inventory_data <- fixed_inventory_data_3 %>%
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
  tidyr::pivot_longer(level_0_1:level_8_9,
    names_to = "col_names",
    values_to = "levels"
  ) %>%
  dplyr::select(-col_names) %>%
  dplyr::group_by(year, levels, greenhouse_gas) %>%
  dplyr::summarise(emissions = sum(emissions)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!grepl(":NA$", levels))

# split levels into parent-child pairs
split_cols <-
  stringr::str_split_fixed(layered_inventory_data$levels, ":", 2)

# formatting for plotting
parent_child_inventory_data <- layered_inventory_data %>%
  cbind(split_cols) %>%
  dplyr::select(
    parent = "1",
    child = "2",
    greenhouse_gas,
    emissions,
    year
  )

# bad repeated code for negative emissions --------------------------------

layered_inventory_data_negative <- fixed_inventory_data_negative %>%
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
  tidyr::pivot_longer(level_0_1:level_8_9,
    names_to = "col_names",
    values_to = "levels"
  ) %>%
  dplyr::select(-col_names) %>%
  dplyr::group_by(year, levels, greenhouse_gas) %>%
  dplyr::summarise(emissions = sum(emissions)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!grepl(":NA$", levels))

split_cols <-
  stringr::str_split_fixed(layered_inventory_data_negative$levels, ":", 2)

parent_child_inventory_data_negative <-
  layered_inventory_data_negative %>%
  cbind(split_cols) %>%
  dplyr::select(
    parent = "1",
    child = "2",
    greenhouse_gas,
    emissions,
    year
  )

# Get data for line graphs ------------------------------------------------

# get data for historic plot
historic_data <- inventory_data %>%
  dplyr::group_by(level_1, year, greenhouse_gas) %>%
  dplyr::summarise(emissions = sum(emissions))

# Get options for Shiny selects -----------------------------------------

# get the years, sectors and gases of data available for user to select from
years <- unique(parent_child_inventory_data$year)
gases <- unique(parent_child_inventory_data$greenhouse_gas)
sectors <- unique(inventory_data$level_1)


# R Shiny design ----------------------------------------------------------

# create user interface for R shiny
ui <- shiny::fluidPage(
  # app title
  shiny::titlePanel("Net Zero Inventory Mapping",
    windowTitle = "Net Zero Inventory Mapping"
  ),
  shiny::tabsetPanel(
    shiny::tabPanel(
      "Annual Sources",
      shiny::br(),
      shiny::sidebarPanel(
        # allow user to select a year, default set to most recent year
        shiny::selectInput(
          "selectedYear",
          "Choose year:",
          selected = max(years),
          choices = years
        ),

        # allow user to select a gas, default set to all
        shiny::checkboxGroupInput(
          "selectedGas",
          "Choose gases:",
          selected = gases,
          choices = gases
        ),

        # display the total emissions for that year
        shiny::htmlOutput("total")
      ),
      shiny::mainPanel( # display the treemap
        plotly::plotlyOutput("treemap")
      )
    ),
    shiny::tabPanel(
      "Historic Totals",
      shiny::br(),
      shiny::sidebarPanel(
        # allow user to select sectors, default set to all
        shiny::checkboxGroupInput(
          "selectedSectorsHistoric",
          "Choose sectors:",
          selected = sectors,
          choices = sectors
        ),

        # allow user to select which years to graph, default set to all
        shiny::checkboxGroupInput(
          "selectedYearsHistoric",
          "Choose years:",
          selected = years,
          choices = years
        ),

        # allow user to select gases, default set to all
        shiny::checkboxGroupInput(
          "selectedGasHistoric",
          "Choose gases:",
          selected = gases,
          choices = gases
        )
      ),
      shiny::mainPanel( # display the historic plot
        plotly::plotlyOutput("historic")
      )
    ),
    shiny::tabPanel(
      "Negative Emissions",
      shiny::br(),
      shiny::helpText("Note: Not to scale with with annual sources plot."),
      plotly::plotlyOutput("negative_treemap")
    )
  )
)


# R Shiny calculations ----------------------------------------------------

# define the code that will create the app
server <- function(input, output, session) {
  # define a function for creating treemap
  produce_plot <- shiny::reactive({
    plot_data <- parent_child_inventory_data %>%
      dplyr::filter(year == input$selectedYear &
        greenhouse_gas %in% input$selectedGas) %>%
      dplyr::group_by(parent, child) %>%
      dplyr::summarise(emissions = sum(emissions)) %>%
      dplyr::mutate(
        color = dplyr::case_when(
          child == "Energy" ~ "red",
          child == "Industrial Processes" ~ "yellow",
          child == "Agriculture" ~ "green",
          child == "LULUCF" ~ "blue",
          child == "Waste" ~ "purple"
        )
      ) # code a colour for each sector

    # create treemap
    plotly::plot_ly(
      type = "treemap",
      labels = plot_data$child,
      # specify categories
      parents = plot_data$parent,
      # specify parents
      values = plot_data$emissions,
      # specify values
      branchvalues = "total",
      #' specify summing method (values for higher level categories are not in
      #' addition to categories below)
      height = 800,
      marker = list(colors = plot_data$color),
      # specify group colours
      texttemplate = "%{label} %{value:.2f} MtCO2eq",
      hovertemplate = "%{label} %{value:.2f} MtCO2eq<extra></extra>"
    )
  })

  #' need to work out how best to do this as how should I make the negative
  #' emissions the right scale when compared to positive - could just report a
  #' summary in a table
  produce_plot_negative <- shiny::reactive({
    plot_data_negative <- parent_child_inventory_data_negative %>%
      dplyr::filter(year == input$selectedYear &
        greenhouse_gas %in% input$selectedGas) %>%
      dplyr::group_by(parent, child) %>%
      dplyr::summarise(emissions = -sum(emissions)) %>%
      dplyr::mutate(
        color = dplyr::case_when(
          child == "Energy" ~ "red",
          child == "Industrial Processes" ~ "yellow",
          child == "Agriculture" ~ "green",
          child == "LULUCF" ~ "blue",
          child == "Waste" ~ "purple"
        )
      ) # code a colour for each sector

    # create treemap
    plotly::plot_ly(
      type = "treemap",
      labels = plot_data_negative$child,
      # specify categories
      parents = plot_data_negative$parent,
      # specify parents
      values = plot_data_negative$emissions,
      # specify values
      branchvalues = "total",
      #' specify summing method (values for higher level categories are not in
      #' addition to categories below)
      marker = list(colors = plot_data_negative$color),
      # specify group colours
      texttemplate = "%{label} %{value:.2f} MtCO2eq",
      hovertemplate = "%{label} %{value:.2f} MtCO2eq<extra></extra>"
    )
  })


  # define function for calculating total
  calc_total <- shiny::reactive({
    inventory_data %>%
      dplyr::filter(year == input$selectedYear) %>%
      dplyr::filter(greenhouse_gas %in% input$selectedGas) %>%
      dplyr::summarise(
        emissionTotal = sum(emissions),
        .groups = "drop_last"
      ) # sum
  })

  # define a function for creating historic plot
  produce_historic_plot <- shiny::reactive({
    # filter to chosen data
    historic_data %<>%
      dplyr::filter(
        year %in% input$selectedYearsHistoric &
          greenhouse_gas %in% input$selectedGasHistoric &
          level_1 %in% input$selectedSectorsHistoric
      ) %>%
      dplyr::group_by(level_1, year) %>%
      dplyr::summarise(emissions = sum(emissions))

    # create historic graph
    plotly::plot_ly(
      historic_data,
      # define x and y axes and colours of lines
      x = ~year,
      y = ~emissions,
      color = ~level_1,
      # specify graph type
      type = "scatter",
      mode = "lines+markers",
      # format data point labels and axis titles
      hoverinfo = "text+name",
      text = ~ paste(round(emissions, 2), "MtCO2eq")
    ) %>%
      plotly::layout(
        yaxis = list(title = "Emissions (MtCO2eq)"),
        xaxis = list(title = "Year", dtick = 1)
      )
  })

  # send ouputs to shiny app
  output$treemap <- plotly::renderPlotly(produce_plot())
  output$total <-
    shiny::renderText(shiny::HTML(paste0(
      "<b>",
      "Total emissions: ",
      "</b>",
      round(calc_total(), 2),
      " MtCO2eq"
    ))) # do a bit of formatting on the total emissions to make it look nice
  output$historic <- plotly::renderPlotly(produce_historic_plot())
  output$negative_treemap <- plotly::renderPlotly(produce_plot_negative())
}

# Run R Shiny app ---------------------------------------------------------

# run the app
shiny::shinyApp(ui, server)
