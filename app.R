# Load packages
pkgs <- c("shiny", "shinythemes", "leaflet", "leaflet.extras", 
          "tidyverse", "plotly", "scales")

library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("cols", "vroom")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("layout", "plotly")

lapply(pkgs, require, character.only = TRUE)
# {scales} is for breaks_pretty() & breaks_extended()


### SECTION 1: Read & clean data, make static objects ###

# 1.1. Read population data
population <- read_csv("population.csv",
                       col_types = cols(quarter = col_double(),
                                        region = col_character(),
                                        population = col_integer())) 

# 1.2. Read and clean COVID-19 data 
covid_data <- read_csv("covid19_canada_data.csv", 
         # read_csv figures type by 1st 1000 rows; if all NA == problem.
                  col_types = cols_only(prname = col_character(),
                                        date = col_date(format = "%d-%m-%Y"),
                                        numdeaths = col_integer(),
                                        numtotal = col_integer(),
                                        numrecover = col_integer(),
                                        numtested = col_integer(),
                                        numtests = col_integer(),
                                        numtoday = col_integer()), 
                  na = c("", "NA", "N/A")) %>% 
  rename("region" = "prname", "total_deaths" = "numdeaths", 
         "total_cases" = "numtotal", "total_recovered" = "numrecover", 
         "new_cases" = "numtoday") %>% 
  mutate(total_tested = coalesce(numtested, numtests)) %>% 
  select(-c(numtested, numtests)) %>% 
  mutate_at(vars("total_deaths":"total_tested"), # remove all non-numeric characters, if any
            list(~str_remove_all(., "[^0-9]"))) %>% 
  mutate_at(vars("total_deaths":"total_tested"), as.numeric) %>% 
  mutate_if(is.numeric, abs) %>% 
  filter(!grepl("^Repatriated", region)) %>% # drop 'repatriated travelers' category (irrelevant)
  filter(date >= as.Date("2020-03-21")) %>% # removes highly incomplete data prior to March 21
  mutate(quarter = lubridate::quarter(lubridate::ymd(date, truncated = 1), 
                                      with_year = TRUE)) %>% # create key column to join population estimates by quarter
  left_join(population, by = c("quarter", "region")) %>% 
  group_by(region) %>% 
  mutate(new_tested = total_tested - lag(total_tested,
                                         default = first(total_tested), 
                                         order_by = date))  %>% 
  mutate(new_deaths = total_deaths - lag(total_deaths, 
                                         default = first(total_deaths), 
                                         order_by = date))  %>%           # next 2 lines remove highly incomplete recoveries data prior to April 18
  mutate(tmp_na = if_else(date < as.Date("2020-04-18"), NA_real_, 0)) %>%
  mutate(total_recovered = total_recovered + tmp_na) %>% # a hack to convert some vals to NAs based on vals in the 'date' col
  mutate(active_cases = total_cases - total_recovered) %>% 
  select(-total_recovered, -tmp_na, -quarter) %>% # these cols no longer needed
  mutate(case_fatality_rate = round(total_deaths / total_cases * 100, 2)) %>% # case fatality rate
  mutate(case_fatality_rate = replace_na(case_fatality_rate, 0)) %>% # replace NAs introduced by division by 0
  mutate(mortality_per_100000 = round(total_deaths / population * 100000, 2)) %>% 
  mutate(cases_per_100000 = round(total_cases / population * 100000, 2)) %>% # prevalence: cases per 100,000
  mutate(active_cases_per_100000 = round(active_cases / population * 100000, 2)) %>% 
  mutate(new_cases_per_100000 = round(new_cases / population * 100000, 2)) %>% 
  mutate(tests_per_1000 = round(total_tested / population * 1000, 2)) %>%
  ungroup() %>%             # Making sure that 'covid$region' and 'provinces$region' are in the same order,
  group_by(date) %>%        # else will incorrectly map regions to polygons in Leaflet.
  arrange(date, region) %>% 
  ungroup() %>%             
  naniar::replace_with_na_if(.predicate = is.numeric, condition = ~.x < 0)
  
# Re-arrange columns for convenience  
covid_data <- covid_data[c("region", "population", "date", 
                 "total_cases", "cases_per_100000", 
                 "active_cases", "active_cases_per_100000", 
                 "new_cases", "new_cases_per_100000", 
                 "total_deaths", "new_deaths", 
                 "case_fatality_rate", "mortality_per_100000",
                 "total_tested", "new_tested", "tests_per_1000")]

# 1.3. Read Canada's geography
provinces <- sf::st_read("provinces.gpkg", 
                         stringsAsFactors = FALSE,
                         quiet = TRUE)

# 1.4. CRS to re-project map
epsg2163 <- leafletCRS(crsClass = "L.Proj.CRS",
                       code = "EPSG:2163",
                       resolutions = 2^(16:7),
                       proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0=-97 +lat_0=62.3 +units=m")

# 1.5. List dates for which colorQuantile() breaks are not unique
broken_dates <- covid_data %>% 
  filter(date >= "2020-05-01") %>% 
  pull(date) %>% 
  unique()

# 1.6. Make logical operator opposite of %in% for convenience
"%not_in%" <- Negate("%in%")

# 1.7. Make color palette for Canada and regions for plotly plot
colors_ca <- c("#1B9E77", "#6A3D9A", "#EF432C", "#E7298A", 
               "#66A61E", "#D95F02", "#A6761D", "#08306B", 
               "#E6AB02", "#1F78B4", "#7570B3", "#333333",
               "#993404", "#666666")

# 1.8. Specify font for use in plotly plot
my_font <- list(size = 14, color = "black") 

# 1.9. Make list of legend items to be selected by default in plotly plot  
legend_items <- list("Alberta" = TRUE, 
                     "British Columbia" = TRUE,
                     "Canada" = TRUE,
                     "Manitoba" = TRUE, 
                     "New Brunswick" = TRUE,
                     "Newfoundland and Labrador" = TRUE,
                     "Northwest Territories" = TRUE,
                     "Nova Scotia" = TRUE,
                     "Nunavut" = TRUE,
                     "Ontario" = TRUE,
                     "Prince Edward Island" = TRUE,
                     "Quebec" = TRUE,
                     "Saskatchewan" = TRUE,
                     "Yukon" = TRUE)

# 1.10. Make named list of all indicators to pass to the 'choices' arg
indicators_all <- list("Total cases" = "total_cases",
                       "Cases per 100,000" = "cases_per_100000",
                       "Active cases" = "active_cases",
                       "Active cases per 100,000" = "active_cases_per_100000",
                       "New cases" = "new_cases",
                       "New cases per 100,000" = "new_cases_per_100000",
                       "Total deaths" = "total_deaths",
                       "New deaths" = "new_deaths",
                       "Case fatality rate, %" = "case_fatality_rate",
                       "Mortality per 100,000" = "mortality_per_100000",
                       "Total tested / Total tests" = "total_tested",
                       "New tested / New tests" = "new_tested",
                       "Tested per 1,000 / Tests per 1,000" = "tests_per_1000")

# 1.11. Make named list of comparative indicators to pass to the 'choices' arg
indicators_comp <- list("Cases per 100,000" = "cases_per_100000",
                        "Active cases per 100,000" = "active_cases_per_100000",
                        "New cases per 100,000" = "new_cases_per_100000",
                        "Case fatality rate, %" = "case_fatality_rate",
                        "Mortality per 100,000" = "mortality_per_100000",
                        "Tested per 1,000 / Tests per 1,000" = "tests_per_1000")

# 1.12. Make list of regions to pass to the 'choices' arg: 
regions <- list("Canada", "Alberta", "British Columbia", "Manitoba", 
                "New Brunswick", "Newfoundland and Labrador", 
                "Northwest Territories", "Nova Scotia", "Nunavut", 
                "Ontario", "Prince Edward Island", "Quebec", 
                "Saskatchewan", "Yukon")


### Section 2. Make app ###

## 2.1. Define UI ##
ui <- 
navbarPage(
  title = "Open Data and Visualization for COVID-19 Impact on Canada", 
  theme = shinytheme("flatly"),
  tabPanel(title = "Main dashboard",
    sidebarLayout(
      sidebarPanel(
        h4("Make your selection"),
        width = 3,
        selectInput(inputId = "my_indicator", 
                    label = h5("Indicator - map and plot"), 
                    selected = "cases_per_100000",
                    choices = indicators_all),
        dateInput(inputId = "my_date", 
                  label = h5("Date - map only"), 
                  value = max(covid_data$date), # Set to last day !NA in the dataset.
                  min = min(covid_data$date),   # For most indicators, N too small before Mar 21, causes
                  max = max(covid_data$date),   # error in colorQuantile(): 'breaks' are not unique.
                  format = "dd MM yyyy"),
        tags$p(HTML("<div style='font-family:Inconsolata; font-size:11pt;'>If data for the chosen indicator is not available for the selected date, the date will revert to the last day, for which there is data.</div>")),
        selectInput(inputId = "my_region", 
                    label = h5("Region - plot only"), 
                    selected = "Canada",
                    choices = regions),
        dateRangeInput(inputId = "my_daterange", 
                       label = h5("Date range - plot only"), 
                       start = min(covid_data$date),
                       end = max(covid_data$date),
                       min = min(covid_data$date),
                       max = max(covid_data$date),
                       format = "dd M yyyy"),
        tags$p(HTML("<div style='font-family:Inconsolata; font-size:11pt;'>If data for the chosen indicator is not available for the selected date range, the date range will revert to the range, for which there is data.</div>")),
      ),
      mainPanel(width = 9,
        fluidRow(htmlOutput("map_title")),
        fluidRow( 
          splitLayout(cellWidths = c("64%", "34%"), # Prevents jittering caused by screen width conflict
                      leafletOutput("covid_map", height = "600px"),
                      plotOutput("covid_plot"))
        )
      )
    )
  ),
  tabPanel(title = "Compare regions",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput(inputId = "my_indicator2", 
                    label = h4("Choose indicator"), 
                    selected = "cases_per_100000",
                    choices = indicators_comp),
        tags$p(HTML("<div style='font-family:Inconsolata;'><h5>How to use:</h5></div>")),
        includeMarkdown("compare.md"),
        tags$p(HTML("")
        )
      ),
      mainPanel(
        width = 9,
        plotlyOutput(outputId = "plotly_plot",
                     height = "600px")
      )
    )
  ),
  tabPanel(title = "Download data",
    sidebarLayout(
      sidebarPanel(
        h4("Select and download data"),
        width = 5,
          checkboxGroupInput(inputId = "my_regions_group",
                           label = h5("Regions"),
                           choices = regions,
                           selected = "Canada",
                           inline = TRUE),
        actionButton(inputId = "select_all_regions", 
                     label = "Select all / Deselect all"),
        checkboxGroupInput(inputId = "my_indicators_group",
                           label = h5("Indicators"),
                           choices = indicators_all,
                           selected = indicators_all,
                           inline = TRUE),
        actionButton(inputId = "select_all_indicators", 
                     label = "Select all / Deselect all"),
        dateRangeInput(inputId = "my_daterange2", 
                       label = h5("Date range"), 
                       start = max(covid_data$date) - 30,
                       end = max(covid_data$date),
                       min = min(covid_data$date),
                       max = max(covid_data$date),
                       format = "dd M yyyy"),
        downloadButton(outputId = "downloader", 
                       label = h6("Download Selected Data")),
      ),
      mainPanel(
        width = 7,
        tableOutput("my_table")
      )
    )
  )
) # UI BLOCK ENDS


## 2.2. Define server logic ##
server <- function(input, output, session) {

# Disable scientific notation for readability
options(scipen = 999)

# 2.2.1. Get data 

# Get data for provinces
current_prov <- reactive({
  req(input$my_date, input$my_indicator)
    covid_data %>%
      filter(date == input$my_date) %>% 
      select(input$my_indicator, everything()) %>% 
      right_join(provinces, by = "region")         
})
  
# Get data for Canada
current_ca <- reactive({
  req(input$my_date)
  filter(covid_data, region == "Canada") %>% 
  filter(date == input$my_date) 
})

# Get data for the selected region
current_region <- reactive({
  req(input$my_region, input$my_daterange, input$my_indicator)
  covid_data %>%
    filter(region == input$my_region) %>%
    filter(between(date, input$my_daterange[1], input$my_daterange[2])) %>%
    select("indicator" = input$my_indicator, "region", "date")
  })

# 2.2.2. Create palette type-dependent on user's choice of indicator
# selector_i() and selector_d() conductors to help choose palette
selector_i <- reactive({
  input$my_indicator 
})

selector_d <- reactive({
  input$my_date
})

covid_pal <- 
  reactive({ 
    if (selector_i() == "total_cases" |
        selector_i() == "cases_per_100000" |
        (selector_i() == "active_cases" &
         selector_d() %not_in% broken_dates) |
        (selector_i() == "active_cases_per_100000" &
         selector_d() %not_in% broken_dates) |
        selector_i() == "total_tested" |
        selector_i() == "tests_per_1000") {
      colorQuantile(palette = "BuPu", n = 7,
                    domain = current_prov()[[1]]) # Shiny 'Data' input goes here (1)
    } else {
      colorNumeric(palette = "BuPu", 
                   domain = current_prov()[[1]])# Shiny 'Data' input goes here (1)
                   #bins = 5#, pretty = TRUE) 
    }
  })

# 2.2.3. Make map

# Create text output to use as map title
output$map_title <- 
  renderUI({
    HTML(
      paste(
        h4(str_to_sentence(str_replace_all(input$my_indicator, "_", " ")),
           "as of", 
           format(input$my_date, format = "%d %B %Y")),
        h6("Click province or marker for more information")))
  })

# Make map as a reactive conductor as potentially slower operation
leaflet_map <- reactive({
  leaflet(provinces, options = leafletOptions(crs = epsg2163,
                                              minZoom = 3, 
                                              maxZoom = 5)) %>% 
    setView(lng = -90, lat = 63.3, zoom = 3) %>% 
    setMapWidgetStyle(style = list(background = "#f5f5f5")) %>% 
    addPolygons(data = provinces,
                color = "black",
                weight = 1,
                opacity = .7,
                fillOpacity = .8, 
                fillColor = ~covid_pal()(current_prov()[[1]]),
                label = current_prov()$region,
                popup = paste("<div style='font-size: 15px'><b><h5>", 
                              current_prov()$region, "</h5></b>", 
                              "Date:", current_prov()$date, "<br>",
                              "Total cases:", current_prov()$total_cases, "<br>",
                              "Cases per 100,000:", current_prov()$cases_per_100000, "<br>",
                              "Active cases:", current_prov()$active_cases, "<br>",
                              "Active cases per 100,000:", current_prov()$active_cases_per_100000, "<br>",
                              "New cases:", current_prov()$new_cases, "<br>",
                              "New cases per 100,000:", current_prov()$new_cases_per_100000, "<br>",
                              "Total deaths:", current_prov()$total_deaths, "<br>",
                              "New deaths:", current_prov()$new_deaths, "<br>",
                              "Case fatality rate:", current_prov()$case_fatality_rate, "%", "<br>",
                              "Mortality per 100,000:", current_prov()$mortality_per_100000, "<br>",
                              "Total tested / Total tests:", current_prov()$total_tested, "<br>",
                              "New tested / New tests:", current_prov()$new_tested, "<br>",
                              "Tested per 1,000 / Tests per 1,000:", current_prov()$tests_per_1000, "</div>"),
                labelOptions = labelOptions(textsize = "15px",
                                            style = list("font-weight" = "bold")),
                highlightOptions = highlightOptions(weight = 1,
                                                    color = "#ffffff",
                                                    fillOpacity = 1,
                                                    bringToFront = TRUE)) %>% 
    addMarkers(lng = -85, 
               lat = 59, 
               label = "CANADA",
               popup = paste("<div style='font-size: 15px'><b><h5>", #*
                             "Canada", "</h5></b>", # note no <br> tag
                             "Date:", current_ca()$date, "<br>",
                             "Total cases:", current_ca()$total_cases, "<br>",
                             "Cases per 100,000:", current_ca()$cases_per_100000, "<br>",
                             "Active cases:", current_ca()$active_cases, "<br>",
                             "Active cases per 100,000:", current_ca()$active_cases_per_100000, "<br>",
                             "New cases:", current_ca()$new_cases, "<br>",
                             "New cases per 100,000:", current_ca()$new_cases_per_100000, "<br>",
                             "Total deaths:", current_ca()$total_deaths, "<br>",
                             "New deaths:", current_ca()$new_deaths, "<br>",
                             "Case fatality rate:", current_ca()$case_fatality_rate, "%", "<br>",
                             "Mortality per 100,000:", current_ca()$mortality_per_100000, "<br>",
                             "Total tested / Total tests:", current_ca()$total_tested, "<br>",
                             "New tested / New tests:", current_ca()$new_tested, "<br>",
                             "Tested per 1,000 / Tests per 1,000:", current_ca()$tests_per_1000, "</div>"),
               labelOptions = labelOptions(textsize = "15px",
                                           style = list("font-weight" = "bold")))
  })

# Pass map to outputs
output$covid_map <- renderLeaflet({leaflet_map()})

# 2.2.4. Make ggplot2 plot
output$covid_plot <- 
  renderPlot({
    current_region() %>%  
    ggplot(aes(x = date, 
               y = indicator)) + 
      geom_line(size = 1.2, color = "orchid1", na.rm = TRUE) +
      geom_area(fill = "orchid", alpha = .3, 
                na.rm = TRUE, 
                position = "identity") + 
      scale_x_date(breaks = breaks_pretty(n = 10)) +
      scale_y_continuous(breaks = breaks_extended(n = 8)) +
      theme_bw() +
      theme(plot.title = element_text(size = 14, 
                                      face = "bold", 
                                      hjust = .5,
                                      margin = margin(b = 10)),
            plot.margin = unit(c(0, 0.5, 1, 0.5), "cm"),
            panel.grid.minor = element_blank(),
            panel.grid.major = element_line(colour = "grey85"),
            axis.text = element_text(size = 13, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.title = element_blank()) +
      labs(title = paste0(input$my_region, ": ",
                          str_replace_all(input$my_indicator, "_", " "), 
                          ",\n", 
                          format(input$my_daterange[1], format = "%d %b %Y"),
                          " to ",
                          format(input$my_daterange[2], format = "%d %b %Y")))
  })

# 2.2.5. Make plotly plot to compare regions
# Make a reactive plot title
plot_title <- reactive({
  req(input$my_indicator2)
  str_replace_all(string = names(covid_data[input$my_indicator2]), "_", " ") %>%  
  str_to_title() %>% 
  paste("by Region")
})

# Make a reactive dataset, pre-subsetted for plotting
covid_plotly <- reactive({
  req(input$my_indicator2)
  covid_data %>% 
    group_by(region) %>% 
    select("region", "date", "indicator" = input$my_indicator2)
})

# Make plotly plot
output$plotly_plot <- 
  renderPlotly({
    my_plot <- 
      covid_plotly() %>% 
      plot_ly(x = ~date, 
              y = ~indicator, 
              color = ~region) %>% 
      add_lines(line = list(width = 3), 
                colors = colors_ca) %>% 
      layout(title = list(text = plot_title(),
                          x = .05, # align left
                          font = list(size = 18, color = "black")),
             legend = list(font = my_font),
             hoverlabel = list(font = list(size = 14)),
             margin = list(t = 50, pad = 5), # pad sets distance between tick marks and ticks
             xaxis = list(title = "", 
                          tickfont = my_font,
                          gridcolor = "#E0E0E0"),
             yaxis = list(title = "",
                          tickfont = my_font,
                          gridcolor = "#E0E0E0")) 

    my_plot <- plotly_build(my_plot)
    
    for (i in seq_along(my_plot$x$data)) {
      my_plot$x$data[[i]]$visible <- legend_items[[my_plot$x$data[[i]]$name]]
    }
    my_plot
  })

# 2.2.6. Make data table
selected_data <- reactive({
  req(input$my_regions_group, input$my_daterange2)
  covid_data %>% 
    filter(region %in% input$my_regions_group) %>% 
    filter(between(date, input$my_daterange2[1], input$my_daterange2[2])) %>% 
    select("region", "date", input$my_indicators_group) %>% 
    mutate_at(vars("date"), as.character)
})

output$my_table <- renderTable({selected_data()}, hover = TRUE)

# Select/deselect all regions and "Select all" button 
observe({
  if (input$select_all_regions == 0) return(NULL) 
  else if (input$select_all_regions %% 2 == 0) {
    updateCheckboxGroupInput(session, 
                             inputId = "my_regions_group",
                             choices = regions,
                             inline = TRUE)
  } else {
    updateCheckboxGroupInput(session, 
                             inputId = "my_regions_group",
                             choices = regions, 
                             selected = regions,
                             inline = TRUE)
  }
})

# Select/deselect all indicators and "Select all" button 
observe({
  if (input$select_all_indicators == 0) return(NULL) 
  else if (input$select_all_indicators %% 2 != 0) {
    updateCheckboxGroupInput(session, 
                             inputId = "my_indicators_group",
                             choices = indicators_all,
                             inline = TRUE)
  } else {
    updateCheckboxGroupInput(session, 
                             inputId = "my_indicators_group",
                             choices = indicators_all, 
                             selected = indicators_all,
                             inline = TRUE)
  }
})

# 2.2.7. Data downloader
output$downloader <-
  downloadHandler(
    filename = "covid_open_data_canada.csv",
    content = function(file) {
      write_csv(selected_data(), file)
    }
  )

} # SERVER BLOCK ENDS
  
## 2.3. Run app ##
shinyApp(ui = ui, server = server)