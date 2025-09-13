# Install required packages
packages <- c("shiny", "shinydashboard", "shinyWidgets", "plotly", "ggplot2", "dplyr", 
              "seriation", "GGally", "RColorBrewer", "leaflet", "leaflet.providers", 
              "shinyBS", "geojsonio", "corrgram", "summarytools", "shinycssloaders", "rlang")

# Install missing packages
installed_packages <- installed.packages()[,"Package"]
missing_packages <- packages[!packages %in% installed_packages]
if(length(missing_packages)) install.packages(missing_packages)

# Load libraries
lapply(packages, library, character.only = TRUE)

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(shinyWidgets)
library(leaflet)
library(dplyr)
library(rlang)
library(summarytools)
library(corrgram)
library(reshape2)
library(RColorBrewer)
library(tidyr)
library(shinycssloaders)
library(geojsonio)
library(shinyBS)
library(htmlwidgets)


country_name <- ""
country <- "Australia"

page_title <- tags$a(
  href = ".", 
  tags$div(
    tags$img(src = "logo.png", height = '50', width = '50'), 
    'GeoStat', 
    style = "color: #40B8AF; font-family: Arial, Helvetica, sans-serif; font-size: 25px; vertical-align: 10px"
  )
)

file_path <- "./../data/world-data-2023.csv"
con <- file(file_path, open = "r")
line <- readLines(con, n = 1)
elements <- strsplit(line, ",")[[1]]
elements <- c(elements, "Continent")
close(con)

df_org_world_data <- read.csv(file_path)
df_continents <- read.csv("./../data/Continents.csv")
df_org_world_data <- left_join(df_org_world_data, df_continents, by = "Country")

df_world_data <- df_org_world_data
colnames(df_world_data) <- c("Country", "Density", "Abbreviation", "AgriculturalLand", "LandArea", "ArmedForces", "BirthRate", "CallCode", "CapitalCity", "CO2Emissions", "CPI", "CPIChange", "CurrencyCode", "FertilityRate", "ForestArea", "GasPrice", "GDP", "PrimaryEducation", "TertiaryEducation", "InfantMortality", "LargestCity", "LifeExpectancy", "MaternalMortalityRatio", "MinWage", "OfficialLanguage", "HealthExpenditure", "PhysiciansPerThousand", "Population", "LaborForce", "TaxRevenue", "TaxRate", "UnemploymentRate", "UrbanPopulation", "Latitude", "Longitude", "Continent")

# Download the GeoJSON file
download.file("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", destfile="countries.geojson")

# Read the GeoJSON file
world <- geojson_read("countries.geojson", what = "sp")


# clean the data to remove ',', '$' and '%' characters
columns <- c()

for (col in names(df_world_data)) {
  if (any(grepl("\\$", df_world_data[[col]]))) {
    df_world_data[[col]] <- gsub("\\$", "", df_world_data[[col]])
    columns <- c(columns, col)
  }
}

for (col in names(df_world_data)) {
  if (any(grepl(",", df_world_data[[col]]))) {
    columns <- c(columns, col)
    df_world_data[[col]] <- gsub(",", "", df_world_data[[col]])
    
  }
}
for (col in names(df_world_data)) {
  if (any(grepl("%", df_world_data[[col]]))) {
    columns <- c(columns, col)
    df_world_data[[col]] <- gsub("%", "", df_world_data[[col]])
  }
}

# apply appropriate datatypes to columns 
for (col in columns){
  if(col != 'LargestCity' & col != 'CapitalCity'){
    if (any(grepl(".", df_world_data[[col]]))) {
      df_world_data[[col]] <- as.numeric(df_world_data[[col]])
    }
    else{
      df_world_data[[col]] <- as.integer( df_world_data[[col]])
    }
  }
}

# Ensure the country names match between your data and the GeoJSON file
# You might need to adjust this step based on the actual country names in your dataset
colnames(world@data) = c("id", "Country")

# Merge the GeoJSON data with your dataset based on country names
world@data <- left_join(world@data, df_world_data, by = "Country")

# Convert relevant columns to numeric
df_world_data$Population <- as.numeric(gsub(",", "", df_world_data$Population))

# Filter and select necessary columns
world_data <- df_world_data %>%
  select(Country, Population, CO2Emissions, GDP, UrbanPopulation, Latitude, Longitude)

# reusable footer block
footer <- box("GeoStat - Analysing Global Socio-Economic Patterns",
              tags$a(class = "pull-right", tags$div(tags$img(src = "logo.png", height = '30', width = '30'), 'GeoStat', style="color: #40B8AF; font-family: Arial, Helvetica, sans-serif;font-size: 15px;vertical-align: 10px"), href = "."),
              width = 12, background = 'black', style="color:#FBF5F3", height = 50
)

# UI-side code
ui <- fluidPage(
  shinyUI(
    dashboardPage(
      title = "GeoStat: Analysing Global Socio-Economic Patterns",
      # header
      dashboardHeader(title = page_title, titleWidth = 200),
      
      # sidebar
      dashboardSidebar( width = 200,
                        sidebarMenu(id = "sidebarMenu",
                                    menuItem(text = "Dashboard", tabName = "dashboard", icon = icon("dashboard", class = "fa-lg", style = "margin-right:5px;")),
                                    menuItem(text = "Global Data", icon = icon("chart-column", class = "fa-lg", style = "margin-right:5px;"),
                                             menuSubItem(text = "Global Country Data - 2023", tabName = "world_data", icon = icon("table", class = "fa-lg", style = "margin-right:5px;")),
                                             menuSubItem(text = "Data Summary View", tabName = "summary_view", icon = icon("bars-progress", class = "fa-lg", style = "margin-right:5px;"))
                                    ),
                                    menuItem(text = "Data Visualisation", icon = icon("map", class = "fa-lg", style = "margin-right:5px;"),
                                             menuSubItem(text = "Pairs Plot", tabName = "pairs_plot_tab", icon = icon("chart-bar", class = "fa-lg", style = "margin-right:5px;")),
                                             menuSubItem(text = "Data Correlation", tabName = "data_correlations_tab", icon = icon("cogs", class = "fa-lg", style = "margin-right:5px;")),
                                             menuSubItem(text = "Homogeneity of Data", tabName = "homogeneity_tab", icon = icon("line-chart", class = "fa-lg", style = "margin-right:5px;")),
                                             menuSubItem(text = "Heatmap of Data", tabName = "heatmap_tab", icon = icon("calendar", class = "fa-lg", style = "margin-right:5px;")),
                                             menuSubItem(text = "Scatter Plots", tabName = "scatter_plots_tab", icon = icon("bullseye", class = "fa-lg", style = "margin-right:5px;")),
                                             menuSubItem(text = "Box Plots", tabName = "box_plots_tab", icon = icon("camera", class = "fa-lg", style = "margin-right:5px;"))
                                    ),
                                    menuItem(text = "Advance Chart", icon = icon("chart-column", class = "fa-lg", style = "margin-right:5px;"),
                                             menuSubItem(text = "Economic Impact on Health", tabName = "advance_gdp_tab", icon = icon("chart-pie", class = "fa-lg", style = "margin-right:5px;")),
                                             menuSubItem(text = "Economic Impact on Environment", tabName = "advance_socio_tab", icon = icon("globe", class = "fa-lg", style = "margin-right:5px;"))
                                    ),
                                    menuItem(text = "About", tabName = "about_app", icon = icon("file-lines", class = "fa-lg", style = "margin-right:5px;"), selected = TRUE)
                        )
                        
      ),
      
      # body
      dashboardBody(
        # apply CSS, browser title and icon
        tags$head(tags$title("GeoStat"),
                  tags$link(rel = 'stylesheet', type = 'text/css', href = "custom.css"),
                  tags$link(rel = "icon", type = "image/x-icon", href = "logo.ico")
                  ),
        
        # tab pages
        tabItems(
          # Dashboard
          tabItem(tabName = "dashboard",
                  fluidRow(style="font-weight: bold;",
                           box(title = "", 
                               fluidRow(
                                 column( width = 3,
                                         div(style="color: black; font-family: Arial, Helvetica, sans-serif;font-size: 15px;vertical-align: 10px",
                                             div(id = "country_icon","Total Number of Countries", style = "text-indent: 10px;padding-top: 5px;background-color:#FBF5F3; height: 40px; margin-bottom: 2px;border-radius:4px 4px 0px 0px", 
                                                 icon("info-circle", style="float: right;margin-right:5px;"),
                                                 bsTooltip(id = "country_icon", title = "This is the total number of countries in our dataset.", placement = "right", trigger = "hover")
                                              ),
                                             div(nrow(df_world_data), style = "padding-top: 15px;background-color:#FBF5F3; font-size: 50px; font-weight: bold;text-align: center;height: 100px; border-radius:0px 0px 4px 4px"),
                                         )
                                 ),
                                 
                                 column(
                                   width = 3, 
                                   div(style="color: black; font-family: Arial, Helvetica, sans-serif;font-size: 15px;vertical-align: 10px",
                                       div(id = "population_icon", "Global Population", style = "text-indent: 10px;padding-top: 5px;background-color:#FBF5F3; height: 40px; margin-bottom: 2px;border-radius:4px 4px 0px 0px", 
                                           icon("info-circle", style="float: right;margin-right:5px;"),
                                           bsTooltip(id = "population_icon", title = "This is the total population of all the countries in our dataset.", placement = "right", trigger = "hover")
                                        ),
                                       div(prettyNum(sum(df_world_data$Population, na.rm = TRUE), big.mark = ",", scientific = FALSE), style = "padding-top: 15px;background-color:#FBF5F3; font-size: 50px; font-weight: bold;text-align: center;height: 100px; border-radius:0px 0px 4px 4px"),
                                   )
                                 ),
                                 column(
                                   width = 3, 
                                   div(style="color: black; font-family: Arial, Helvetica, sans-serif;font-size: 15px;vertical-align: 10px",
                                       div(id = "gdp_icon", "Global GDP (Billion)", style = "text-indent: 10px;padding-top: 5px;background-color:#FBF5F3; height: 40px; margin-bottom: 2px; border-radius:4px 4px 0px 0px", 
                                           icon("info-circle", style="float: right;margin-right:5px;"),
                                           bsTooltip(id = "gdp_icon", title = "This is the total GDP in billion dollors of all the countries in our dataset.", placement = "right", trigger = "hover")
                                        ),
                                       div(paste0("$ ", prettyNum(as.numeric(sprintf("%.2g", sum(df_world_data$GDP, na.rm = TRUE)/100000000)), big.mark = ",", scientific = FALSE)), style = "padding-top: 15px;background-color:#FBF5F3; font-size: 50px; font-weight: bold;text-align: center;height: 100px; border-radius:0px 0px 4px 4px"),
                                   )
                                 ),
                                 
                                 column(
                                   width = 3, 
                                   div(style="color: black; font-family: Arial, Helvetica, sans-serif;font-size: 15px;vertical-align: 10px",
                                       div(id = "CO2_icon", "Global CO2 Emissions (tons)", style = "text-indent: 10px;padding-top: 5px;background-color:#FBF5F3; height: 40px; margin-bottom: 2px; border-radius:4px 4px 0px 0px", 
                                           icon("info-circle", style="float: right;margin-right:5px;"),
                                           bsTooltip(id = "CO2_icon", title = "This is the total CO2 Emission in tons of all the countries in our dataset.", placement = "left", trigger = "hover")
                                        ),
                                       div(prettyNum(sum(df_world_data$CO2Emissions, na.rm = TRUE), big.mark = ",", scientific = FALSE), style = "padding-top: 15px;background-color:#FBF5F3; font-size: 50px; text-align: center;height: 100px; border-radius:0px 0px 4px 4px"),
                                   )
                                 )
                               ),
                               br(),
                               fluidRow(
                                 column( width = 6),
                                 column( width = 6,
                                         div(selectInput("selected_attribute", "Select a Variable:", choices = setdiff(names(df_world_data)[sapply(df_world_data, is.numeric)], c("CallCode", "Latitude", "Longitude")), selected = "LifeExpectancy"))
                                 )
                               ),
                               br(),
                               fluidRow(
                                 column( width = 12,
                                         div(style = "padding-top: 15px;background-color:#FBF5F3; font-size: 50px; font-weight: bold;text-align: center;height: 650px; border-radius:0px 0px 4px 4px",
                                             box(
                                               tags$style(type = 'text/css', '#dashboard_map {height: calc(100vh -80px) !important;}'),
                                               leafletOutput("dashboard_map", height = 600),
                                               width = 12)     
                                         )),
                               ),
                               
                               br(),
                               fluidRow(
                                 column( width = 6,
                                         div(
                                           div(style = "padding-top: 15px;background-color:#FBF5F3; font-size: 50px; font-weight: bold;text-align: center;height: 425px; border-radius:0px 0px 4px 4px",
                                               plotlyOutput("dashboard_country_continent_pie") 
                                           ),
                                         )
                                 ),
                                 column( width = 6,
                                         div(
                                           div(style = "padding-top: 15px;background-color:#FBF5F3; font-size: 50px; font-weight: bold;text-align: center;height: 425px; border-radius:0px 0px 4px 4px",
                                               plotlyOutput("dashboard_country_world_pie") 
                                           ),
                                         )
                                 )
                               ),
                               br(),
                               fluidRow(
                                 column( width = 12, 
                                         tabsetPanel(
                                           tabPanel("Country Data",
                                                div(style = "padding-top: 15px; background-color:#FBF5F3;font-size: 20px; padding-left: 25px;font-weight: bold;text-align: left;color: black;height: 800px; border-radius:0px 0px 4px 4px",
                                                    column( width = 6,
                                                        div(tableOutput(outputId = "dashboard_country_data_left"))
                                                    ),
                                                    column( width = 6,
                                                        div(tableOutput(outputId = "dashboard_country_data_right"))
                                                    )
                                                )
                                           ),
                                           tabPanel("Variable Data",
                                                div(style = "padding-top: 15px; background-color:#FBF5F3;font-size: 20px; padding-left: 25px;font-weight: bold;text-align: left;color: black;height: 700px; border-radius:0px 0px 4px 4px",
                                                  column( width = 6, 
                                                      h3("5-Point Summary"),
                                                      verbatimTextOutput("dashboard_summary_output")
                                                    ),
                                                  column( width = 6,
                                                      h3("Box Plot"),
                                                      plotlyOutput("attribute_boxplot", height = "600px")
                                                  )
                                                ),
                                         ),
                                      )  
                                 )
                               ),
                               br(),
                               br(),
                               fluidRow(
                                 column( width = 4, 
                                      div(class = "slider-container",
                                         sliderInput("top_countries_slider", 
                                                     "Select the Top Number of Countries:", 
                                                     min = 10, 
                                                     max = 50, 
                                                     value = 20, 
                                                     step = 10,
                                                     ticks = TRUE)
                                      ),
                                 ),
                               ),
                               br(),
                               fluidRow(
                                 column( width = 12,
                                         div(style = "padding-top: 15px; background-color:#FBF5F3;font-size: 20px; padding-left: 25px;font-weight: bold;text-align: left;color: black;height: 650px; border-radius:0px 0px 4px 4px",
                                             plotlyOutput(outputId = "top_country_barchart", height = "600px") 
                                         )
                                 )
                               ),
                               br(),
                               fluidRow(
                                 column( width = 12)),
                               width = 12,  background = 'black', style = "overflow-x: auto; overflow-y: auto;min-height: 500px;")
                           
                  ),
                  fluidRow(footer)
          ),
          
          # connecting_dots tab content
          tabItem(tabName = "world_data",
                  fluidRow(
                    box(
                      column( width = 6, 
                              pickerInput(
                                inputId = "select_fields",
                                label = "Select Variables:",
                                choices = colnames(df_world_data),
                                options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                multiple = TRUE,
                                selected = setdiff(colnames(df_world_data), c("Abbreviation", "Calling.Code", "Capital.Major.City", "Largest.city", "Official.language","Currency.Code", "Latitude", "Longitude")),
                              )
                      ),
                      column(width = 6,
                             pickerInput(
                               inputId = "select_continent",
                               label = "Select Continent:",
                               choices = sort(unique(df_continents$Continent)),
                               options = list(`actions-box` = TRUE, `live-search` = TRUE),
                               multiple = TRUE,
                               selected = c(unique(df_continents$Continent)),
                             )),
                      
                      column(width = 12,
                             br(),br(),
                             tableOutput("worldData")), width = 12, background = 'black', style = "overflow-x: auto; overflow-y: auto;min-height: 800px;")
                  ),
                  
                  fluidRow(footer)
          ),
          
          
          tabItem(tabName = "summary_view",
                  tags$style(HTML("
              .box {
                width: 100%;
              }
              .box-body {
                overflow-x: auto;
              }
              table {
                width: 100% !important;
              }")),
                  fluidRow(
                    box(
                      uiOutput("summary"),
                      width = 12)
                  ),
                  fluidRow(footer)
          ),
          
          # data_correlations tab content
          tabItem(tabName = "data_correlations_tab",
                  fluidRow(
                    box(
                      column( width = 4, 
                              selectInput(
                                inputId = "correlation_type", 
                                label = "Select the Correlation Type:", 
                                choices = c("Simple Correlation", "Grouped Correlation", "Absolute Correlation"),
                                selected = "Absolute Correlation",
                                multiple = FALSE
                              )),
                      column( width = 4, 
                              selectInput(
                                inputId = "correlation_method", 
                                label = "Select the Correlation Method:", 
                                choices = c("pearson", "kendall", "spearman"),
                                selected = "spearman",
                                multiple = FALSE
                              )),
                      column( width = 4, 
                              selectInput(
                                inputId = "correlation_order", 
                                label = "Select the Correlation Order:", 
                                choices = c("OLO", "PCA", "HC", "GW"),
                                selected = "OLO",
                                multiple = FALSE
                              )),
                      column( width = 12, 
                              br(),
                              br(),
                              plotOutput("correlations_plot", height = "600px")
                      ), 
                      width = 12, height = 750, background = 'black')
                  ),
                  fluidRow(footer)
          ),
          
          
          #homogeneity_tab
          tabItem(tabName = "homogeneity_tab",
                  fluidRow(
                    box( br(), 
                        plotlyOutput("mat_plot", height = "680px"),
                                 #plotlyOutput("heatmap_plot", height = "680px")
                      width = 12, height = 750, background = 'black')
                  ),
                  fluidRow(footer)
          ),
          
          tabItem(tabName = "heatmap_tab",
                  fluidRow(
                    box(br(),
                      plotlyOutput("heatmap_plot", height = "680px"),
                      width = 12, height = 750, background = 'black')
                  ),
                  fluidRow(footer)
          ),
          
          
          
          # pairs plot tab
          tabItem(tabName = "pairs_plot_tab",
                  fluidRow(
                    box(
                      column( width = 6),
                      column( width = 6, 
                              pickerInput(
                                inputId = "selected_cols_pairs_plot_picker",
                                label = "Select Variables:",
                                choices = setdiff(colnames(df_world_data), c("Country", "Continent", "CapitalCity", "LargestCity", "Abbreviation", "CallCode", "Latitude", "Longitude", "Density", "CurrencyCode", "OfficialLanguage")),
                                options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                multiple = TRUE,
                                selected = setdiff(colnames(df_world_data), c("Country", "Continent", "CapitalCity", "LargestCity", "Abbreviation", "CallCode", "Latitude", "Longitude", "Density", "CurrencyCode", "OfficialLanguage"))
                              ),
                              br(),
                              actionButton(inputId = "render_plot", label = "Render Plot", class = "btn-custom")
                      ),
                      column( width = 12, 
                              br(),
                              tabsetPanel(
                                tabPanel("Simple Pairs Plot",
                                         plotOutput(outputId = "sim_pair_plots", width = "3000px", height = "3000px")  %>% withSpinner(color = "#0dc5c1")
                                ),
                                tabPanel("GG Pairs Plot",
                                         plotOutput(outputId = "gg_pair_plots", width = "3000px", height = "3000px")  %>% withSpinner(color = "#0dc5c1")
                                ),
                              ),
                      ),
                      width = 12, background = 'black', style = "overflow-x: auto; overflow-y: auto;min-height: 3225px;"), 
                    
                  ),
                  fluidRow(footer)
          ),
          
          
          # scatter plots tab tab content
          tabItem(tabName = "scatter_plots_tab",
                  fluidRow(
                    box(
                      column( width = 4,
                          selectInput("scatter_xvariable", "Select X-axis Variable:", choices = names(df_world_data)[sapply(df_world_data, is.numeric)], selected = "BirthRate"),
                      ),
                      column(width = 4),
                      column( width = 4, 
                          selectInput("scatter_yvariable", "Select Y-axis Variable:", choices = names(df_world_data)[sapply(df_world_data, is.numeric)], selected = "LifeExpectancy")
                      ),
                      column(width = 12,
                             br(),br(),
                      ),
                      column(width = 12,
                             plotlyOutput("scatterplot", height = "600px")
                      ),
                      width = 12, height = 750, background = 'black')
                  ),
                  fluidRow(footer)
          ),
          
          # box plots tab tab content
          tabItem(tabName = "box_plots_tab",
                  fluidRow(
                    box(
                      column( width = 4,
                              selectInput("selected_column", "Select a Variable:", choices = setdiff(names(df_world_data)[sapply(df_world_data, is.numeric)], c("CallCode", "Latitude", "Longitude")), selected = "BirthRate")
                      ),
                      column(width = 12,br(),
                             plotlyOutput("boxPlot", height = "600px")
                      ),
                      width = 12, height = 750, background = 'black')
                  ),
                  fluidRow(footer)
          ),
          
          # options_chart tab content
          tabItem(tabName = "about_app",
                  fluidRow(
                    box(
                      tags$img(src = "logo.png", height = '500', width = '500', style="display: block;margin-left: auto;margin-right: auto;"),
                      column(width = 12, div(br(), br(), br())),
                      column(width = 4,
                             div(style = "text-indent: 10px;padding-top: 5px;color:#FBF5F3;",
                                 "Understanding global socio-economic trends requires the efficient analysis and interpretation of extensive country-level data. This capability is essential for policymakers, researchers, and organizations to make informed decisions. The Global Country Information Dataset 2023 encompasses a wide range of indicators across demographic, economic, environmental, and healthcare spheres. This dataset offers a valuable opportunity to explore various aspects across all countries and derive meaningful insights.", 
                             ),
                      ),
                      column(width = 4,
                             div(style = "text-indent: 10px;padding-top: 5px;color:#FBF5F3;",
                                 "Socio-economic data analysis enables stakeholders to identify patterns and trends that can inform policy decisions, economic strategies, and social interventions. By examining indicators such as GDP, employment rates, literacy rates, and life expectancy, we can gain a comprehensive understanding of a country's development status and potential areas for improvement.",
                             ),
                      ),
                      column(width = 4,
                             div(style = "text-indent: 10px;padding-top: 5px;color:#FBF5F3;",  
                                 "The primary objective of this application is to empower users to conduct their own exploratory data analysis, manipulate data, and visualize outcomes. By providing a user-friendly interface and robust analytical tools, the application aims to facilitate a deeper understanding of global trends. Users can interact with and analyze the data through various computational and visual tools, enabling comparisons and insights across different countries.",
                             ),
                      ),
                      width = 12, height = 750, background = 'black')
                  ),
                  fluidRow(footer)
          ),
          
          
          # Advance charts - economic impact analysis tab content
          tabItem(tabName = "advance_gdp_tab",
                  fluidRow(
                    box(
                      column( width = 12,
                              plotlyOutput("adv_gdp_life_plot", height = "1200px")  %>% withSpinner(color = "#0dc5c1")
                      ),
                      width = 12, height = 1225, background = 'black')
                  ),
                  fluidRow(footer)
          ),
          
          
          # Advance charts -social impact analysis tab content
          tabItem(tabName = "advance_socio_tab",
                  fluidRow(
                    box(
                      column( width = 12,
                              plotlyOutput("adv_gdp_emission_plot", height = "1400px")  %>% withSpinner(color = "#0dc5c1")
                      ),
                      width = 12, height = 1425, background = 'black')
                  ),
                  fluidRow(footer)
          )
          
        )
      )
    )
  )
)


# server-side code
server <- function(input, output, session) {
  
  rv <- reactiveValues()
  
  observeEvent(input$openModal, {
    showModal(
      modalDialog(title = "Some title",
                  p("Some information"))
    )
  })
  
  
  
  output$worldData <- renderTable(df_world_data[df_world_data$Continent %in% c(input$select_continent), c(input$select_fields)])

  output$dashboard_maps <- renderLeaflet({
    leaflet(df_world_data) %>%
      addTiles(group = "OpenStreetMap") %>%
      addCircles(
        lng = ~Longitude, lat = ~Latitude,
        color = ~colorBin("YlOrRd", Population, bins = 5)(Population),
        popup = ~paste("<strong>Country:</strong>", Country,
                       "<br><strong>Population:</strong>", Population,
                       "<br><strong>CO2 Emissions:</strong>", CO2Emissions,
                       "<br><strong>GDP:</strong>", GDP,
                       "<br><strong>Urban Population:</strong>", UrbanPopulation)
      )
  })
  
  output$data_summary_output <- renderUI(
    SumProfile <- print(dfSummary(df_world_data), omit.headings = FALSE, method = 'render')
    #SumProfile
  )
  
  
  #output$correlations_plot <- render
  output$dashboard_country_continent_pie <- renderPlotly({
    #country <- input$selected_country
    country <- rv$county
    if (is_empty(country))
      country <- "New Zealand"
    
    attribute <- input$selected_attribute
    
    continent <- df_world_data[df_world_data$Country == country, "Continent"]
    country_data <- df_world_data[df_world_data$Country == country, c("Country", attribute)]
    
    # Get the total of the continent the country belongs to
    continent_value <- sum(df_world_data[df_world_data$Continent == continent, attribute], na.rm = TRUE)
    
    new_row <- data.frame(
      Country = continent,
      attribute = continent_value
    )
    
    new_row <- setNames(new_row, c("Country", attribute))
    
    data_combined <- rbind(country_data, new_row)
    
    title <- sprintf("%s Distribution of %s vs continent %s", attribute, country, continent)
    
    values <- data_combined %>% pull(!!sym(attribute))
    
    # Create the pie chart
    plot <- plot_ly(data_combined, labels = ~Country, values = values, type = 'pie', hole = 0.4, textposition = 'inside', textinfo = 'label+percent', 
                    insidetextfont = list(color = '#FFFFFF'), marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1)),
                    showlegend = FALSE) %>%
      layout(title =title,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list (showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
    
    return(plot)
  })
  
  #dashboard_country_world_pie
  output$dashboard_country_world_pie <- renderPlotly({
    country <- rv$county
    if (is_empty(country))
      country <- "New Zealand"
    
    attribute <- input$selected_attribute
    
    country_data <- df_world_data[df_world_data$Country == country, c("Country", attribute)]
    world_att <- sum(df_world_data[[attribute]], na.rm = TRUE)
    
    new_row <- data.frame(
      Country = "World",
      attribute = world_att
    )
    
    new_row <- setNames(new_row, c("Country", attribute))
    
    data_combined <- rbind(country_data, new_row)
    
    title <- sprintf("%s Distribution of %s vs world", attribute, country)
    
    values <- data_combined %>% pull(!!sym(attribute))
    
    # Create the pie chart
    plot <- plot_ly(data_combined, labels = ~Country, values = values, type = 'pie', hole = 0.4, textposition = 'inside', textinfo = 'label+percent', 
                    insidetextfont = list(color = '#FFFFFF'), marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1)),
                    showlegend = FALSE) %>%
      layout(title =title,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list (showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
    
    return(plot)
  })
  

  
  # dashboard country data - left
  output$dashboard_country_data_left <- renderTable({
    country <- rv$county
    if (is_empty(country))
      country <- "New Zealand"
    
    country_data <- df_org_world_data[df_org_world_data$Country == country, ]
    
    num_columns <- length(country_data)
    half <- ceiling(num_columns / 2)
    
    country_data <- country_data[,1:half]
    col_names <- elements[1:half]
    
    country_data <- as.vector(t(country_data))
    country_data_left <- data.frame(col_names, country_data)
    colnames(country_data_left) <- NULL
    
    return(country_data_left)
  }, striped = FALSE, hover = FALSE, align = 'l')
  
  # dashboard country data - right
  output$dashboard_country_data_right <- renderTable({
    country <- rv$county
    if (is_empty(country))
      country <- "New Zealand"
    country_data <- df_org_world_data[df_org_world_data$Country == country, ]
    
    num_columns <- length(country_data)
    half <- ceiling(num_columns / 2) + 1
    
    country_data <- country_data[ , half:num_columns]
    col_names <- elements[half: num_columns]
    
    country_data <- as.vector(t(country_data))
    country_data_right <- data.frame(col_names, country_data)
    colnames(country_data_right) <- NULL
    
    return(country_data_right)
    
  }, striped = FALSE, hover = FALSE, align = 'l')
  
  # summary view
  output$dashboard_summary_output <- renderPrint({
    attribute <- input$selected_attribute
    summary_data <- summary(df_world_data[[attribute]])
    list(
      Minimum = summary_data[1],
      `1st Quartile` = summary_data[2],
      Median = summary_data[3],
      `3rd Quartile` = summary_data[5],
      Maximum = summary_data[6]
    )
  })
  
  
  # box plot in attribute tab of dashboard
  output$attribute_boxplot <- renderPlotly({
    attribute <- input$selected_attribute
    data <- df_world_data[[attribute]]
    
    # Create the box plot
    p <- plot_ly(
      y = ~data,
      type = "box",
      name = attribute
    )
    
    # Customize the layout
    p <- p %>% layout(
      title = list(text = paste("Box Plot of", attribute),font = list(size = 17, family = "Arial", weight = "bold")),
      yaxis = list(title = attribute),
      paper_bgcolor = 'rgba(0,0,0,0)',
      plot_bgcolor = 'rgba(0,0,0,0)',
      height = 600
    )
    
  })
  
  # dashboard top_country_barchart
  output$top_country_barchart <- renderPlotly({
    
    attribute <- input$selected_attribute
    
    top_countries <- df_world_data %>% 
      arrange(desc(!!sym(attribute))) %>% 
      head(input$top_countries_slider) %>%
      mutate(Country = factor(Country, levels = Country[order(-!!sym(attribute))]))
    
    plot_ly(
      data = top_countries,
      x = ~Country,
      y = as.formula(paste0("~`", attribute, "`")),
      type = 'bar',
      color = ~Country,
      colors = colorRampPalette(brewer.pal(9, "Set1"))(10) # Using a color palette
    ) %>% layout(title = sprintf("Top %g Countries by %s", input$top_countries_slider, attribute),
                 xaxis = list(title = "Country"),
                 yaxis = list(title = attribute),
                 showlegend = FALSE,
                 paper_bgcolor = 'rgba(0,0,0,0)', plot_bgcolor = 'rgba(0,0,0,0)')
  })
  
  
  
  output$summary <- renderUI({
    Global_Country_Information <- df_world_data %>% select(-Country, -CapitalCity, -LargestCity, -Abbreviation, -CallCode, -Latitude, -Longitude)
    print(dfSummary(Global_Country_Information), method = "render", footnote = NA)
  })
  
  # Reactive value to track the button click state
  plot_ready <- reactiveVal(FALSE)
  
  # Observe changes in the pickerInput and reset plot_ready
  observeEvent(input$selected_cols_pairs_plot_picker, {
    plot_ready(FALSE)
  })
  
  observeEvent(input$render_plot, {
    plot_ready(TRUE)
  })
  
  output$sim_pair_plots <- renderPlot({
    req(plot_ready())  # Ensure the plot is rendered only when plot_ready is TRUE
    
    dataset <- df_world_data %>% select(all_of(input$selected_cols_pairs_plot_picker))
    plot(dataset)
  })
    
  
  output$gg_pair_plots <- renderPlot({
    req(plot_ready())  # Ensure the plot is rendered only when plot_ready is TRUE
    
    #dataset <- df_world_data %>% select(-Country, -CapitalCity, -LargestCity, -Abbreviation, -CallCode, -Latitude, -Longitude, -Density, -CurrencyCode, -OfficialLanguage)
    cols <- c(input$selected_cols_pairs_plot_picker, "Continent")
    dataset <- df_world_data %>% select(all_of(cols))
    GGally::ggpairs(data = dataset,  mapping = aes(colour = Continent), title = "Pairs of world data", progress = FALSE)
  
  })#, width = 800, height = 600
  
    
  output$correlations_plot <- renderPlot({
    country <- reactiveVal("Australia")
    
    dataset <- df_world_data %>% select(-Country, -CapitalCity, -LargestCity, -Abbreviation, -CallCode, -Latitude, -Longitude, -Continent, -Density, -CurrencyCode, -OfficialLanguage)
    
    if (input$correlation_type == "Simple Correlation")
      return (corrgram::corrgram(dataset, abs = FALSE, cor.method = input$correlation_method, main = "Dataset Correlation"))
    else if (input$correlation_type == "Grouped Correlation")
      return (corrgram::corrgram(dataset, order = input$correlation_order, abs = FALSE, cor.method = input$correlation_method, main = "Data correlation using OLO grouping"))
    else if (input$correlation_type == "Absolute Correlation")
      return (corrgram::corrgram(dataset, order = input$correlation_order, abs = TRUE, cor.method = input$correlation_method, main = "Data absolute correlation using OLO grouping"))
  })
  
  # Matplot Homogeneity plot under homogeneity_plot_tab
  output$mat_plot <- renderPlotly({
    cols <- names(df_world_data)[sapply(df_world_data, is.numeric)]
    numData <- scale(df_world_data[, cols], center = TRUE, scale = TRUE)
    
    # Convert the data to a long format for plotly
    numData_long <- data.frame(
      Observation = rep(1:nrow(numData), ncol(numData)),
      Value = as.vector(numData),
      Variable = rep(cols, each = nrow(numData))
    )
    
    # Create the plot
    plot <- plot_ly(numData_long, x = ~Observation, y = ~Value, color = ~Variable, type = 'scatter', mode = 'lines') %>%
      layout(xaxis = list(title = "Observations in sequence"),
             yaxis = list(title = "Value"))
    
    plot
  })
  
  
  # heatmap plot under homogeneity_plot_tab
  output$heatmap_plot <- renderPlotly({
    # Select relevant numeric columns for correlation
    selected_columns <- names(df_world_data)[sapply(df_world_data, is.numeric)]
    # Remove specific columns
    selected_columns <- setdiff(selected_columns, c("callCode", "Latitude", "Longitude"))
    
    numeric_data <- df_world_data[selected_columns]
    
    # Calculate correlation matrix
    cor_matrix <- cor(numeric_data, use = "complete.obs")
    # Perform hierarchical clustering
    dist_matrix <- as.dist((1 - cor_matrix) / 2)
    hc <- hclust(dist_matrix)
    dend <- as.dendrogram(hc)
    
    # Order the correlation matrix based on clustering
    ordered_columns <- order.dendrogram(dend)
    sorted_cor_matrix <- cor_matrix[ordered_columns, ordered_columns]
    # Melt the correlation matrix for plotly
    melted_cor_matrix <- melt(cor_matrix)
    
    plot_ly(
      x = colnames(sorted_cor_matrix),
      y = rownames(sorted_cor_matrix),
      z = sorted_cor_matrix,
      type = "heatmap",
      colorscale = "RdBu",
      zmin = -1,
      zmax = 1
    ) %>%
      layout(
        title = "Correlation Heatmap",
        xaxis = list(title = ""),
        yaxis = list(title = ""),
        colorbar = list(title = "Correlation")
      )
  })
  
  # scatter plot
  output$scatterplot <- renderPlotly({
    plot_ly(
      data = df_world_data,
      x = ~get(input$scatter_xvariable),
      y = ~get(input$scatter_yvariable),
      type = 'scatter',
      mode = 'markers'
    ) %>%
      layout(
        xaxis = list(title = input$scatter_xvariable),
        yaxis = list(title = input$scatter_yvariable)
      )
  })
  
  #continent based box plots
  output$boxPlot <- renderPlotly({
    p <- ggplot(df_world_data, aes_string(x = "Continent", y = input$selected_column)) +
      geom_boxplot() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(title = paste("Box Plot of", input$selected_column, "by Continent"),
           x = "Continent",
           y = input$selected_column)
    
    ggplotly(p)
  })
  
  # advance gdp-life plot
  output$adv_gdp_life_plot <- renderPlotly({
    indicators <- c("InfantMortality","LifeExpectancy", "MaternalMortalityRatio","BirthRate", "FertilityRate", "PhysiciansPerThousand")
    
    # Calculate IQR and filter out GDP outliers
    gdp_iqr <- IQR(df_world_data$GDP, na.rm = TRUE)
    gdp_quantiles <- quantile(df_world_data$GDP, probs = c(0.25, 0.75), na.rm = TRUE)
    gdp_lower_bound <- gdp_quantiles[1] - 1.5 * gdp_iqr
    gdp_upper_bound <- gdp_quantiles[2] + 1.5 * gdp_iqr
    
    # filter out the outlier GDP values
    df_filtered <- df_world_data %>%
      filter(GDP >= gdp_lower_bound & GDP <= gdp_upper_bound)
    
    # Create a ggplot object for each selected indicator
    plots <- lapply(indicators, function(indicator) {
      y_label <- indicator
      
      ggplot(df_filtered, aes_string(x = "GDP", y = indicator)) +
        geom_point() +
        geom_smooth(method = 'lm') +
        labs(title = "GDP Impact On Parameters",
             x = "GDP ($)",
             y = y_label)
    })
    
    # Combine the plots into a single plotly object
    combined_plot <- subplot(plots, nrows = length(plots), shareX = TRUE, titleX = TRUE, titleY = TRUE)%>%
      layout(height = 200 * length(plots))  # Increase height for each plot
    
    # Convert to plotly
    ggplotly(combined_plot)
  })
  
  # advance gdp_emission plot
  output$adv_gdp_emission_plot <- renderPlotly({
    indicators <- c("CO2Emissions", "ArmedForces", "UrbanPopulation", "Population", "LandArea", "ForestArea", "AgriculturalLand")
    
    # Calculate IQR and filter out GDP outliers
    gdp_iqr <- IQR(df_world_data$GDP, na.rm = TRUE)
    gdp_quantiles <- quantile(df_world_data$GDP, probs = c(0.25, 0.75), na.rm = TRUE)
    gdp_lower_bound <- gdp_quantiles[1] - 1.5 * gdp_iqr
    gdp_upper_bound <- gdp_quantiles[2] + 1.5 * gdp_iqr
    
    # filter out the outlier GDP values
    df_filtered <- df_world_data %>%
      filter(GDP >= gdp_lower_bound & GDP <= gdp_upper_bound)
    
    # Create a ggplot object for each selected indicator
    plots <- lapply(indicators, function(indicator) {
      y_label <- indicator
      
      ggplot(df_filtered, aes_string(x = "GDP", y = indicator)) +
        geom_point() +
        geom_smooth(method = 'lm') +
        labs(title = "GDP Impact On Parameters",
             x = "GDP ($)",
             y = y_label)
    })
    
    # Combine the plots into a single plotly object
    combined_plot <- subplot(plots, nrows = length(plots), shareX = TRUE, titleX = TRUE, titleY = TRUE)%>%
      layout(height = 200 * length(plots))  # Increase height for each plot
    
    # Convert to plotly
    ggplotly(combined_plot)
  })
  
  
  output$adv_gdp_life_plot3 <- renderPlotly({
    indicators <- c("GDP", "InfantMortality","LifeExpectancy", "MaternalMortalityRatio","BirthRate", "FertilityRate", "PhysiciansPerThousand")
    
    # Filter data based on selected indicators and convert to long format
    data_long <- df_world_data %>%
      select(GDP, one_of(indicators)) %>%
      pivot_longer(cols = -GDP, names_to = "Indicator", values_to = "Value")
    
    # Create the ggplot object with facets
    p <- ggplot(data_long, aes(x = GDP, y = Value)) +
      geom_point() +
      geom_smooth(method = 'lm') +
      facet_wrap(~ Indicator, scales = "free_y", ncol = 1) +
      labs(title = "GDP vs. Selected Indicators",
           x = "GDP ($)",
           y = "Value") +
      theme(strip.text.x = element_text(size = 12), 
            plot.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 10),
            axis.title = element_text(size = 12))
    
    # Convert to plotly
    ggplotly(p)
  })
  
  output$dashboard_mapss <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addLayersControl(
        baseGroups = c("Toner"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  # Render the leaflet map
  output$dashboard_maps <- renderLeaflet({
    # Create a color palette based on population size
    pal <-  colorNumeric(
      palette = c("#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026", "#4A1486"),
      domain = world@data[[input$selected_attribute]]
    )
    leaflet(world) %>%
      addTiles(group = "Open Street", options = tileOptions(minZoom = 2, maxZoom = 4)) %>%
      addProviderTiles(provider = "Esri.WorldStreetMap", group = "World Street", options = tileOptions(minZoom = 2, maxZoom = 4)) %>%
      addProviderTiles(provider = "MtbMap", group = "MTB Hiking", options = tileOptions(minZoom = 2, maxZoom = 4)) %>%
      addLayersControl(
        baseGroups = c("Open Street", "World Street", "MTB Hiking"),
	      option = layersControlOptions(collapsed = TRUE)
      )%>%
      #OpenStreetMap
      addPolygons(
        fillColor = ~pal(world@data[[input$selected_attribute]]),
        fillOpacity = 0.9,
        color = "#BDBDC3",
        weight = 1,
        popup = ~paste0("<strong>Country: </strong>", Country, "<br><strong>", input$selected_attribute, ": </strong>", world@data[[input$selected_attribute]]),
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        layerId = ~Country
      ) %>%
      addLegend(
        pal = pal,
        values = world@data[[input$selected_attribute]],
        title = input$selected_attribute,
        opacity = 0.7,
        position = "bottomright"
      )%>%
      htmlwidgets::onRender(
        "function(el, x) {
          window.tester = ''; // Define a global variable in the window scope
          
          this.on('click', function(e) {
            if (e.layer && e.layer.feature) {
              var country = e.layer.feature.properties.Country;
              window.tester = country; // Assign the country to the global variable
              console.log('Country clicked:', window.tester); // Log the global variable
              Shiny.setInputValue('clicked_country', window.tester);
            }
          });
        }"
      )
  })
  
 
  
  output$dashboard_map <- renderLeaflet({
    pal <-  colorNumeric(
      palette = c("#FFEDA0", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026", "#4A1486"),
      domain = world@data[[input$selected_attribute]]
    )
    leaflet(world) %>%
      addTiles(group = "Open Street", options = tileOptions(minZoom = 2, maxZoom = 4)) %>%
      addProviderTiles(provider = "Esri.WorldStreetMap", group = "World Street", options = tileOptions(minZoom = 2, maxZoom = 4)) %>%
      addProviderTiles(provider = "MtbMap", group = "MTB Hiking", options = tileOptions(minZoom = 2, maxZoom = 4)) %>%
      addLayersControl(
        baseGroups = c("World Street", "Open Street", "MTB Hiking"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addPolygons(
        fillColor = ~pal(world@data[[input$selected_attribute]]),
        fillOpacity = 0.9,
        color = "#BDBDC3",
        weight = 1,
        popup = ~paste0("<strong>Country: </strong>", Country, "<br><strong>", input$selected_attribute, ": </strong>", world@data[[input$selected_attribute]]),
        highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
        layerId = ~Country,
        #country_name <<- ~Country
      ) %>%
      addLegend(
        pal = pal,
        values = world@data[[input$selected_attribute]],
        title = input$selected_attribute,
        opacity = 0.7,
        position = "bottomright"
      )
  })
  
  observe({ 
    rv$county <- input$dashboard_map_shape_click$id
  })
  
}

shinyApp(ui, server)