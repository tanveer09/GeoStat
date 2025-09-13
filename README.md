# GeoStat Shiny App

## Project Overview

This R Shiny application provides an interactive visualization of various socio-economic indicators for countries around the world. The app allows users to explore data through a geographic interface and provides insights into global statistics.

## Directory Structure

- `code/`: Contains the R scripts and necessary resources for the Shiny app.
  - `GeoStat.R`: Main R script for the Shiny app.
  - `www/`: Directory for web resources such as CSS files and images.
    - `custom.css`: Custom CSS for styling the app.
    - `logo.ico`: Favicon for the app.
    - `logo.png`: Logo image used in the app.
  - `countries.geojson`: GeoJSON file containing geographical data for countries.

- `data/`: Directory for data files used in the app.
  - `Continents.csv`: Data file containing continent information.
  - `world-data-2023.csv`: Primary dataset for the app.
  - `www/`: Additional data files for the app.
    - `world_data.json`: JSON file containing world data.

- `GeoStat-Presentation.pdf`: Presentation slides summarizing the project.

## Installation

To run this Shiny app locally, you need to have R and the Shiny package installed on your system. You can install Shiny by running the following command in R:

```R
install.packages("shiny")
```

Additionally, you may need to install other dependencies used in the app. You can install them by running:

```R
install.packages(c("leaflet", "dplyr", "ggplot2"))
```

## Running the App

Once you have all the necessary packages installed, you can run the app by executing the following commands in R:

```R
library(shiny)

# Set the working directory to the location of your GeoStat.R file
setwd("path/to/your/code")

# Run the app
runApp("GeoStat.R")
```

## Usage

The Shiny app provides an interactive interface for exploring global socioeconomic data. Users can:
- View various indicators on a world map.
- Filter data by regions or specific countries.
- Explore detailed statistics and trends through visualizations.

NOTE:
Make sure to replace `path/to/your/code` with the actual path to the `code` directory on your system.