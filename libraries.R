# Install pacman-package to check if required packages are installed. If not, they are automatically installed.
if (!require("pacman")) install.packages("pacman")

# Pacman will now load the installed packages or make a new install if missing
# Data Wrangling ----------------------------------------------------------
pacman::p_load(tidyverse, data.table, lubridate, vctrs)

# Database ----------------------------------------------------------------
pacman::p_load(RSQLite, DBI)

# DataViz -----------------------------------------------------------------
pacman::p_load(shiny, shinyjs, shinydashboard, plotly, leaflet)

# Machine Learning --------------------------------------------------------
pacman::p_load(forecast)