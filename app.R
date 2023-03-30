# Load packages here so that it only runs once
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)
library(scales)
library(bslib)

# Source for ui and server (Only do this on the app, nowhere else)
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
