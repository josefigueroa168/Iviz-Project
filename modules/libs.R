dependencies <- c("dplyr", "geojsonio", "here", "leaflet", "shiny", "tidyr")

for (dep in dependencies) {
  if(!(dep %in% installed.packages()[, "Package"])) {
    install.packages(dep)
  }
}
library(dplyr)
library(geojsonio)
library(here)
library(leaflet)
library(shiny)
library(tidyr)