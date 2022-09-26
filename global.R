

library(shiny)
library(dplyr)
library(echarts4r)
library(lubridate)
library(reactable)
library(sever)
library(leaflet)
library(bslib)
library(shinyWidgets)

path <- "Data/Sample Sales Database - Global Superstore.xlsx"
orders <- readxl::read_excel(path, sheet = "Orders")
returns <- readxl::read_excel(path, sheet = "Returns")
people <- readxl::read_excel(path, sheet = "People")
locs <- readxl::read_excel("Data/locs.xlsx")

rm(path)

orders$`Order Date` <- as.Date(orders$`Order Date`)
orders$`Ship Date` <- as.Date(orders$`Ship Date`)




orders <- 
orders |> 
  left_join(
    people, by = c("Region"="Region")
  ) |> 
  left_join(
    returns |> 
      select(!Region), by = c("Order ID" = "Order ID")
  ) |> 
  mutate(
    location = paste(sep = ", ", Country, State, City)
  ) |> 
  left_join(
    locs, by = c("location" = "location")
  )

theme <- bs_theme(
  bg = "#000000", fg = "#B8BCC2",
  "input-border-color" = "#a6a6a6"
)

opts <- list(
  `actions-box` = TRUE,
  `live-search`=TRUE)





