library(aws.s3)
library(readr)
library(readxl)
library(jsonlite)
library(dplyr)
library(data.table)
library(ggplot2)
library(pwr)
library(tidyverse)
library(hash)
library(shiny)
library(openxlsx)
library(reticulate)
library(DT)
library(shinydashboard)
library(shinyBS)
library(corrplot)
library(httpuv)
library(Rcpp)
library(shinyjs)
library(httr)
library(bcrypt)


ui <- function(request) {
  
  useShinyjs()
  
  dashboardPage(
    dashboardHeader(title = "AFOTEC Test Design Suite"),
    dashboardSidebar(sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("dashboard")),
      #this menu item is rendered conditionally based on whether a TDW is inputted, factors and levels are entered manually, or a third party viewer is checking out the designs.
      menuItemOutput("menu_module"),
      menuItem(
        "Design Evaluation",
        tabName = "designevaluation",
        icon = icon("dashboard")
      ),
      menuItem(
        "Test Event Matrix",
        tabName = "tem",
        icon = icon("dashboard")
      )
    )),
    dashboardBody(
      tabItems(
        tabItem(tabName = "documentcenter", uiOutput('fileupload')),
        tabItem(tabName = "factorlevels", uiOutput('enterfactorlevels')),
        tabItem(tabName = "doe", uiOutput("doetabsfordesign")),
        tabItem(tabName = "loadprogramview", uiOutput("loadprogram")),
        
        
        tabItem(tabName = "designevaluation", uiOutput('generated_designs')),
        tabItem(
          tabName = "home",
          class = "active",
          fluidPage(h1(
            strong("Welcome to AFOTEC's Design of Experiments Dashboard!")
          ),
          fluidRow(
            column(
              width = 6,
              radioButtons(
                inputId = "module",
                label = h2("Which module would you like to work in?"),
                choiceNames = c(
                  "Upload a completed Factor Level Worksheet",
                  "Enter Factors and Levels from Scratch"
                ),
                choiceValues = c("tdw_template", "enter_fls"),
                width  = 800
              ),
              actionButton("start", strong("Start"))
            ),
            column(width  = 6, uiOutput('load_data'))
          ))
          
          
        ),
        tabItem(tabName = "tem", uiOutput("testeventmatrix"))
      )
    )
  )
} 
