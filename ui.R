### LINK TO THE SHINY APP: https://szabolcskerekes.shinyapps.io/R_skills_final_assignment/

library(shiny)
library(shinythemes)
library(data.table)
library(DT)
library(ggplot2)
library(tidyr)
library(ggmap)
library(maps)
library(forecast)
library(scales)
library(ggrepel)
library(gridExtra)

honey_prod <- fread('honeyproduction.csv') # using this here for the inline codes on the first panel

navbarPage(title = "Honey production in the US", theme = shinytheme("cosmo"),
           
           ### Summary panel
           tabPanel("Summary",   
                    h1("Honey production in the USA (1998-2012)"),
                    p("This Shiny application analyises and visualises the honey production data from the United States.
                      The original dataset has", nrow(honey_prod), " observations and", ncol(honey_prod), " variables. 
                      this has been expanded with longitude and latitude coordinates as well as the name of the states with
                      the help of the ", span("States", style = "color:blue"), " dataset. There are ", sum(is.na(honey_prod)),"  
                      missing values in the dataset. The first table below provides an overview
                      of the basic summary statistics of the original dataset, while the second table below shows additional
                      information for the selected original variable in the selected year."),
                    mainPanel(
                      tableOutput("summary_text"),
                      fluidRow(
                      column(6,
                        sliderInput("years_sum",
                                  "Years:",
                                  min = 1998,
                                  max = 2012,
                                  value = 1998),
                      selectInput('variable_no', 'Variable', c("numcol",
                                                            "yieldpercol",
                                                            "totalprod",
                                                            "stocks",
                                                            "priceperlb",
                                                            "prodvalue")),
                      textOutput("variable_do"),
                      tableOutput("var_col")),
                      column(6, 
                             plotOutput("var_col_plot")))
                    )

           ),
           
           ### Production charts by states
           tabPanel("Production", sidebarLayout(
             sidebarPanel(
               sliderInput("years",
                           "Years:",
                           min = 1998,
                           max = 2012,
                           value = 2012),
             width = 2),
                    
             mainPanel(
               p("On this page the annual production volumes and values can be selected for any of the years,
                 which is then presented by countries both geographically and ordered from the largest to the smallest."),
               tabsetPanel(
               tabPanel("Volumes", 
                        plotOutput("ggplot_plot_vol1"),
                        plotOutput("ggplot_plot_vol2")),
               tabPanel("Values", 
                        plotOutput("ggplot_plot_val1"),
                        plotOutput("ggplot_plot_val2")))
             ))),
           tabPanel("Forecasts", sidebarLayout(
             sidebarPanel(
               sliderInput("years_fc",
                           "Until year:",
                           min = 2013,
                           max = 2022,
                           value = 2013),
               selectInput('measure', 'Measurement', c("Value", "Volume")),
               width = 2),
             
             mainPanel(
               p("On the below panel arima, naive and SES forecasts can be simulated via selecting the forecast horizon,
                 and also the projection measures. Please note that for visual presentation at least two periods (2014 as
                 the end year) needs to be selected."),
               tabsetPanel(
                 tabPanel("ARIMA",
                          plotOutput("arima_plot"),  
                          verbatimTextOutput("arima_list")),
                 tabPanel("NAIVE",
                          plotOutput("naive_plot"),  
                          verbatimTextOutput("naive_list")),
                 tabPanel("SES",
                          plotOutput("ses_plot"),  
                          verbatimTextOutput("ses_list"))
             )))),
           tabPanel("Models", sidebarLayout(
             sidebarPanel(
               sliderInput("years_model",
                           "For year:",
                           min = 1998,
                           max = 2012,
                           value = 2012),
             selectInput('variable_mod1', 'Variable X', c("numcol",
                                                      "yieldpercol",
                                                      "totalprod",
                                                      "stocks",
                                                      "priceperlb",
                                                      "prodvalue")),
             selectInput('variable_mod2', 'Variable Y', c("numcol",
                                                      "yieldpercol",
                                                      "totalprod",
                                                      "stocks",
                                                      "priceperlb",
                                                      "prodvalue")),
             width = 2),
           mainPanel(
             p("This section can be used for modelling relationships between the variables for a given year. 
               The below plots show the relationship visualed between the selected variables."),
             plotOutput("reactive_model_plot"),
             br(),
             p("The details of the linear model can be seen below."),
             verbatimTextOutput("model_summary")
           ))
           )
)