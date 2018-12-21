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
library(ggpubr)

honey_prod <- fread('honeyproduction.csv')

##### Summary setup #####

summary_data <- summary(honey_prod)
summary_data <- as.data.table(summary_data)

seq_output <- NULL

for (i in 1:8){
  seq_temp <- seq(1, 6, 1)
  seq_output <- c(seq_output, seq_temp)
}

summary_data$ID <- seq_output

summary_wide <- spread(summary_data, V2, N)
summary_wide$ID <- NULL
summary_wide$V1 <- NULL

##### States setup #####

states <- cbind(state.abb,state.name)
colnames(states) <- c("state", "name")
states <- data.frame(states)

state_location <- state.center

for (i in 1:length(states$state)){
  states$lon[i] <- state_location[[1]][i]
  states$lat[i] <- state_location[[2]][i]
}

states <- as.data.table(states)

us_map <- map_data('usa')

honey_prod <- merge(honey_prod, states, by = 'state')

##### Forecasting setup #####

honey_prod_ts <- honey_prod[, .(V1 = sum(totalprod/1000), V2 = sum(prodvalue/1000)), by = year]
honey_prod_ts$totalprod <- ts(honey_prod_ts$V1, start = 1998, frequency = 1)
honey_prod_ts$prodvalue <- ts(honey_prod_ts$V2, start = 1998, frequency = 1)

##### Active functions #####

shinyServer(function(input, output) {
  
### Summary  
  my_reactive_summary <- reactive({  
    summary_wide
    
  })

  output$summary_text <- renderTable(my_reactive_summary())
  
  reactive_varname <- reactive({
    input$variable_no
  })
  
  output$varname <- renderText(reactive_varname())

  reactive_variable <- reactive({
    
    variable_des <- c("numcol: Number of honey producing colonies",
                      "yieldpercol: Yield per colony (lbs)",
                      "totalprod: Total production (numcol*yieldpercol), (lbs)",
                      "stocks: Stocks held by producers on Dec 15 (lbs)",
                      "priceperlb: Average price per pound ($)",
                      "prodvalue: Value of production (totalprod*prodvalue), ($)")
    
    var_no <- ifelse("numcol" %in% input$variable_no, 1,
                     ifelse("yieldpercol" %in% input$variable_no, 2,
                            ifelse("totalprod" %in% input$variable_no, 3,
                                   ifelse("stocks" %in% input$variable_no, 4,
                                          ifelse("priceperlb" %in% input$variable_no, 5, 6)))))
    
    return(variable_des[var_no])
  })  
  
  output$variable_do <- renderText(reactive_variable())
  
  ### numcol
  reactive_num_col <- reactive({
    rbind(
    c("Number of observations",length(honey_prod[year == input$years_sum,]$numcol)),
    c("Minimum", round(min(honey_prod[year == input$years_sum,]$numcol), digits = 2)),
    c("Maximum", round(max(honey_prod[year == input$years_sum,]$numcol), digits = 2)),
    c("Mean", round(mean(honey_prod[year == input$years_sum,]$numcol), digits = 2)),
    c("Median", round(median(honey_prod[year == input$years_sum,]$numcol), digits  = 2)),
    c("Standard deviation", round(sd(honey_prod[year == input$years_sum,]$numcol), digits = 2))
  )
  })
  
  reactive_num_col_ggplot <- reactive({
    g <- ggplot(data = honey_prod[year == input$years_sum,], aes(x = numcol/1000)) +
      geom_histogram(fill = "orange") +
      labs(x = "Thousand colonies", y = "Frequency") + 
      ggtitle(paste0("Honey producing colonies in year ", input$years_sum)) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 18))
            
    return(g)
  })
  
  ### yieldpercol
  reactive_yieldpercol <- reactive({
    rbind(
      c("Number of observations",length(honey_prod[year == input$years_sum,]$yieldpercol)),
      c("Minimum", round(min(honey_prod[year == input$years_sum,]$yieldpercol), digits = 2)),
      c("Maximum", round(max(honey_prod[year == input$years_sum,]$yieldpercol), digits = 2)),
      c("Mean", round(mean(honey_prod[year == input$years_sum,]$yieldpercol), digits = 2)),
      c("Median", round(median(honey_prod[year == input$years_sum,]$yieldpercol), digits  = 2)),
      c("Standard deviation", round(sd(honey_prod[year == input$years_sum,]$yieldpercol), digits = 2))
    )
  }) 
  
  reactive_yieldpercol_ggplot <- reactive({
    g <- ggplot(data = honey_prod[year == input$years_sum,], aes(x = yieldpercol)) +
      geom_histogram(fill = "orange") +
      labs(x = "Yield per colony (lbs)", y = "Frequency") + 
      ggtitle(paste0("Yield per colony in year ", input$years_sum)) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 18))
    
    return(g)
  })
  
  ### totalprod
  reactive_totalprod <- reactive({
    rbind(
      c("Number of observations",length(honey_prod[year == input$years_sum,]$totalprod)),
      c("Minimum", round(min(honey_prod[year == input$years_sum,]$totalprod), digits = 2)),
      c("Maximum", round(max(honey_prod[year == input$years_sum,]$totalprod), digits = 2)),
      c("Mean", round(mean(honey_prod[year == input$years_sum,]$totalprod), digits = 2)),
      c("Median", round(median(honey_prod[year == input$years_sum,]$totalprod), digits  = 2)),
      c("Standard deviation", round(sd(honey_prod[year == input$years_sum,]$totalprod), digits = 2))
    )
  })
  
  reactive_totalprod_ggplot <- reactive({
    g <- ggplot(data = honey_prod[year == input$years_sum,], aes(x = totalprod/1000)) +
      geom_histogram(fill = "orange") +
      labs(x = "Thousand lbs", y = "Frequency") + 
      ggtitle(paste0("Total production in year ", input$years_sum)) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 18))
    
    return(g)
  })
  
  ### stocks
  reactive_stocks <- reactive({
    rbind(
      c("Number of observations",length(honey_prod[year == input$years_sum,]$stocks)),
      c("Minimum", round(min(honey_prod[year == input$years_sum,]$stocks), digits = 2)),
      c("Maximum", round(max(honey_prod[year == input$years_sum,]$stocks), digits = 2)),
      c("Mean", round(mean(honey_prod[year == input$years_sum,]$stocks), digits = 2)),
      c("Median", round(median(honey_prod[year == input$years_sum,]$stocks), digits  = 2)),
      c("Standard deviation", round(sd(honey_prod[year == input$years_sum,]$stocks), digits = 2))
    )
  })
  
  reactive_stocks_ggplot <- reactive({
    g <- ggplot(data = honey_prod[year == input$years_sum,], aes(x = stocks/1000)) +
      geom_histogram(fill = "orange") +
      labs(x = "Thousand lbs", y = "Frequency") + 
      ggtitle(paste0("Stocks held by producers in year ", input$years_sum)) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 18))
    
    return(g)
  })
  
  ### priceperlb
  reactive_priceperlb <- reactive({
    rbind(
      c("Number of observations",length(honey_prod[year == input$years_sum,]$priceperlb)),
      c("Minimum", round(min(honey_prod[year == input$years_sum,]$priceperlb), digits = 2)),
      c("Maximum", round(max(honey_prod[year == input$years_sum,]$priceperlb), digits = 2)),
      c("Mean", round(mean(honey_prod[year == input$years_sum,]$priceperlb), digits = 2)),
      c("Median", round(median(honey_prod[year == input$years_sum,]$priceperlb), digits  = 2)),
      c("Standard deviation", round(sd(honey_prod[year == input$years_sum,]$priceperlb), digits = 2))
    )
  })
  
  reactive_priceperlb_ggplot <- reactive({
    g <- ggplot(data = honey_prod[year == input$years_sum,], aes(x = priceperlb)) +
      geom_histogram(fill = "orange") +
      labs(x = "USD", y = "Frequency") + 
      ggtitle(paste0("Price per lbs in year ", input$years_sum)) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 18))
    
    return(g)
  })
  
  ### prodvalue
  reactive_prodvalue <- reactive({
    rbind(
      c("Number of observations",length(honey_prod[year == input$years_sum,]$prodvalue)),
      c("Minimum", round(min(honey_prod[year == input$years_sum,]$prodvalue), digits = 2)),
      c("Maximum", round(max(honey_prod[year == input$years_sum,]$prodvalue), digits = 2)),
      c("Mean", round(mean(honey_prod[year == input$years_sum,]$prodvalue), digits = 2)),
      c("Median", round(median(honey_prod[year == input$years_sum,]$prodvalue), digits  = 2)),
      c("Standard deviation", round(sd(honey_prod[year == input$years_sum,]$prodvalue), digits = 2))
    )
  })
  
  reactive_prodvalue_ggplot <- reactive({
    g <- ggplot(data = honey_prod[year == input$years_sum,], aes(x = prodvalue/1000)) +
      geom_histogram(fill = "orange") +
      labs(x = "Thousand USD", y = "Frequency") + 
      ggtitle(paste0("Value of production in year ", input$years_sum)) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 18))
    
    return(g)
  })
  
  ### selector functions
  reactive_var_col <- reactive({

    if ("numcol" %in% input$variable_no) {
      reactive_num_col()
    } else if ("yieldpercol" %in% input$variable_no) {
      reactive_yieldpercol()
    } else if ("totalprod" %in% input$variable_no) {
      reactive_totalprod()
    } else if ("stocks" %in% input$variable_no) {
      reactive_stocks()
    } else if ("priceperlb" %in% input$variable_no) {
      reactive_priceperlb()
    } else {
      reactive_prodvalue()
    }
    
  })
  
  output$var_col <- renderTable(reactive_var_col())
  
  reactive_var_col_plot <- reactive({
    
    c <- if ("numcol" %in% input$variable_no) {
        reactive_num_col_ggplot()
      } else if ("yieldpercol" %in% input$variable_no) {
        reactive_yieldpercol_ggplot()
      } else if ("totalprod" %in% input$variable_no) {
        reactive_totalprod_ggplot()
      } else if ("stocks" %in% input$variable_no) {
        reactive_stocks_ggplot()
      } else if ("priceperlb" %in% input$variable_no) {
        reactive_priceperlb_ggplot()
      } else {
        reactive_prodvalue_ggplot()
      }
    
    return(c)
  })
  
  output$var_col_plot <- renderPlot(reactive_var_col_plot())
  
### Production plots  
  my_reactive_ggplot_vol1 <- reactive({  
    g <- ggplot() +
      geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey") +
      geom_point(data = honey_prod[year == input$years,], aes(lon, lat, color = totalprod / 1000), size = 8) +
      geom_text_repel(data = states, aes(lon, lat, label = name)) +
      scale_color_gradient(low="green", high="red") +
      guides(color = guide_legend(title="Production in klbs")) +
      ggtitle(paste0("Honey production volume in year ",input$years)) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 18),
            legend.position="right") +
    coord_fixed(1.3) 
   
    return(g)
  })
  
  output$ggplot_plot_vol1 <- renderPlot(my_reactive_ggplot_vol1())
  
  my_reactive_ggplot_vol2 <- reactive({  
    g <- ggplot(data = honey_prod[year == input$years,], aes(x = reorder(name, -totalprod), y = totalprod / 1000, fill = totalprod/1000)) +
      geom_col() +
      scale_fill_gradient(low="green", high="red") +
      guides(fill = FALSE) +
      labs(x = "State", y = "Production in klbs") + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 18),
            axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position="right")

    return(g)
  })
  
  output$ggplot_plot_vol2 <- renderPlot(my_reactive_ggplot_vol2())
    
  my_reactive_ggplot_val1 <- reactive({  
    g <- ggplot() +
      geom_polygon(data = us_map, aes(x = long, y = lat, group = group), fill = "grey") +
      geom_point(data = honey_prod[year == input$years,], aes(lon, lat, color = prodvalue / 1000), size = 8) +
      geom_text_repel(data = states, aes(lon, lat, label = name)) +
      scale_color_gradient(low="green", high="red") +
      guides(color = guide_legend(title="Production in kUSD")) +
      ggtitle(paste0("Honey production value in year ",input$years)) +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 18),
            legend.position="right") +
      coord_fixed(1.3) 
    
    return(g)
  })
  
  output$ggplot_plot_val1 <- renderPlot(my_reactive_ggplot_val1())

  my_reactive_ggplot_val2 <- reactive({  
    g <- ggplot(data = honey_prod[year == input$years,], aes(x = reorder(name, -prodvalue), y = prodvalue / 1000, fill = totalprod/1000)) +
      geom_col() +
      scale_fill_gradient(low="green", high="red") +
      guides(fill = FALSE) +
      labs(x = "State", y = "Production in kUSD") + 
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            panel.background = element_rect(fill = "white"),
            axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
            axis.title = element_text(size = 12),
            plot.title = element_text(size = 18),
            axis.text.x = element_text(angle = 90, hjust = 1),
            legend.position="right")
    
    return(g)
  })
  
  output$ggplot_plot_val2 <- renderPlot(my_reactive_ggplot_val2())
  
### Forecast plots and tables
  
  ## ARIMA
  reactive_arima_plot <- reactive({  
    ifelse("Value" %in% input$measure, 
           fit <- arima(honey_prod_ts$prodvalue),
           fit <- arima(honey_prod_ts$totalprod))
    a <- autoplot(forecast(fit, h = (input$years_fc-2012)))
    return(a)
  })
  
  output$arima_plot <- renderPlot(reactive_arima_plot())
  
  reactive_arima_list <- reactive({  
    ifelse("Value" %in% input$measure, 
           fit <- arima(honey_prod_ts$prodvalue),
           fit <- arima(honey_prod_ts$totalprod))
    b <- forecast(fit, h = (input$years_fc-2012))
    return(b)
  })
  
  output$arima_list <- renderPrint(reactive_arima_list())
  
  ## NAIVE
  reactive_naive_plot <- reactive({  
    ifelse("Value" %in% input$measure, 
           fit <- naive(honey_prod_ts$prodvalue),
           fit <- naive(honey_prod_ts$totalprod))
    a <- autoplot(forecast(fit, h = (input$years_fc-2012)))
    return(a)
  })
  
  output$naive_plot <- renderPlot(reactive_naive_plot())
  
  reactive_naive_list <- reactive({  
    ifelse("Value" %in% input$measure, 
           fit <- naive(honey_prod_ts$prodvalue),
           fit <- naive(honey_prod_ts$totalprod))
    b <- forecast(fit, h = (input$years_fc-2012))
    return(b)
  })
  
  output$naive_list <- renderPrint(reactive_naive_list())  
  
  ## SES
  reactive_ses_plot <- reactive({  
    ifelse("Value" %in% input$measure, 
           fit <- ses(honey_prod_ts$prodvalue),
           fit <- ses(honey_prod_ts$totalprod))
    a <- autoplot(forecast(fit, h = (input$years_fc-2012)))
    return(a)
  })
  
  output$ses_plot <- renderPlot(reactive_ses_plot())
  
  reactive_ses_list <- reactive({  
    ifelse("Value" %in% input$measure, 
           fit <- ses(honey_prod_ts$prodvalue),
           fit <- ses(honey_prod_ts$totalprod))
    b <- forecast(fit, h = (input$years_fc-2012))
    return(b)
  })
  
  output$ses_list <- renderPrint(reactive_ses_list())  
  
  
##### Plots and models
  
  reactive_var1 <- reactive({
  var1 <- if ("numcol" %in% input$variable_mod1) {
    honey_prod[year == input$years_model, numcol, ] / 1000
  } else if ("yieldpercol" %in% input$variable_mod1) {
    honey_prod[year == input$years_model, yieldpercol, ]
  } else if ("totalprod" %in% input$variable_mod1) {
    honey_prod[year == input$years_model, totalprod, ] / 1000
  } else if ("stocks" %in% input$variable_mod1) {
    honey_prod[year == input$years_model, stocks, ] / 1000
  } else if ("priceperlb" %in% input$variable_mod1) {
    honey_prod[year == input$years_model, priceperlb, ]
  } else {
    honey_prod[year == input$years_model, prodvalue, ] / 1000
  }
  return(var1)
})
  
reactive_var2 <- reactive({
  var2 <- if ("numcol" %in% input$variable_mod2) {
    honey_prod[year == input$years_model, numcol, ] / 1000
  } else if ("yieldpercol" %in% input$variable_mod2) {
    honey_prod[year == input$years_model, yieldpercol, ]
  } else if ("totalprod" %in% input$variable_mod2) {
    honey_prod[year == input$years_model, totalprod, ] / 1000
  } else if ("stocks" %in% input$variable_mod2) {
    honey_prod[year == input$years_model, stocks, ] / 1000
  } else if ("priceperlb" %in% input$variable_mod2) {
    honey_prod[year == input$years_model, priceperlb, ]
  } else {
    honey_prod[year == input$years_model, prodvalue, ] / 1000
  }
  return(var2)
})
  
reactive_model_ggplot <- reactive ({

g1 <- ggplot(data = honey_prod[year == input$years_model], aes(x = reactive_var1(), y = reactive_var2(), label = name)) +
   geom_point() +
   geom_text_repel() +
   geom_smooth() +
   labs(x = "Variable x", y = "Variable y") + 
   ggtitle("Lowess model") +
   theme(panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         panel.background = element_rect(fill = "white"),
         axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
         axis.title = element_text(size = 12),
         plot.title = element_text(size = 18))

g2 <- ggplot(data = honey_prod[year == input$years_model], aes(x = reactive_var1(), y = reactive_var2(), label = name)) +
  geom_point() +
  geom_text_repel() +
  geom_smooth(method = lm) +
  labs(x = "Variable x", y = "Variable y") + 
  ggtitle("Linear model") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.title = element_text(size = 12),
        plot.title = element_text(size = 18))

g <- grid.arrange(g1, g2, ncol = 2)

return (g)
  })
  
output$reactive_model_plot <- renderPlot(reactive_model_ggplot())
  
reactive_model_summary <- reactive({
  summary(lm(reactive_var1() ~ reactive_var2(), data = honey_prod[year == input$years_model]))
})  

  output$model_summary <- renderPrint(reactive_model_summary())
  
})
