library(shiny)
library(dplyr)
library(ggplot2)
library(patchwork)
library(RColorBrewer)
library(shinydashboard)
library(reshape2)

 
#source('./R/power_func.R')     
 
#Power with a single look at different sample sizes
# power <-lapply( c(0.05, 0.025,0.01,0.001), power.func ) %>%
#   bind_rows() %>%
# saveRDS('./Results/power_point.rds')

power <- readRDS('./Results/power_point.rds')


#Power for a sequential trial with looks every Nth subject
# look_freq <- c(500, 100,250,1000,2000)
# set.alpha <-  c(0.05, 0.025,0.01,0.001)
# combos <- expand.grid(look_freq, set.alpha) %>% split( 1:nrow(.))
# 
# cum_power <- pbapply::pblapply(combos, cum_power_func) %>%  bind_rows() %>% saveRDS('./Results/power_cum.rds') 

cum_power <-  readRDS('./Results/power_cum.rds') 

prior.vector <- unique(power$prior.info)

true.ve.vector <- unique(power$ve.new.trial)
outcome.ve.vector <- unique(power$outcome)


app2 <- shinyApp(
  
  ui = dashboardPage(
    
    dashboardHeader(title = "Performance of simulated RSV vaccine trial",titleWidth=500),
    
    dashboardSidebar(
      selectInput("prior", "Select prior:", 
                  prior.vector, multiple=T),
      
      selectInput("ve.new", "True Vaccine effectiveness in new trial:", 
                  true.ve.vector, multiple=T),
      
      selectInput("endpoint.ve", "Endpoint for stopping trial:", 
                  outcome.ve.vector, multiple=F),
      
      selectInput("set.alpha", "Alpha:", 
                  unique(cum_power$alpha), multiple=F),
      
      selectInput("looks", "Evaluate results after every Nth subject:", 
                 unique(cum_power$look_freq), multiple=F)
    ),
    dashboardBody(
      fluidRow(
        box(tabPanel("power.point", plotOutput("power.point")), width=4),
        box(tabPanel("power.cum", plotOutput("power.cum")), width=4)
      )
    )
  ), 
  
  server = function(input, output) {
    
    output$power.point = renderPlot({
      p1 <-  power %>%
        filter(outcome==input$endpoint.ve & prior.info %in% input$prior & alpha==set.alpha) %>%
        ggplot(aes( x=pop, y=proportion, group=prior.info, color=prior.info)) +
        geom_line() +
        facet_wrap(~ ve.new.trial) +
        theme_classic() +
        geom_hline(yintercept = c(0.05, 0.8), lty=2, col='gray')+
        ggtitle(paste0('Proportion of trials with >', 100*(1-alpha),  '% probability that effect is', input$endpoint.ve))
      p1
    })
    
    output$power.cum = renderPlot({
      p2 = cum_power %>%
        filter(look_freq %in% input$looks & outcome==input$endpoint.ve & prior.info %in% input$prior) 
        stopped_efficacy1 %>%
        ggplot(aes( x=pop, y=prop_stopped, group=prior.info, color=prior.info)) +
        geom_line() +
        geom_point() +
        facet_wrap(~ ve.new.trial) +
        theme_classic() +
        geom_hline(yintercept = c(0.05, 0.8), lty=2, col='gray')+
        ggtitle(paste0('Cumulative proportion with >', 100*(1-alpha),  '% prob that effect is ', input$endpoint.ve))
      p2
    })
    
  }
)