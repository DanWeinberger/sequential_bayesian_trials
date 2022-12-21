library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(shinydashboard)
library(reshape2)

 
#source('./R/power_func.R')     
 
#Power with a single look at different sample sizes
# power <-lapply( c(0.05, 0.025,0.01,0.001), power.func ) %>%
#   bind_rows() %>%
# saveRDS('./Results/power_point.rds')


#Power for a sequential trial with looks every Nth subject
# look_freq <- c(500, 100,200,1000,2000)
# set.alpha <-  c(0.05, 0.025,0.01,0.001)
# combos <- expand.grid(look_freq, set.alpha) %>% split( 1:nrow(.))
# 
#  cum_power <- pbapply::pblapply(combos, cum_power_func) %>%  bind_rows() %>% saveRDS('./Results/power_cum.rds')

power <- readRDS('./Results/power_point.rds')

cum_power <-  readRDS('./Results/power_cum.rds') %>%
  dplyr::filter(look_freq !=250)

prior.vector <- unique(power$prior.info)

true.ve.vector <- unique(power$ve.new.trial)
outcome.ve.vector <- unique(power$outcome)


shinyApp(
  
  ui = dashboardPage(
    
    dashboardHeader(title = "Performance of simulated RSV vaccine trial",titleWidth=500),
    
    dashboardSidebar(
      selectInput("endpoint.ve", "Endpoint for stopping trial:", 
                  outcome.ve.vector, multiple=F),
      
      selectInput("set.alpha", "Alpha:", 
                  unique(cum_power$alpha), multiple=F),
      
      selectInput("looks", "Evaluate results after every Nth subject:", 
                  unique(cum_power$look_freq), multiple=F),
      selectInput("prior", "Select prior:", 
                  prior.vector, multiple=T, selected=prior.vector),
      
      selectInput("ve.new", "True Vaccine effectiveness in new trial:", 
                  true.ve.vector, multiple=T, selected=true.ve.vector)
      
  
    ),
    dashboardBody(
      fluidRow(
        tabPanel(title="",
                 tabBox( title="Trial performance", id='tabset1a',height='auto',width=12,
                         tabPanel(title='Single readout',
                                  plotlyOutput("power.point" )),
                         tabPanel(title='sequential trial',
                                  plotlyOutput("power.cum")   ))
        )
        
    )
  )
  ), 
  
  server = function(input, output) {
    
    output$power.point = renderPlotly({
      prob.cut <- 100*(1-as.numeric(input$set.alpha))
      
      p1 <-  power %>%
        dplyr::filter(outcome==input$endpoint.ve & prior.info %in% input$prior & alpha==input$set.alpha) %>%
        ggplot(aes( x=pop, y=proportion, group=prior.info, color=prior.info)) +
        geom_line() +
        facet_wrap(~ ve.new.trial) +
        theme_classic() +
        geom_hline(yintercept = c(0.05, 0.8), lty=2, col='gray')+
        xlab('Number of participants in vaccine arm') +
        ggtitle(paste0('Proportion of trials with >',prob.cut,  '% probability that effect is ', input$endpoint.ve))
      ggplotly(p1)
      p1
    })
    
    output$power.cum = renderPlotly({
      prob.cut <- 100*(1-as.numeric(input$set.alpha))
      p2 = cum_power %>%
        mutate(ve.new.trial=as.factor(ve.new.trial)) %>%
        dplyr::filter(alpha == input$set.alpha & look_freq %in% input$looks & outcome==input$endpoint.ve & prior.info %in% input$prior)  %>%
       ggplot(aes( x=pop, y=prop_stopped, group=prior.info, color=prior.info)) +
        geom_line() +
        geom_point() +
        facet_wrap(~ ve.new.trial) +
        theme_classic() +
        geom_hline(yintercept = c(0.05, 0.8), lty=2, col='gray')+
        xlab('Number of participants in vaccine arm') +
        ggtitle(paste0('Cumulative proportion with >', prob.cut,  '% prob that effect is ', input$endpoint.ve))
      ggplotly(p2)
      p2
    })
    
  }
)
