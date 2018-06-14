
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(here)
library(plotly)
library(ggplot2)
#source(here("code/read_wrangle.R"))


dados = read_csv(here("data/respostas-merepresenta-2016.csv"))
respostas= dados%>% gather(key = "tema", value = "resposta", 10:23)


shinyServer(function(input, output) {

  output$candidatoSelecao <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$partidos))
      return()
    
    # Get the data set with the appropriate name
    partido  = input$partidos
    
    candidatos_partido = unique(respostas%>%filter(sigla_partido==partido)%>%select(nome_urna))
    
    selectInput("candidatos","Candidato:",
                choices = c(Choose='' , candidatos_partido))
  })
  
    output$main<- renderPlotly({
      
      if(input$partidos== '' && input$votos=='' && input$pautas== ''){
      #ta faltando a condição de todos vazios 
        pauta_count = respostas%>% 
          group_by(tema) %>% 
          summarise(total_p_causa = sum(resposta == "S"))
        
        total= sum(pauta_count$total_p_causa)
        
        pauta_porcentagem = pauta_count%>% 
          group_by(tema) %>% 
          summarise(porcentagem = (total_p_causa*100)/total)
        
        
     p<- plot_ly(pauta_porcentagem, labels = ~tema, values = ~porcentagem, type = 'pie') %>%
          layout(title = 'Porcentagem de Votos por Tema da Pauta',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     
     
      }
      
      if(input$partidos!= '' && input$votos=='' && input$pautas== ''){
        
        respostas_partido = respostas%>%group_by(tema)%>% 
          filter(sigla_partido %in% c(input$partidos))%>%
          summarise(total_sim = sum(resposta == "S" ),
                    total_nao = sum(resposta == "N"))
        
        p <- plot_ly(respostas_partido, x = ~tema, y = ~total_sim, type = 'bar', color="Sim") %>%
          add_trace(y = ~total_nao, color = "Não") %>%
          layout(yaxis = list(title = 'Votos'), barmode = 'stack')
        
        
     
      }
      if(input$partidos!= '' && input$votos!='' && input$pautas== ''){
        
        respostas_candidato_partido = respostas%>%group_by(tema, nome_urna)%>% 
          filter(sigla_partido %in% c(input$partidos), resposta %in% c(input$votos) )
        
        p <- plot_ly(
          respostas_candidato_partido, x = ~nome_urna,y = ~tema, type = 'scatter',
          mode = "markers", marker = list(color = "pink")) %>%
          layout(
            title = "Voto s/n p partido",
            xaxis = list(title = "Causa"),
            margin = list(l = 100)
          )
      }
      ggplotly(p) 
      
        })  
 
   
    

  })
