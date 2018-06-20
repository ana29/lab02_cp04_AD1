
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

    output$main<- renderPlotly({
      
      if(input$partidos== '' &&  input$pautas== ''){
        pauta_count = respostas%>% 
          group_by(tema) %>% 
          summarise(total_p_causa = sum(resposta == "S"))
        
        total= sum(pauta_count$total_p_causa)
        
        pauta_porcentagem = pauta_count%>% 
          group_by(tema) %>% 
          summarise(porcentagem = (total_p_causa*100)/total)
        
        pauta_count
        p<- plot_ly(pauta_porcentagem, labels = ~tema, values = ~porcentagem, type = 'pie') %>%
          layout(title = 'Porcentagem de Votos Sim por Tema da Pauta',
                 xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
     
      }
      
    
      if(input$partidos!= '' && input$pautas== ''){
        
        genero_avaliacao = respostas%>%
          filter(sigla_partido %in% c(input$partidos))%>%
          group_by(genero) %>% 
          summarise(genero_quant_s = sum(resposta == "S"),
                    genero_quant_n = sum(resposta == "N"))
        
        genero_avaliacao$total <- rowSums(genero_avaliacao[2:3])
        freq_relativa =genero_avaliacao%>% group_by(genero) %>% summarise(fqr = (genero_quant_s*100/total))
        
        p <- freq_relativa%>%ggplot(aes(genero,fqr)) +
          geom_col(fill="pink")+
          ggtitle( "Porcentagem de Votos Sim Separados Por Gênero por Partido")
      }
      
      if(input$partidos!= '' && input$pautas!= ''){
        
        respostas_candidatos = respostas%>%group_by(nome_urna)%>% 
          filter(sigla_partido %in% c(input$partidos),tema %in% c(input$pautas), resposta%in% c("S") )
        
        p <- plot_ly(respostas_candidatos, 
                     x = ~nome_urna,
                     y = ~votos_recebidos,
                     text = ~situacao_candidatura,
                     type = 'bar', 
                     color=~situacao_candidatura)%>% 
        layout(
          title = 'Situação eleitoral e Votos Recebidos para os Candidato que Votaram Sim para o Tema',
               xaxis = list(title = 'Nome na Urna'),
               yaxis = list(title = 'Votos Recebidos'))
          
        
      }
      if(input$partidos== '' && input$pautas!= ''){
        estado_avaliacao = respostas_long%>%
          filter(tema %in% c(input$pautas
                             ))%>% 
          group_by(sigla_estado)%>% 
          summarise(quantidade_S = sum(resposta == "S"),
                    quantidade_N = sum(resposta == "N"))
        
        estado_avaliacao$total <- rowSums(estado_avaliacao[2:3])
        freq_relativa =estado_avaliacao%>% group_by(sigla_estado) %>% summarise(fqr = (quantidade_S*100/total))
        
        p <-freq_relativa%>%ggplot(aes(sigla_estado,fqr)) +
          geom_col(aes(fill= sigla_estado))+
          ggtitle('Quantidade de Votos Sim por Estado de Acordo com o Tema da Pauta')
        
        
      }
      ggplotly(p) 
      
        })  
 
   
    

  })
