
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shiny)
library(tidyverse)
library(here)
library(plotly)

dados = read_csv(here("data/respostas-merepresenta-2016.csv"))
respostas= dados%>% gather(key = "tema", value = "resposta", 10:23)



shinyUI(fluidPage(

  # Application title
  titlePanel("#MeRepresenta - XXXXXXXXXXX "),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("partidos", "Partido:",
                  choices =c(Choose='' ,respostas$sigla_partido) ),
      
      selectInput("votos", "Voto:",
                  choices = c(Choose='', "Sim", "Não") ),

      uiOutput("candidatoSelecao"),
      
      selectInput("pautas","Pautas:",
                  choices = c(Choose='' ,respostas$tema))
   
    ),

    # Show a plot of the generated distribution
    mainPanel(
      #plotOutput("main")
      plotlyOutput("main") 
      
      
      
    )
  )
))
