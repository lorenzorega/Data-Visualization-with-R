#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(dplyr)
library(tidyverse)

ruoli<-c("Tutti", "Difensore", "Centrocampista", "Attaccante")
marcatori<-read.csv("./data/Marcatori2021.csv")
squadre<-sort(unique(marcatori$Squadra))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("APP 2"),
fluidRow(
   column(4, fileInput("file", "Load csv file Marcatori Serie A", accept = c(".csv"))),
  column(4,  selectInput("ruolo","Seleziona un ruolo", ruoli, selected ="Tutti")),
 column(4,  selectInput("squadra","Seleziona una o più squadre", squadre, multiple=TRUE))),
 fluidRow( 
     column(12, align="center", tableOutput("tabellaSquadre"))),
    
    )




# Define server logic required to draw a histogram
server <- function(input, output) {

    df<- reactive({
        #se non mi ha caricato nessun file, non faccio niente
        req(input$file)
        read.csv(input$file$datapath, header = TRUE, sep=",")})
    
    t.ruolo<-reactive(input$ruolo)
    t.squadre<-reactive(input$squadra)
    
    output$tabellaSquadre<-renderTable({
        req(df)
        req(t.ruolo)
        req(t.squadre)
        
        df()%>%
            filter(
                if (t.ruolo() == "Tutti") {
                    Ruolo!="Tutti"
                } else {
                    Ruolo==t.ruolo()
                }
            )%>%
            filter( Squadra%in%t.squadre())%>%
            group_by(Squadra)%>%
            summarise("Marcatori"=length(Squadra))%>%
            arrange(desc(Marcatori))
        #potevi anche usare count
        
        #arrange ordina le righe in base al valore di una o più colonne
        #ricordati sempre che quando accedi a un elemento reactive, devi mettere le due parentesi ()
            
        
    })
    #questa volta ha fatto un passaggio in piu, all'output passa solo la tabella
}

# Run the application 
shinyApp(ui = ui, server = server)
