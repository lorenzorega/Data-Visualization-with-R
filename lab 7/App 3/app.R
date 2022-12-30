#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(tidyverse)


ruoli<-c("Tutti", "Difensore", "Centrocampista", "Attaccante")
marcatori<-read.csv("./data/Marcatori2021.csv")
squadre<-sort(unique(marcatori$Squadra))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("App3"),
    sidebarLayout(  
        
        sidebarPanel(
            #notato che ci tiene a numerare i titoli
            
            h2("0. Carica il file"),
            
            fileInput("file", "Load csv file Marcatori Serie A", accept = c(".csv")),
          h2("1. Seleziona i ruoli"),
            selectInput("ruolo","Seleziona un ruolo", ruoli, selected ="Tutti"),
        #  selectInput("squadra","Seleziona una o più squadre", squadre, multiple=TRUE),
   ), 
     mainPanel( 
          tabsetPanel(
            tabPanel( "Marcatori: tabella", tableOutput("tabellaSquadre") ),
            tabPanel( "Marcatori: grafico", plotOutput("graficoSquadre"))
          )
             ),
   )
    )
#ho sidebarLayout 
#dentro sidebarPanel e main panel specificando cosa va a lato e cosa più sulla parte centro-destra




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    df<- reactive({req(input$file)
        read.csv(input$file$datapath, header = TRUE, sep=",")})
    
    t.ruolo<-reactive(input$ruolo)
    t.squadre<-reactive(input$squadra)
    
   
    
    
    
    tabella<-reactive({
        req(df)
        req(t.ruolo)
      #  req(t.squadre)
        
        df()%>%
            filter(
                if (t.ruolo() == "Tutti") {
                    Ruolo!="Tutti"
                } else {
                    Ruolo==t.ruolo()
                }
            )%>%
          #  filter( Squadra%in%t.squadre())%>%
            group_by(Squadra)%>%
            summarise("Marcatori"=length(Squadra))%>%
            arrange(desc(Marcatori))
    } 
    )
    
    output$tabellaSquadre<-renderTable(tabella())
    
    output$graficoSquadre<-renderPlot({  req(tabella)
        tabella()%>%
            ggplot(aes(x=Marcatori, y=reorder(Squadra, Marcatori)))+
            geom_col()+
            ylab("Team")+ 
            xlab("Numero marcatori")+
            ggtitle("Numero di marcatori per squadra", "aggiornato alla 27esima giornata di Serie A" )+
            #secondo parametro è il sottotitolo
            geom_text(aes(label=Marcatori ),
                      hjust=1.5, color="white")+
            theme_bw(base_size = 18)
        
        
        })     #procedimenti per disegnare grafico, ma la tabella prima
    
}

# Run the application 
shinyApp(ui = ui, server = server)

