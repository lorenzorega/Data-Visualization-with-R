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

#lettura dati

marcatori<-read.csv("./data/Marcatori2021.csv")

marcatori
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
   # titlePanel("APP1"),
    h1("APP:1- Statistiche generali"),
    h2("1. Statistiche descrittive"),
  fluidRow( column(6,
    
   # textOutput("mean"),
    #textOutput("mediana"),
    #textOutput("midrange"),
    #textOutput("moda")),
   p(strong("1.1 Misure di tendenza per il numero di gol")),
   verbatimTextOutput("valuesTendency")
   #la sua soluzione usa verbatim, tu avevi tanti textOutput, come hai lasciato dopo 
  ),column(6,
           p(strong("1.2 Misure di dispersione per il numero di gol")) ,
    textOutput("devst"),
    textOutput("md"),
    textOutput("interq"),
    textOutput("r"))),
  h2("2. Tabelle"),
  
 fluidRow(   
   
   column(6,
          p(strong("2.1- Frequenze")),
          numericInput("numeroGolMax","Inserire max gol", value =0),
  actionButton("doTabella", "Calcola"),
    tableOutput("tabellaFrequenze")),
  column(6, 
         p(strong("2.2 - frequeze")),
  sliderInput("rangeGol","Range", value = c(2,5), min=min(marcatori$Gol), max=max(marcatori$Gol), step=1),
  #controlla poi quest'argomento value. Per ora sembrerebbe che vada specificato per forza
    dataTableOutput("dinamica")))
)

  


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    
    output$valuesTendency<-renderText(
      {
        media<- paste0("Media:",round(mean(marcatori$Gol),1))
        mediana<-paste0("Mediana: ",median(marcatori$Gol))
        midrange<-paste0("Midarange: ",(round((max(marcatori$Gol) + min(marcatori$Gol))/2,1)))
        moda<- paste0("Moda: ",   getmode(marcatori$Gol))
        paste(media, mediana, midrange, moda, sep="\n")
        
        
      }
    )
    devst<-round(sd(marcatori$Gol),1)
    #notato che lui arrotonda sempre
    m<-round(mad(marcatori$Gol),1)
    iq<-IQR(marcatori$Gol)
    r<-max(marcatori$Gol)- min(marcatori$Gol)
    t<-tibble(marcatori$Gol)
    
    t<-t%>%
        rename("Gol"= "marcatori$Gol")%>%
        mutate(Gol=as.integer(Gol))%>%
        group_by(Gol)%>%
        summarise("Frequenza"=length(Gol))
    
    #potevi anche usare count(Gol)
        
    
 #   r.max.goals <- reactive(input$numeroGolMax)#parametro golmax che cambia con l'input
    #così cambiava senza premere un bottone
    #se il bottone viene premuto, assumi il valore dell'input numeroGolMax
    r.max.goals<- eventReactive(input$doTabella,{ input$numeroGolMax })
    range<-reactive(input$rangeGol)
    #raccomandazione è aspettare che venga premuto un pulsante,
    #non che venga aggiornato subito
    
    #eventReactive serve per inserire un evento che dipende da quello
    #che è la pressione di un pulsante
    #appena viene premuto, viene valutata l'espressione numeroGolMax
    
 #   output$mean<-renderText(paste0("La media vale : ",media))
#    output$mediana<-renderText(paste0("La mediana vale : ",mediana))
 #   output$midrange<-renderText(paste0("Il midrange vale : ",midrange))
  #  output$moda<-renderText(paste0("La moda vale : ",moda))
    output$devst<-renderText(paste0("La deviazione standard vale : ",devst))
    output$md<-renderText(paste0("La MAD vale : ",m))
    output$interq<-renderText(paste0("L' IQR vale : ",iq))
    output$r<-renderText(paste0("Il range vale : ",r))
    output$tabellaFrequenze<-renderTable( { req(r.max.goals()) #quest'istruzione si è rilevata fondamentale
          #aspetta che ci sia un valore in quel campo, altriemnti non
      #fa il renderTable
         #si assicura che questo valore ci sia prima di procedere 
                                           t%>%
                                               filter(Gol<=r.max.goals())
})
    #
    output$dinamica<-renderDataTable(options=list(pageLength=20),{
      #parentesi graffe per scrivere codice r
        req(range())
        t%>%
            filter(Gol>=range()[1],Gol<=range()[2] )
           
        
    })
 
}
# Run the application 
shinyApp(ui = ui, server = server)

