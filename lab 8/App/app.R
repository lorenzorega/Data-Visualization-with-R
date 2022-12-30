#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library("shinyFeedback")

media_mobile <- function(v,n){
  stopifnot(length(v)>=n)
  res = rep(NA,floor((n-1)/2))
  while(length(v) >= n){
    res = c(res,mean(v[1:n]))
    v=tail(v,-1)
  }
  res = c(res,rep(NA,floor(n/2)))
  return( res )
}

fileiss="C:/Users/Lorenzo/Desktop/POLI/VIQ/laboratorio3/lab-8/covid_19-iss.xlsx"

co<- read_xlsx(fileiss,"casi_inizio_sintomi") %>% 
  mutate(CASI = as.numeric(CASI)) %>% 
  rename(SINTOMI = CASI) %>% 
  inner_join(
    read_xlsx(fileiss,"casi_prelievo_diagnosi")%>% 
      mutate(CASI = as.numeric(CASI)) %>% 
      rename(DIAGNOSI = CASI),
    by =c("DATA_INIZIO_SINTOMI"="DATA_PRELIEVO_DIAGNOSI")
  ) %>%
  rename(Data = DATA_INIZIO_SINTOMI) %>% 
  select(- starts_with("iss_date")) %>% 
  mutate(Data = as.Date(Data,format="%d/%m/%Y")) %>% 
  filter(! is.na(Data)) %>% 
  pivot_longer(c("SINTOMI","DIAGNOSI"),names_to="Tipo",values_to="CASI") %>% 
  group_by(Tipo) %>% arrange(Data) %>% 
  mutate(CASI_MM=media_mobile(CASI,7)) %>% 
  filter(!is.na(CASI_MM))

range<-co$Data

range


scelte<-c("SINTOMI", "DIAGNOSI")


# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(column(12, align="center",
  h1("VIQ dashboard Covid-19", align="center"))
  ),
  
  fluidRow(column(12,align="center",
  useShinyFeedback(),
    fileInput("file", "Caricare file di tipo xlsx",accept = c(".xlsx")))
  #placeholder per specificare cosa vuoi nella barra (seconda etichetta in pratica)
  ),
  
  fluidRow(
    column(4,
    checkboxGroupInput("filtraggio","Visualizza", scelte, selected="SINTOMI"), inline=TRUE),
    column(8, sliderInput("filtroData", "Range date", min = min(range),
                          max = max(range),
                          value = c(min(range), max(range))
                          
                          #tu provavi a mettere tutte le date dal dataset, invece
                          #nella soluzione ha messo date min e max scelte da lui, non
                          #le prende dal dataset
                          # as.Date("2020-02-01")
                          #oppure
                          #usa timeFormat
    ))
  ),
  fluidRow(
   plotOutput("grafico",
              brush=brushOpts(id="sel"))
   #gli ha messo anche restOnNew=TRUE
  ),
  fluidRow(
    dataTableOutput("tabellaBrush")
  )

   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   covidfile<- reactive({
     
     hideFeedback("file") #rimuove eventuali warning attivi da precedenti errori
     #controllo su estensione file
     ok <- (tools::file_ext(input$file$name) == "xlsx")
     feedbackWarning(inputId="file", show = !ok, text = "Caricare un file di tipo xlsx !" )
     req(ok) # si procede solo se estensione ok
     #ci da warning su file
     input$file$datapath
        })
   
   #lui ha usato eventReactive
   
   #generato contenuto covidfile(), eseguita quella funzione
   #mette tryCatch cerca di leggere tab inizio sintomi
   #se tutto va bene, ok, se warning, lo stampa in console(in html non cambia)
   #se ho avuto errore (es tab che non esiste) feedBackWarning
   #questo per due volte
   #validate va a verificare che due variabili non siano null(senno si ferma)
   #sennò messaggio di errore
   
   filtro<-reactive({
     input$filtraggio
   })
   
   covid_casi_data <- reactive({
     
     
     req(covidfile())
    
     read_xlsx(covidfile(),"casi_inizio_sintomi") %>% 
       mutate(CASI = as.numeric(CASI)) %>% 
       rename(SINTOMI = CASI) %>% 
       inner_join(
         read_xlsx(covidfile(),"casi_prelievo_diagnosi")%>% 
           mutate(CASI = as.numeric(CASI)) %>% 
           rename(DIAGNOSI = CASI),
         by =c("DATA_INIZIO_SINTOMI"="DATA_PRELIEVO_DIAGNOSI")
       ) %>%
       rename(Data = DATA_INIZIO_SINTOMI) %>% 
       select(- starts_with("iss_date")) %>% 
       mutate(Data = as.Date(Data,format="%d/%m/%Y")) %>% 
       filter(! is.na(Data)) %>% 
       pivot_longer(c("SINTOMI","DIAGNOSI"),names_to="Tipo",values_to="CASI") %>% 
       group_by(Tipo) %>% arrange(Data) %>% 
       mutate(CASI_MM=media_mobile(CASI,7)) %>% 
       filter(!is.na(CASI_MM))
     
     #tutto questo lui lo scrive in r.dati() che è un reactiveVal inizia a null
   #reactive val mi dice che quello sarà un valore, ma non me lo definisce ancora
     #spesso lo si usa per i valori intermedi
   
    
   }
   )
   
   date<-reactive(
     {
       input$filtroData
     }
   )
   
   dati_filtrati<-reactive({
     req(covid_casi_data)
     req(filtro)
     req(date)
     
   covid_casi_data<-  covid_casi_data()%>%
       filter(Data>date()[1], Data<date()[2])
       
     
     
     if(length(filtro())==1){
       covid_casi_data%>%
         filter(Tipo==filtro())
     }else if(length(filtro())==2){
       covid_casi_data
     }
     else{
       validate("Nessuna scelta effettuata")
     }
    
     
   })
   
   
   
   
  #validate(need che la lunghezza di r.tipo>0, "non c'è nessun conteggio")
   #e non continua con renderPlot
   
   g<-reactive(
     
     #lui qui presume di avere già tutto
     #es le date in automatico le passa, anche i due sintomi e diagnosi
     #subito passate
     {
    grafico<-   req(dati_filtrati)
       ggplot(dati_filtrati(),aes(x=Data,y=CASI_MM,color=Tipo,group=Tipo))+
         geom_line()+
         scale_color_brewer(type="qual",guide="none")+
         geom_point(data=~.x %>% filter(Data==max(Data)))+
         geom_text(aes(label=Tipo),data=~.x %>% filter(Data==max(Data)),
                   hjust="left",vjust="bottom",nudge_x = 3, show.legend = FALSE)+
         geom_text(aes(label=round(CASI_MM)),data=~.x %>% filter(Data==max(Data)),
                   hjust="left",vjust="top",nudge_x = 3,nudge_y=-200,size=2,color="gray20")+
         scale_x_date(expand=expansion(add=c(0,40)))+
         ylab("Casi")+
         ggtitle("Andamento dei casi","Per data di diagnosi o inizio sintomi, media mobile a 7gg")+
         theme_minimal()
       
       #la parte della media:
       #coppia variabili reactive
       #r.avg<-reactiveValues(sintomi=NULL, diagnosi=NULL)
       #sono i valori medi della selezione che è stata fatta
       #come assegno un valore a loro?
       #se selezioneDati(con brush)>0
       #da valore a sintomi
       #stessa cosa per diagnosi
       #se non ho selezionato alcun con diagnosi (o sintomi) do valore null
       #a queste variabili
       #se ho la media per sinotmi !is.null(r.avg$sintomi)
       
       #metto hline (horizontal line) y intercept è il valore medio calcolato
       #e poi usa annotate
       #la x in un caso è la data minima, in un caso la data massima
       #per allineare a sx e a dx le annotazioni
      
       
       #quando cambia data invalida dei valori con observeEvent(r.date(), ...)
    
       
       
     }
   )
   
   output$grafico<-renderPlot(g())
   
   
   
   output$tabellaBrush<-renderDataTable(
     {
       req(input$sel)
       req(dati_filtrati)
       brushedPoints(dati_filtrati(), input$sel)
       
     }
   )
   
   #observeEvent su variazione del brush e poi fatte un po' di elaborazioni
   #formatta i tipi delle variabili 
   #se sintomi tra i r.tipo() li tengo, senno no
   
   
   #metto nella variabile reactive i dati
   #controllo se sel_brush nrow>0 o no 
   #req(sel_brush())
   #validate(need(nrow(sel_brush())>0), "Nessun dato selezionato")
   #sel_brush() se non era valido esce il messaggio e non questo
       }
    
   


# Run the application 
shinyApp(ui = ui, server = server)
