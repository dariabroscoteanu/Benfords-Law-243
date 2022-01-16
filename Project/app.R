
#Marton Sergiu & Gherghescu Andreea & Broscoteanu Daria
#Grupa 243
#Proiect - Legea lui Benford 


library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Benford's Law"),
  helpText("Project by Marton Sergiu, Gherghescu Andreea, Broscoteanu Daria - Grupa 243"),
  navbarPage("Benford App",
             #experiment 1
             tabPanel("US Births and Deaths 2020 - 2021",
                      sidebarLayout(      
                        sidebarPanel(
                          #input conditional pentru datele oficiale
                          radioButtons("column2", "Pick domain:",
                                      choiceNames = c('Births 2020', 'Births 2021', 'Deaths 2020', 'Deaths 2021'),
                                      choiceValues = c('BIRTHS2020', 'BIRTHS2021', 'DEATHS2020', 'DEATHS2021')
                          ),
                          sliderInput("n2", "Marime de referinta", min = 0, max =60, value = 20),
                          
                          hr(),
                          helpText("Sursa date:"),
                          tags$a(href="https://www.kaggle.com/datasnaek/youtube-new?select=CAvideos.csv", "Kaggle.com-Trending YouTube Video Statistics (2019)")
                        ),
                        mainPanel(
                          #creare tab-uri
                          tabsetPanel(
                            tabPanel("Grafic", plotOutput("date2")), 
                            tabPanel("Vizualizare date tabel", DT::dataTableOutput("mytable2") )
                          )
                        )
                      )
             ),
             tabPanel("Pokemon",
                      sidebarLayout(      
                        sidebarPanel(
                          #selectInput("type", "Pick data type:", 
                                      #choices=c('Pokemon')),
                          #input conditional pentru datele oficiale
                          
                           radioButtons("column3", "Pick domain:",
                                        choiceNames = c('HP', 'Attack', 'Defense', 'Speed'),
                                        choiceValues = c('HP', 'Attack', 'Defense', 'Speed')
                                        ),
                            
                          sliderInput("n3", "Marime de referinta", min = 0, max = 1000, value = 200),
                          
                          hr(),
                          helpText("Sursa date:"),
                          tags$a(href="https://www.kaggle.com/datasnaek/youtube-new?select=CAvideos.csv", "Kaggle.com-Trending YouTube Video Statistics (2019)")
                        ),
                        mainPanel(
                          #creare tab-uri
                          tabsetPanel(
                            tabPanel("Grafic", plotOutput("date3")), 
                            tabPanel("Vizualizare date tabel", DT::dataTableOutput("mytable3") )
                          )
                        )
                      )
             ),
             
             tabPanel("US Population",
                      #h2("Legea lui Benford"),
                      #uiOutput('formula'),
                      #h4("Experimentul YouTube (Date de referinta Canada 2019)"),
                      #textOutput("text1"),
                      #textOutput("text2"),
                      
                      sidebarLayout(      
                        sidebarPanel(
                          
                          #input conditional pentru datele oficiale
                          
                          radioButtons("column", "Pick domain:",
                                       choiceNames = c('Population 2010', 'Population 2011', 'Population 2012', 'Population 2013', 'Population 2014', 'Population 2015', 'Population 2016', 'Population 2017', 'Population 2018', 'Population 2019'),
                                       choiceValues = c('ESTIMATESBASE2010', 'ESTIMATESBASE2011', 'ESTIMATESBASE2012', 'ESTIMATESBASE2013', 'ESTIMATESBASE2014', 'ESTIMATESBASE2015', 'ESTIMATESBASE2016', 'ESTIMATESBASE2017', 'ESTIMATESBASE2018', 'ESTIMATESBASE2019')
                                       ),
                          sliderInput("n", "Marime de referinta", min = 0, max =5000, value = 2000),
                          
                          hr(),
                          helpText("Sursa date:"),
                          tags$a(href="https://www.kaggle.com/datasnaek/youtube-new?select=CAvideos.csv", "Kaggle.com-Trending YouTube Video Statistics (2019)")
                        ),
                        
                        #afisare grafic / tabel cu date
                        mainPanel(
                          #creare tab-uri
                          tabsetPanel(
                            tabPanel("Grafic", plotOutput("date")), 
                            tabPanel("Vizualizare date tabel", DT::dataTableOutput("mytable1") )
                          )
                        )
                        
                      ),
             ),
             tabPanel("Dogs Names",
                      sidebarLayout(      
                        sidebarPanel(
                          #input conditional pentru datele oficiale
                          
                          radioButtons("column4", "Pick domain:",
                                      choiceNames = c('Count'),
                                      choiceValues = c('Count')
                          ),
                          sliderInput("n4", "Marime de referinta", min = 0, max =16000, value = 1000),
                          
                          hr(),
                          helpText("Sursa date:"),
                          tags$a(href="https://www.kaggle.com/datasnaek/youtube-new?select=CAvideos.csv", "Kaggle.com-Trending YouTube Video Statistics (2019)")
                        ),
                        mainPanel(
                          #creare tab-uri
                          tabsetPanel(
                            tabPanel("Grafic", plotOutput("date4")), 
                            tabPanel("Vizualizare date tabel", DT::dataTableOutput("mytable4") )
                          )
                        )
                        )
              ),
            
             tabPanel("Utilizari ale Legii lui Benford",
                      h3("Analiza vanzarilor din supermarket (2019)"),
                      textOutput("text3"),
                      textOutput("text4"),
                      
                      
                      fluidRow(
                        column(4,
                               h4("Rating"),
                               helpText("Cauza: limitare interval"),
                               plotOutput("grafic1")
                               
                        ),
                        column(4, 
                               h4("Cantitate"),
                               helpText("Cauza: nu creste exponential"),
                               plotOutput("grafic2")
                               
                        ),
                        column(4,
                               h4("ID Factura"),
                               helpText("Cauza: numere unice"),
                               plotOutput("grafic3")
                               
                        )
                      )
             ),
             
  ),
)

#################################################################################
#
# Server function
#
server <- function(session,input,output) {
  #zona de text
  output$text1<-renderText({
    "Este cunoscut faptul ca numarul vizualizarilor
    video-urilor de pe YouTube respecta ipotezele de examinare a datelor de catre lege:
    sa fie numere generate aleator, predispuse cresterii exponentiale, nerestrictionate de maxime sau minime, iar 
    calculele sa aiba loc pe esantioane mari de date.
    "
  })
  output$text2<-renderText({
    "
    Se poate observa cum de la primele 1500 de vizualizari procentele se aproprie
    de respectarea legii. Tabelul cu date neoficiale contine date modificate manual, neconforme cu 
    realitatea care demonstreaza faptul ca Legea lui Benford poate sa semnaleze in anumite cazuri frauda.
    
    "
  })
  output$text3<-renderText({
    "
    Setul de date reprezinta vanzarile unei companii de 
    supermarketuri din Myanmar care a inregistrat timp de 3 luni,
    date din fiecare oras din tara. 
    "
  })
  output$text4<-renderText({
    "
    Se poate observa ca Legea lui Benford nu este aplicabila pe anumite tipuri de date.
    "
  })
  output$formula <- renderUI({
    withMathJax(
      #afisare formula legea lui Benford
      helpText('$$P(d)=lg (d+1) -lg(d)=lg(\\frac{d+1}{d})=lg(1+\\frac {1}{d}), d\\in\\{1..9\\}$$')
    )
  })
  
  
  #functie care genereaza graficul legii lui Benford pentru un set de date
  Benfords_law <- function(rate, number_of_lines)
  {
  
    firstDigit <- function(element){
      element = gsub('[0.]', '', element)
       as.numeric(substr(element, 1, 1))
    }
    
    
    frequencies <- c (0, 0, 0, 0, 0, 0, 0, 0, 0)
    total_frequencies <- 0
    procents <- c (0, 0, 0, 0, 0, 0, 0, 0, 0)
    benford <- c (0, 0, 0, 0, 0, 0, 0, 0, 0)
    
    
    for (i in 1: number_of_lines){
      first_digit = firstDigit(rate[i])
      frequencies[first_digit] <- frequencies[first_digit] + 1
      total_frequencies <- total_frequencies + 1
    }
    
    for (i in 1:9){
      procents[i] <- frequencies[i]/total_frequencies
    }
    
    for (i in 1:9){
      benford[i] <- log10(1 + 1/i)
    }
    
    frame3<-data.frame(
      numere=c("1","2", "3", "4", "5", "6", "7", "8", "9"),
      ben=benford,
      procente=procents
    )
    #generare grafic
    ggplot(data = frame3, aes(x = numere, group = 1))+ geom_bar(aes(y = procente), stat = "identity", color=NA, fill="yellow") + geom_line(aes(y = ben), stat = "identity", color="red") 
    #generare grafic
  }
  
  
  output$date <- renderPlot({
    
    #citire din fisierul dat ca input din dropdown
    #nume_csv<-paste("USpopulation.csv")
    data1<-(read.csv("USpopulation.csv", header=TRUE))
    
    
    #afisare tabel cu date

    output$mytable1 = DT::renderDataTable(data1)
    
   
    #incheiere afisare tabel cu date
    
    #extrag numele coloanei acolo unde al doilea dropdown a fost activ
    nume_coloana<-input$column
    if (nume_coloana == 'Population 2010')
      rate<-data1$POPESTIMATE2010
    else if (nume_coloana == 'Population 2011')
      rate<-data1$POPESTIMATE2011
    else if (nume_coloana == 'Population 2012')
      rate<-data1$POPESTIMATE2012
    else if (nume_coloana == 'Population 2013')
      rate<-data1$POPESTIMATE2013
    else if (nume_coloana == 'Population 2014')
      rate<-data1$POPESTIMATE2014
    else if (nume_coloana == 'Population 2015')
      rate<-data1$POPESTIMATE2015
    else if (nume_coloana == 'Population 2016')
      rate<-data1$POPESTIMATE2016
    else if (nume_coloana == 'Population 2017')
      rate<-data1$POPESTIMATE2017
    else if (nume_coloana == 'Population 2018')
      rate<-data1$POPESTIMATE2018
    else
      rate<-data1$POPESTIMATE2019
    
    
    #aplic legea lui Benford pt n date de Slidebar
    
    #lucrez doar pe coloana Views
    linii_coloana<-input$n
    
    Benfords_law(rate, linii_coloana)
    
    
  })
  #grafice
  output$date4 <- renderPlot({
    data4<-(read.csv("DogsName.csv", header=TRUE))
    
    
    #afisare tabel cu date
    
    output$mytable4 = DT::renderDataTable(data4)
    
    rate<-data4$Count
    
    #lucrez doar pe coloana Views
    linii_coloana<-input$n4
    
    Benfords_law(rate, linii_coloana)
  })
  
  output$date3 <- renderPlot({
    data3<-(read.csv("Pokemon.csv", header=TRUE))
    
    
    #afisare tabel cu date
    
    output$mytable3 = DT::renderDataTable(data3)
    
    nume_coloana<-input$column3
    if (nume_coloana == 'HP')
      rate<-data3$HP
    else if (nume_coloana == 'Attack')
      rate<-data3$Attack
    else if (nume_coloana == 'Defense')
      rate<-data3$Defense
    else
      rate<-data3$Speed
    
    #lucrez doar pe coloana Views
    linii_coloana<-input$n3
    
    Benfords_law(rate, linii_coloana)
  })
  output$date2 <- renderPlot({
    data2 <- (read.csv("DB.csv", header=TRUE))
    
    output$mytable2 = DT::renderDataTable(data2)
    
    nume_coloana<-input$column2
    if (nume_coloana == 'Births 2020')
      rate<-data2$BIRTHS2020
    else if (nume_coloana == 'Births 2021')
      rate<-data2$BIRTHS2021
    else if (nume_coloana == 'Deaths 2020')
      rate<-data2$DEATHS2020
    else
      rate<-data2$DEATHS2021
    
    linii_coloana<-input$n2
    
    Benfords_law(rate, linii_coloana)
  })

  
}
shinyApp(ui = ui, server = server)