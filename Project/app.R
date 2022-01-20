# Marton Alexandru-Sergiu, Gherghescu Andreea Diana, Broscoteanu Daria-Mihaela
# Grupa 243
# Proiect — Legea lui Benford

library("shiny")
library("ggplot2")
library("plot.matrix")
library("ggpubr")


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  titlePanel("Legea lui Benford"),
  helpText(
    "Proiect realizat de Marton Alexandru-Sergiu, Gherghescu Andreea Diana, Broscoteanu Daria-Mihaela — Grupa 243"
  ),
  navbarPage(
    "Meniu",
    tabPanel("Populația SUA",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   "column1",
                   "Alege domeniu:",
                   choiceNames = sapply(2010:2019, function(x) { paste("Populație", x) }),
                   choiceValues = sapply(2010:2019, function(x) { paste("POPESTIMATE", x, sep="") })
                 ),
                 sliderInput(
                   "n",
                   "Mărime de referință",
                   min = 0,
                   max = 5000,
                   value = 2000
                 ),
                 
                 hr(),
                 helpText("Sursă date:"),
                 tags$a(href = "https://www.kaggle.com/datasnaek/youtube-new?select=CAvideos.csv", "Kaggle.com-Trending YouTube Video Statistics (2019)")
               ),
               mainPanel(tabsetPanel(
                 tabPanel("Grafic", plotOutput("date")),
                 tabPanel("Vizualizare date tabel", DT::dataTableOutput("mytable1"))
               ))
             )),
    tabPanel(
      "Nașteri și decese din SUA 2020–2021",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "column2",
            "Alege domeniu:",
            choiceNames = c('Nașteri 2020', 'Nașteri 2021', 'Morți 2020', 'Morți 2021'),
            choiceValues = c('BIRTHS2020', 'BIRTHS2021', 'DEATHS2020', 'DEATHS2021')
          ),
          sliderInput(
            "n2",
            "Mărime de referință",
            min = 0,
            max = 60,
            value = 20
          ),
          hr(),
          helpText("Sursă date:"),
          tags$a(href = "https://www.kaggle.com/datasnaek/youtube-new?select=CAvideos.csv", "Kaggle.com-Trending YouTube Video Statistics (2019)")
        ),
        mainPanel(tabsetPanel(
          tabPanel("Grafic", plotOutput("date2")),
          tabPanel("Vizualizare date tabel", DT::dataTableOutput("mytable2"))
        ))
      )
    ),
    tabPanel("Pokemon",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   "column3",
                   "Alege domeniu:",
                   choiceNames = c('HP', 'Atac', 'Apărare', 'Viteză'),
                   choiceValues = c('HP', 'Attack', 'Defense', 'Speed')
                 ),
                 sliderInput(
                   "n3",
                   "Mărime de referință",
                   min = 0,
                   max = 1000,
                   value = 200
                 ),
                 hr(),
                 helpText("Sursă date:"),
                 tags$a(href = "https://www.kaggle.com/datasnaek/youtube-new?select=CAvideos.csv", "Kaggle.com-Trending YouTube Video Statistics (2019)")
               ),
               mainPanel(tabsetPanel(
                 tabPanel("Grafic", plotOutput("date3")),
                 tabPanel("Vizualizare date tabel", DT::dataTableOutput("mytable3"))
               ))
             )),
    tabPanel("Nume de câini",
             sidebarLayout(
               sidebarPanel(
                 radioButtons(
                   "column4",
                   "Alege domeniu:",
                   choiceNames = c('Număr'),
                   choiceValues = c('Count')
                 ),
                 sliderInput(
                   "n4",
                   "Mărime de referință",
                   min = 0,
                   max = 16000,
                   value = 1000
                 ),
                 
                 hr(),
                 helpText("Sursă date:"),
                 tags$a(href = "https://www.kaggle.com/datasnaek/youtube-new?select=CAvideos.csv", "Kaggle.com-Trending YouTube Video Statistics (2019)")
               ),
               mainPanel(tabsetPanel(
                 tabPanel("Grafic", plotOutput("date4")),
                 tabPanel("Vizualizare date tabel", DT::dataTableOutput("mytable4"))
               ))
             ))
  )
)



server <- function(session, input, output) {
  Benfords_law <- function(rate, number_of_lines)
  {
    firstDigit <- function(element) {
      element = gsub('[0.]', '', element)
      as.numeric(substr(element, 1, 1))
    }
    
    secondDigit <- function(element) {
      element2 = gsub('[0.]', '', element)
      as.numeric(substr(element, 2, 2))
    }
    
    frequencies <- c (0, 0, 0, 0, 0, 0, 0, 0, 0)
    frequencies2 <- matrix(0, 9, 10)
    total_frequencies <- 0
    total_frequencies2 <- 0
    procents <- c (0, 0, 0, 0, 0, 0, 0, 0, 0)
    procents2 <- matrix(, 9, 10)
    benford <- c (0, 0, 0, 0, 0, 0, 0, 0, 0)
    benford2 <- matrix (, 9, 10)
    
    for (i in 1:number_of_lines) {
      first_digit = firstDigit(rate[i])
      second_digit = secondDigit(rate[i])
      frequencies[first_digit] <- frequencies[first_digit] + 1
      frequencies2[first_digit, second_digit + 1] <-
        frequencies2[first_digit, second_digit + 1] + 1
      total_frequencies2 <- total_frequencies2 + 1
      total_frequencies <- total_frequencies + 1
    }
    # Pentru frequencies2, a 2 a cifra o retinem ca second_digit + 1, deoarece putem sa avem si 0
    # Deci pentru o a 2 a cifra i, noi ii tinem minte valorile in i + 1.
    
    for (i in 1:9) {
      # For only 1 digit
      procents[i] <- frequencies[i] / total_frequencies
      
      # For 2 digits
      for (j in 1:10) {
        procents2[i, j] = frequencies2[i, j] / total_frequencies2
      }
    }
    
    for (i in 1:9) {
      # For only 1 digit
      benford[i] <- log10(1 + 1 / i)
      
      # For 2 digits
      for (j in 1:10) {
        benford2[i, j] = log10(1 + 1 / (i * 10 + (j - 1)))
      }
    }
    
    frame3 <- data.frame(
      numere = c("1", "2", "3", "4", "5", "6", "7", "8", "9"),
      ben = benford,
      procente = procents
    )
    
    numbers <- c()
    for (i in 1:9) {
      for (j in 1:10) {
        x <- as.character(i)
        y <- as.character(j - 1)
        val <- paste(x, y, sep = "")
        numbers <- append(numbers, val)
      }
    }

    frame4 <- data.frame(perechi = numbers,
                         ben2 = c(t(benford2)),
                         procente2 = c(t(procents2)))
    
    # Draw plots
    p1 <-
      ggplot(data = frame3, aes(x = numere, group = 1)) + geom_bar(aes(y = procente),
                                                                   stat = "identity",
                                                                   color = NA,
                                                                   fill = "yellow") + geom_line(aes(y = ben), stat = "identity", color = "red")
    p2 <-
      ggplot(data = frame4, aes(x = perechi, group = 1)) + geom_bar(
        aes(y = procente2),
        stat = "identity",
        color = NA,
        fill = "yellow"
      ) + geom_line(aes(y = ben2), stat = "identity", color = "red")
    ggarrange(p1, p2, nrow = 2)
  }
  
  
  output$date <- renderPlot({
    data <- (read.csv("USpopulation.csv", header = TRUE))
    
    output$mytable1 = DT::renderDataTable(data)
    
    rate <- data[[input$column1]]
    rows <- input$n

    Benfords_law(rate, rows)
  })
  
  output$date2 <- renderPlot({
    data <- (read.csv("DB.csv", header = TRUE))
    
    output$mytable2 = DT::renderDataTable(data)

    rate <- data[[input$column2]]
    rows <- input$n2
    
    Benfords_law(rate, rows)
  })
  
  output$date3 <- renderPlot({
    data <- (read.csv("Pokemon.csv", header = TRUE))
    
    output$mytable3 = DT::renderDataTable(data)

    rate <- data[[input$column3]]
    rows <- input$n3
    
    Benfords_law(rate, rows)
  })
  
  output$date4 <- renderPlot({
    data <- (read.csv("DogsName.csv", header = TRUE))
    
    output$mytable4 = DT::renderDataTable(data)
    
    rate <- data[[input$column4]]
    linii_coloana <- input$n4
    
    Benfords_law(rate, linii_coloana)
  })
}

shinyApp(ui = ui, server = server)