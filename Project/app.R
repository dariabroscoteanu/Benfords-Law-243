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
    tabPanel(
      "Introducere",
      tags$h2("Legea lui Benford"),
      tags$p(
        paste("Legea lui Benford, numită și Legea Primei Cifre, cuprinde ",
              "observații cu privire la frecvența primei cifre a unor seturi ",
              "de date din realitate. Legea atestă faptul că în majoritatea ",
              "colecțiilor alcătuite într-un mod natural, cifra aflată pe ",
              "prima poziție are tendința să fie o cifră mai mică. În seturile ",
              "care respectă această lege, s-a observat faptul că vom avea ",
              "cifra 1 pe prima poziție în aproximativ 30% din cazuri, iar ",
              "cifra 9 în mai puțin de 5% din cazuri. Dacă cifrele ar fi ",
              "distribuite într-o manieră uniformă, fiecare dintre aceste ",
              "cifre ar apărea pe prima poziție în 11,1% din cazuri. Legea ",
              "lui Benford este folosită și pentru prezicerea distribuției ",
              "celei de-a doua cifră, cât și pentru prezicerea unor ",
              "combinații de cifre. Respectarea predicției acestei legi a ",
              "fost observată atât în cazul trecerii prin toate valorile ale ",
              "numărului de locuitori ai unei țări, cât și în cazul șirului ",
              "lui Fibonacci și șirul puterilor lui 2.")
      ),
      tags$p(
        paste(
          "În jurul anului 1938, fizicianul **Frank Benford** a observat faptul ",
          "ca tabelele logaritmice erau mai uzate în primele pagini față de ",
          "ultimele. Acesta a testat ipoteza care susținea că cifrele mai mici ",
          "au o frecvență de apariție mai mare decât cifrele mai mari pe 30 de ",
          "seturi de date, obținând astfel legea. Folosindu-ne de Legea lui ",
          "Benford putem face o predicție cu privire la distribuția cifrelor ",
          "de la 1 la 9 la nivelul unui set de date. Probabilitatea apariției ",
          "este generata astfel de această formulă:"
        )
      ),
      withMathJax(
        tags$p("$$P(D = d)=lg(1+\\frac{1}{d})\\ , unde \\ d\\in\\{1..9\\}$$")
      )
    ),
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
                   "n1",
                   "Mărime de referință",
                   min = 0,
                   max = 3193,
                   value = 2000
                 ),
                 
                 hr(),
                 helpText("Sursă date:"),
                 tags$a(href = "https://www.kaggle.com/datasnaek/youtube-new?select=CAvideos.csv", "Kaggle.com-Trending YouTube Video Statistics (2019)")
               ),
               mainPanel(tabsetPanel(
                 tabPanel("Grafic", plotOutput("date1")),
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
            max = 57,
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
                   max = 799,
                   value = 200
                 ),
                 hr(),
                 helpText("Sursă date:"),
                 tags$a(href = "https://www.kaggle.com/abcsds/pokemon", "kaggle - Pokemon with stats")
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
                   choiceNames = c('Număr apariții'),
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
                 tags$a(href = "https://www.kaggle.com/yamqwe/dog-names-over-timee", "kaggle - Dog Names over Time")
               ),
               mainPanel(tabsetPanel(
                 tabPanel("Grafic", plotOutput("date4")),
                 tabPanel("Vizualizare date tabel", DT::dataTableOutput("mytable4"))
               ))
             )),
    tabPanel(
      "Secvența Fibonacci",
      sidebarLayout(
        sidebarPanel(
          radioButtons(
            "column5",
            "Alege domeniu:",
            choiceNames = c("Valoare"),
            choiceValues = c("Value")
          ),
          sliderInput(
            "n5",
            "Mărime de referință",
            min = 0,
            max = 1477,
            value = 100
          ),
          hr(),
          helpText("Sursă date:"),
          tags$a(href = "https://www.kaggle.com/brandonconrady/fibonacci-sequence-first-10001-numbers", "kaggle - Fibonacci Sequence")
        ),
        mainPanel(tabsetPanel(
          tabPanel("Grafic", plotOutput("date5")),
          tabPanel("Vizualizare date table", DT::dataTableOutput("mytable5"))
        ))
      )
    )
  )
)



server <- function(session, input, output) {
  Benfords_law <- function(rate, number_of_lines)
  {
    firstDigit <- function(element) {
      element <- gsub('[0.]', '', element)
      as.numeric(substr(element, 1, 1))
    }
    
    secondDigit <- function(element) {
      element <- gsub('[0.]', '', element)
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
      first_digit <- firstDigit(rate[i])
      second_digit <- secondDigit(rate[i])
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
        procents2[i, j] <- frequencies2[i, j] / total_frequencies2
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
                         procente = c(t(procents2)))
    
    # Draw plots
    p1 <-
      ggplot(data = frame3, aes(x = numere, group = 1)) + geom_bar(aes(y = procente),
                                                                   stat = "identity",
                                                                   color = NA,
                                                                   fill = "yellow") + geom_line(aes(y = ben), stat = "identity", color = "red")
    p2 <-
      ggplot(data = frame4, aes(x = perechi, group = 1)) + geom_bar(
        aes(y = procente),
        stat = "identity",
        color = NA,
        fill = "yellow"
      ) + geom_line(aes(y = ben2), stat = "identity", color = "red")
    ggarrange(p1, p2, nrow = 2)
  }
  
  renderDataset <- function(index, csvFile) {
    output[[paste("date", index, sep="")]] <- renderPlot({
      data <- read.csv(csvFile, header = TRUE)
      output[[paste("mytable", index, sep="")]] <- DT::renderDataTable(data)
      
      rate <- data[[input[[paste("column", index, sep="")]]]]
      rows <- input[[paste("n", index, sep="")]]
      
      Benfords_law(rate, rows)
    })
  }
  
  renderDataset(1, "USpopulation.csv")
  renderDataset(2, "DB.csv")
  renderDataset(3, "Pokemon.csv")
  renderDataset(4, "DogsName.csv")
  renderDataset(5, "fibonacci_sequence.csv")
}

shinyApp(ui = ui, server = server)