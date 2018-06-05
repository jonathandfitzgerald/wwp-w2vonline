# Word Vector Analysis for Women Writers Online
#
# Created by Jonathan D. Fitzgerald
#
# Last updated: June 5, 2018

library(shiny)
library(magrittr)
library(tidyverse)
library(shinyjs)
library(wordVectors)
library(DT)
#wwp_model = read.vectors("data/wwpData_df.bin")
#wwp_model_adorned <- read.vectors("data/adornedText.bin")
#wwp_model_unadorned <- read.vectors("data/wwo-non-adorned.bin")
clustering = kmeans(wwp_model,centers=150,iter.max = 40)

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(tags$style(
    HTML(
      "
      .shiny-output-error-validation {
      color: #e69b24;
      font-size: 40px;
      text-align:center;
      }
      .container-fluid {
      width: 1000px;
      }
      
      .resetrow {
      padding-top:24px;
      }
      h2 {
      font-size: 40px;
      color: #6c8299;
      text-align:center;
      margin: 30px 0;
      
      
      }
    
      #clustering_panel .resetrow {margin-bottom: 20px;}

      .math_text {font-size: 36px;
      font-weight: bold;
      padding-top:12px;
      text-align: center;}
      "
    )
    )),
  
  # Application title
  titlePanel("Word Vector Analysis for Women Writers Online"),
  sidebarLayout(
    sidebarPanel(width = 3,
      selectInput("modelSelect", "Model", 
                 choices = list("Regularized Model" = "wwp_model_adorned", "Unregularized Model" = "wwp_model_unadorned"),    
                 selected = 1),
      numericInput("all_count", "# of words:", 10),
      actionButton("all_reset_input", "Reset inputs")
    ),
    
    mainPanel(width = 9, id = "mainpanel",
    
    
  navbarPage(
    "",
    
    # Basic Tab
    tabPanel(
      "Basic",
      fluidRow(
        shinyjs::useShinyjs(),
        id = "basic_panel",
        column(9,
               # Sidebar with a inputs
               textInput("basic_word1", "Word 1"),
               hr())
        
      ),

      # Show a plot of the generated distribution
      fluidRow(
               column(
                 9,
                 DT::dataTableOutput("basic_table")
               ))
      
    ),
    
    # Clustering Tab
    
    tabPanel(
      "Clustering",
      fluidRow(
        shinyjs::useShinyjs(),
        id = "clustering_panel",
        column(9,
               class = "resetrow",
               actionButton("clustering_reset_input", "Reset clusters"))
        
      ),
      # Show a plot of the generated distribution
      fluidRow(
        column(
          9,
          DT::dataTableOutput("clustering_table")
        ))
    ),
    
    # Addition Tab
    tabPanel(
      "Addition",
      fluidRow(
        shinyjs::useShinyjs(),
        id = "addition_panel",
        column(4,
               # Sidebar with a inputs
               textInput("addition_word1", "Word 1")),
        column(
          1,
          class = "mathCol",
          tags$p(class="math_text","+")
        ),
        column(4,
               textInput("addition_word2", "Word 2"))
        
      ),
      # Show a plot of the generated distribution
      fluidRow(
               column(
                 9,
                 DT::dataTableOutput("addition_table")
               ))
    ),
    
    # Subtraction Tab
    
    tabPanel(
      "Subtraction",
      fluidRow(
        shinyjs::useShinyjs(),
        id = "subtraction_panel",
        column(4,
               # Sidebar with a inputs
               textInput("subtraction_word1", "Word 1")),
        column(
          1,
          class = "mathCol",
          tags$p(class="math_text","-")
        ),
        column(4,
               textInput("subtraction_word2", "Word 2"))
        
      ),
      # Show a plot of the generated distribution
      fluidRow(
               column(
                 9,
                 DT::dataTableOutput("subtraction_table")
               ))
    ),
    
    
    # Analogies Tab
    
    tabPanel(
      "Analogies",
      fluidRow(
        shinyjs::useShinyjs(),
        id = "analogies_panel",
        column(2,
               # Sidebar with a inputs
               textInput("analogies_word1", "Word 1")),
        column(
          1,
          class = "mathCol",
          tags$p(class="math_text","-")
        ),
        column(2,
               textInput("analogies_word2", "Word 2")),
        column(
          1,
          class = "mathCol",
          tags$p(class="math_text","+")
        ),
        column(2,
               textInput("analogies_word3", "Word 3")
        )
      ),

      # Show a plot of the generated distribution
      fluidRow(
               column(
                 9,
                 DT::dataTableOutput("analogies_table")
               ))
    ),
    
    
    # Advanced Tab
    
    tabPanel(
      "Advanced",
      fluidRow(
        shinyjs::useShinyjs(),
        id = "advanced_panel",
        column(2,
               # Sidebar with a inputs
               textInput("advanced_word1", "Word 1")),
        column(1,
               class = "mathCol",
               selectInput("advanced_math", "Math", 
                           choices = list("+" = "+", "-" = "-", "*" = "*", "/" = "/"),    
                           selected = 1)),
        column(2,
               textInput("advanced_word2", "Word 2")),
        column(1,
               class = "mathCol",
               selectInput("advanced_math2", "Math", 
                           choices = list("+" = "+", "-" = "-", "*" = "*", "/" = "/"),   
                           selected = 1)),
        column(2,
               textInput("advanced_word3", "Word 3")
        )
      ),

      # Show a plot of the generated distribution
      fluidRow(
               column(
                 9,
                 DT::dataTableOutput("advanced_table")
               ))
    )
    

    
    
  )))
    )

# Define server logic required to display table
server <- function(input, output) {
  
  
  
  # Basic Tab 

  output$basic_table <- DT::renderDataTable(DT::datatable({
    validate(need(input$basic_word1 != "", "Enter a search term in Word 1."))
    data <- get(input$modelSelect) %>% closest_to(input$basic_word1, input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
    
  }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(paging = FALSE,searching = FALSE)))
  
  # Addition Tab
  output$addition_table <- DT::renderDataTable(DT::datatable({
    validate(need(input$addition_word1 != "" && input$addition_word2 != "", "Enter search term into Word 1 and Word 2."))
    data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$addition_word1,] + get(input$modelSelect)[rownames(get(input$modelSelect))==input$addition_word2,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
    
  }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(paging = FALSE,searching = FALSE)))
  
  
  # Subtraction tab
  output$subtraction_table <- DT::renderDataTable(DT::datatable({
    validate(need(input$subtraction_word1 != "" && input$subtraction_word2 != "", "Enter search term into Word 1 and Word 2."))
    data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$subtraction_word1,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$subtraction_word2,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
    
  }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(paging = FALSE,searching = FALSE)))
  
  # Analogies tab
  output$analogies_table <- DT::renderDataTable(DT::datatable({
    validate(need(input$analogies_word1 != "" && input$analogies_word2 != "" && input$analogies_word3 != "", "Enter search term into Word 1, Word 2, and Word 3."))
    data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$analogies_word1,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$analogies_word2,] + get(input$modelSelect)[rownames(get(input$modelSelect))==input$analogies_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
    
  }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(paging = FALSE,searching = FALSE)))
  
  # Advanced tab
  output$advanced_table <- DT::renderDataTable(DT::datatable({
    validate(need(input$advanced_word1 != "", "Enter search term into Word 1."))
    data <- get(input$modelSelect) %>% closest_to(input$advanced_word1, input$all_count)
    if (input$advanced_word2 != "" && input$advanced_word3 == "") {
      if (input$advanced_math == "+") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] renderText(input$advanced_math) get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "-") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "*") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] * get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "/") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
    }
    
    if (input$advanced_word2 != "" && input$advanced_word3 != "") {
      
      if (input$advanced_math == "+" && input$advanced_math2 == "+") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] + get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] + get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "+" && input$advanced_math2 == "-") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] + get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "+" && input$advanced_math2 == "*") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] + get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] * get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "+" && input$advanced_math2 == "/") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] + get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] / get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      
      if (input$advanced_math == "-" && input$advanced_math2 == "+") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] + get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "-" && input$advanced_math2 == "-") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "-" && input$advanced_math2 == "*") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] * get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "-" && input$advanced_math2 == "/") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] / get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      
      if (input$advanced_math == "*" && input$advanced_math2 == "+") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] * get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] + get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "*" && input$advanced_math2 == "-") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] * get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "*" && input$advanced_math2 == "*") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] * get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] * get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "*" && input$advanced_math2 == "/") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] * get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] / get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      
      if (input$advanced_math == "/" && input$advanced_math2 == "+") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] / get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] + get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "/" && input$advanced_math2 == "-") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] / get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] - get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "/" && input$advanced_math2 == "*") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] / get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] * get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      if (input$advanced_math == "/" && input$advanced_math2 == "/") {
        data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] / get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] / get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
      }
      
    }
    data
  }, escape = FALSE, colnames=c("Word", "Similarity to word(s)"), options = list(paging = FALSE,searching = FALSE)))
  
  
  # Clustering tab
  output$clustering_table <- DT::renderDataTable(DT::datatable({
    data <- sapply(sample(1:150,10),function(n) {
      paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=",names(clustering$cluster[clustering$cluster==n][1:150]),"'>",names(clustering$cluster[clustering$cluster==n][1:150]),"</a>")
    }) %>% as_data_frame()
  }, escape = FALSE, colnames=c(paste0("cluster_",1:10)), options = list(lengthMenu = c(10, 50, 100, 150), pageLength = 10,searching = TRUE)))
  
  
  
  
  
  
  
  observeEvent(input$basic_reset_input, {
    shinyjs::reset("basic_panel")
  })
  
  observeEvent(input$addition_reset_input, {
    shinyjs::reset("addition_panel")
  })
  
  observeEvent(input$subtraction_reset_input, {
    shinyjs::reset("subtraction_panel")
  })
  
  observeEvent(input$analogies_reset_input, {
    shinyjs::reset("analogies_panel")
  })
  
  observeEvent(input$advanced_reset_input, {
    shinyjs::reset("advanced_panel")
  })
  
  observeEvent(input$all_reset_input, {
    shinyjs::reset("mainpanel")
    shinyjs::reset("all_count")
  })
  
  observeEvent(input$clustering_reset_input, {
    output$clustering_table <- DT::renderDataTable(DT::datatable({
      data <- sapply(sample(1:150,10),function(n) {
        paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=",names(clustering$cluster[clustering$cluster==n][1:150]),"'>",names(clustering$cluster[clustering$cluster==n][1:150]),"</a>")
      }) %>% as_data_frame()
    }, escape = FALSE, colnames=c(paste0("cluster_",1:10)), options = list(lengthMenu = c(10, 50, 100, 150), pageLength = 10,searching = TRUE)))
  })
  
  #output$value <- renderPrint({ dataset() })
}



# Run the application
shinyApp(ui = ui, server = server)
