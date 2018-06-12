selectInput("advanced_math", "Math", 
            choices = list("+" = "+", "-" = "-", "*" = "*", "/" = "/"),    
            selected = 1))



selectInput("model", "Model", 
            choices = list("First Model" = "wwp_model", "Second Model" = "wwp_model2"),   
            selected = 1)),

wwp_model = read.vectors("data/wwpData_df.bin")
wwp_model_adorned <- read.vectors("data/adornedText.bin")
wwp_model_unadorned <- read.vectors("data/wwo-non-adorned.bin")


if (input$advanced_word2 != "" && input$advanced_word3 == "") {
  data <- get(input$modelSelect) %>% closest_to(get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word1,] get(input$advanced_math) get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word2,] get(input$advanced_math2) get(input$modelSelect)[rownames(get(input$modelSelect))==input$advanced_word3,], input$all_count) %>% mutate("Link" <- paste0("<a target='_blank' href='http://wwo.wwp.northeastern.edu/WWO/search?keyword=", .$word,"'>",.$word,"</a>")) %>% .[c(3,2)]
}