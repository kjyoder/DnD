require(shiny)
#require(keras)
# from: shiny.rstudio.com/articles/build.html 2019-09-07


# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("AI generated monster name"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text box to enter desired number of monsters
      numericInput('n_mob', 'How many monsters would you like?',
                   1, min=1, max=40)
      # 
      # # Input: Selector for variable to plot against mpg ----
      # selectInput("variable", "Variable:",
      #             c("Cylinders" = "cyl",
      #               "Transmision" = "am",
      #               "Gears" = "gear")),
      # 
      # # Input: Checkbox for weather outliers should be included ----
      # checkboxInput("outliers", "Show outliers", TRUE)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Pulls from the model ---
      htmlOutput("mobs")
      
      # # Output: Plot of the requested variable against mpg ----
      # plotOutput("mpgPlot")
    )
  )
)

# Data pre-processing ----
model <- keras::load_model_hdf5('MobNet_TOB2.h5')

character_lookup <- data.frame(character = c(letters,".","-"," ","+"))
character_lookup[["character_id"]] <- 1:nrow(character_lookup)
character_lookup$character <- as.character(character_lookup$character)
max_length <- 10



# # Tweak "am" to have nicer factor labels
# mpgData <- mtcars
# mpgData$am <- factor(mpgData$am, labels = c("Automatic", "Manual"))
  
# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # functions for getting model predictions
  
  generate_name <- function(model, character_lookup, max_length, temperature = 1){
    choose_next_char <- function(preds, character_lookup, temperature){
      preds <- log(preds)/temperature
      exp_preds <- exp(preds)
      preds <- exp_preds/sum(exp(preds))
      next_index <- which.max(as.integer(rmultinom(1, 1, preds)))
      character_lookup$character[next_index-1]
    }
    
    in_progress_name <- character(0)
    next_letter <- ""
    
    while(next_letter != "+" && length(in_progress_name) < 30){
      previous_letters_data <- 
        lapply(list(in_progress_name), function(.x){
          character_lookup$character_id[match(.x,character_lookup$character)]
        })
      previous_letters_data <- keras::pad_sequences(previous_letters_data,
                                                    maxlen = max_length)
      previous_letters_data <- keras::to_categorical(previous_letters_data,
                                                     num_classes = 31)
      
      next_letter_probabilities <- 
        predict(model,previous_letters_data)
      
      next_letter <- choose_next_char(next_letter_probabilities,
                                      character_lookup,
                                      temperature)
      
      if(next_letter != "+")
        in_progress_name <- c(in_progress_name,next_letter)
    }
    
    raw_name <- paste0(in_progress_name, collapse="")
    
    capitalized_name <-gsub("\\b(\\w)","\\U\\1",raw_name,perl=TRUE)
    
    capitalized_name
  }
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by
  # output$caption and output$mpgPlot functions
  formulaText <- reactive(
    {
      if (input$n_mob == 1) {
        return(paste("Here is your monster!"))
      } else {
        return(paste("Here are your", input$n_mob, "monsters!"))
      }
      #paste("mpg ~", input$variable)
    }
  )
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText(
    {
      formulaText()
    }
  )
  
  mobText <- reactive(
    {
      mobs <- sapply(1:input$n_mob, function(x) {
          paste('<p>',generate_name(model, character_lookup, max_length),'</p>')
        })
      
      return(paste(mobs))
    }
  )
  
  output$mobs <- renderText(
    {
      mobText()
    }
  )
  
  # # Generate a plot of the requested variable against mpg ----
  # # and only exclude outliers if requested
  # output$mpgPlot <- renderPlot({
  #   boxplot(as.formula(formulaText()),
  #           data = mpgData,
  #           outline = input$outliers,
  #           col = "#75AADB", pch = 19)
  # })
  
}

shinyApp(ui, server)