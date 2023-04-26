library(shiny)
library(shinyjs)
library(shinyBS)

#--------------------------------------------------

# Define UI for data upload app ----

jscode <- "shinyjs.closeWindow = function() { window.close(); }"

ui <- fluidPage(
  
  # App title ----
  titlePanel("GIMME Visuals Customization"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      #Select input and output directories *NEED TO EDIT THESE TO SELECT DIR INSTEAD OF FILE*
      ## Couldn't figure out how to have users select directory, so doing text file paths for now.
      textInput("input_dir", "Enter the path to the input directory:"),
      bsTooltip("input_dir", "The path to the directory where the data files are located. The input directory must contain only the data files for individuals.", 
                placement = "bottom", trigger = "hover"),
      textInput("out_dir", "Enter the path to the output directory:"),
      bsTooltip("out_dir", "The path to the directory where plots returned by the gimmePlot function will be saved. A new directory, gimmePlots, will be created in the specified directory and the customized plots will be saved there.", 
                placement = "bottom", trigger = "hover"),
      
      # Enter object name
      textInput("object", "Enter the name of the plot object created by the gimmeSEM function containing model parameter estimates:", "fit"),
      helpText(HTML("<em>(Note: gimmeSEM defaults object name to 'fit', unless user specifies otherwise or renames the object)</em>")),
      
      # Insert thematic break and intro color selection
      hr(),
      HTML("<p><h4><center>Select path colors for both summary and individual plots</center></h4></p>"),
      HTML("<h5><strong>Colors selected below can be a hex number or name of a color. See <a href='https://htmlcolorcodes.com/color-picker/' target='_blank'>this link</a> for a hex code finder.</strong></h5>"),
      
      # Select color for group paths
      textInput("group", "Enter color for the group-level paths in the summary plot:", "black"),
      helpText(HTML("<em>(Note: Defaults to black)</em>")),
      
      # Select color for subgroup paths
      textInput("subgroup", "Enter color for the subgroup-level paths in the summary plot:", "green3"),
      helpText(HTML("<em>(Note: Defaults to green3)</em>")),
      
      # Select color for individual paths
      textInput("individual", "Enter color for the individual-level paths in the summary plot:", "gray50"),
      helpText(HTML("<em>(Note: Defaults to gray50)</em>")),
      
      # Select color for positive paths
      textInput("positive", "Enter color for the positively weighted paths in the individual-level plots:", "#FF0000FF"),
      helpText(HTML("<em>(Note: Defaults to #FF0000FF)</em>")),
      
      # Select color for negative paths
      textInput("negative", "Enter color for the negatively weighted paths in the individual-level plots:", "#0000FFFF"),
      helpText(HTML("<em>(Note: Defaults to #0000FFFF)</em>")),
      
      # Add button for user to submit inputs
      actionButton("submit", tags$b("Generate R code")),
      
      
    ),
    
    
    
    # Main panel for displaying outputs ----
    mainPanel(HTML("<h4><strong>The R code generated below can be copied and pasted directly into R to run gimmePlot.</h4></strong>"),
              fluidRow(
                (column(2, actionButton("copy", tags$b("Copy R code to clipboard")))),
                (column(2, offset = 1, actionButton("close", tags$b("Click to close app when done"))))),
              HTML("</br>"),
              fluidRow(
                column(12, (verbatimTextOutput("output", placeholder = TRUE)))),
              useShinyjs(),
              extendShinyjs(text = jscode, functions = c("closeWindow")),
              width = 7)
    
  ),
  
  
)




# Define server logic ----
server <- function(input, output) {
  
  # Define a reactive expression to generate the code based on user input
  code <- reactive({
    paste0("gimmePlot(object = ", input$object, ",\n",
           "          data = \"", input$input_dir, "\",\n",
           "          out = \"", input$out_dir, "\",\n",
           "          group = \"", input$group, "\",\n",
           "          subgroup = \"", input$subgroup, "\",\n",
           "          individual = \"", input$individual, "\",\n",
           "          positive = \"", input$positive, "\",\n",
           "          negative = \"", input$negative, "\")")
  })
  
  
  
  # Render the generated code as text output
  output$output <- renderText({
    if (input$submit > 0) {
      code()
    }
  })
  
  # Copy code to clipboard
  observeEvent(input$copy, {
    # use JavaScript to copy the text to the clipboard
    runjs("navigator.clipboard.writeText($('#output').text());")
  })
  
  # Close app when done
  observeEvent(input$close, {
    js$closeWindow()
    stopApp()
  })
}



# Create Shiny app ----
shinyApp(ui, server)


