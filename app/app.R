library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(psych)
library(lme4)
library(readxl)

# Define UI
ui <- fluidPage(
  titlePanel("BiasFix"), 
  
  # Define tabs
  tabsetPanel(
    
    # Home Tab
    tabPanel("Home",
             fluidRow(
               column(12,
                      img(src = "logo.jpg", height = "400px"),  
                      tags$hr(),
                      h4("Welcome to BiasFix"),
                      p("The BiasFix app standardises scores to adjust for examiner bias. The main focus of our app is to mitigate bias in oral examinations."),
                      tags$hr(),
                      h4("Instructions For Use"),
              
                      
                      
                      # Download instructions as a txt file
                      downloadButton("download_instructions", "Download Instructions"),
                      tags$hr(),
                      
                      # Download sample dataset
                      h4("Sample Dataset"),
                      downloadButton("download_sample", "Sample Dataset"),
                      tags$hr(),
                      
                      # Grouped download buttons for templates
                      h4("Templates"),
                      downloadButton("download_template_csv", "Formatted .csv Template"),
                      downloadButton("download_template_xls", "Formatted .xls Template"),
                      downloadButton("download_template_xlsx", "Formatted .xlsx Template"),
                      tags$hr()
                      
               )
             )
    ),
    
    tabPanel("Standardisation",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Upload CSV/XLS/XLSX File",  
                           accept = c("text/csv", 
                                      "text/comma-separated-values,text/plain", 
                                      ".csv", 
                                      ".xls", 
                                      ".xlsx")),
                 tags$hr(),
                 helpText("Upload a CSV or Excel file containing examination scores with columns: Student_ID, Examiner, and Score."),
                 tags$hr(),
                 numericInput("total_score", "Input Total Score:", value = 100),
                 tags$hr(),
                 textInput("original_boxplot_name", "Filename for Original Boxplot", value = "original_boxplot.png"),
                 downloadButton("downloadOriginalBoxplot", "Download Original Boxplot"),
                 tags$hr(),
                 textInput("retransformed_boxplot_name", "Filename for Retransformed Boxplot", value = "retransformed_boxplot.png"),
                 downloadButton("downloadRetransformedBoxplot", "Download Retransformed Boxplot"),
                 tags$hr(),
                 textInput("summary_stats_name", "Filename for Summary Statistics", value = "summary_statistics.csv"),
                 downloadButton("downloadSummaryStats", "Download Summary Statistics"),
                 tags$hr()
               ),
               mainPanel(
                 plotlyOutput("original_boxplot"),
                 plotlyOutput("retransformed_boxplot"),
                 plotlyOutput("raw_histogram"),  
                 plotlyOutput("standardized_histogram"),  
                 tableOutput("summary_stats"),
                 downloadButton("downloadData", "Download Modified Data"),
                 tags$hr()
               )
             )
    ),
    
    tabPanel("Linear Mixed Effects Model",
             sidebarLayout(
               sidebarPanel(
                 textInput("model_results_name", "Filename for Model Results", value = "model_results.txt"),
                 downloadButton("downloadModelResults", "Download Model Results"),
                 tags$hr(),
                 textInput("random_intercepts_name", "Filename for Random Intercepts", value = "random_intercepts.csv"),
                 downloadButton("downloadRandomIntercepts", "Download Random Intercepts"),
                 tags$hr()
               ),
               mainPanel(
                 h4("Mixed Effects Model Results"),
                 verbatimTextOutput("model_results"),
                 tags$hr(),
                 h4("Random Intercepts for Each Examiner"),
                 tableOutput("random_intercepts")
               )
             )
    ),
    
    tabPanel("About",
             fluidRow(
               column(12,
                      h4("About this Application"),
                      p("Created using Shiny, ggplot2, dplyr, plotly, psych, and lme4 packages."),
                      p("Version: 1.0 beta"),
                      p("Released Date: TBD")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Serve the logo properly
  output$logo <- renderImage({
    list(src = "www/logo.jpg", contentType = 'image/jpeg', width = 200, height = 200)
  }, deleteFile = FALSE)
  
  # Instructions for Use - Opens the instructions in a new window
  observeEvent(input$instructions_btn, {
    showModal(modalDialog(
      title = "Instructions for Use",
      includeText("www/instructions.txt"),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # Download Instructions as a .pdf file
  output$download_instructions <- downloadHandler(
    filename = function() { "instructions.pdf" },
    content = function(file) {
      file.copy("www/instructions.pdf", file, overwrite = TRUE)
    }
  )
  
  # Download formatted .csv template
  output$download_template_csv <- downloadHandler(
    filename = function() { "csv_template.csv" },
    content = function(file) {
      file.copy("www/csv_template.csv", file, overwrite = TRUE)
    }
  )
  
  # Download formatted .xls template
  output$download_template_xls <- downloadHandler(
    filename = function() { "xls_template.xls" },
    content = function(file) {
      file.copy("www/xls_template.xls", file, overwrite = TRUE)
    }
  )
  
  # Download formatted .xlsx template
  output$download_template_xlsx <- downloadHandler(
    filename = function() { "xlsx_template.xlsx" },
    content = function(file) {
      file.copy("www/xlsx_template.xlsx", file, overwrite = TRUE)
    }
  )
  
  # Download sample dataset
  output$download_sample <- downloadHandler(
    filename = function() { "sample_dataset.csv" },
    content = function(file) {
      file.copy("www/sample_dataset.csv", file, overwrite = TRUE)
    }
  )
  
  # Load data with support for csv, xls, xlsx formats
  data <- reactive({
    req(input$file)
    ext <- tools::file_ext(input$file$datapath)
    
    if (ext == "csv") {
      df <- read.csv(input$file$datapath, na.strings = c("", "NA"))
    } else if (ext %in% c("xls", "xlsx")) {
      df <- read_excel(input$file$datapath)
    } else {
      stop("Invalid file format. Please upload a .csv, .xls, or .xlsx file.")
    }
    
    df$Examiner <- as.factor(df$Examiner)
    df$Score <- as.numeric(as.character(df$Score))
    df <- df %>% filter(!is.na(Score))
    
    return(df)
  })
  
  observeEvent(data(), {
    df <- data()
    mean_original <- mean(df$Score, na.rm = TRUE)
    sd_original <- sd(df$Score, na.rm = TRUE)
    
    total_score <- input$total_score  
    
    # Update original boxplot
    output$original_boxplot <- renderPlotly({
      bp <- ggplot(df, aes(x = Examiner, y = Score)) +
        geom_boxplot(outlier.shape = NA) + 
        geom_hline(yintercept = mean_original, linetype = "dashed", color = "blue") +
        labs(title = "Raw Scores by Examiner",
             x = "Examiner",
             y = "Score") +
        theme(axis.text.x = element_text(angle = 90)) +
        ylim(0, input$total_score)
      
      ggplotly(bp)
    })
    
    # Download original boxplot
    output$downloadOriginalBoxplot <- downloadHandler(
      filename = function() { input$original_boxplot_name },
      content = function(file) {
        bp <- ggplot(df, aes(x = Examiner, y = Score)) +
          geom_boxplot(outlier.shape = NA) +  # Make outliers invisible
          geom_hline(yintercept = mean_original, linetype = "dashed", color = "blue") +
          labs(title = "Raw Scores by Examiner",
               x = "Examiner",
               y = "Score") +
          theme(axis.text.x = element_text(angle = 90)) +
          ylim(0, input$total_score)
        
        ggsave(file, bp)
      }
    )
    
    # Update retransformed boxplot and summary statistics
    df <- df %>%
      group_by(Examiner) %>%
      mutate(
        Standardized_Score = scale(Score, center = TRUE, scale = TRUE),
        Retransformed_Score = pmin(Standardized_Score * sd_original + mean_original, total_score)
      )
    
    output$retransformed_boxplot <- renderPlotly({
      bp <- ggplot(df, aes(x = Examiner, y = Retransformed_Score)) +
        geom_boxplot() +
        geom_hline(yintercept = mean_original, linetype = "dashed", color = "blue", 
                   aes(group = 1, linetype = "Mean")) +
        labs(title = "Retransformed Scores by Examiner",
             x = "Examiner",
             y = "Retransformed Score") +
        theme(axis.text.x = element_text(angle = 90)) +
        ylim(0, input$total_score) +  
        guides(linetype = guide_legend(title = "Lines"))  
      
      ggplotly(bp)
    })
    
    # Download retransformed boxplot
    output$downloadRetransformedBoxplot <- downloadHandler(
      filename = function() { input$retransformed_boxplot_name },
      content = function(file) {
        bp <- ggplot(df, aes(x = Examiner, y = Retransformed_Score)) +
          geom_boxplot(outlier.shape = NA) +  # Make outliers invisible
          geom_hline(yintercept = mean_original, linetype = "dashed", color = "blue") +
          labs(title = "Retransformed Scores by Examiner",
               x = "Examiner",
               y = "Retransformed Score") +
          theme(axis.text.x = element_text(angle = 90)) +
          ylim(0, input$total_score)
        
        ggsave(file, bp)
      }
    )
    
    # Update histogram of raw scores
    output$raw_histogram <- renderPlotly({
      hist <- ggplot(df, aes(x = Score)) +
        geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
        labs(title = "Frequency Histogram of Raw Scores",
             x = "Raw Score",
             y = "Frequency")
      
      ggplotly(hist)
    })
    
    # Update histogram of retransformed scores
    output$standardized_histogram <- renderPlotly({
      hist <- ggplot(df, aes(x = Retransformed_Score)) +
        geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
        labs(title = "Frequency Histogram of Retransformed Scores",
             x = "Retransformed Score",
             y = "Frequency")
      
      ggplotly(hist)
    })
    
    # Summary statistics
    summary_stats <- describe(df$Retransformed_Score) 
    
    output$summary_stats <- renderTable({ summary_stats })
    
    output$downloadSummaryStats <- downloadHandler(
      filename = function() { input$summary_stats_name },
      content = function(file) { write.csv(summary_stats, file, row.names = FALSE) }
    )
    
    # Linear mixed effects model
    model <- lmer(Score ~ 1 + (1 | Examiner), data = df)
    model_summary <- summary(model)
    
    output$model_results <- renderPrint({ model_summary })
    
    output$downloadModelResults <- downloadHandler(
      filename = function() { input$model_results_name },
      content = function(file) { write.table(capture.output(model_summary), file) }
    )
    
    # Random intercepts
    random_intercepts <- as.data.frame(ranef(model)$Examiner)
    colnames(random_intercepts) <- c("Random_Intercept")
    random_intercepts$Examiner <- rownames(random_intercepts)
    rownames(random_intercepts) <- NULL
    
    output$random_intercepts <- renderTable({ random_intercepts })
    
    output$downloadRandomIntercepts <- downloadHandler(
      filename = function() { input$random_intercepts_name },
      content = function(file) { write.csv(random_intercepts, file, row.names = FALSE) }
    )
    
    # Download modified data
    output$downloadData <- downloadHandler(
      filename = function() { "modified_data.csv" },
      content = function(file) { write.csv(df, file, row.names = FALSE) }
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
