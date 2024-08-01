library(shiny)
library(shinyjs)
library(DT)

fluidPage(
  useShinyjs(),
  titlePanel("Control Dataset Generator"),
  sidebarLayout(
    sidebarPanel(
      fileInput("saintList", "SAINT list.txt (ALL OF THEM)", multiple = TRUE),
      fileInput("msstatsFile", "MSstats results-annotated.txt"),
      fileInput("ttestFile", "T-test.txt"),
      actionButton("generate", "Generate dataset"),
      textOutput("output_text")
    ),
    mainPanel(
      fluidRow(style = 'overflow-x: auto',DT::dataTableOutput("mergedTable")),
      downloadButton("download_mergedTable", "Download Control Raw results Table"),
      downloadButton("download_final_results", "Download Control Dataset Results")
      
    )
  )
)
