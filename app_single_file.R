library(shiny)
library(shinycssloaders)
library(shinybusy)
library(quarto)
library(jsonlite)
library(RESQUER)

ui <- fluidPage(

  titlePanel("RESQUE Profile Builder"),

  add_busy_bar(color = "#FF0000"),

  sidebarLayout(

    sidebarPanel(
      HTML("The RESQUE Profile App creates a visual 'fingerprint' of your personal research style. You first have to enter the necessary data into the <a href='https://resque-framework.github.io/collector-app/'>RESQUE Collector app</a>."),
      HTML("Save your entries as a local .json file (go to 'Save to file ...' on the top left), and upload that .json file here (see below).<br>"),
      fileInput("upload", "Upload your RESQUE json file here:", buttonLabel = "Upload...", multiple = FALSE, accept = ".json"),

      uiOutput("download_button"),

      add_busy_spinner(position="top-left", spin="fingerprint", margin=c(10, 350)),
      HTML("<br><span style='font-size: 80%;'><b>Privacy note:</b> While the Collector app for entering the data does not store any data on our servers (everything is stored only locally on your machine), this Profile Builder needs to store a temporary copy of your json file and your resulting profile on a secured server of the Ludwig-Maximilians-Universität München. The files are not permanently stored.</span><br><br>"),
      HTML(paste0("<span style='font-size: 60%;'>Package versions: RESQUER ", packageVersion("RESQUER"), "; OAmetrics ", packageVersion("OAmetrics"),"</span><br><br>"))
    ),

    mainPanel(
      h2("Here is a preview of your data: "),
      p("(visible after uploading your json file)"),
      tableOutput("jsonhead")
    )
  )
)


server <- function(input, output) {

  data <- reactive({
    req(input$upload)

    return(read_json(input$upload$datapath, simplifyVector = TRUE))
  })


  output$jsonhead <- renderTable({
    print(str(data()))
    data()[-1, ][, c("Title", "Year", "DOI")]
  })

  output$download_button <- renderUI({
    req(input$upload)
    downloadButton(outputId = "report", label = "Generate and Download Report (~15 seconds)", icon = shiny::icon("download")) })

  output$report <- downloadHandler(
    filename = paste0("RESQUE_report_", data()[1, "LastName"], "_", Sys.Date(), "_", format(Sys.time(), "%H_%M_%S"), ".html"),
    content = function(file) {
      Sys.setenv(QUARTO_PATH = "/usr/local/bin/quarto")  # Adjust this path as necessary
      outfile <- render_profile(input$upload$datapath)
      file.copy(outfile, file)
    }
  )

}

# Run the application
shinyApp(ui = ui, server = server)


# quarto::quarto_render("create_profile.qmd")
