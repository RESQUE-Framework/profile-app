library(shiny)
library(shinycssloaders)
library(shinybusy)
library(quarto)
library(jsonlite)
library(RESQUER)
library(lzstring)

ui <- fluidPage(

    titlePanel("RESQUE Profile Builder"),

    add_busy_bar(color = "#FF0000"),

    sidebarLayout(

      sidebarPanel(
        HTML("The RESQUE Profile App creates a visual 'fingerprint' of your personal research style. You first have to enter the necessary data into the <a href='https://resque-framework.github.io/collector-app/'>RESQUE Collector app</a>."),
        HTML("Save your entries as a local .json file (go to 'Save to file ...' on the top left), and upload that .json file here (see below).<br>"),
        fileInput("upload", "Upload your RESQUE json file(s) here:", buttonLabel = "Upload...", multiple = TRUE, accept = ".json"),

        uiOutput("downloadButtons"),  # Dynamic UI for download buttons

        add_busy_spinner(position="top-left", spin="fingerprint", margin=c(10, 350)),
        HTML("<br><span style='font-size: 80%;'><b>Privacy note:</b> While the Collector app for entering the data does not store any data on our servers (everything is stored only locally on your machine), this Profile Builder needs to store a temporary copy of your json file and your resulting profile on a secured server of the Ludwig-Maximilians-Universität München. The files are not permanently stored.</span><br><br>"),
        HTML(paste0("<span style='font-size: 60%;'>Package versions: RESQUER ", packageVersion("RESQUER"), "; OAmetrics ", packageVersion("OAmetrics"),"</span><br><br>"))
      ),

      mainPanel(
        h2("Here is a preview of your data: "),
        p("(visible after uploading your json file)"),
        uiOutput("jsonhead")
      )
    )
)


server <- function(input, output) {

  data <- reactive({
    req(input$upload)

    # Each uploaded file content is one element in the list
    data_list <- lapply(input$upload$datapath, read_json, simplifyVector = TRUE)
    return(data_list)
  })

  # Preview the publications in each json file
  output$jsonhead <- renderUI({
    dat <- data()

    # Create list of UI elements
    tagList(
      h2(paste0("You uploaded ", length(dat), " json files.")),

      # Loop through each dataset
      lapply(seq_along(dat), function(i) {
        current_data <- dat[[i]]
        tagList(
          h3(paste0("Publications of ", current_data[1, "LastName"])),
          renderTable({
            current_data[-1, c("Title", "Year", "DOI")]
          })
        )
      })
    )

  })


  # Generate the UI for all download buttons
  output$downloadButtons <- renderUI({
    btns <- lapply(seq_len(length(data())), function(i) {
      # Each downloadButton will trigger the associated downloadHandler.
      downloadButton(outputId = paste0("downloadBtn", i, "_btn"),
                     label = paste0("Download Report for ", data()[[i]][1, "LastName"], " (~15 seconds)"),
                     icon = shiny::icon("download"))
    })
    do.call(tagList, btns)
  })

  # Create observers for download handlers dynamically
  observe({
    n_files <- length(data())

    lapply(1:n_files, function(i) {
      output[[paste0("downloadBtn", i, "_btn")]] <- downloadHandler(
        filename = paste0("RESQUE_report_", data()[[i]][1, "LastName"], "_", Sys.Date(), "_", format(Sys.time(), "%H_%M_%S"), ".html"),
        content = function(file) {
          Sys.setenv(QUARTO_PATH = "/usr/local/bin/quarto")  # Adjust this path as necessary
          outfile <- render_profile(input$upload$datapath[[i]])
          file.copy(outfile, file)
        }
      )
    })
  })


}

# Run the application
shinyApp(ui = ui, server = server)


# quarto::quarto_render("create_profile.qmd")
