library(shiny)
library(shinycssloaders)
library(shinybusy)
library(quarto)
library(jsonlite)
library(RESQUER)
library(lzstring)
library(semver)

app_version <- "1.10.0"
min_core_pub_version <- "0.9.0"

ui <- fluidPage(

    titlePanel("RESQUE Profile Builder"),

    add_busy_bar(color = "#FF0000"),

    sidebarLayout(

      sidebarPanel(
        add_busy_spinner(position="top-left", spin="fingerprint", margin=c(10, 350)),
        HTML("The RESQUE Profile App creates a visual 'fingerprint' of your personal research style. You first have to enter the necessary data into the <a href='https://resque-framework.github.io/collector-app/'>RESQUE Collector app</a>."),
        HTML("Save your entries as a local .json file (go to 'Save to file ...' on the top left), and upload that .json file here (see below).<br><br>"),
        fileInput("upload", "Upload your RESQUE json file(s) here (you can upload multiple files):", buttonLabel = "Upload...", multiple = TRUE, accept = ".json"),

        HTML("<i>One or more download buttons are visible after uploading your json file(s). You can click all the download buttons in a row, and the reports are generated in the background (you don't have to wait for them to finish one by one).</i>"),

        HTML("<hr style='border: 2px solid #555; margin: 20px 0;'>"),

        h4("Options for individual reports"),
        checkboxInput("show_inter", "Compute Internationalization and Interdisciplinarity indexes (beta)", value = TRUE),
        uiOutput("downloadButtons"),  # Dynamic UI for download buttons for single profiles

        HTML("<hr style='border: 2px solid #555; margin: 20px 0;'>"),
        h4("Options for overview report"),
        checkboxInput("anonymous", "Anonymize the output of the overview sheet", value = FALSE),
        uiOutput("downloadButtonOverview"),  # Dynamic UI for the overview download buttons

        HTML("<hr style='border: 2px solid #555; margin: 20px 0;'>"),
        HTML("<span style='font-size: 80%;'><b>Privacy note:</b> While the Collector app for entering the data does not store any data on our servers (everything is stored only locally on your machine), this Profile Builder needs to store a temporary copy of your json file and your resulting profile on a secured server of the Ludwig-Maximilians-Universität München. The files are not permanently stored.</span><br>"),
        HTML(paste0("<span style='font-size: 60%;'>Package versions: RESQUER ", packageVersion("RESQUER"), "; OAmetrics ", packageVersion("OAmetrics"),". Profile Builder App ", app_version, "</span>"))
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

  version_too_low_r <- reactive({
    dat <- data()
    # Check all uploaded files for version compatibility
    any(sapply(dat, function(d) {
      current_core_pub_version <- d$forms$pub$versions[1, 1]
      parse_version(current_core_pub_version) < min_core_pub_version
    }))
  })

  # Preview the publications in each json file
  output$jsonhead <- renderUI({
    dat <- data()

    # compare the uploaded version of core-pubs to the minimal version
    all_versions <- sapply(dat, function(d) d$forms$pub$versions[1, 1])
    min_current_core_pub_version <- all_versions |> parse_version() |> min() |> as.character()
    version_too_low <- version_too_low_r()

    # Get the collector URL from the json file, or use default
    collector_url <- if (!is.null(dat[[1]]$CollectorURL[1])) {
      dat[[1]]$CollectorURL[1]
    } else {
      "https://resque-framework.github.io/collector-app/"
    }

    if (version_too_low) {
      tag_warning <- div(
        class = "alert alert-danger",
        role = "alert",
        HTML(paste0("<strong>Version Error:</strong> At least one of the uploaded json files has core-pubs version ", min_current_core_pub_version, ". It needs to have at least version ", min_core_pub_version, ". Please go to the <a href='", collector_url, "'>collector app</a>, import the json file, and export it again to update the version. No reports can be generated until the version is updated."))
      )
    } else {
      # tag_warning <- div(
      #   class = "alert alert-success",
      #   role = "alert",
      #   HTML("<strong>Version OK:</strong> The uploaded file version is compatible.")
      # )
      tag_warning <- NULL
    }

    # Create list of UI elements
    tagList(

      # show a warning if the version does not match
      tag_warning,

      HTML(paste0("<p><b>You uploaded ", length(dat), " json files.</b></p>")),

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


  # Generate the UI for all individual download buttons
  output$downloadButtons <- renderUI({
    version_too_low <- version_too_low_r()
    btns <- lapply(seq_len(length(data())), function(i) {
      btn <- downloadButton(outputId = paste0("downloadBtn", i, "_btn"),
                     label = paste0("Download Report for ", data()[[i]][1, "LastName"], " (~25 seconds)"),
                     icon = shiny::icon("download"),
                     class = "btn-success")
      if (version_too_low) {
        btn <- tagAppendAttributes(
          btn,
          disabled = "disabled",
          style = "background-color: #dfdfe0; border-color: #6c757d; color: #ffffff; opacity: 1; cursor: not-allowed;"
        )
      }
      btn
    })
    do.call(tagList, btns)
  })


  # Generate the UI for the overview download button
  output$downloadButtonOverview <- renderUI({
    version_too_low <- version_too_low_r()
    btn <- downloadButton(outputId = "overview_btn",
                   label = paste0("Download Overview for ", length(data()), " profiles (~", length(data())*13, " seconds)"),
                   icon = shiny::icon("download"),
                   class = "btn-success")
    if (version_too_low) {
      btn <- tagAppendAttributes(
        btn,
        disabled = "disabled",
        style = "background-color: #dfdfe0; border-color: #6c757d; color: #ffffff; opacity: 1; cursor: not-allowed;"
      )
    }
    btn
  })


  # Create observers for download handlers dynamically
  observe({
    n_files <- length(data())

    lapply(1:n_files, function(i) {
      output[[paste0("downloadBtn", i, "_btn")]] <- downloadHandler(
        filename = paste0("RESQUE_report_", data()[[i]][1, "LastName"], "_", Sys.Date(), "_", format(Sys.time(), "%H_%M_%S"), ".html"),
        content = function(file) {
          Sys.setenv(QUARTO_PATH = "/usr/local/bin/quarto")  # Adjust this path as necessary
          outfile <- render_profile(
            input$upload$datapath[[i]],
            show_inter = input$show_inter
          )
          file.copy(outfile, file)
        }
      )
    })
  })


  # Server-side handler for the overview download button
  output$overview_btn <- downloadHandler(
    filename = function() {
      paste0("Overview_", Sys.Date(), "_", format(Sys.time(), "%H_%M_%S"), ".html")
    },
    content = function(file) {

      Sys.setenv(QUARTO_PATH = "/usr/local/bin/quarto")  # Adjust this path as necessary
      outfile <- render_overview(
        json_folder= input$upload$datapath[[1]] |> normalizePath() |> dirname(),
        anonymous = input$anonymous,
        clear_cache = TRUE
      )
      file.copy(outfile, file)
    }
  )


}

# Run the application
shinyApp(ui = ui, server = server)


# quarto::quarto_render("create_profile.qmd")

# Testing:

# dat <- lapply('/Users/felix/LMU/DGPs Kommission Open Science/RESQUE/resque_schönbrodt_URL.json', read_json, simplifyVector = TRUE)

