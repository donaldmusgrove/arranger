#' Arranger RTF files using a Shiny UI.
#'
#' \code{arrange_rtf} is used to create a lookup file that places RTF files
#'   in a desired order. The user "uploads" the RTF files, which are copied to
#'   a local temporary directory. The user can then drag/drop rows to order
#'   the RTF files as desired.
#'
#' @param path_save Optional. Desired location of the lookup file. Users
#'   may specify a save path where `order.csv` will be created, with columns
#'   `File Name`, `Table Name`, and `FilePath`. `FilePath` will include a dynamically
#'   created temporary directory. A dataframe `rtf_order` will be created in the
#'   workspace with columns `File Name`, `Table Name`, and `FilePath` regardless of
#'   the `path_save` value.
#'
#' @details Upon execution of `arrange_rtf`, users should click the Selection
#'   button and select the RTF files to be combined. A text appears describing
#'   what dataframe was created, and potentially where a csv file was saved.
#'   Additionally, a table appears with RTF file details. This table can be manipulated
#'   by clicking on the row number on the left and dragging the row to change the order.
#'   This is especially helpful when the table names differ from the file name, or
#'   a specific order is desired.
#'
#'   The `FilePath` column shows where the files now reside, inside a temporary folder.
#'   This temporary folder may not persist once RStudio is closed, so it is advised
#'   to use the row order information within the current R session.
#'
#'   It is possible to click the `Selection` button multiple times, which is
#'   especially useful if files in multiple locations need to be combined.
#'
#' @returns A Shiny app that can be run within RStudio or in a browser. The function
#'   does not return an object, it instead creates a dataframe in
#'   the workspace, and, optionally, a csv file at `path_save`, both of which
#'   will have the same data.
#'
#' @examples
#' \dontrun{
#' # Run with a path input
#' arrange_rtf("/my/save/path")
#'
#' # Run without a path input - same result:
#' arrange_rtf()
#' }
#'
#' @import dplyr
#' @import rhandsontable
#' @import shiny
#' @import stringr
#' @import striprtf
#' @export
arrange_rtf <- function(path_save, copy_local=TRUE){

  # Check path input
  mf <- match.call(expand.dots = FALSE)
  m1 <- any(names(mf) %in% "path_save")
  if(!m1){
    path_save <- NULL
  }

  # Define the Shiny UI
  ui <- basicPage(
    titlePanel("RTF Ordering"),

    fileInput("upload", NULL,
              accept = ".rtf",
              buttonLabel = "RTF Selection",
              multiple = TRUE),
    br(),
    textOutput("path_display"),
    br(),
    # Close app when finished
    actionButton("quit", "Finished"),
    br(),br(),

    # Create a table of file names
    rHandsontableOutput("table_files")
  )



  # Define the Shiny server
  server <- function(input, output, session) {

    #---------------------------------------------------------------------------
    # File information
    #---------------------------------------------------------------------------
    # Get file name and path
    file_info    <- reactive({input$upload})

    #---------------------------------------------------------------------------
    # Filename table display
    #---------------------------------------------------------------------------
    # Create new filepath to path_temp_rtf
    if(copy_local){
      path_temp_rtf <- file.path(tempdir(), sample(1000:9099,1))
      dir.create(path_temp_rtf)
    }

    # Put table/rtf info in a nice format for display
    data_files <- reactive({
      if(length(file_info())==0){
        return(NULL)
      } else{

        # Place uploaded file info into a tibble
        rtf_upload <- tibble(file_info())

        # Copy uploaded files to path_temp_rtf
        if(copy_local){
          FilePathNew <- file.path(path_temp_rtf, rtf_upload$name)
          c1          <- file.copy(rtf_upload$datapath, FilePathNew)

          # In case of multiple RTF selections, get the list of files again
          FilePathReNew <- normalizePath(list.files(path_temp_rtf, full.names=TRUE))
        } else{
          FilePathReNew <- normalizePath(list.files(rtf_upload$datapath, full.names=TRUE))
        }

        # Load RTF files as vectors of strings into a list
        files_rtf <- lapply(FilePathReNew, read_rtf)

        # Format rtf files for display
        rtf_order <- tibble(`File Name`  = basename(FilePathReNew),
                            `Table Name` = unlist(sapply(files_rtf, parse_caption_rtf)),
                            FilePath     = FilePathReNew)

        rtf_order$`Table Name` <- ifelse(is.na(rtf_order$`Table Name`),
                                         rtf_order$`File Name`,
                                         rtf_order$`Table Name`)

        return(rtf_order)
      }
    })

    # Display lookup table path
    output$path_display <- renderText({
      if(length(file_info())>0){
        if(m1 & !is.null(path_save)){
          sprintf("Lookup file order.csv created at %s and in the rtf_order dataframe.",path_save)
        } else{
          "Lookup dataframe rtf_order created."
        }
      }
    })

    # Output table
    output$table_files <- renderRHandsontable({
      if(length(data_files()) == 0){
        return(NULL)
      } else{
        rhandsontable(data_files(),
                      manualRowMove = TRUE)
      }
    })

    #---------------------------------------------------------------------------
    # Output table to csv each time a change is made
    #---------------------------------------------------------------------------
    # Reactive vals
    rvs <- reactiveValues(table_files2 = NULL)

    # Listener -- listen for cell changes and row movements
    listen_table <- reactive({
      list(input$table_files$changes$changes,input$table_files$changes$ind)
    })

    # Save to csv each time a change is made and place in workspace
    observeEvent(listen_table(),{
      rvs$table_files2 <- hot_to_r(input$table_files)

      if(m1 & !is.null(path_save)){
        write.csv(rvs$table_files2,
                  file=file.path(path_save,"order.csv"),
                  row.names = FALSE)
      }

      rtf_order <<- rvs$table_files2
    })

    #---------------------------------------------------------------------------
    # Clean-up
    #---------------------------------------------------------------------------
    # Quit when Finished button is clicked
    observeEvent(input$quit, {
      # Stop the app
      stopApp()
    })

  }

  shinyApp(ui=ui, server=server)
}




#' RTF caption parser
#'
#' \code{parse_caption_rtf} is used to parse out the caption from an RTF file
#'   created by `r2rtf`.
#'
#' @param x character. Should be a multi-vector result of using `striprtf::read_rtf`
#'   to load an RTF file.
#'
#' @details `parse_caption_rtf` tries to parse out the table or figure caption from
#'   an RTF. The function finds the first instance of figure or table in the RTF.
#'   Next, it selects the lines containing figure or table and minimal clean-up is
#'   attempted to present a well-formatted result.
#'
#' @returns A character string.
#'
#' @examples
#' \dontrun{
#' # Create an RTF
#' file <- tempfile(fileext = ".rtf")
#'
#' file1 <- head(iris) %>%
#'   rtf_title(title = "Table 1. My table") %>%
#'   rtf_body() %>%
#'   rtf_encode() %>%
#'   write_rtf(file)
#'
#' x <- striprtf::read_rtf(file)
#'
#' parse_caption_rtf(x)
#' }
#'
#' @export
parse_caption_rtf <- function(x){
  x0 <- x[str_detect(tolower(x), "table|figure")][1]

  # Remove any leading cell
  x1 <- trimws(str_remove(x0, "\\*\\|"))

  # Remove any ending Cell
  x2 <- str_remove(x1, "\n \\|")

  # Remove double spaces
  x3 <- str_squish(x2)

  if(length(x3) == 0){
    x3 <- NA
  }
  x3
}

