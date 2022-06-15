run_choice_ui <- function(id, si_label, btn_label, multi) {
  ns <- NS(id)
  
  tagList(
    selectInput(ns('allRuns'),
                label = si_label,
                selected = NULL,
                choices = NULL,
                multiple = multi,
                width = '100%'),
    shinyDirButton(id = ns("directory"), 
                   label = btn_label,
                   icon = icon("fa-solid fa-folder"),
                   title = "Please select a folder"
    ),
    actionButton(ns('clear'),
                 label = 'Clear'),
    actionButton(ns('go'), 
                 label = 'Enter')
  )
 
}

run_choice_server <- function(id, root_dir) {
  moduleServer(id, function(input, output, session) {
    
    volumes <- c(Home = root_dir, "R Installation" = R.home(), getVolumes()())
    shinyDirChoose(input, 'directory', roots = volumes, session = session, restrictions = system.file(package = "base"), allowDirCreate = FALSE)
    
    r <- reactiveValues(runs = c()) # store selected runs
    
    observeEvent(input$directory, {
      if(!is.integer(input$directory)) { # if a file is selected, add path to list
        dir.path <- parseDirPath(volumes, input$directory)
        run.name <- input$directory$path[[length(input$directory$path)]] # run directory is last element of list
        names(dir.path) <- run.name
        r$runs <- c(r$runs, dir.path)
      }
      
      if(length(r$runs > 0)) {
        # if run(s) have been selected, populate widget with the runs
        updateSelectInput(session,
                          'allRuns',
                          selected = r$runs,
                          choices = r$runs)
      }
    })

    observeEvent(input$clear, {
      # clear contents of Run Choice(s) box
      updateSelectInput(session,
                        'allRuns',
                        selected = '',
                        choices = '')
    })
  })
}

