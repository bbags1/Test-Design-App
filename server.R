#library(skpr)
library(aws.s3)
library(readr)
library(readxl)
library(writexl)
library(jsonlite)
library(dplyr)
library(data.table)
library(ggplot2)
library(pwr)
library(tidyverse)
library(hash)
library(shiny)
library(openxlsx)
library(reticulate)
library(DT)
library(shinydashboard)
library(shinyBS)
library(corrplot)
library(httpuv)
library(Rcpp)
library(shinyjs)
library(bcrypt)
library(httr)




server <- function(input, output, session) {
    
  
  #set authentication here for storage
  
  
  
    #empty folders during processing
    
    #################################################################
    folder_paths <- c("fltables/", "fltablesfull/")
    
  for (x in folder_paths) {
    files_to_remove <- dir(x, full.names = TRUE)
    
    # Remove each file in the folder except "blank.txt"
    lapply(files_to_remove, function(file_path) {
      if (basename(file_path) != "blank.txt") {
        file.remove(file_path)
      }
    })
  }
  
  #remove temp tdw file from local directory
  file.remove("tmptdw.xlsx")
    #################################################################
  
    detachments <<- c("Det 2", "Det 3", "Det 5", "Det 6", "No Det")
    program_names <<- list()
    
    for (x in detachments){
      program_names[[x]] <- get_folder_names(bucket, paste0(folder, x, "/"))
    }
  
    
    #display all programs that are stored within S3. To add more detachments/buckets of data, simple copy and paste starting from the tabPanel and ending at the 4th parenthesis after "Load Program Data".
  output[["load_data"]] <- renderUI({
    tabsetPanel(
      tabPanel(title = "Det 2",
               fluidRow(selectizeInput(inputId = "programnames_det2", h3("Det 2 Existing Programs"), choices = program_names[["Det 2"]], multiple = FALSE, size = 10, width = 600)),
               fluidRow(actionButton(inputId = "load_det2", label = strong("Load Program Data")))),
      tabPanel(title = "Det 3",
               fluidRow(selectizeInput(inputId = "programnames_det3", h3("Det 3 Existing Programs"), choices = program_names[["Det 3"]], multiple = FALSE, size = 10, width = 600)),
               fluidRow(actionButton(inputId = "load_det3", label = strong("Load Program Data")))),
      tabPanel(title = "Det 5",
               fluidRow(selectizeInput(inputId = "programnames_det5", h3("Det 5 Existing Programs"), choices = program_names[["Det 5"]], multiple = FALSE, size = 10, width = 600)),
               fluidRow(actionButton(inputId = "load_det5", label = strong("Load Program Data")))),
      tabPanel(title = "Det 6",
               fluidRow(selectizeInput(inputId = "programnames_det6", h3("Det 6 Existing Programs"), choices = program_names[["Det 6"]], multiple = FALSE, size = 10, width = 600)),
               fluidRow(actionButton(inputId = "load_det6", label = strong("Load Program Data")))),
      tabPanel(title = "No Det",
               fluidRow(selectizeInput(inputId = "programnames_nodet", h3("No Det Assignened"), choices = program_names[["No Det"]], multiple = FALSE, size = 10, width = 600)),
               fluidRow(actionButton(inputId = "load_nodet", label = strong("Load Program Data"))))
    )
    })
    
  
 
  
  
  observeEvent(input[['module']], {
    module <- input[['module']]
    
    observeEvent(input[['start']], {
      if (module == "tdw_template") {
        # Build menu item for uploading factors and levels
        output$menu_module <- renderMenu({
          menuItem("Test Design", tabName = "testdesign", icon = icon("dashboard"),
                   menuSubItem("Document Center", tabName = "documentcenter", icon = icon("dashboard")),
                   menuSubItem("Build Your Test Design", "doe", icon = icon("dashboard"))
          )
        })
        
        # Display upload buttons on the upload page
        output$fileupload <- renderUI({
          fluidPage(
            fluidRow(
              box(
                fluidRow(
                  fileInput("TDW", "FL Worksheet Upload", buttonLabel = "Upload an xlsx file...", accept = c(".xlsx"))),
                  fluidRow(textInput("programname", label = "Program Name", width = 400)
                ),
                fluidRow(passwordInput("program_password", label = "Enter a Password for the Program. Store this Password Somewhere Secure for Future Use.")),
                fluidRow(passwordInput("second_password", label = "Please Re-enter your Password")),
                fluidRow(passwordInput("resetphrase", label = "Please Choose a Password Reset Phrase")),
                fluidRow(selectInput(inputId = "det", label = "Select Detachment", choices = c("Det 2", "Det 3", "Det 5", "Det 6"))),
                fluidRow(
                  tabPanel(
                    "Submit",
                    value = "end",
                    p("Click here to submit your document"),
                    hr(),
                    actionButton("submit", strong("Submit")),
                    br(),
                    hr()
                  )
                )
              )
            )
          )
        })
        
        
            observeEvent(input[["submit"]], {
              if (input[["programname"]] %in% program_names) {
                showModal(modalDialog(
                  title = "Program Already Created",
                  "The program that you have chosen to create has already been created. Choosing this program name will force all data previously created to be overwritten!"
                ))
                NULL
              }
              
              else if (any(sapply(list(input$TDW, input$programname, input$program_password, input$det), is.null))){
                showModal(
                  modalDialog(
                    title = "Error",
                    "Please fill in all the fields.",
                    easyClose = FALSE, footer = modalButton("Dismiss")
                  )
                )
              }
              
              else if (input$program_password != input$second_password){
                showModal(
                  modalDialog(
                    title = "Error",
                    "Please make sure that your passwords match",
                    easyClose = FALSE, footer = modalButton("Dismiss")
                  )
                )
              }
              else {
                password = bcrypt::hashpw(input$program_password)
                aws.s3::s3write_using(password, FUN = write_lines, object = paste0(folder, input$det, "/", input$programname,"/program_data/password.txt"),    
                                    bucket = bucket
                )
                recovery_key = bcrypt::hashpw(input$resetphrase)
                
                aws.s3::s3write_using(recovery_key, FUN = write_lines, object = paste0(folder, input$det, "/", input$programname,"/program_data/resetphrase.txt"),    
                                      bucket = bucket
                )
                showModal(
                  modalDialog(
                    title = "Success!",
                    "Please wait a few seconds while your TDW is parsed and the environment is set-up, Proceed to the Test Design tab to begin creating designs.",
                    easyClose = FALSE, footer = modalButton("Dismiss")
                  )
                )
                
              writeLines(paste0(folder, input$det, "/", input$programname, "/", input$TDW$name[1]), "currentWD.txt")
                
              aws.s3::put_object(
                file = input$TDW$datapath[1],
                object = paste0(folder, input$det, "/", input$programname, "/", input$TDW$name[1]),
                bucket = bucket
              )
              
              file.copy(from = input$TDW$datapath[1], to = "temptdw.xlsx")
              
              
            
              
              
              
              aws.s3::s3write_using(
                module,
                FUN = writeLines,
                object = paste0(folder, input$det, "/",input$programname, "/session_data/session_type/", "session_type.txt"),
                bucket = bucket
              )
              
              system("mkdir -p /tmp/shiny-server/python")
              system("pip install sys io json os openpyxl pandas numpy boto3 requests -t /tmp/shiny-server/python")
              use_python("/opt/app-root/bin/python")
              py_run_file("FL_table_generator.py")
              filenames <- list.files(path = "fltables/", full.names = TRUE)
              filenames <- remove_string_from_list(filenames, "blank.txt")
              test <<- inputFLs(filenames = filenames)
              other_factors_filenames <- list.files(path = "fltablesfull/", full.names = TRUE)
              other_factors_filenames <- remove_string_from_list(other_factors_filenames, "blank.txt")
              other_factors <<- import_other_factors(other_factors_filenames)
              
             
              
          
             
              
              params <<- hash()
              reactivegendatamaster = reactiveValues()
              reactivedata <- reactiveValues()
              
             
              for (x in 1:length(names(other_factors))){
                for (y in 1:length(names(other_factors[[paste0("COI ", x)]]))){
                  aws.s3::s3write_using(as.character(length(names(other_factors))), FUN = writeLines,  object = paste0(folder,input$det, "/",input$programname,"/session_data/TEMs/COIs/", "total_COIs.txt"),    
                                        bucket = bucket)
                  aws.s3::s3write_using(as.character(length(names(other_factors[[paste0("COI ", x)]]))), FUN = writeLines,  object = paste0(folder,input$det, "/",input$programname,"/session_data/TEMs/ops/", x, "ops.txt"),    
                                        bucket = bucket)
                  aws.s3::s3write_using(other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]], FUN = write.csv,  object = paste0(folder,input$det, "/",input$programname,"/session_data/TEMs/otherfactors/", x, y, "otherfactors.csv"),    
                                        bucket = bucket)
                  
                }
              }
            
            
              
              for (x in length(names(test)):length(names(other_factors))){
                for (y in length(names(test[[paste0("COI ", x)]])):length(names(other_factors[[paste0("COI ", x)]]))){
                  tryCatch(
                    expr = {
                      problemID_df <- create_demo_and_log_cols(other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]])
                  output[[paste0('problemid', x, y)]] <- DT::renderDT({datatable(problemID_df, editable = FALSE, selection = list(mode = 'multiple', target = 'row'))})
                  aws.s3::s3write_using(problemID_df, FUN = write.csv,  object = paste0(folder,input$det, "/",input$programname,"/session_data/TEMs/problemid/", x, y, "problem_id.csv"),    
                                        bucket = bucket)
                  
                  
                  },
                  error = function(e){})
                }
              }
              
              
   
              
              
              #Create named data tables for each COI and operation. This is important because it allows row selection to be called in order to delete rows        
              lapply(1:length(test), function(x){                                          
                lapply(1:length(test[[paste0("COI ", x)]]), function(y){
                  num_cois = length(test)
                  num_ops = length(test[[paste0("COI ", x)]])
                  name_dt = paste0("coi", x,  "operation", y, "dt")
                  params[[paste0('COI ', x)]][[paste0('Operation ', y)]] <- modelparams(test[[paste0('COI ', x)]][[paste0('Operation ', y)]])
                  reactivedata[[name_dt]] <- test[[paste0('COI ', x)]][[paste0('Operation ', y)]]
                  output[[name_dt]] <- DT::renderDT({datatable(reactivedata[[name_dt]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                  
                  #write number of COIs to S3
                  aws.s3::s3write_using(as.character(num_cois), FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/cois/", "COIs", ".txt"),    
                                        bucket = bucket
                  )
                  #write number of Operations to S3
                  
                  aws.s3::s3write_using(as.character(num_ops), FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/ops/",x, "ops", ".txt"),    
                                        bucket = bucket
                  )
                  
                  #write default to s3 for saving progress
                  aws.s3::s3write_using(reactivedata[[name_dt]], FUN = write.csv,
                                        object = paste0(folder,input$det, "/",input$programname,"/session_data/default_designs/", x,y,"default", ".csv"),    
                                        bucket = bucket
                  )
                  
                  #Delete Button
                  
                  observeEvent(input[[paste0("delete_row",x,y)]],{
                    name_rows_selected = noquote(paste0("coi",x,"operation",y,"dt","_rows_selected"))
                    name_dt = paste0("coi", x,  "operation", y, "dt")
                    reactivedata[[name_dt]] <- reactivedata[[name_dt]][-as.numeric(input[[name_rows_selected]]),]
                    output[[name_dt]] <- DT::renderDT({datatable(reactivedata[[name_dt]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                    
                    
                
                    
                  })
                  
                  #reset Button
                  
                  observeEvent(input[[paste0("reset",x,y)]],{
                    name_dt = paste0("coi", x,  "operation", y, "dt")
                    reactivedata[[name_dt]] <- test[[paste0("COI ", x)]][[paste0("Operation ", y)]]
                    output[[name_dt]] <- DT::renderDT({datatable(reactivedata[[name_dt]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                    
                  
                    
                    
                  })
                  
                  #generate Design
                  
                  observeEvent(input[[paste0("gendesign",x,y)]],{
                    name_dt = paste0("coi", x,  "operation", y, "dt")
                    name_generated_design = paste0("coi", x,  "operation", y, "dt", "generated")
                    trials = as.numeric(input[[paste0("runs",x,y)]])
                    optimality = input[[paste0("criteria", x, y)]]
                    tryCatch(
                      expr = {
                        form = paste0("~", paste(input[[paste0('interactions',x,y)]], collapse = "+"))
                        text_formula = paste(input[[paste0('interactions',x,y)]], collapse = "+")
                        reactivegendatamaster[[name_generated_design]] <- 
                          gen_design(candidateset = reactivedata[[name_dt]],
                                     model = formula(form), trials = trials, 
                                     optimality = optimality)
                        output[[name_generated_design]] <- DT::renderDT({datatable(reactivegendatamaster[[name_generated_design]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                        
                        output[[paste0("powerplot",x,y)]] <- renderPlot({
                          PowerCurveGenerator(reactivegendatamaster[[name_generated_design]],model_input = formula(form), name_of_plot = paste0(input[["programname"]], " COI ", x, " Operation ", y))
                          
                        
                        })
                        
                        output[[paste0("corplot",x,y)]] <- renderPlot({
                          plot_correlations(reactivegendatamaster[[name_generated_design]], customcolors = c("blue", "red", "yellow", "green"))})
                        
                        if (input[[paste0('checkpowerbysample',x, y)]] == TRUE) {
                          
                         
                          
                        output[[paste0("powerbysample",x,y)]] <- renderPlot({
                          power_by_sample(candidate_set = reactivedata[[name_dt]], 
                                          min_sample_size = as.numeric(input[[paste0("minruns", x, y)]]), 
                                          max_sample_size = as.numeric(input[[paste0("maxruns", x, y)]]), 
                                          targetSNR = as.numeric(input[[paste0("targetsnr", x, y)]]), 
                                          parameters = formula(form), step = input[[paste0('stepsize',x,y)]], nameplot = paste0(input[["programname"]], " COI ", x, " Operation ", y))
                        })
                        
                        min_size = as.character(input[[paste0("minruns", x, y)]])
                        max_size = as.character(input[[paste0("maxruns", x, y)]])
                        snr = as.character(input[[paste0("targetsnr", x, y)]])
                        step_size = as.character(input[[paste0('stepsize',x,y)]])
                        
                        aws.s3::s3write_using(min_size , FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/powerbysample/COI",x,"/Operation", y, "/min_sample.txt"),    
                                              bucket = bucket)
                        aws.s3::s3write_using(max_size , FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/powerbysample/COI",x,"/Operation", y, "/max_sample.txt"),    
                                              bucket = bucket)
                        aws.s3::s3write_using(snr , FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/powerbysample/COI",x,"/Operation", y, "/snr.txt"),    
                                              bucket = bucket)
                        aws.s3::s3write_using(step_size , FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/powerbysample/COI",x,"/Operation", y, "/step.txt"),    
                                              bucket = bucket)
                        
                        }
                        else {
                          NULL
                        }
                        
                        output[[paste0("error", x, y)]] <- renderText({"Success! Proceed to the Design Evaluation tab to evaluate the design."})
                        
                        #write formulas to S3 
                        aws.s3::s3write_using(text_formula, FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/formulas/",x, y, "formula", ".txt"),    
                                              bucket = bucket)
                        
                        #write generated design to s3 for saving progress
                        aws.s3::s3write_using(reactivegendatamaster[[name_generated_design]], FUN = write.csv,
                                              object = paste0(folder,input$det, "/",input$programname,"/session_data/generated_designs/", x,y,"generated", ".csv"),    
                                              bucket = bucket
                        )
                        
                        if(is.null(input[[paste0("notes", x, y)]])) {
                          NULL
                        }
                        else {
                          output[[paste0("design_notes", x, y)]] <- renderText({
                            input[[paste0("notes", x, y)]]
                          })
                        }
                      },
                      error = function(e){
                        output[[paste0("error", x, y)]] <- renderText({ 
                          "Error in generating Design. This error could be a result of a number of factors. To fix this error, please make sure that there are no interaction terms for dissallowed combinations. Next, try increasing the number of runs for your test." 
                          
                          
                        })
                      }, 
                      
                      warning = function(w){
                        output[[paste0("warning", x, y)]] <- renderText({ 
                          message(w)
                        })
                      },
                      finally = {}
                      
                    )
  #################################################################### Generate TEM observer ############################################################################                  
                    
                    
                    observeEvent(input[[paste0('generatetem', x, y)]], {
                      name_generated_design = paste0("coi", x,  "operation", y, "dt", "generated")
                      if (!is.null(reactivegendatamaster[[name_generated_design]]) & 
                          !is.null(other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]])) {
                        
                        dfs <- generate_dataframes(as.data.frame(reactivegendatamaster[[name_generated_design]]), other_factors = other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]])
                        rownames(dfs$characterize_df) <- 1:nrow(dfs$characterize_df)
                        
                        output[[paste0("characterize", x, y)]] <- DT::renderDT({datatable(dfs$characterize_df, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Characterize")})
                        output[[paste0('problemid', x, y)]] <- DT::renderDT({datatable(dfs$problemID_df, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Problem ID")})
                        
                        aws.s3::s3write_using(dfs$problemID_df, FUN = write.csv,  object = paste0(folder,input$det, "/",input$programname,"/session_data/TEMs/problemid/", x, y, "problem_id.csv"),    
                                              bucket = bucket)
                        
                        aws.s3::s3write_using(dfs$characterize_df, FUN = write.csv,  object = paste0(folder,input$det, "/",input$programname,"/session_data/TEMs/characterize/", x, y, "characterize.csv"),    
                                              bucket = bucket)
                        
                        
                      } else if(!is.null(reactivegendatamaster[[name_generated_design]])) {
                        characterize_df <- count_duplicates(reactivegendatamaster[[name_generated_design]])
                        output[[paste0("characterize", x, y)]] <- DT::renderDT({datatable(characterize_df, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Characterize")})
                        
                        aws.s3::s3write_using(characterize_df, FUN = write.csv,  object = paste0(folder,input$det, "/",input$programname,"/session_data/TEMs/characterize/", x, y, "characterize.csv"),    
                                              bucket = bucket)
                        
                      } else if (!is.null(other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]])) {
                        problemID_df <- create_demo_and_log_cols(other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]])
                        output[[paste0('problemid', x, y)]] <- DT::renderDT({datatable(problemID_df, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Problem ID")})
                       
                         aws.s3::s3write_using(problemID_df, FUN = write.csv,  object = paste0(folder,input$det, "/",input$programname,"/session_data/TEMs/problemid/", x, y, "problem_id.csv"),    
                                              bucket = bucket)
                        
                        }
                      
                    })
                    
                    
                    output$generated_designs <- renderUI({                       
                      do.call(
                        tabsetPanel, c(id = 't', lapply(1:length(names(test)), function(x){
                          tabPanel(paste0('COI ', title = x), 
                                   do.call(
                                     tabsetPanel, c(id = 'ops',lapply(1:length(names(test[[paste0("COI ", x)]])), function(y){
                                       tabPanel(paste0('Operation ', title = y), 
                                                fluidRow(
                                                  column(width = 6, offset = 0,
                                                         DTOutput(paste0("coi",x,"operation",y,"dt", "generated"))),      
                                                  column(width = 6, offset = 0, 
                                                         tabsetPanel(id = 'plots', 
                                                                     tabPanel(title = "Power Curves", plotOutput(paste0("powerplot",x,y))),
                                                                     tabPanel(title = "Correlation Plot", plotOutput(paste0("corplot",x,y))),
                                                                     tabPanel(title = "Power by Sample Size", plotOutput(paste0("powerbysample",x,y)))),
                                                         fluidRow(textOutput(outputId = paste0("design_notes", x, y))))
                                                  
                                                  
                                                  
                                                ),
                                                fluidRow(actionButton(inputId = paste0("generatetem", x, y), label = "Generate Test Event Matrix"))
                                                
                                       )}))))})))})
                    
                    output$testeventmatrix <- renderUI({                       
                      do.call(
                        tabsetPanel, c(id = 'b', lapply(1:length(names(other_factors)), function(x){
                          tabPanel(paste0('COI ', title = x), 
                                   do.call(
                                     tabsetPanel, c(id = 'opsb',lapply(1:length(names(other_factors[[paste0("COI ", x)]])), function(y){
                                       tabPanel(paste0('Operation ', title = y), 
                                                fluidRow(actionButton(inputId = paste0("generatetem", x, y), label = "Generate Test Event Matrix")),
                                                fluidRow(
                                                  column(width = 6, offset = 0,
                                                         DTOutput(paste0("characterize", x, y)), 
                                                         DTOutput(paste0("problemid", x, y))) 
                                                        
                                                  
                                                  
                                                  
                                                  
                                                )
                                                
                                       )}))))})))})
                    
                  })
                  
                })})
              
              
              #Creates tabs for set of COIs and their operations, displays the full factorial design candidate set, also creates action buttons within the design page in order select the type of design you wish to create.       
              output$doetabsfordesign <- renderUI({                       
                do.call(
                  tabsetPanel, c(id = 't', lapply(1:length(names(test)), function(x){
                    tabPanel(paste0('COI ', title = x), 
                             do.call(
                               tabsetPanel, c(id = 'ops',lapply(1:length(names(test[[paste0("COI ", x)]])), function(y){
                                 #params = modelparams(test[[paste0('COI ',x)]][[paste0('Operation ', y)]])
                                 tabPanel(paste0('Operation ', title = y), 
                                          fluidRow(
                                            column(width = 6, offset = 0, 
                                                   actionButton(paste0("delete_row", x, y), "Delete Row", icon("edit")), 
                                                   actionButton(paste0("reset",x,y), "Reset Table", icon("refresh")), 
                                                   DTOutput(paste0("coi",x,"operation",y,"dt"))),      #make all of the action buttons tabs that can be open to create the design for better UI experience.
                                            column(width = 6, offset = 0, 
                                                   tabsetPanel(id = paste0(x, y), 
                                                               tabPanel(paste0("Create an Optimal Design"),
                                                                        column(width = 6, offset = 0, 
                                                                               fluidRow(numericInput(inputId = paste0("runs", x, y), 
                                                                                                     label = "Runs", value =  1, width = 200), 
                                                                                        bsTooltip(id = paste0("runs", x, y), 
                                                                                                  title = "Select the number of runs that you would like to include in your design. This number cannot be zero",
                                                                                                  placement = "left", trigger = "hover")), 
                                                                               fluidRow(radioButtons(inputId = paste0("criteria", x, y), 
                                                                                                     label = "Optimality Criterion", 
                                                                                                     choiceNames = c("D-Optimal (Recommended)", "I-Optimal", "A-Optimal", "ALIAS", "G-Optimal", "T-Optimal", "E-Optimal"), 
                                                                                                     choiceValues = c("D", "I", "A", "ALIAS", "G", "T", "E"))),
                                                                               fluidRow(selectInput(inputId = paste0("interactions",x,y), 
                                                                                                    label = "Select Interactions to Include in the Model",choices = params[[paste0('COI ', x)]][[paste0('Operation ', y)]], multiple = TRUE, width = 400)),
                                                                                                    fluidRow(checkboxInput(inputId = paste0("checkpowerbysample", x, y), label = "Create power by sample size curve", value = FALSE, )),
                                                                                                    
                                                                               fluidRow(numericInput(inputId = paste0("minruns", x, y), 
                                                                                                     label = "Min Runs", value = 1, width = 100), 
                                                                                        numericInput(inputId = paste0("maxruns", x, y),
                                                                                                     label = "Max Runs", value = as.numeric(2*length(DTOutput(paste0("coi",x,"operation",y,"dt")))), width = 100), 
                                                                                        numericInput(inputId = paste0("targetsnr", x, y), label = "Target SNR", value = 1.5, width = 100),
                                                                                        numericInput(inputId = paste0('stepsize',x,y), label = "Step Size", value = 10, width = 100)),
                                                                               fluidRow(actionButton(inputId = paste0("gendesign", x, y), 
                                                                                                     strong("Generate Design"))),
                                                                               fluidRow(textOutput(paste0("error", x, y)), width = 400), ),
                                                                        column(width = 6, offset = 0,
                                                                               fluidRow(textAreaInput(inputId = paste0('notes', x, y), 
                                                                                                      label = "Please Include any design consideration comments that you have here."), width = 400, rows = 10))), 
                                                               tabPanel(paste0("Expand Full Factorial Design"),
                                                                        fluidRow(numericInput(inputId = paste0("full_design",x,y), label = "Number of Replicates", value = 0)), 
                                                                        fluidRow(actionButton(inputId = paste0("genfulldesign", x, y), 
                                                                                              strong("Generate Design")))
                                                               )))))}))))})))})
              
              
              
              
              
              
           }})
        
        
        
        
      }
   
############################################################################################# Input Factor Levels Module #####################################################################################################      
      
         
      else if (module == "enter_fls"){
        #build menu item for entering Factors and levels
        output$menu_module <- renderMenu({
          menuItem("Test Design", tabName = "testdesign", icon = icon("dashboard"), 
                   menuSubItem("Factor Level Builder", tabName = "factorlevels", icon = icon("dashboard")),
                   menuSubItem("Build Your Test Design", "doe", icon = icon("dashboard")))
        })
        #display a UI to enter factors and levels.
        output$enterfactorlevels <- renderUI(
          fluidPage(
            fluidRow(
              box(
                textInput(inputId = 'programname', value = 'Enter Program Name', width = 300, label = strong("Input your program name here.")),
                fluidRow(passwordInput("program_password", label = "Enter a Password for the Program. Store this Password Somewhere Secure for Future Use.")),
                fluidRow(passwordInput("second_password", label = "Please Re-enter your Password")),
                fluidRow(passwordInput("resetphrase", label = "Please Choose a Password Reset Phrase")),
                fluidRow(selectInput(inputId = "det", label = "Select Detachment", choices = c("Det 2", "Det 3", "Det 5", "Det 6", "No Det"))),
                numericInput(inputId = 'num_cois', value = 1, max = 20, min = 1, label = "How many COIs would you like to generate?"),
                box(
                  actionButton(inputId = 'start_coi', label = "Create COIs")
                )
              )
            )
          )
        )
        
        
        
        observeEvent(input[['start_coi']],{
            aws.s3::s3write_using(module, FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/session_type/", "session_type.txt"),    
                                bucket = bucket)
          if (input[["programname"]] %in% program_names) {
            showModal(modalDialog(
              title = "Program Already Created",
              "The program that you have chosen to create has already been created. Choosing this program name will force all data previously created to be overwritten!"
            ))
            NULL
          }
          
          else if (any(sapply(list(input$programname, input$program_password, input$det, input$resetphrase), is.null))){
            showModal(
              modalDialog(
                title = "Error",
                "Please fill in all the fields.",
                easyClose = TRUE
              )
            )
          }
          
          else if (input$program_password != input$second_password){
            showModal(
              modalDialog(
                title = "Error",
                "Please make sure that your passwords match",
                easyClose = TRUE
              )
            )
          }
          else {
            password = bcrypt::hashpw(input$program_password)
            aws.s3::s3write_using(password, FUN = write_lines, object = paste0(folder, input$det, "/", input$programname,"/program_data/password.txt"),    
                                  bucket = bucket
            )
            recovery_key = bcrypt::hashpw(input$resetphrase)
            
            aws.s3::s3write_using(recovery_key, FUN = write_lines, object = paste0(folder, input$det, "/", input$programname,"/program_data/resetphrase.txt"),    
                                  bucket = bucket
            )
            
          
          
          
          num_cois = reactiveVal()
          num_cois = input[["num_cois"]]
          num_ops = reactiveValues()
          num_factors = reactiveValues()
          factor_str = reactiveValues()
          level_str = reactiveValues()
          test = reactiveValues()
          reactivedata = reactiveValues()
          reactivegendatamaster <- reactiveValues()
          params = reactiveValues()
          
          
          for (x in c(1:num_cois)){
           num_ops[[paste0(x,"ops")]] = 1
            
            for (y in c(1:1)){
            num_factors[[paste0("factors", x, y)]] <- 1
              
              for (z in c(1:1)){
                 factor_str[[paste0(x,y,z,"factor")]] = ""
                  level_str[[paste0(x,y,z,"levels")]] = ""
                
                
              } 
            }
          }
          
          output$enterfactorlevels <- renderUI({
            fluidPage(uiOutput("cois"))
          })
          
########################################################################### Render COI Structure #################################################################
          
          output$cois <- renderUI({
            fluidPage(fluidRow(numericInput(inputId = 'num_cois', label = 'Enter the Number of COIs', value = num_cois)),
                      fluidRow(do.call(tabsetPanel, c(id = "t", lapply(1:num_cois, function(x){
                        tabPanel(paste0('COI ', title = x), fluidRow(numericInput(inputId = paste0("num_ops",x), label = "Enter the number of Operations for this COI", value = num_ops[[paste0(x,"ops")]])), 
                                 fluidRow(uiOutput(paste0("operation", x))))
                      })))))
          })

########################################################################### Observe Changes in the number of COIs #################################################################
          observeEvent(input[['num_cois']], {
            if (is.na(input[['num_cois']]) == TRUE){
              NULL
            }
            else{
              num_cois = input[['num_cois']]
              aws.s3::s3write_using(as.character(num_cois), FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/cois/", "COIs", ".txt"),    
                                    bucket = bucket
              )
              output$cois <- renderUI({
                fluidPage(fluidRow(numericInput(inputId = 'num_cois', label = 'Enter the Number of COIs', value = num_cois)),
                          fluidRow(do.call(tabsetPanel, c(id = "t", lapply(1:num_cois, function(x){
                            tabPanel(paste0('COI ', title = x), fluidRow(numericInput(inputId = paste0("num_ops",x), label = "Enter the number of Operations for this COI", value = ifelse(is.null(num_ops[[paste0(x,"ops")]]), 1, num_ops[[paste0(x,"ops")]]))), 
                                     fluidRow(uiOutput(paste0("operation", x))))
                          })))))
              })}
          })          
          
########################################################################### Render Operation structure #################################################################
          observeEvent(input[['num_cois']], {
            
            num_cois = input[['num_cois']]
          lapply(1:num_cois, function(x){
            observeEvent(input[[paste0("num_ops", x)]],{
              num_ops[[paste0(x, "ops")]] = input[[paste0("num_ops", x)]]
              aws.s3::s3write_using(as.character(num_ops[[paste0(x, "ops")]]), FUN = writeLines, object = paste0(folder, input$det, "/", input$programname,"/session_data/ops/",x, "ops", ".txt"),    
                                    bucket = bucket)
              output[[paste0("operation", x)]] <- renderUI({
                do.call(tabsetPanel, c(id = "ops", lapply (1:num_ops[[paste0(x, "ops")]], function(y){
                  tabPanel(paste0('Operation ', title = y),
                           fluidRow(numericInput(inputId = paste0('add_factors', x, y), label = "How many factors would you like to add?", value = ifelse(is.null(num_factors[[paste0("factors", x, y)]]), 1, num_factors[[paste0("factors", x, y)]]))),
                           column(width = 11, uiOutput(paste0('factor_inputs', x, y))),
                           actionButton(inputId = paste0('generate_design_table', x, y), strong('Create Candidate Design Table')))
                  
                })))
              })
            })
          })
        })
          

            
          
        
########################################################################### Render Factor Structure #################################################################
          observeEvent(input[['num_cois']], {
            num_cois <- input[['num_cois']]
            lapply(1:num_cois, function(x) {
              observeEvent(input[[paste0("num_ops", x)]], {
                num_ops[[paste0(x, "ops")]] <- input[[paste0("num_ops", x)]]
                lapply(1:num_ops[[paste0(x, "ops")]], function(y) {
                  observeEvent(input[[paste0('add_factors', x, y)]], {
                    num_factors[[paste0("factors", x, y)]] <- input[[paste0('add_factors', x, y)]]
                      output[[paste0('factor_inputs', x, y)]] <- renderUI({
                        isolate({
                          num_factors_val <- num_factors[[paste0("factors", x, y)]]
                          factor_inputs <- lapply(1:num_factors_val, function(i) {
                            observeEvent(input[[paste0("level", x, y, i)]], {
                              level_str[[paste0(x, y, i, "levels")]] <- input[[paste0("level", x, y, i)]]
                            })
                            observeEvent(input[[paste0("factor", x, y, i)]], {
                              factor_str[[paste0(x, y, i, "factor")]] <- input[[paste0("factor", x, y, i)]]
                            })
                            
                            box(
                              textInput(paste0("factor", x, y, i), label = paste0("Factor ", i, ":"), 
                                        value = ifelse(is.null(factor_str[[paste0(x, y, i, "factor")]]), "", factor_str[[paste0(x, y, i, "factor")]]), 
                                        width = 200), 
                              textInput(paste0("level", x, y, i), label = paste0("Factor ", i, " Levels:"), 
                                        value = ifelse(is.null(level_str[[paste0(x, y, i, "levels")]]), "", level_str[[paste0(x, y, i, "levels")]]), 
                                        width = 400),
                              collapsible = FALSE
                            )
                          })
                          do.call(fluidRow, factor_inputs)
                        })
                      })
                    
                  })
                })
              })
            })
          })
      
########################################################################### Generate Default Design (full factorial) #################################################################
          observeEvent(input[["num_cois"]],{
            num_cois = input[["num_cois"]]
          lapply(1:num_cois, function(x){
            observeEvent(input[[paste0("num_ops", x)]],{
              num_ops[[paste0(x, "ops")]] <- input[[paste0("num_ops", x)]]
             lapply(1:num_ops[[paste0(x, "ops")]], function(y){
              observeEvent(input[[paste0('generate_design_table', x, y)]], {
                print("hello")
                
                factor_list = list()
                for (z in  c(1:num_factors[[paste0("factors", x, y)]])) {
                  factor = input[[paste0("factor",x, y, z)]]
                  
                  aws.s3::s3write_using(factor, FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/factors/", x, y, z, "factors", ".txt"),    
                                        bucket = bucket)
                  
                  level = input[[paste0("level",x, y, z)]]
                  
                  aws.s3::s3write_using(level, FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/levels/", x, y, z, "levels", ".txt"),    
                                        bucket = bucket)
                  
                  names_vec <- strsplit(level, split = ",")
                  factor_list[factor] = names_vec
                  
                }
                max_length = max(lengths(factor_list))
                factors <- lapply(factor_list, function(x) {
                  if (length(x) < max_length) {
                    c(x, rep(NA, max_length - length(x)))
                  } else {
                    x
                  }
                })
                factors_df <- data.frame(factors)
                
                result <- drop_na(expand.grid(factors_df))
                
                test[[paste0('COI', x)]][[paste0('Operation', y)]] = result
                params[[paste0('coi', x)]][[paste0('operation', y)]] <- modelparams(result)
                name_dt = paste0("coi", x,  "operation", y, "dt")
                
                reactivedata[[name_dt]] <- result
                output[[name_dt]] <- DT::renderDT({datatable(reactivedata[[name_dt]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                
                
                #Delete Button
                
                observeEvent(input[[paste0("delete_row",x,y)]],{
                  name_rows_selected = noquote(paste0("coi",x,"operation",y,"dt","_rows_selected"))
                  name_dt = paste0("coi", x,  "operation", y, "dt")
                  reactivedata[[name_dt]] <- reactivedata[[name_dt]][-as.numeric(input[[name_rows_selected]]),]
                  output[[name_dt]] <- DT::renderDT({datatable(reactivedata[[name_dt]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                  
                  
                  
                  
                })
                
                #reset Button
                
                observeEvent(input[[paste0("reset",x,y)]],{
                  factor_list = list()
                  for (z in  c(1:num_factors[[paste0("factors", x, y)]])) {
                    factor = input[[paste0("factor",x, y, z)]]
                    level = input[[paste0("level",x, y, z)]]
                    names_vec <- strsplit(level, split = ",")
                    factor_list[factor] = names_vec
                  }
                  max_length = max(lengths(factor_list))
                  factors <- lapply(factor_list, function(x) {
                    if (length(x) < max_length) {
                      c(x, rep(NA, max_length - length(x)))
                    } else {
                      x
                    }
                  })
                  factors_df <- data.frame(factors)
                  result <- drop_na(expand.grid(factors_df))
                  test[[paste0('COI', x)]][[paste0('Operation', y)]] = result
                  params[[paste0('coi', x)]][[paste0('operation', y)]] <- modelparams(result)
                  name_dt = paste0("coi", x,  "operation", y, "dt")
                  
                  reactivedata[[name_dt]] <- result
                  output[[name_dt]] <- DT::renderDT({datatable(reactivedata[[name_dt]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                  
                  
                  
                })
                
                
                
                
                aws.s3::s3write_using(reactivedata[[name_dt]], FUN = write.csv, object = paste0(folder,input$det, "/",input$programname,"/session_data/default_designs/",x,y,"default" , ".csv"),    
                                      bucket = bucket
                )
                
                aws.s3::s3write_using(as.character(num_factors[[paste0("factors", x, y)]]), FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/numfactors/", x, y, "factors", ".txt"),    
                                      bucket = bucket)
               
                
      ########################################################################### Generate Design  #################################################################                
                
                observeEvent(input[[paste0("gendesign",x,y)]],{
                  name_dt = paste0("coi", x,  "operation", y, "dt")
                  name_generated_design = paste0("coi", x,  "operation", y, "dt", "generated")
                  trials = as.numeric(input[[paste0("runs",x,y)]])
                  optimality = input[[paste0("criteria", x, y)]]
                  tryCatch(
                    expr = {
                      form = paste0("~", paste(input[[paste0('interactions',x,y)]], collapse = "+"))
                      text_formula = paste(input[[paste0('interactions',x,y)]], collapse = "+")
                      form
                      
                      reactivegendatamaster[[name_generated_design]] <- 
                        gen_design(candidateset = reactivedata[[name_dt]],
                                   model = formula(form), trials = trials, 
                                   optimality = optimality)
                      output[[name_generated_design]] <- DT::renderDT({datatable(reactivegendatamaster[[name_generated_design]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                      
                      output[[paste0("powerplot",x,y)]] <- renderPlot({
                        PowerCurveGenerator(reactivegendatamaster[[name_generated_design]],model_input = formula(form), name_of_plot = paste0(input[["programname"]], " COI ", x, " Operation ", y))
                        
                        
                      })
                      
                      output[[paste0("corplot",x,y)]] <- renderPlot({
                        plot_correlations(reactivegendatamaster[[name_generated_design]], customcolors = c("blue", "red", "yellow", "green"))})
                      
                      if (input[[paste0('checkpowerbysample',x, y)]] == TRUE) {
                        
                        output[[paste0("powerbysample",x,y)]] <- renderPlot({
                          power_by_sample(candidate_set = reactivedata[[name_dt]], 
                                          min_sample_size = as.numeric(input[[paste0("minruns", x, y)]]), 
                                          max_sample_size = as.numeric(input[[paste0("maxruns", x, y)]]), 
                                          targetSNR = as.numeric(input[[paste0("targetsnr", x, y)]]), 
                                          parameters = formula(form), step = input[[paste0('stepsize',x,y)]], nameplot = paste0(input[["programname"]], " COI ", x, " Operation ", y))
                        })
                        
                        min_size = as.character(input[[paste0("minruns", x, y)]])
                        max_size = as.character(input[[paste0("maxruns", x, y)]])
                        snr = as.character(input[[paste0("targetsnr", x, y)]])
                        step_size = as.character(input[[paste0('stepsize',x,y)]])
                        
                        aws.s3::s3write_using(min_size , FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/powerbysample/COI",x,"/Operation", y, "/min_sample.txt"),    
                                              bucket = bucket)
                        aws.s3::s3write_using(max_size , FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/powerbysample/COI",x,"/Operation", y, "/max_sample.txt"),    
                                              bucket = bucket)
                        aws.s3::s3write_using(snr , FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/powerbysample/COI",x,"/Operation", y, "/snr.txt"),    
                                              bucket = bucket)
                        aws.s3::s3write_using(step_size , FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/powerbysample/COI",x,"/Operation", y, "/step.txt"),    
                                              bucket = bucket)
                        
                        
                        
                      }
                      
                      else {
                        NULL
                      }
                      
                      output[[paste0("error", x, y)]] <- renderText({"Success! Proceed to the Design Evaluation tab to evaluate the design."})
                      
                      #write formulas to S3 
                      aws.s3::s3write_using(text_formula, FUN = writeLines, object = paste0(folder,input$det, "/",input$programname,"/session_data/formulas/",x, y, "formula", ".txt"),    
                                            bucket = bucket)
                      
                      #write generated design to s3 for saving progress
                      aws.s3::s3write_using(reactivegendatamaster[[name_generated_design]], FUN = write.csv,
                                            object = paste0(folder,input$det, "/", input$programname,"/session_data/generated_designs/", x,y,"generated", ".csv"),    
                                            bucket = bucket
                      )
                      
                      #write power by sample size data to s3
                      
                      
                      
                    },
                    error = function(e){
                      output[[paste0("error", x, y)]] <- renderText({ 
                        "Error in generating Design. This error could be a result of a number of factors. To fix this error, please make sure that there are no interaction terms for dissallowed combinations. Next, try increasing the number of runs for your test." 
                        
                        
                      })
                    }, 
                    
                    warning = function(w){
                      output[[paste0("warning", x, y)]] <- renderText({ 
                        message(w)
                      })
                    },
                    finally = {}
                    
                  )
                  
                })
                
                
                
                
              }) 
            })
          
            ########################################################################### Default Design UI  #################################################################                
            
             observeEvent(input[[paste0('generate_design_table', x, y)]], {
              output$doetabsfordesign <- renderUI({                       
                do.call(
                  tabsetPanel, c(id = 't', lapply(1:num_cois, function(x){
                    tabPanel(paste0('COI ', title = x), 
                             do.call(
                               tabsetPanel, c(id = 'ops',lapply(1:num_ops[[paste0(x, "ops")]], function(y){
                                 tabPanel(paste0('Operation ', title = y), 
                                          fluidRow(
                                            column(width = 6, offset = 0, 
                                                   actionButton(paste0("delete_row", x, y), "Delete Row", icon("edit")), 
                                                   actionButton(paste0("reset",x,y), "Reset Table", icon("refresh")), 
                                                   DTOutput(paste0("coi",x,"operation",y,"dt"))),      
                                            column(width = 6, offset = 0, 
                                                   tabsetPanel(id = paste0(x, y), 
                                                               tabPanel(paste0("Create an Optimal Design"),
                                                                        column(width = 6, offset = 0, 
                                                                               fluidRow(numericInput(inputId = paste0("runs", x, y), 
                                                                                                     label = "Runs", value =  1, width = 200), 
                                                                                        bsTooltip(id = paste0("runs", x, y), 
                                                                                                  title = "Select the number of runs that you would like to include in your design. This number cannot be zero",
                                                                                                  placement = "left", trigger = "hover")), 
                                                                               fluidRow(radioButtons(inputId = paste0("criteria", x, y), 
                                                                                                     label = "Optimality Criterion", 
                                                                                                     choiceNames = c("D-Optimal (Recommended)", "I-Optimal", "A-Optimal", "ALIAS", "G-Optimal", "T-Optimal", "E-Optimal"), 
                                                                                                     choiceValues = c("D", "I", "A", "ALIAS", "G", "T", "E"))),
                                                                               fluidRow(selectInput(inputId = paste0("interactions",x,y), 
                                                                                                    label = "Select Interactions to Include in the Model",
                                                                                                    choices = params[[paste0('coi', x)]][[paste0('operation', y)]], multiple = TRUE, width = 400)),
                                                                               fluidRow(checkboxInput(inputId = paste0("checkpowerbysample", x, y), label = "Create power by sample size curve", value = FALSE, )),
                                                                               fluidRow(numericInput(inputId = paste0("minruns", x, y), 
                                                                                                     label = "Min Runs", value = 1, width = 100), 
                                                                                        numericInput(inputId = paste0("maxruns", x, y),
                                                                                                     label = "Max Runs", value = as.numeric(2*length(DTOutput(paste0("coi",x,"operation",y,"dt")))), width = 100), 
                                                                                        numericInput(inputId = paste0("targetsnr", x, y), label = "Target SNR", value = 1.5, width = 100),
                                                                                        numericInput(inputId = paste0('stepsize',x,y), label = "Step Size", value = 10, width = 100)),
                                                                               fluidRow(actionButton(inputId = paste0("gendesign", x, y), 
                                                                                                     strong("Generate Design"))),
                                                                               fluidRow(textOutput(paste0("error", x, y)), width = 400), ),
                                                                        column(width = 6, offset = 0,
                                                                               fluidRow(textAreaInput(inputId = paste0('notes', x, y), 
                                                                                                      label = "Please Include any design consideration comments that you have here."), width = 400, rows = 10))), 
                                                               tabPanel(paste0("Expand Full Factorial Design"),
                                                                        fluidRow(numericInput(inputId = paste0("full_design",x,y), label = "Number of Replicates", value = 0)), 
                                                                        fluidRow(actionButton(inputId = paste0("genfulldesign", x, y), 
                                                                                              strong("Generate Design")))
                                                               )))))}))))})))})})
             
          
            ########################################################################### Generated Design UI  #################################################################                
            
            
              output$generated_designs <- renderUI({                       
                do.call(
                  tabsetPanel, c(id = 't', lapply(1:num_cois, function(x){
                    tabPanel(paste0('COI ', title = x), 
                             do.call(
                               tabsetPanel, c(id = 'ops',lapply(1:num_ops[[paste0(x, "ops")]], function(y){
                                 tabPanel(paste0('Operation ', title = y), 
                                          fluidRow(
                                            column(width = 6, offset = 0,
                                                   DTOutput(paste0("coi",x,"operation",y,"dt", "generated"))),      
                                            column(width = 6, offset = 0, 
                                                   tabsetPanel(id = 'plots', 
                                                               tabPanel(title = "Power Curves", plotOutput(paste0("powerplot",x,y))),
                                                               tabPanel(title = "Correlation Plot", plotOutput(paste0("corplot",x,y))),
                                                               tabPanel(title = "Power by Sample Size", plotOutput(paste0("powerbysample",x,y))))),
                                            fluidRow(actionButton(inputId = paste0("generatetem", x, y), label = "Generate Test Event Matrix"))
                                            
                                            
                                            
                                          )
                                          
                                 )}))))})))})
             
             output$testeventmatrix <- renderUI({                       
               do.call(
                 tabsetPanel, c(id = 'b', lapply(1:num_cois, function(x){
                   tabPanel(paste0('COI ', title = x), 
                            do.call(
                              tabsetPanel, c(id = 'opsb',lapply(1:num_ops[[paste0(x, "ops")]], function(y){
                                tabPanel(paste0('Operation ', title = y), 
                                         fluidRow(actionButton(inputId = paste0("generatetem", x, y), label = "Generate Test Event Matrix")),
                                         fluidRow(
                                           column(width = 6, offset = 0,
                                                  DTOutput(paste0("characterize", x, y)), 
                                                  DTOutput(paste0("problemid", x, y))) 
                                           
                                           
                                           
                                           
                                           
                                         )
                                         
                                )}))))})))})
             
             observeEvent(input[[paste0('generatetem', x, y)]], {
               name_generated_design = paste0("coi", x,  "operation", y, "dt", "generated")
               
                 
                 df <- count_duplicates(as.data.frame(reactivegendatamaster[[name_generated_design]]))
                 rownames(df) <- 1:nrow(df)
                 
                 output[[paste0("characterize", x, y)]] <- DT::renderDT({datatable(df, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Characterize")})

                 aws.s3::s3write_using(df, FUN = write.csv,  object = paste0(folder,input$det, "/",input$programname,"/session_data/TEMs/characterize/", x, y, "characterize.csv"),    
                                       bucket = bucket)
                 
                 
               
               
             })
             
             
             
          })  
          }) 
         
          })
            
              
     
          
            
        }    
      })


        
            
        
        

          
         
          
          
          
          
          
          
          
        
        
        
        
      } 
      
    })})
##################################################### Load feature ###############################################################################################################

 lapply(detachments, function(det){
  observeEvent(input[[paste0("programnames_", tolower(gsub(" ", "", det)))]],{
    observeEvent(input[[paste0("load_", tolower(gsub(" ", "", det)))]],{
      selected_detachment <<- det
      loaded_program <<- input[[paste0("programnames_", tolower(gsub(" ", "", det)))]]
      password_hash = aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/", loaded_program, "/program_data/password.txt"), bucket = bucket)
      
      reset_hash = aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/", loaded_program, "/program_data/resetphrase.txt"), bucket = bucket)
     
      showModal(
        modalDialog(
          title = "Enter Password",
          textInput("password", "Password:", ""),
          footer = tagList(
            actionButton("submit_password", "Submit"),
            actionButton("third_party", "Load with View Only"),
            actionButton("forgot_password", "Forgot Password"),
            modalButton("Dismiss")
          )
        )
      )
      
      observeEvent(input[['forgot_password']],{
        showModal(
          modalDialog(
            title = "Enter Password Reset Phrase",
            textInput("password_reset", "Reset Phrase:", ""),
            footer = tagList(
              actionButton("submit_password_reset", "Submit"),
            )
          )
        )
      })
      
      observeEvent(input[["submit_password_reset"]], {
        entered_reset = input[['password_reset']]
        is_correct <- bcrypt::checkpw(entered_reset, reset_hash)
        if(!is_correct){
          showModal(
            modalDialog(
              title = "Error",
              "Incorrect Password Reset Phrase. Please Try again",
              easyClose = TRUE
            )
          )
        } else {
          showModal(
            modalDialog(
              title = "Enter a new password",
              passwordInput("program_password", label = "Enter a Password for the Program."),
              passwordInput("second_password", label = "Please Re-enter your Password"),
              actionButton("submit_new_password", "Submit")
              
            )
          )
        }
        observeEvent(input[["submit_new_password"]], {
          if (input[["program_password"]] != input[["second_password"]]){
            showModal(
              modalDialog(
                title = "Error",
                "Please make sure the passwords match.",
                easyClose = TRUE
              )
            )
          }
          else {
            password = bcrypt::hashpw(input$program_password)
            aws.s3::s3write_using(password, FUN = write_lines, object = paste0(folder, det, "/", loaded_program,"/program_data/password.txt"),    
                                  bucket = bucket
            )
            showModal(
              modalDialog(
                title = "Enter Password",
                textInput("password", "Password:", ""),
                footer = tagList(
                  actionButton("submit_password", "Submit"),
                  actionButton("third_party", "Load with View Only"),
                  actionButton("forgot_password", "Forgot Password"),
                  modalButton("Dismiss")
                )
              )
            )
            
          }
        })
        
      })
      
      observeEvent(input$submit_password, {
        entered_password = input$password
        
        is_correct <- bcrypt::checkpw(entered_password, password_hash)
        if (!is_correct) {
          showModal(
            modalDialog(
              title = "Error",
              "Incorrect Password. Please Try again",
              textInput("password", "Password:", ""),
              footer = tagList(
                actionButton("submit_password", "Submit")),
              easyClose = TRUE
            )
          )
        }
        else {
          
          
             
       
      
      type = aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/", loaded_program, "/session_data/session_type/session_type.txt"), bucket = bucket)
      
##################################################### Load feature for programs created using a TDW ##############################################################################      

      if (type == "tdw_template"){
        params <<- hash()
        reactivedata <- reactiveValues()
        reactivegendatamaster <- reactiveValues()
        characterize_tem <- reactiveValues()
        problemid_tem <- reactiveValues()
        other_factors <- reactiveValues()
        num_ops = hash()
        num_ops_tem = hash()
        output$menu_module <- renderMenu({
          menuItem("Test Design", tabName = "testdesign", icon = icon("dashboard"), 
                   
                   menuSubItem("Build Your Test Design", "doe", icon = icon("dashboard")))
        })
        
        

        
        num_cois = as.numeric(aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/cois/COIs.txt"), bucket = bucket))
        num_cois_tem = as.numeric(aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/TEMs/COIs/total_COIs.txt"), bucket = bucket))
        lapply(1:ifelse(!is.null(num_cois_tem), num_cois_tem, num_cois), function(x){
          tryCatch (
            expr = {
              num_ops_tem[[paste0(x,"ops")]] = as.numeric(aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/TEMs/ops/", x, "ops.txt"), bucket = bucket))
              num_ops[[paste0(x,"ops")]] = as.numeric(aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/ops/", x, "ops.txt"), bucket = bucket))
            },
            error = function(b) {
              NULL
            }
          )
          lapply(1:ifelse(!is.null(num_ops_tem[[paste0(x,"ops")]]), num_ops_tem[[paste0(x,"ops")]], num_ops[[paste0(x,"ops")]]), function(y){
            name_dt = paste0("coi", x,  "operation", y, "dt")
            tryCatch({
            ################################################## read in default datatables from s3 ################################################
            dt = aws.s3::s3read_using(FUN = read.csv, object = paste0(folder, det, "/",loaded_program, "/session_data/default_designs/", x, y, "default.csv"), bucket = bucket)
            dt$X <- NULL
            params[[paste0('coi', x)]][[paste0('operation', y)]] <- modelparams(dt)
            reactivedata[[name_dt]] <- dt
            output[[name_dt]] <- DT::renderDT({datatable(reactivedata[[name_dt]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
           
           ######################################################## read in generated datatables from s3 #####################################################################
            
              dt_generated = aws.s3::s3read_using(FUN = read.csv, object = paste0(folder, det, "/",loaded_program, "/session_data/generated_designs/", x, y, "generated.csv"), bucket = bucket)
              dt_generated$X <- NULL
              name_generated_design = paste0("coi", x,  "operation", y, "dt", "generated")
              
              reactivegendatamaster[[name_generated_design]] <- dt_generated
              
  
              
              
              #read in formulas from s3
              textformula = aws.s3::s3read_using(FUN = read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/formulas/", x, y, "formula.txt"), bucket = bucket)
              form = paste0("~", textformula)
              
              #create objects within shiny for generated designs
              output[[name_generated_design]] <- DT::renderDT({datatable(reactivegendatamaster[[name_generated_design]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
              
              output[[paste0("powerplot",x,y)]] <- renderPlot({
                PowerCurveGenerator(reactivegendatamaster[[name_generated_design]],model_input = formula(form), name_of_plot = paste0(loaded_program, " COI ", x, " Operation ", y))
                
                
              })
              
              output[[paste0("corplot",x,y)]] <- renderPlot({
                plot_correlations(reactivegendatamaster[[name_generated_design]], customcolors = c("blue", "red", "yellow", "green"), model = formula(form), pow = 2)
                
                })
              
              
              
             min_size = as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/min_sample.txt"),    
                                    bucket = bucket))
             max_size = as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/max_sample.txt"),    
                                    bucket = bucket))
             target_snr = as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/snr.txt"),    
                                    bucket = bucket))
             step_size =  as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/step.txt"),    
                                    bucket = bucket))
              
              
              output[[paste0("powerbysample",x,y)]] <- renderPlot({
                power_by_sample(candidate_set = reactivedata[[name_dt]], 
                                min_sample_size = min_size, 
                                max_sample_size = max_size, 
                                targetSNR = target_snr, 
                                parameters = formula(form), step = step_size, nameplot = paste0(loaded_program, " COI ", x, " Operation ", y))
                              })
              
            }, error = function(e) {
              # handle the error gracefully (in this case, do nothing)
            })
            
            tryCatch(
              expr = {
                characterize_dt <- aws.s3::s3read_using(FUN = read.csv, object = paste0(folder,det, "/",loaded_program,"/session_data/TEMs/characterize/", x, y, "characterize.csv"),    
                                                        bucket = bucket)
                characterize_dt$X <- NULL
                
                output[[paste0("characterize", x, y)]] <- DT::renderDT({datatable(characterize_dt, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Characterize")})
                
                
              }, 
              error = function(b){
                NULL
              }
            )
            tryCatch(
              expr = {
                problemid_dt <- aws.s3::s3read_using(FUN = read.csv, object = paste0(folder,det, "/",loaded_program,"/session_data/TEMs/problemid/", x, y, "problem_id.csv"),    
                                                     bucket = bucket)
                problemid_dt$X <- NULL
                
                output[[paste0('problemid', x, y)]] <- DT::renderDT({datatable(problemid_dt, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Problem ID")})
                
              },
              error = function(s){
                NULL
              }
            )
            
            tryCatch(
              expr = {
            
                other_factors_dt <- aws.s3::s3read_using(FUN = read.csv, object = paste0(folder,det, "/",loaded_program,"/session_data/TEMs/otherfactors/", x, y, "otherfactors.csv"),    
                                                         bucket = bucket)
                
                other_factors_dt$X <- NULL
                other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]]  <- other_factors_dt
              },
              error = function(c) {
                NULL
              }
            )
           
            
            
            
            #generate Design
            
            observeEvent(input[[paste0("gendesign",x,y)]],{
              name_dt = paste0("coi", x,  "operation", y, "dt")
              name_generated_design = paste0("coi", x,  "operation", y, "dt", "generated")
              trials = as.numeric(input[[paste0("runs",x,y)]])
              optimality = input[[paste0("criteria", x, y)]]
              tryCatch(
                expr = {
                  form = paste0("~", paste(input[[paste0('interactions',x,y)]], collapse = "+"))
                  text_formula = paste(input[[paste0('interactions',x,y)]], collapse = "+")
                  
                  reactivegendatamaster[[name_generated_design]] <- 
                    gen_design(candidateset = reactivedata[[name_dt]],
                               model = formula(form), trials = trials, 
                               optimality = optimality)
                  output[[name_generated_design]] <- DT::renderDT({datatable(reactivegendatamaster[[name_generated_design]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                  
                  output[[paste0("powerplot",x,y)]] <- renderPlot({
                    PowerCurveGenerator(reactivegendatamaster[[name_generated_design]],model_input = formula(form), name_of_plot = paste0(loaded_program, " COI ", x, " Operation ", y))
                    
                    
                  })
                  
                  output[[paste0("corplot",x,y)]] <- renderPlot({
                    plot_correlations(reactivegendatamaster[[name_generated_design]], customcolors = c("blue", "red", "yellow", "green"))
                    
                    })
                  
                  if (input[[paste0('checkpowerbysample', x, y)]] == TRUE) {
                    
                    print(input[[paste0("minruns", x, y)]])
                    output[[paste0("powerbysample",x,y)]] <- renderPlot({
                     power_by_sample(candidate_set = reactivedata[[name_dt]], 
                                     min_sample_size = as.numeric(input[[paste0("minruns", x, y)]]), 
                                     max_sample_size = as.numeric(input[[paste0("maxruns", x, y)]]), 
                                      targetSNR = as.numeric(input[[paste0("targetsnr", x, y)]]), 
                                      parameters = formula(form), step = as.numeric(input[[paste0('stepsize',x,y)]]), nameplot = paste0(loaded_program, " COI ", x, " Operation ", y))
                    })
                    
                    min_size = as.character(input[[paste0("minruns", x, y)]])
                    max_size = as.character(input[[paste0("maxruns", x, y)]])
                    snr = as.character(input[[paste0("targetsnr", x, y)]])
                    step_size = as.character(input[[paste0('stepsize',x,y)]])
                    
                    aws.s3::s3write_using(min_size , FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/min_sample.txt"),    
                                          bucket = bucket)
                    aws.s3::s3write_using(max_size , FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/max_sample.txt"),    
                                          bucket = bucket)
                    aws.s3::s3write_using(snr , FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/snr.txt"),    
                                          bucket = bucket)
                    aws.s3::s3write_using(step_size , FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/step.txt"),    
                                          bucket = bucket)
                    
                    
                    
                    }
                  else {
                    NULL
                  }
                  
                  output[[paste0("error", x, y)]] <- renderText({"Success! Proceed to the Design Evaluation tab to evaluate the design."})
                  
                  
                    
                   
                 
                  
                  
                  
                 
                  
                },
                error = function(e){
                  output[[paste0("error", x, y)]] <- renderText({ 
                    "Error in generating Design. This error could be a result of a number of factors. To fix this error, please make sure that there are no interaction terms for dissallowed combinations. Next, try increasing the number of runs for your test." 
                    
                    
                  })
                }, 
                
                warning = function(w){
                  output[[paste0("warning", x, y)]] <- renderText({ 
                    message(w)
                  })
                },
                finally = {}
                
              )
              
              #write formulas to S3 
              aws.s3::s3write_using(text_formula, FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/formulas/",x, y, "formula", ".txt"),    
                                    bucket = bucket)
              
              #write generated design to s3 for saving progress
              aws.s3::s3write_using(reactivegendatamaster[[name_generated_design]], FUN = write.csv,
                                    object = paste0(folder,det, "/",loaded_program,"/session_data/generated_designs/", x,y,"generated", ".csv"),    
                                    bucket = bucket
              )
              
            
              })
            
            observeEvent(input[[paste0('generatetem', x, y)]], {
              name_generated_design = paste0("coi", x,  "operation", y, "dt", "generated")
              if (!is.null(reactivegendatamaster[[name_generated_design]]) & 
                  !is.null(other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]])) {
                
                dfs <- generate_dataframes(as.data.frame(reactivegendatamaster[[name_generated_design]]), other_factors = as.data.frame(other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]]))
                rownames(dfs$characterize_df) <- 1:nrow(dfs$characterize_df)
                
                output[[paste0("characterize", x, y)]] <- DT::renderDT({datatable(dfs$characterize_df, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Characterize")})
                output[[paste0('problemid', x, y)]] <- DT::renderDT({datatable(dfs$problemID_df, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Problem ID")})
                
                aws.s3::s3write_using(dfs$problemID_df, FUN = write.csv,  object = paste0(folder,det, "/",loaded_program,"/session_data/TEMs/problemid/", x, y, "problem_id.csv"),    
                                      bucket = bucket)
                
                aws.s3::s3write_using(dfs$characterize_df, FUN = write.csv,  object = paste0(folder,det, "/",loaded_program,"/session_data/TEMs/characterize/", x, y, "characterize.csv"),    
                                      bucket = bucket)
                
                
              } else if(!is.null(reactivegendatamaster[[name_generated_design]])) {
                characterize_df <- count_duplicates(reactivegendatamaster[[name_generated_design]])
                output[[paste0("characterize", x, y)]] <- DT::renderDT({datatable(characterize_df, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Characterize")})
                
                aws.s3::s3write_using(characterize_df, FUN = write.csv,  object = paste0(folder,det, "/",loaded_program,"/session_data/TEMs/characterize/", x, y, "characterize.csv"),    
                                      bucket = bucket)
                
              } else if (!is.null(other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]])) {
                problemID_df <- create_demo_and_log_cols(other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]])
                output[[paste0('problemid', x, y)]] <- DT::renderDT({datatable(problemID_df, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Problem ID")})
                
                aws.s3::s3write_using(problemID_df, FUN = write.csv,  object = paste0(folder,det, "/",loaded_program,"/session_data/TEMs/problemid/", x, y, "problem_id.csv"),    
                                      bucket = bucket)
                
              }
              
            })
            
            
            
            output$generated_designs <- renderUI({                       
              do.call(
                tabsetPanel, c(id = 't', lapply(1:num_cois, function(x){
                  tabPanel(paste0('COI ', title = x), 
                           do.call(
                             tabsetPanel, c(id = 'ops',lapply(1:num_ops[[paste0(x,"ops")]], function(y){
                               tabPanel(paste0('Operation ', title = y), 
                                        fluidRow(
                                          column(width = 6, offset = 0,
                                                 DTOutput(paste0("coi",x,"operation",y,"dt", "generated"))),      
                                          column(width = 6, offset = 0, 
                                                 tabsetPanel(id = 'plots', 
                                                             tabPanel(title = "Power Curves", plotOutput(paste0("powerplot",x,y))),
                                                             tabPanel(title = "Correlation Plot", plotOutput(paste0("corplot",x,y))),
                                                             tabPanel(title = "Power by Sample Size", plotOutput(paste0("powerbysample",x,y))))),
                                          fluidRow(actionButton(inputId = paste0("generatetem", x, y), label = "Generate Test Event Matrix"))
                                          
                                          
                                        )
                                        
                               )}))))})))})
            
            
            output$testeventmatrix <- renderUI({                       
              do.call(
                tabsetPanel, c(id = 'b', lapply(1:num_cois_tem, function(x){
                  tabPanel(paste0('COI ', title = x), 
                           do.call(
                             tabsetPanel, c(id = 'opsb',lapply(1:num_ops_tem[[paste0(x,"ops")]], function(y){
                               tabPanel(paste0('Operation ', title = y), 
                                        fluidRow(actionButton(inputId = paste0("generatetem", x, y), label = "Generate Test Event Matrix")),
                                        fluidRow(
                                          column(width = 6, offset = 0,
                                                 DTOutput(paste0("characterize", x, y)), 
                                                 DTOutput(paste0("problemid", x, y))) 
                                          
                                          
                                          
                                          
                                          
                                        )
                                        
                               )}))))})))})
            
            
            
            #UI for design generation
            output$doetabsfordesign <- renderUI({                       
              do.call(
                tabsetPanel, c(id = 't', lapply(1:num_cois, function(x){
                  tabPanel(paste0('COI ', title = x), 
                           do.call(
                             tabsetPanel, c(id = 'ops',lapply(1:num_ops[[paste0(x,"ops")]], function(y){
                              
                               tabPanel(paste0('Operation ', title = y), 
                                        fluidRow(
                                          column(width = 6, offset = 0, 
                                                 actionButton(paste0("delete_row", x, y), "Delete Row", icon("edit")), 
                                                 actionButton(paste0("reset",x,y), "Reset Table", icon("refresh")), 
                                                 DTOutput(paste0("coi",x,"operation",y,"dt"))),      #make all of the action buttons tabs that can be open to create the design for better UI experience.
                                          column(width = 6, offset = 0, 
                                                 tabsetPanel(id = paste0(x, y), 
                                                             tabPanel(paste0("Create an Optimal Design"),
                                                                      column(width = 6, offset = 0, 
                                                                             fluidRow(numericInput(inputId = paste0("runs", x, y), 
                                                                                                   label = "Runs", value =  1, width = 200), 
                                                                                      bsTooltip(id = paste0("runs", x, y), 
                                                                                                title = "Select the number of runs that you would like to include in your design. This number cannot be zero",
                                                                                                placement = "left", trigger = "hover")), 
                                                                             fluidRow(radioButtons(inputId = paste0("criteria", x, y), 
                                                                                                   label = "Optimality Criterion", 
                                                                                                   choiceNames = c("D-Optimal (Recommended)", "I-Optimal", "A-Optimal", "ALIAS", "G-Optimal", "T-Optimal", "E-Optimal"), 
                                                                                                   choiceValues = c("D", "I", "A", "ALIAS", "G", "T", "E"))),
                                                                             fluidRow(selectInput(inputId = paste0("interactions",x,y), 
                                                                                                  label = "Select Interactions to Include in the Model",choices = params[[paste0('coi', x)]][[paste0('operation', y)]], multiple = TRUE, width = 400)),
                                                                             fluidRow(checkboxInput(inputId = paste0("checkpowerbysample", x, y), label = "Create power by sample size curve", value = FALSE)),
                                                                             
                                                                             fluidRow(numericInput(inputId = paste0("minruns", x, y), 
                                                                                                   label = "Min Runs", value = 1, width = 100), 
                                                                                      numericInput(inputId = paste0("maxruns", x, y),
                                                                                                   label = "Max Runs", value = 20, width = 100), 
                                                                                      numericInput(inputId = paste0("targetsnr", x, y), label = "Target SNR", value = 1.5, width = 100),
                                                                                      numericInput(inputId = paste0('stepsize',x,y), label = "Step Size", value = 5, width = 100)),
                                                                             fluidRow(actionButton(inputId = paste0("gendesign", x, y), 
                                                                                                   strong("Generate Design"))),
                                                                             fluidRow(textOutput(paste0("error", x, y)), width = 400), ),
                                                                      column(width = 6, offset = 0,
                                                                             fluidRow(textAreaInput(inputId = paste0('notes', x, y), 
                                                                                                    label = "Please Include any design consideration comments that you have here."), width = 400, rows = 10))), 
                                                             tabPanel(paste0("Expand Full Factorial Design"),
                                                                      fluidRow(numericInput(inputId = paste0("full_design",x,y), label = "Number of Replicates", value = 0)), 
                                                                      fluidRow(actionButton(inputId = paste0("genfulldesign", x, y), 
                                                                                            strong("Generate Design")))
                                                             )))))}))))})))})    
            
              
          })
          
        })
        
        
      }
########################################################################### Load feature for programs created using factor level input ##########################################################################

            else if (type == "enter_fls") {
              #load menu layout
              output$menu_module <- renderMenu({
                menuItem("Test Design", tabName = "testdesign", icon = icon("dashboard"), 
                         menuSubItem("Factor Level Builder", tabName = "factorlevels", icon = icon("dashboard")),
                         menuSubItem("Build Your Test Design", "doe", icon = icon("dashboard")))
              })
              
              num_cois = as.numeric(aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/cois/COIs.txt"), bucket = bucket))
              num_ops = reactiveValues()
              num_factors = reactiveValues()
              factor_str = reactiveValues()
              level_str = reactiveValues()
              
              params = reactiveValues()
              reactivedata = reactiveValues()
              reactivegendatamaster = reactiveValues()
              
              for (x in c(1:num_cois)){
                tryCatch({
                  num_ops[[paste0(x,"ops")]] = as.numeric(aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/ops/",x, "ops.txt"), bucket = bucket))
                },
                error = function(e){
                  num_ops[[paste0(x, "ops")]] = 1
                }
                )

                for (y in c(1:num_ops[[paste0(x, "ops")]])){
                  num_factors[[paste0("factors", x, y)]] <- try(as.numeric(aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/numfactors/",x,y, "factors.txt"), bucket = bucket)))
                  
                  if (inherits(num_factors[[paste0("factors", x, y)]], "try-error")) {
                    #Handle the error condition
                    num_factors[[paste0("factors", x, y)]] <- 1 # set to NA or other default value
                    message("Error reading file: Value set to default, 1.") # print an informative error message
                  }
                  
                  tryCatch({
                    
                    #read in default datatables from s3
                    name_dt = paste0("coi", x,  "operation", y, "dt")
                    dt = aws.s3::s3read_using(FUN = read.csv, object = paste0(folder, det, "/",loaded_program, "/session_data/default_designs/", x, y, "default.csv"), bucket = bucket)
                    
                    dt$X <- NULL
                    params[[paste0('coi', x)]][[paste0('operation', y)]] <- modelparams(dt)
                    reactivedata[[name_dt]] <- dt
                    output[[name_dt]] <- DT::renderDT({datatable(dt, editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                    
                    
                    #read in generated datatables from s3
                    dt_generated = aws.s3::s3read_using(FUN = read.csv, object = paste0(folder, det, "/",loaded_program, "/session_data/generated_designs/", x, y, "generated.csv"), bucket = bucket)
                    dt_generated$X <- NULL
                    name_generated_design = paste0("coi", x,  "operation", y, "dt", "generated")
                    reactivegendatamaster[[name_generated_design]] <- dt_generated
                    
                    #read in formulas from s3
                    textformula = aws.s3::s3read_using(FUN = read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/formulas/", x, y, "formula.txt"), bucket = bucket)
                    form = paste0("~", textformula)
                    
                    #create objects within shiny for generated designs
                    output[[name_generated_design]] <- DT::renderDT({datatable(reactivegendatamaster[[name_generated_design]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                    
                    output[[paste0("powerplot",x,y)]] <- renderPlot({
                      PowerCurveGenerator(reactivegendatamaster[[name_generated_design]],model_input = formula(form), name_of_plot = paste0(input[["programname"]], " COI ", x, " Operation ", y))
                      
                      
                    })
                    
                    output[[paste0("corplot",x,y)]] <- renderPlot({
                      plot_correlations(reactivegendatamaster[[name_generated_design]], customcolors = c("blue", "red", "yellow", "green"), model = formula(form), pow = 2)
                      
                      })
                    
                    min_size = as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/min_sample.txt"),    
                                                               bucket = bucket))
                    max_size = as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/max_sample.txt"),    
                                                               bucket = bucket))
                    target_snr = as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/snr.txt"),    
                                                                 bucket = bucket))
                    step_size =  as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/step.txt"),    
                                                                 bucket = bucket))
                    
                    output[[paste0("powerbysample",x,y)]] <- renderPlot({
                      power_by_sample(candidate_set = reactivedata[[name_dt]], 
                                      min_sample_size = min_size, 
                                      max_sample_size = max_size, 
                                      targetSNR = target_snr, 
                                      parameters = formula(form), step = step_size, nameplot = paste0(loaded_program, " COI ", x, " Operation ", y))
                    })
                    
                    
                  }, error = function(e) {
                    # handle the error gracefully (in this case, do nothing)
                  })
                  
                  
                  
                  for (z in c(1:num_factors[[paste0("factors", x, y)]])){
                    tryCatch({
                      factor_str[[paste0(x,y,z,"factor")]] = aws.s3::s3read_using(FUN = read_lines, object = paste0(folder,det, "/",loaded_program,"/session_data/factors/", x, y, z, "factors.txt"),    
                                                                                 bucket = bucket)
                      
                      level_str[[paste0(x,y,z,"levels")]] = aws.s3::s3read_using(FUN = read_lines, object = paste0(folder,det, "/",loaded_program,"/session_data/levels/", x, y, z, "levels.txt"),    
                                                                                bucket = bucket)
                      },
                      error = function(e) {
                        message("Error reading file")
                        factor_str[[paste0(x,y,z,"factor")]] = ""
                        level_str[[paste0(x,y,z,"levels")]] = ""
                      }
                    )
                  
                  }
                }
              }
              
              output$enterfactorlevels <- renderUI({
                fluidPage(uiOutput('cois')
                )
              })              
                
              output$cois <- renderUI({
                fluidPage(fluidRow(numericInput(inputId = 'num_cois', label = 'Enter the Number of COIs', value = num_cois, min = 1, max = 20)),
                fluidRow(do.call(tabsetPanel, c(id = "t", lapply(1:num_cois, function(x){
                  tabPanel(paste0('COI ', title = x))
                })))))
              })

# function to check for changes in the number of COIs                           
              observeEvent(input[['num_cois']], {
                if (input[['num_cois']] == 0) {
                  NULL
                }
                else if (is.na(input[['num_cois']]) == TRUE){
                  NULL
                }
                else{
                  num_cois = input[['num_cois']]
                  aws.s3::s3write_using(as.character(num_cois), FUN = writeLines, object = paste0(folder,input$det, "/",loaded_program,"/session_data/cois/", "COIs", ".txt"),    
                                        bucket = bucket
                  )
                output$cois <- renderUI({
                  fluidPage(fluidRow(numericInput(inputId = 'num_cois', label = 'Enter the Number of COIs', value = num_cois)),
                           fluidRow(do.call(tabsetPanel, c(id = "t", lapply(1:num_cois, function(x){
                             tabPanel(paste0('COI ', title = x), fluidRow(numericInput(inputId = paste0("num_ops",x), label = "Enter the number of Operations for this COI", value = num_ops[[paste0(x, "ops")]])), 
                                                                          fluidRow(uiOutput(paste0("operation", x))))
                           })))))
                })}
              })
              
#function to check for changes in the number of operations
            
              observeEvent(input[['num_cois']], {
                num_cois = input[['num_cois']]
              lapply(1:num_cois, function(x){
                observeEvent(input[[paste0("num_ops", x)]],{
                  num_ops[[paste0(x, "ops")]] = input[[paste0("num_ops", x)]]
                  aws.s3::s3write_using(as.character(num_ops[[paste0(x, "ops")]]), FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/ops/",x, "ops", ".txt"),    
                                        bucket = bucket)
                  output[[paste0("operation", x)]] <- renderUI({
                    do.call(tabsetPanel, c(id = "ops", lapply (1:num_ops[[paste0(x, "ops")]], function(y){
                      print(num_factors[[paste0("factors", x, y)]])
                      tabPanel(paste0('Operation ', title = y),
                               fluidRow(numericInput(inputId = paste0('add_factors', x, y), label = "How many factors would you like to add?", value = ifelse(is.null(num_factors[[paste0("factors", x, y)]]), 1, num_factors[[paste0("factors", x, y)]]))),
                               column(width = 11, uiOutput(paste0('factor_inputs', x, y))),
                               actionButton(inputId = paste0('generate_design_table', x, y), strong('Create Candidate Design Table')))
                      
                      })))
                  })
                  
                  
                })
              })
            })
              
#function to generate factor input UI
            
              observeEvent(input[['num_cois']], {
                num_cois <- input[['num_cois']]
                lapply(1:num_cois, function(x) {
                  observeEvent(input[[paste0("num_ops", x)]], {
                    num_ops[[paste0(x, "ops")]] <- input[[paste0("num_ops", x)]]
                    lapply(1:num_ops[[paste0(x, "ops")]], function(y) {
                      observeEvent(input[[paste0('add_factors', x, y)]], {
                        num_factors[[paste0("factors", x, y)]] <- input[[paste0('add_factors', x, y)]]
                        output[[paste0('factor_inputs', x, y)]] <- renderUI({
                          isolate({
                            factor_inputs <- lapply(1:num_factors[[paste0("factors", x, y)]], function(i) {
                              observeEvent(input[[paste0("level", x, y, i)]], {
                                level_str[[paste0(x, y, i, "levels")]] <- input[[paste0("level", x, y, i)]]
                              })
                              observeEvent(input[[paste0("factor", x, y, i)]], {
                                factor_str[[paste0(x, y, i, "factor")]] <- input[[paste0("factor", x, y, i)]]
                              })
                              box(
                                textInput(paste0("factor", x, y, i), label = paste0("Factor ", i, ":"), 
                                          value = ifelse(is.null(factor_str[[paste0(x, y, i, "factor")]]), "", factor_str[[paste0(x, y, i, "factor")]]), 
                                          width = 200),
                                textInput(paste0("level", x, y, i), label = paste0("Factor ", i, " Levels:"), 
                                          value = ifelse(is.null(level_str[[paste0(x, y, i, "levels")]]), "", level_str[[paste0(x, y, i, "levels")]]), 
                                          width = 400),
                                collapsible = FALSE
                              )
                            })
                            do.call(fluidRow, factor_inputs)
                          })
                        })
                        
                      })
                    })
                  })
                })
              })

              ########################################################################### Generate Default Design (full factorial) #################################################################
              observeEvent(input[["num_cois"]],{
                num_cois = input[["num_cois"]]
                lapply(1:num_cois, function(x){
                  observeEvent(input[[paste0("num_ops", x)]],{
                    num_ops[[paste0(x, "ops")]] <- input[[paste0("num_ops", x)]]
                    lapply(1:num_ops[[paste0(x, "ops")]], function(y){
                      observeEvent(input[[paste0('generate_design_table', x, y)]], {
                        print("hello")
                        
                        factor_list = list()
                        for (z in  c(1:num_factors[[paste0("factors", x, y)]])) {
                          factor = input[[paste0("factor",x, y, z)]]
                          
                          aws.s3::s3write_using(factor, FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/factors/", x, y, z, "factors", ".txt"),    
                                                bucket = bucket)
                          
                          level = input[[paste0("level",x, y, z)]]
                          
                          aws.s3::s3write_using(level, FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/levels/", x, y, z, "levels", ".txt"),    
                                                bucket = bucket)
                          
                          names_vec <- strsplit(level, split = ",")
                          factor_list[factor] = names_vec
                          
                        }
                        max_length = max(lengths(factor_list))
                        factors <- lapply(factor_list, function(x) {
                          if (length(x) < max_length) {
                            c(x, rep(NA, max_length - length(x)))
                          } else {
                            x
                          }
                        })
                        factors_df <- data.frame(factors)
                        
                        result <- drop_na(expand.grid(factors_df))
                        
                        test[[paste0('COI', x)]][[paste0('Operation', y)]] = result
                        params[[paste0('coi', x)]][[paste0('operation', y)]] <- modelparams(result)
                        name_dt = paste0("coi", x,  "operation", y, "dt")
                        reactivedata[[name_dt]] <- result
                        output[[name_dt]] <- DT::renderDT({datatable(reactivedata[[name_dt]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                        
                        
                        
                        
                        aws.s3::s3write_using(reactivedata[[name_dt]], FUN = write.csv, object = paste0(folder,det, "/",loaded_program,"/session_data/default_designs/",x,y,"default" , ".csv"),    
                                              bucket = bucket
                        )
                        
                        aws.s3::s3write_using(as.character(num_factors[[paste0("factors", x, y)]]), FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/numfactors/", x, y, "factors", ".txt"),    
                                              bucket = bucket)
                        
                        
                        ########################################################################### Generate Design  #################################################################                
                        
                        observeEvent(input[[paste0("gendesign",x,y)]],{
                          name_dt = paste0("coi", x,  "operation", y, "dt")
                          name_generated_design = paste0("coi", x,  "operation", y, "dt", "generated")
                          trials = as.numeric(input[[paste0("runs",x,y)]])
                          optimality = input[[paste0("criteria", x, y)]]
                          tryCatch(
                            expr = {
                              form = paste0("~", paste(input[[paste0('interactions',x,y)]], collapse = "+"))
                              text_formula = paste(input[[paste0('interactions',x,y)]], collapse = "+")
                              form
                              
                              reactivegendatamaster[[name_generated_design]] <- 
                                gen_design(candidateset = reactivedata[[name_dt]],
                                           model = formula(form), trials = trials, 
                                           optimality = optimality)
                              output[[name_generated_design]] <- DT::renderDT({datatable(reactivegendatamaster[[name_generated_design]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                              
                              output[[paste0("powerplot",x,y)]] <- renderPlot({
                                PowerCurveGenerator(reactivegendatamaster[[name_generated_design]],model_input = formula(form), name_of_plot = paste0(loaded_program, " COI ", x, " Operation ", y))
                                
                                
                              })
                              
                              output[[paste0("corplot",x,y)]] <- renderPlot({
                                plot_correlations(reactivegendatamaster[[name_generated_design]], customcolors = c("blue", "red", "yellow", "green"))})
                              
                              if (input[[paste0('checkpowerbysample', x, y)]] == TRUE) {
                                
                                output[[paste0("powerbysample",x,y)]] <- renderPlot({
                                  power_by_sample(candidate_set = reactivedata[[name_dt]], 
                                                  min_sample_size = as.numeric(input[[paste0("minruns", x, y)]]), 
                                                  max_sample_size = as.numeric(input[[paste0("maxruns", x, y)]]), 
                                                  targetSNR = as.numeric(input[[paste0("targetsnr", x, y)]]), 
                                                  parameters = formula(form), step = input[[paste0('stepsize',x,y)]], nameplot = paste0(loaded_program, " COI ", x, " Operation ", y))
                                })
                                
                                min_size = as.character(input[[paste0("minruns", x, y)]])
                                max_size = as.character(input[[paste0("maxruns", x, y)]])
                                snr = as.character(input[[paste0("targetsnr", x, y)]])
                                step_size = as.character(input[[paste0('stepsize',x,y)]])
                                
                                aws.s3::s3write_using(min_size , FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/min_sample.txt"),    
                                                      bucket = bucket)
                                aws.s3::s3write_using(max_size , FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/max_sample.txt"),    
                                                      bucket = bucket)
                                aws.s3::s3write_using(snr , FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/snr.txt"),    
                                                      bucket = bucket)
                                aws.s3::s3write_using(step_size , FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/step.txt"),    
                                                      bucket = bucket)
                                
                                }
                              else {
                                NULL
                              }
                              
                              output[[paste0("error", x, y)]] <- renderText({"Success! Proceed to the Design Evaluation tab to evaluate the design."})
                              
                              #write formulas to S3 
                              aws.s3::s3write_using(text_formula, FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/formulas/",x, y, "formula", ".txt"),    
                                                    bucket = bucket)
                              
                              #write generated design to s3 for saving progress
                              aws.s3::s3write_using(reactivegendatamaster[[name_generated_design]], FUN = write.csv,
                                                    object = paste0(folder,det, "/",loaded_program,"/session_data/generated_designs/", x,y,"generated", ".csv"),    
                                                    bucket = bucket
                              )
                              
                              #write power by sample size data to s3
                              
                              
                              
                            },
                            error = function(e){
                              output[[paste0("error", x, y)]] <- renderText({ 
                                "Error in generating Design. This error could be a result of a number of factors. To fix this error, please make sure that there are no interaction terms for dissallowed combinations. Next, try increasing the number of runs for your test." 
                                
                                
                              })
                            }, 
                            
                            warning = function(w){
                              output[[paste0("warning", x, y)]] <- renderText({ 
                                message(w)
                              })
                            },
                            finally = {}
                            
                          )
                          
                        })
                        
                        
                        
                        
                      }) 
                    })
                    
                    ########################################################################### Default Design UI  #################################################################                
                    
                  
                      output$doetabsfordesign <- renderUI({                       
                        do.call(
                          tabsetPanel, c(id = 't', lapply(1:num_cois, function(x){
                            tabPanel(paste0('COI ', title = x), 
                                     do.call(
                                       tabsetPanel, c(id = 'ops',lapply(1:num_ops[[paste0(x, "ops")]], function(y){
                                         tabPanel(paste0('Operation ', title = y), 
                                                  fluidRow(
                                                    column(width = 6, offset = 0, 
                                                           actionButton(paste0("delete_row", x, y), "Delete Row", icon("edit")), 
                                                           actionButton(paste0("reset",x,y), "Reset Table", icon("refresh")), 
                                                           DTOutput(paste0("coi",x,"operation",y,"dt"))),      
                                                    column(width = 6, offset = 0, 
                                                           tabsetPanel(id = paste0(x, y), 
                                                                       tabPanel(paste0("Create an Optimal Design"),
                                                                                column(width = 6, offset = 0, 
                                                                                       fluidRow(numericInput(inputId = paste0("runs", x, y), 
                                                                                                             label = "Runs", value =  1, width = 200), 
                                                                                                bsTooltip(id = paste0("runs", x, y), 
                                                                                                          title = "Select the number of runs that you would like to include in your design. This number cannot be zero",
                                                                                                          placement = "left", trigger = "hover")), 
                                                                                       fluidRow(radioButtons(inputId = paste0("criteria", x, y), 
                                                                                                             label = "Optimality Criterion", 
                                                                                                             choiceNames = c("D-Optimal (Recommended)", "I-Optimal", "A-Optimal", "ALIAS", "G-Optimal", "T-Optimal", "E-Optimal"), 
                                                                                                             choiceValues = c("D", "I", "A", "ALIAS", "G", "T", "E"))),
                                                                                       fluidRow(selectInput(inputId = paste0("interactions",x,y), 
                                                                                                            label = "Select Interactions to Include in the Model",
                                                                                                            choices = params[[paste0('coi', x)]][[paste0('operation', y)]], multiple = TRUE, width = 400)),
                                                                                       fluidRow(checkboxInput(inputId = paste0("checkpowerbysample", x, y), label = "Create power by sample size curve", value = FALSE, )),
                                                                                       fluidRow(numericInput(inputId = paste0("minruns", x, y), 
                                                                                                             label = "Min Runs", value = 1, width = 100), 
                                                                                                numericInput(inputId = paste0("maxruns", x, y),
                                                                                                             label = "Max Runs", value = as.numeric(2*length(DTOutput(paste0("coi",x,"operation",y,"dt")))), width = 100), 
                                                                                                numericInput(inputId = paste0("targetsnr", x, y), label = "Target SNR", value = 1.5, width = 100),
                                                                                                numericInput(inputId = paste0('stepsize',x,y), label = "Step Size", value = 10, width = 100)),
                                                                                       fluidRow(actionButton(inputId = paste0("gendesign", x, y), 
                                                                                                             strong("Generate Design"))),
                                                                                       fluidRow(textOutput(paste0("error", x, y)), width = 400), ),
                                                                                column(width = 6, offset = 0,
                                                                                       fluidRow(textAreaInput(inputId = paste0('notes', x, y), 
                                                                                                              label = "Please Include any design consideration comments that you have here."), width = 400, rows = 10))), 
                                                                       tabPanel(paste0("Expand Full Factorial Design"),
                                                                                fluidRow(numericInput(inputId = paste0("full_design",x,y), label = "Number of Replicates", value = 0)), 
                                                                                fluidRow(actionButton(inputId = paste0("genfulldesign", x, y), 
                                                                                                      strong("Generate Design")))
                                                                       )))))}))))})))})
                    
                    #Delete Button
                    
                    observeEvent(input[[paste0("delete_row",x,y)]],{
                      name_rows_selected = noquote(paste0("coi",x,"operation",y,"dt","_rows_selected"))
                      name_dt = paste0("coi", x,  "operation", y, "dt")
                      reactivedata[[name_dt]] <- reactivedata[[name_dt]][-as.numeric(input[[name_rows_selected]]),]
                      output[[name_dt]] <- DT::renderDT({datatable(reactivedata[[name_dt]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                      
                      
                      
                      
                    })
                    
                    #reset Button
                    
                    observeEvent(input[[paste0("reset",x,y)]],{
                      
                      factor_list = list()
                      for (z in  c(1:num_factors[[paste0("factors", x, y)]])) {
                        factor = input[[paste0("factor",x, y, z)]]
                        
                        #aws.s3::s3write_using(factor, FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/factors/", x, y, z, "factors", ".txt"),    
                                           #   bucket = bucket)
                        
                        level = input[[paste0("level",x, y, z)]]
                        
                        #aws.s3::s3write_using(level, FUN = writeLines, object = paste0(folder,det, "/",loaded_program,"/session_data/levels/", x, y, z, "levels", ".txt"),    
                                             # bucket = bucket)
                        
                        names_vec <- strsplit(level, split = ",")
                        factor_list[factor] = names_vec
                        
                      }
                      max_length = max(lengths(factor_list))
                      factors <- lapply(factor_list, function(x) {
                        if (length(x) < max_length) {
                          c(x, rep(NA, max_length - length(x)))
                        } else {
                          x
                        }
                      })
                      factors_df <- data.frame(factors)
                      
                      result <- drop_na(expand.grid(factors_df))
                      
                      test[[paste0('COI', x)]][[paste0('Operation', y)]] = result
                      params[[paste0('coi', x)]][[paste0('operation', y)]] <- modelparams(result)
                      name_dt = paste0("coi", x,  "operation", y, "dt")
                      reactivedata[[name_dt]] <- result
                      output[[name_dt]] <- DT::renderDT({datatable(reactivedata[[name_dt]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                      
                    })
                    
                    
                    
                    ########################################################################### Generated Design UI  #################################################################                
                    
                    
                    output$generated_designs <- renderUI({                       
                      do.call(
                        tabsetPanel, c(id = 't', lapply(1:num_cois, function(x){
                          tabPanel(paste0('COI ', title = x), 
                                   do.call(
                                     tabsetPanel, c(id = 'ops',lapply(1:num_ops[[paste0(x, "ops")]], function(y){
                                       tabPanel(paste0('Operation ', title = y), 
                                                fluidRow(
                                                  column(width = 6, offset = 0,
                                                         DTOutput(paste0("coi",x,"operation",y,"dt", "generated"))),      
                                                  column(width = 6, offset = 0, 
                                                         tabsetPanel(id = 'plots', 
                                                                     tabPanel(title = "Power Curves", plotOutput(paste0("powerplot",x,y))),
                                                                     tabPanel(title = "Correlation Plot", plotOutput(paste0("corplot",x,y))),
                                                                     tabPanel(title = "Power by Sample Size", plotOutput(paste0("powerbysample",x,y)))))
                                                  
                             )
                            )
                           }
                          )
                         )
                        )
                       )
                      }
                     )
                    )
                   )
                  }
                 ) 
                }
               )  
              }
             ) 
            }
           )
          }
         }
        }
       ) 
     
   
   
  
############################################################################################### View Only Module #################################################################################################  
   
  #Watch for third party viewer button to be selected.
   
  observeEvent(input[['third_party']],{
    
    showModal(
      modalDialog(
        title = "Success", 
        "Proceed over to the Design Evaluation tab to View Designs.",
        )
      )
    
          
        
          num_cois = as.numeric(aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/cois/COIs.txt"), bucket = bucket))
          num_cois_tem = as.numeric(aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/TEMs/COIs/total_COIs.txt"), bucket = bucket))
          num_ops = reactiveValues()
          num_ops_tem =
          reactivegendatamaster = reactiveValues()
          params = reactiveValues()
         
            lapply(1:ifelse(!is.null(num_cois_tem), num_cois_tem, num_cois), function(x){
              tryCatch (
                expr = {
                  num_ops_tem[[paste0(x,"ops")]] = as.numeric(aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/TEMs/ops/", x, "ops.txt"), bucket = bucket))
                  num_ops[[paste0(x,"ops")]] = as.numeric(aws.s3::s3read_using(read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/ops/", x, "ops.txt"), bucket = bucket))
                },
                error = function(b) {
                  NULL
                }
              )
              lapply(1:ifelse(!is.null(num_ops_tem[[paste0(x,"ops")]]), num_ops_tem[[paste0(x,"ops")]], num_ops[[paste0(x,"ops")]]), function(y) {
                tryCatch({
                  dt_generated = aws.s3::s3read_using(FUN = read.csv, object = paste0(folder, det, "/",loaded_program, "/session_data/generated_designs/", x, y, "generated.csv"), bucket = bucket)
                  dt_generated$X <- NULL
                  name_generated_design = paste0("coi", x,  "operation", y, "dt", "generated")
                  reactivegendatamaster[[name_generated_design]] <- dt_generated
                  
                  #read in formulas from s3
                  textformula = aws.s3::s3read_using(FUN = read_lines, object = paste0(folder, det, "/",loaded_program, "/session_data/formulas/", x, y, "formula.txt"), bucket = bucket)
                  form = paste0("~", textformula)
                  
                  #create objects within shiny for generated designs
                  output[[name_generated_design]] <- DT::renderDT({datatable(reactivegendatamaster[[name_generated_design]], editable = TRUE, selection = list(mode = 'multiple', target = 'row'))})
                  
                  output[[paste0("powerplot",x,y)]] <- renderPlot({
                    PowerCurveGenerator(reactivegendatamaster[[name_generated_design]],model_input = formula(form), name_of_plot = paste0(input[["programname"]], " COI ", x, " Operation ", y))
                    
                    
                  })
                  
                  output[[paste0("corplot",x,y)]] <- renderPlot({
                    plot_correlations(reactivegendatamaster[[name_generated_design]], customcolors = c("blue", "red", "yellow", "green"), model = formula(form), pow = 2)
                    
                    })
                  
                  min_size = as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/min_sample.txt"),    
                                                             bucket = bucket))
                  max_size = as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/max_sample.txt"),    
                                                             bucket = bucket))
                  target_snr = as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/snr.txt"),    
                                                               bucket = bucket))
                  step_size =  as.numeric(aws.s3::s3read_using(FUN = readLines, object = paste0(folder,det, "/",loaded_program,"/session_data/powerbysample/COI",x,"/Operation", y, "/step.txt"),    
                                                               bucket = bucket))
                  
                  output[[paste0("powerbysample",x,y)]] <- renderPlot({
                    power_by_sample(candidate_set = reactivedata[[name_dt]], 
                                    min_sample_size = min_size, 
                                    max_sample_size = max_size, 
                                    targetSNR = target_snr, 
                                    parameters = formula(form), step = step_size, nameplot = paste0(loaded_program, " COI ", x, " Operation ", y))
                  
                  })
                }, error = function(e) {
                  # handle the error gracefully (in this case, do nothing)
                })
               
                tryCatch(
                  expr = {
                    characterize_dt <- aws.s3::s3read_using(FUN = read.csv, object = paste0(folder,det, "/",loaded_program,"/session_data/TEMs/characterize/", x, y, "characterize.csv"),    
                                                            bucket = bucket)
                    characterize_dt$X <- NULL
                    
                    output[[paste0("characterize", x, y)]] <- DT::renderDT({datatable(characterize_dt, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Characterize")})
                    
                    
                  }, 
                  error = function(b){
                    NULL
                  }
                )
                tryCatch(
                  expr = {
                    problemid_dt <- aws.s3::s3read_using(FUN = read.csv, object = paste0(folder,det, "/",loaded_program,"/session_data/TEMs/problemid/", x, y, "problem_id.csv"),    
                                                         bucket = bucket)
                    problemid_dt$X <- NULL
                    
                    output[[paste0('problemid', x, y)]] <- DT::renderDT({datatable(problemid_dt, editable = FALSE, selection = list(mode = 'multiple', target = 'row'), caption = "Problem ID")})
                    
                  },
                  error = function(s){
                    NULL
                  }
                )
                
                tryCatch(
                  expr = {
                    
                    other_factors_dt <- aws.s3::s3read_using(FUN = read.csv, object = paste0(folder,det, "/",loaded_program,"/session_data/TEMs/otherfactors/", x, y, "otherfactors.csv"),    
                                                             bucket = bucket)
                    
                    other_factors_dt$X <- NULL
                    other_factors[[paste0("COI ", x)]][[paste0("Operation ", y)]]  <- other_factors_dt
                  },
                  error = function(c) {
                    NULL
                  }
                )
                
                
                
                
              })
              
            })
            
          
          
          
          output$generated_designs <- renderUI({
              do.call(
                tabsetPanel, c(id = 't', lapply(1:num_cois, function(x){
                  tabPanel(paste0('COI ', title = x), 
                           do.call(
                             tabsetPanel, c(id = 'ops',lapply(1:num_ops[[paste0(x, "ops")]], function(y){
                               tabPanel(paste0('Operation ', title = y), 
                                        fluidRow(
                                          column(width = 6, offset = 0,
                                                 DTOutput(paste0("coi",x,"operation",y,"dt", "generated"))),      
                                          column(width = 6, offset = 0, 
                                                 tabsetPanel(id = 'plots', 
                                                             tabPanel(title = "Power Curves", plotOutput(paste0("powerplot",x,y))),
                                                             tabPanel(title = "Correlation Plot", plotOutput(paste0("corplot",x,y)))
                                                             ))
                                          
                                        )
                               )
                             }
                             )
                             )
                           )
                  )
                }
                )
                )
              )
            
          }
          )
          
          output$testeventmatrix <- renderUI({                       
            do.call(
              tabsetPanel, c(id = 'b', lapply(1:num_cois_tem, function(x){
                tabPanel(paste0('COI ', title = x), 
                         do.call(
                           tabsetPanel, c(id = 'opsb',lapply(1:num_ops_tem[[paste0(x,"ops")]], function(y){
                             tabPanel(paste0('Operation ', title = y), 
                                      fluidRow(
                                        column(width = 6, offset = 0,
                                               DTOutput(paste0("characterize", x, y)), 
                                               DTOutput(paste0("problemid", x, y))) 
                                        
                                        
                                        
                                        
                                        
                                      )
                                      
                             )}))))})))})
          
          
          
          
          
          
      
    
    

      
      
      
    
    
    
    
    
    
    
     })
    })
   })    
  })
  
}
  
  