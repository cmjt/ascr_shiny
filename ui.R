
shinyUI(fluidPage(
    theme = shinytheme("spacelab"),
    ## App title ----
    titlePanel("acoustic spatial capture-recapture (ascr)", windowTitle = "ascr"),

    ## Sidebar layout with input and output definitions ----
    sidebarLayout(

        ## Sidebar panel for inputs ----
        sidebarPanel(
            useShinyjs(),
            id = "side-panel",
            h3(icon("table"), tags$b("Read in data")),
            ## example data loading
            checkboxInput("example", "Use example data",value = FALSE), ## example
            radioButtons("which_example", "Chose example data to load",
                         choices = c( "Simple" = "simple",
                                     "With bearings (rad)" = "bearings",
                                     "With distance (m)" = "distance",
                                     "With bearings (rad) and distance (m)" = "bd"),
                         inline = TRUE),


            ## user data loading
            fileInput("file1", "Choose CSV file of trap locations",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),  ## Input: Select a csv file of trap locations
            
            fileInput("file2", "Choose CSV file of detections",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),  ## Input: Select a csv file of detection locations
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ",",inline = TRUE),
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"',inline = TRUE),

            radioButtons("disp", "Display",
                         choices = c( All = "all",
                                     Head = "head"),
                         selected = "all",inline = TRUE),
            
            radioButtons("bearing_range", "Chose bearing measurements",
                         choices = c("Degrees" = "bd",
                                     "Radians" = "rad"),
                         selected = "rad",inline = TRUE),
            numericInput("show.call.num", "Choose call number to display in location plot:",
                         min = 1, max = 1000,step = 1,
                         value = 1),
            h3(icon("puzzle-piece"),tags$b("Build mask")),
            ## Input: integer of mask buffer in meters (this is updated based on trap info when file is loaded)
            sliderInput("buffer", "Choose mask buffer (m):",
                        min = 1, max = 10000,
                        value = 1000),
            ## Input: integer of mask spacing in meters (this is updated based on trap info when file is loaded)
            sliderInput("spacing", "Choose mask spacing (m):",
                        min = 1, max = 1000,
                        value = 250),
            downloadButton('downloadMask', 'Mask plot'),
            ## horizontal lines before model options,
            
            h3(icon("cogs"),tags$b("Modelling")),
            ## select box for detetion functions
            selectInput("select", label = "Chose a detection function", 
                        choices = list("halfnormal" = 'hn', "hazard rate" = 'hr', "threshold" = 'th'), 
                        selected = "hn"),
            ## check box conditional on value of detfn chosen
            uiOutput("fixedParamSelection"),
            ## fix g0 to what value
            uiOutput("fixedg0"),
            ## fix sigma to what value
            uiOutput("fixedsigma"),
            ## fix z to what value
            uiOutput("fixedz"),
            ## fix shape to what value
            uiOutput("fixedshape"),
            ## fix scale to what value
            uiOutput("fixedscale"),
            ## fix shape.1 to what value
            uiOutput("fixedshape.1"),
            ## fix shape.2 to what value
            uiOutput("fixedshape.2"),
            ## horizontal lines before choosing call number for estimated group location
            
            
            actionButton("fit", "Fit model",icon("cogs")),
            hidden(p(id = "processing", "Processing...")),
            hr(),
            downloadButton('downloadSurfPlot', 'Detection surface'),
            downloadButton('downloadContPlot', 'Detection contour'),
            downloadButton('downloadDetPlot', 'Detection function'),
            numericInput("call.num", "Choose call number to display in estimated location plot:",
                         min = 1, max = 1000,step = 1,
                         value = 1),
            actionButton("reset_locplot", "Reset location plot",icon("refresh")),
            numericInput("distD", "Choose distance at which to plot distance error distribution (m):",
                         min = 1, max = 10000,step = 1,
                         value = 1),
            downloadButton('downloadbearingPlot', 'Bearing distribution (rad)'),
            downloadButton('downloaddistancePlot', 'Distance distribution (m)'),
            ## Other stuff
            h3(icon("ellipsis-h"),tags$b("Other")),
            numericInput("anispeed","Animation frame rate for report (s)",
                         min = 0.1,max = 5,step = 0.1,
                         value = 1),
            downloadButton("report", "Model report"),
            hidden(p(id = "proc_report", "Processing report...")),
            
            actionButton("reset_input", "Reset sidebar",icon("refresh")),
            actionButton("close", "Shut down",icon("power-off")),
            checkboxInput("advanced","Advanced options"),
            conditionalPanel(
                condition = "input.advanced == true",
                checkboxGroupInput("advancedOptions", "Advanced options",
                                   choices = list("increase mask buffer"  = "inc",
                                                  "choose parameter starting values" = "sv",
                                                  "build finer mask for plotting" = "fine"),inline = TRUE),
                conditionalPanel(
                    condition = "input.advancedOptions.includes('sv')",
                    uiOutput("startParamSelection"),
                    uiOutput("svg0"), ## chose g0 sv
                    uiOutput("svsigma"), ## chose sigma sv
                    uiOutput("svz"), ## chose z sv
                    uiOutput("svshape"), ## chose shape sv
                    uiOutput("svscale"), ## chose scale sv
                    uiOutput("svshape.1"), ## chose shape.1 sv
                    uiOutput("svshape.2") ## chose shape.2 sv
                ),
                conditionalPanel(
                    condition = "input.advancedOptions.includes('inc')",
                    numericInput("incmaskbuffer","Chose higher bound for the mask buffer",
                                 min = 1, max = 10000000,step = 1,
                                 value = 1000)
                ),
                conditionalPanel(
                    condition = "input.advancedOptions.includes('fine')",
                    numericInput("plotmaskspacing","Chose mask spacing (plotting purposes only)",
                                 min = 1, max = 10000000,step = 1,
                                 value = 250)
                ),
                downloadButton('downloadModel', 'Save model .RData file')
            )
        ),
        ## Main panel for displaying outputs ----
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel(h4(icon("pencil"), tags$b("Details")),
                                 tabsetPanel(
                                     tabPanel(h4(icon("pencil-square-o"),tags$b("How-to")),
                                              includeMarkdown(file.path("text", "details.md"))),
                                     tabPanel(h4(icon("info-circle"),tags$b("ascr")),
                                              includeMarkdown(file.path("text", "ascr.md")))
                                     )),
                                 
                        tabPanel( h4(icon("bar-chart"), tags$b("Data")),
                                 tabsetPanel(
                                     tabPanel(h5(icon("map-marker"), tags$b("Traps")),
                                              fluidRow(
                                                  column(width = 4,
                                                         h4(icon("table"),"Raw data"),
                                                         tableOutput("traps")),
                                                  column(width = 4, 
                                                         h4(icon("map-marker"),"Trap locations"),
                                                         plotOutput("trapsPlot"))
                                              )),
                                     tabPanel(h5(icon("map-pin"), tags$b("Detections")),
                                              fluidRow(
                                                  column(width = 4,
                                                         h4(icon("table"),"Raw data"),
                                                         tableOutput("detections")),
                                                  column(width = 4, 
                                                         h4(icon("map-pin"),"Capture history matrix"),
                                                         tableOutput("capt.hist")))),
                                     tabPanel(h5(icon("map-signs"),tags$b("Traps & detections")),
                                              column(width = 12, align = "center",
                                                     plotOutput( height = "700px",width = "700px","show")))
                                 )),
                        tabPanel(h4(icon("puzzle-piece"), tags$b("Mask")),
                                 fluidRow(
                                     column(width = 12, align="center",
                                            textOutput("maskinfo")
                                         )),
                                     column(width = 12, align="center",
                                            withSpinner(plotOutput("maskPlot"),type = 5,color = "#D3D3D3"))
                                ),
                        tabPanel(h4(icon("cogs"), tags$b("Model")),
                                 tabsetPanel(
                                     tabPanel(h5(icon("area-chart"), tags$b("Output")),
                                              fluidRow(
                                                  column(width = 3,
                                                         fluidRow(
                                                             h4(icon("pencil-square"),"Parameter estimates"),
                                                             withSpinner(tableOutput("coefs"),type = 5,color = "#D3D3D3")),
                                                         fluidRow(
                                                             h4(icon("volume-control-phone"),"Density")),
                                                             withSpinner(tableOutput("denst"),type = 5,color = "#D3D3D3")
                                                         ),
                                                  column(width = 3,
                                                         h4(icon("info-circle"),"Model info"),
                                                         withSpinner(tableOutput("AIClL"),type = 5,color = "#D3D3D3")),
                                                  column(width = 6,
                                                         h4(icon("line-chart"),"Detection function"),
                                                         withSpinner(plotOutput("detfn"),type = 5,color = "#D3D3D3"))
                                              ),
                                              fluidRow(
                                                  h4(icon("area-chart"),"Detection surface"),
                                                  withSpinner(plotOutput("detectionsurf"),type = 5,color = "#D3D3D3")
                                              ),
                                              fluidRow(class = "locs",
                                                       h4(icon("map-signs"),
                                                          "Location estimates (interactive plot---drag and double-click to zoom)"),
                                                       column(12,
                                                              withSpinner(plotOutput("locs", height = "700px",
                                                                                     width = "700px",
                                                                                     dblclick = "locsplot_dblclick",
                                                                                     brush = brushOpts(
                                                                                         id = "locsplot_brush",
                                                                                         resetOnNew = TRUE)),
                                                                          type = 5,color = "#D3D3D3"),
                                                              tags$head(tags$style(".locs{height:750px}"))
                                                       )),
                                              fluidRow(
                                                  h4(icon("line-chart"),"Measurement error distributions"),
                                                  column(6, align="center",
                                                         withSpinner(plotOutput("bearing_pdf"),type = 5,color = "#D3D3D3")
                                                         ),
                                                  column(6, align="center",
                                                         withSpinner(plotOutput("distance_pdf"),type = 5,color = "#D3D3D3")
                                                         )
                                              )),
                                     tabPanel(h5(icon("paper-plane"), tags$b("R messages")),
                                              pre(id = "console")
                                              ))
                                 )
                        )
        )
    )
))
 
