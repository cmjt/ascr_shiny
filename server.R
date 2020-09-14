options(shiny.maxRequestSize=30*1024^2) ## increase file upload to 30MB
shinyServer(function(input, output,session) {
    ## initiate trap type
    trapType <- "single"
    ## read in input data
     traps <- reactive({
         if(input$example == TRUE){
             if(input$trapType_ex== "single"){
                 load("shiny_example_traps.RData")
                 traps <- shiny_example_traps
                 traps
             }else{
                 if(input$trapType_ex== "multi"){
                     load("shiny_multi_traps.RData")
                     traps <- shiny_multi_traps
                     traps
                 }
             }
         }else{
             req(input$file1)
             traps <- read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep,
                               quote = input$quote)
             
             validate(need("x" %in% names(traps) & "y" %in% names(traps) & "post" %in% names(traps),
                           "Trap file must contain columns named x, y, and post"))
             validate(need(class(traps$post)=="integer", "Please give post ID as a whole number"))
             validate(need(class(traps$x)%in%c("integer","numeric"), "Please ensure x coordinate is numeric"))
             validate(need(class(traps$y)%in%c("integer","numeric"), "Please ensure y coordinate is numeric"))
             return(traps)
         } 
     })
    detections <- reactive({
        if("simple" %in% input$which_example & input$example == TRUE & input$trapType_ex== "single"){
            load("shiny_example_detections.RData")
            detections <- shiny_example_detections[,1:3]
            disable("bearing_range")
            hide("bearing_range")
            return(detections)
        }else{
            if("bearings" %in% input$which_example & input$example == TRUE & input$trapType_ex== "single"){
                load("shiny_example_detections.RData")
                detections <- shiny_example_detections[,1:4]
                enable("bearing_range")
                shinyjs::show("bearing_range")
                return(detections)
            }else{
                if("distance" %in% input$which_example & input$example == TRUE & input$trapType_ex == "single"){
                    load("shiny_example_detections.RData")
                    detections <- shiny_example_detections[,-4]
                    disable("bearing_range")
                    hide("bearing_range")
                    return(detections)
                }else{
                    if("bd" %in% input$which_example & input$example == TRUE & input$trapType_ex == "single"){
                        load("shiny_example_detections.RData")
                        detections <- shiny_example_detections
                        enable("bearing_range")
                        shinyjs::show("bearing_range")
                        return(detections)
                    }else{
                        if("simple" %in% input$which_example_multi& input$example == TRUE & input$trapType_ex== "multi"){
                            load("shiny_multi_detections.RData")
                            detections <- shiny_multi_detections[,1:4]
                            disable("bearing_range")
                            hide("bearing_range")
                            return(detections)
                        }else{
                            if("bearings" %in% input$which_example_multi & input$example == TRUE & input$trapType_ex== "multi"){
                                load("shiny_multi_detections.RData")
                                detections <- shiny_multi_detections[,1:5]
                                enable("bearing_range")
                                shinyjs::show("bearing_range")
                                return(detections)
                            }else{
                                req(input$file2)
                                detections <- read.csv(input$file2$datapath,
                                                       header = input$header,
                                                       sep = input$sep,
                                                       quote = input$quote)
                                
                                validate(need("occasion" %in% names(detections) & "group" %in% names(detections) &
                                              "post" %in% names(detections),
                                              "Detections file must contain columns named occasion, group, and post"))
                                validate(need(class(detections$group) == "integer", "Please give group ID as a whole number"))
                                validate(need(class(detections$occasion) == "integer", "Please give occasion ID as a whole number"))
                                validate(need(class(detections$post) == "integer", "Please give post ID as a whole number"))
                                if("bearing" %in% names(detections)){
                                    validate(need(class(detections$bearing) == "numeric" | class(detections$bearing) == "integer", "Please make sure all bearings are numeric"))
                                    enable("bearing_range")
                                    shinyjs::show("bearing_range")
                                }else{
                                    disable("bearing_range")
                                    hide("bearing_range")
                                }
                                if("distance" %in% names(detections)){
                                    validate(need(class(detections$distance) == "numeric" | class(detections$distance) == "integer" , "Please make sure all distances are numeric"))
                                    
                                }
                                return(detections)
                            }
                        }
                    }
                }
            }
        }
    })
    ## single/multi variable
    trapType <- reactive({
        if("array"%in%names(detections()) & "array"%in%names(traps()) & input$trapType_ex == "multi"){
            trapType <- "multi"
        }else{
            if( "array"%in%names(detections()) & "array"%in%names(traps()) & input$trapType_us == "multi"){
                trapType <- "multi"
            }else{
                trapType <- "single"
            }
        }
    })
    ## covariates
    covariates <- reactive({
        if("yes"%in%input$example_covariates){
            lst <-  list(covariate = raster::raster("example_raster.tif"))
        }else{
            req(input$covs)
            files <- input$covs
            lst <- list()
            for(i in 1:length(files[,1])){
                lst[[i]] <- raster::raster(files[[i,"datapath"]])
            }
            names(lst) <- unlist(strsplit(files[,1],".tif"))
        }
        lst
        })
    output$cov.list <- renderPlot({
        req(covariates())
        covariates <- covariates()
        covs <- lapply(covariates,function(x) gplot(x) +
                                              geom_tile(aes(fill = value)) +
                                              xlab("x-axis") + ylab("y-axis") +
                                              theme(panel.background = element_blank(),
                                                    panel.border = element_rect(colour = "black", fill=NA, size=1)))
        for(i in 1:length(covariates)){covs[[i]] <- covs[[i]] + ggtitle(names(covariates)[i])}
        do.call(grid.arrange,covs)
    })
    ## covariate buttons
    output$covariate_controls <- renderUI({
        req(covariates())
        covariates <- covariates()
        checkboxGroupInput("covariate.choose", "Choose covariates to include in your model",
                           names(covariates))
    })
    ## covariate factor buttons
    output$cov_factor <- renderUI({
        req(covariates())
        req(input$covariate.choose)
        which.covariates <- input$covariate.choose
        covariates <- covariates()
        covariates <- covariates[which.covariates]
        checkboxGroupInput("covariate.factor", "Which covariates are factor covariates (tick for yes)",
                           names(covariates))
        })
    cov.use <- reactive({
        req(covariates())
        req(input$covariate.choose)
        req(mask())
        mask <- mask()
        if(class(mask) == "list"){
            msk.locs <- lapply(mask, function(x) cbind(x[,1],x[,2]))
            which.covariates <- input$covariate.choose
            covariates <- covariates()
            covariates <- covariates[which.covariates]
            if(names(covariates) %in% input$covariate.factor){
                covariates[[input$covariate.factor]] <- as.factor(covariates[[input$covariate.factor]])
                levels(covariates[[input$covariate.factor]])[[1]]$category <- as.factor(levels(covariates[[input$covariate.factor]])[[1]]$ID)
            }
            covariates.use <- list()
            for(i in 1:length(msk.locs)){
                tmp <- lapply(covariates,function(x) if(is.factor(x)){
                                                         factorValues(x,extract(x,msk.locs[[i]]))
                                                     }else{
                                                         data.frame(extract(x,msk.locs[[i]]))
                                                     }
                              )
                covariates.use[[i]] <- as.data.frame(tmp)
                names(covariates.use[[i]]) <- names(covariates)
            }
            return(covariates.use)
        }else{
            msk.locs <- cbind(mask[,1],mask[,2])
            which.covariates <- input$covariate.choose
            covariates <- covariates()
            covariates <- covariates[which.covariates]
            covariates.use <- lapply(covariates, function(x) if(is.factor(x)){
                                                                 factorValues(x,extract(x,msk.locs))
                                                             }else{
                                                                 extract(x,msk.locs)
                                                             }
                                     )
            names(covariates.use) <- names(covariates)
            return(as.data.frame(covariates.use))
        }
    })
    ## which array raw
    output$which_array_raw <- renderUI({
        detections <- detections()
        traps <- traps()
        validate(need(!is.null(traps),""))
        validate(need(!is.null(detections),""))
        validate(need("array"%in%names(detections),""))
        validate(need("array"%in%names(traps),""))
        validate(need(trapType() == "multi",""))
        arrs <- as.numeric(names(table(traps$array)))
        mn <- min(arrs)
        mx <- max(arrs)
        numericInput("choose_trap_raw", "Choose trap array for plot",
                     min = mn, max = mx,
                     value = mn)
    })
    
    ## which array capt
    output$which_array_capt <- renderUI({
        detections <- detections()
        traps <- traps()
        validate(need(!is.null(traps),""))
        validate(need(!is.null(detections),""))
        validate(need("array"%in%names(detections),""))
        validate(need("array"%in%names(traps),""))
        validate(need(trapType() == "multi",""))
        arrs <- as.numeric(names(table(traps$array)))
        mn <- min(arrs)
        mx <- max(arrs)
        numericInput("choose_trap_capt", "Choose trap array for capture history matrix",
                     min = mn, max = mx,
                     value = mn)
    })
    ## which array output plots
    output$which_array <- renderUI({
        detections <- detections()
        traps <- traps()
        validate(need(!is.null(traps),""))
        validate(need(!is.null(detections),""))
        validate(need("array"%in%names(detections),""))
        validate(need("array"%in%names(traps),""))
        validate(need(trapType() == "multi",""))
        ## validate(need(length(table(traps$array))==length(table(detections$array)),"Need equal number of arrays in detection file as in trap file"))
        arrs <- as.numeric(names(table(traps$array)))
        mn <- min(arrs)
        mx <- max(arrs)
        numericInput("choose_trap", "Choose trap array for output plots",
                     min = mn, max = mx,
                     value = mn)
    })
    ## Clunky way of enabling/disabling buttons
    observe({
        if(input$example == TRUE){
            disable("file1")
            disable("file2")
            disable("header")
            disable("sep")
            disable("quote")
            hide("file1")
            hide("file2")
            hide("header")
            hide("sep")
            hide("quote")
            enable("example_covariates")
            shinyjs::show("example_covariates")
        }else{
            enable("file1")
            enable("file2")
            enable("header")
            enable("sep")
            enable("quote")
            shinyjs::show("file1")
            shinyjs::show("file2")
            shinyjs::show("header")
            shinyjs::show("sep")
            shinyjs::show("quote")
            disable("which_example_multi")
            shinyjs::hide("which_example_multi")
            disable("which_example")
            shinyjs::hide("which_example")
            disable("example_covariates")
            shinyjs::hide("example_covariates")
        }
        if(trapType() == "single" & input$example == TRUE){
            enable("which_example")
            enable("example_covariates")
            shinyjs::show("example_covariates")
            shinyjs::show("which_example")
            disable("which_example_multi")
            shinyjs::hide("which_example_multi")
        }
        if(trapType() == "multi" & input$example == TRUE){
            disable("example_covariates")
            shinyjs::hide("example_covariates")
            disable("which_example")
            shinyjs::hide("which_example")
            enable("which_example_multi")
            shinyjs::show("which_example_multi")
        }
        ## initislly disable some options if no model fitted
        observeEvent(!input$fit,{
            disable("downloadSurfPlot")
            disable("downloadContPlot")
            disable("downloadDetPlot")
            disable("call.num")
            disable("reset_locplot")
            disable("distD")
            disable("downloadbearingPlot")
            disable("downloaddistancePlot")
            disable("anispeed")
            disable("report")
        })
        observeEvent(input$fit,{
            enable("downloadSurfPlot")
            enable("downloadContPlot")
            enable("downloadDetPlot")
            enable("call.num")
            enable("reset_locplot")
            enable("distD")
            enable("downloadbearingPlot")
            enable("downloaddistancePlot")
            enable("anispeed")
            enable("report")
        })
        
        if(input$example == FALSE | isTruthy(input$file1) == FALSE){
            disable("downloadMask")
            disable("buffer")
            disable("spacing")
        }
        if(isTruthy(input$file1) == TRUE | input$example == TRUE){
            enable("downloadMask")
            enable("buffer")
            enable("spacing") 
        }
        if(input$example == FALSE | isTruthy(input$file1) == FALSE & isTruthy(input$file2) == FALSE){
            disable("select")
            disable("fixedParamSelection")
            disable("fit")
            hide("bearing_range")
        }
        if(input$example == TRUE | isTruthy(input$file1) == TRUE & isTruthy(input$file2) == TRUE){
            enable("select")
            enable("fixedParamSelection")
            enable("fit")
            shinyjs::show("bearing_range")
            }
    })
    
    ## output trap locations
    output$traps <- renderTable({
        traps <- traps()
        if(input$disp == "head") {
            return(head(traps))
        }else{
            return(traps)
        }

    },
    striped = TRUE)
    ## code to plot trap locations
    output$trapsPlot <- renderPlot({
        traps <- traps()
        if(!is.null(traps$post)){
            plot(traps$x,traps$y,asp = 1,type = "n",xlab = "x-axis",ylab = "y-axis")
            text(traps$x,traps$y,traps$post,lwd = 2)
        }else{
            plot(traps$x,traps$y,asp = 1,pch = 4,cex = 2,lwd = 3,xlab = "x-axis",ylab = "y-axis")
        }
    })
    
    output$detections <- renderTable({
        detections <- detections()
        if(input$disp == "head") {
            return(head(detections))
        }else{
            return(detections)
        }   
    },
    striped = TRUE)

    capthist <- reactive({
        detections <- detections()
        traps <- traps()
        validate(need(!is.null(traps),""))
        validate(need(!is.null(detections),""))
        if(trapType() == "multi"){
            traps <- split(traps, traps$array)
            capt.hist <- get.capt.hist(detections, traps = traps)
            for(i in 1:length(capt.hist)){
                colnames(capt.hist[[i]][[1]]) <- paste(1:ncol(capt.hist[[i]][[1]]))
                n.row <- nrow(capt.hist[[i]][[1]])
                if(n.row != 0 ){
                    oc <- detections$occasion[detections$array == i]
                    grp <- detections$group[detections$array == i]
                    rownames(capt.hist[[i]][[1]]) <- unique(paste("occasion",oc, "group",
                                                                  grp))
                }
            }
        }else{
            validate(need(length(table(traps$array))==length(table(detections$array)),
                          "Need equal number of arrays in detection file as in trap file"))
            validate(need(trapType() == "single",""))
            capt.hist <- get.capt.hist(detections, traps = traps)
            colnames(capt.hist[[1]]) <- paste(1:ncol(capt.hist[[1]]))
            rownames(capt.hist[[1]]) <- unique(paste("occasion",detections$occasion, "group", detections$group))
        }
        return(capt.hist)
    })

    output$capt.hist <- renderTable({
        capthist <- capthist()
        traps <- traps()
        validate(need(!is.null(capthist),""))
        if(trapType() == "single"){
            if(input$disp == "head") {
                return(head(capthist[[1]]))
            }else{
                return(capthist[[1]])
            }   
        }else{
            if(trapType() == "multi"){
                if(input$disp == "head") {
                    validate(need(input$choose_trap <= max(as.numeric(names(table(traps$array)))),"Please provide valid array"))
                    return(head(capthist[[input$choose_trap_capt]][[1]]))
                }else{
                    validate(need(input$choose_trap <= max(as.numeric(names(table(traps$array)))),"Please provide valid array"))
                    try(capthist[[input$choose_trap_capt]][[1]],silent = TRUE)
                }  
                
            }
        }
    },striped = TRUE,rownames = TRUE,colnames = TRUE,digits = 0)
    ## chage buffer slider based on trap range
    ## chage spacing slider based on trap range
    observe({
        if(!is.null(traps())) {
            traps <- traps()
            detections <- detections()
            if(trapType() == "single"){
                maxdistance <- diff(range(traps$x,traps$y))/4
                updateNumericInput(session, "spacing", max = maxdistance, value = maxdistance/2)
                maxdistance <- 4*diff(range(traps$x,traps$y))
                updateNumericInput(session, "buffer", max = maxdistance,value = maxdistance/2) 
            }else{
                validate(need("array"%in%names(detections),""))
                validate(need("array"%in%names(traps),""))
                traps <- split(traps, traps$array)
                maxdistance <- diff(range(traps[[1]]$x,traps[[1]]$y))/4
                updateNumericInput(session, "spacing", max = maxdistance, value = maxdistance/2)
                maxdistance <- 4*diff(range(traps[[1]]$x,traps[[1]]$y))
                updateNumericInput(session, "buffer", max = maxdistance,value = maxdistance/2) 
            }   
        }
    })
    ## show all plot for raw data
    output$show <- renderPlot({
        traps <- traps()
            capt.hist <- capthist()
            if(trapType() == "single"){
                validate(need(input$show.call.num,"Please provide a call number"))
                validate(need(input$show.call.num <= nrow(capt.hist$bincapt),"Please provide a valid call number"))
                show.data(traps, capt.hist,id = input$show.call.num)
                legend("top",legend = paste("call",input$show.call.num,sep = " "),bty = "n")
            }else{
                validate(need(trapType() == "multi",""))
                traps <- split(traps, traps$array)
                validate(need(input$show.call.num,"Please provide a call number"))
                try(show.data(traps[[input$choose_trap_raw]], capt.hist[[input$choose_trap_raw]],id = input$show.call.num),silent = TRUE)
                legend("top",legend = paste("array", input$choose_trap_raw, " call",input$show.call.num,sep = " "),bty = "n")
            }
    })
    ## change buffer sliding in advanced increase buffer option chosen
    observe({
        if("inc" %in% input$advancedOptions) {
        maxdistance <- input$incmaskbuffer
        updateSliderInput(session, "buffer", max = maxdistance,value = maxdistance/2)
        }
    })
    ## plot of mask
    mask <- eventReactive(input$msk,{
        shinyjs::show("processing_msk") ## stuff to disable fitting button
        traps <- traps()
        validate(need(!is.null(traps),""))
        validate(need(input$buffer > input$spacing,"The mask buffer cannot be less than the spacing"))
        validate(need(input$buffer/input$spacing < 80, "Infeasibly fine mask"))
        if(trapType() == "single"){
            traps <- as.matrix(cbind(traps$x,traps$y))
            mask <- create.mask(traps,input$buffer,input$spacing)
        }else{
            traps <- split(traps, traps$array)
            traps <- lapply(traps,function(x) cbind(x$x,x$y))
            mask <- lapply(traps,create.mask,buffer = input$buffer,spacing = input$spacing)
        }
        hide("processing_msk")
        enable("fit")
        return(mask)
    })
    output$maskPlot <- renderPlot({
        traps <- traps()
        mask <- mask()
        if(trapType() == "single"){
            grid.arrange(show.mask(mask,traps))
        }else{
            traps <- split(traps, traps$array)
            validate(need(is.list(mask), ""))
            m.lst <- list()
            for(i in 1:length(traps)){ m.lst[[i]] <- show.mask(mask[[i]], traps = traps[[i]])}
            do.call(grid.arrange, m.lst)
        }
        })
    ## print out mask buffer info
    output$maskinfo <- renderText({
        validate(need(!is.null(mask()),""))
        paste("This mask is assuming that a distance of ",input$buffer,
              "meters is the maximum distance at which a detection is feasibly possible")
    })
    ## choose which parameters of which detection function to fit, conditional numeric input for fixing param values
    output$fixedParamSelection <- renderUI({
        params.fix <- cbind(c("g0","sigma","g0","sigma","z","lambda0","sigma","shape","scale"),
                            c("hn","hn","hr","hr","hr","hhn","hhn","th","th"))
        checkboxGroupInput("parameter", "Fix which parameters:",
                           choices = as.character(params.fix[params.fix[,2] == input$select,1]),inline = TRUE)
       
    })
    output$fixedg0 <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('g0')",       
                         numericInput("g0","fix g0 to:",value = 1,min = 1,max = 100,step = 1)
                         )
    })
    output$fixedsigma <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('sigma')",       
                         numericInput("sigma","fix sigma to:",value = 1,min = 1,max = 100,step = 1)
                         )
    })
    output$fixedz <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('z')",       
                         numericInput("z","fix z to:",value = 1,min = 1,max = 100,step = 1)
                         )
    })
    output$fixedlambda0 <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('lambda0')",       
                         numericInput("lambda0","fix lambda0 to:",value = 1,min = 1,max = 100,step = 1)
                         )
    })
    output$fixedshape <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('shape')",       
                         numericInput("shape","fix shape to:",value = 1,min = 1,max = 100,step = 1)
                         )
    })

    output$startParamSelection <- renderUI({
        params.fix <- cbind(c("g0","sigma","g0","sigma","z","lambda0","sigma","shape","scale"),
                            c("hn","hn","hr","hr","hr","hhn","hhn","th","th"))
        checkboxGroupInput("parset", "Set starting values for which parameters:",
                           choices = as.character(params.fix[params.fix[,2] == input$select,1]),inline = TRUE)
       
    })
    output$svg0 <- renderUI({
        conditionalPanel(condition = "input.parset.includes('g0') && !input.parameter.includes('g0')",
                         numericInput("svg0","g0 start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) ## set starting value of g0 ensure it isn't already fixed
     output$svsigma <- renderUI({
        conditionalPanel(condition = "input.parset.includes('sigma') && !input.parameter.includes('sigma')",
                         numericInput("svsigma","sigma start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) ## set starting value of sigma ensure it isn't already fixed
    output$svz <- renderUI({
        conditionalPanel(condition = "input.parset.includes('z') && !input.parameter.includes('z')",
                         numericInput("svz","z start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) ## set starting value of z ensure it isn't already fixed
    output$svlambda0 <- renderUI({
        conditionalPanel(condition = "input.parset.includes('lambda0') && !input.parameter.includes('lambda0')",
                         numericInput("svlambda0","lambda0 start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) ## set starting value of lambda0 ensure it isn't already fixed
    output$svshape <- renderUI({
        conditionalPanel(condition = "input.parset.includes('shape') && !input.parameter.includes('shape')",
                         numericInput("svshape","shape start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) ## set starting value of shape ensure it isn't already fixed
    output$svscale <- renderUI({
        conditionalPanel(condition = "input.parset.includes('scale') && !input.parameter.includes('scale')",
                         numericInput("svscale","scale start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) ## set starting value of scale ensure it isn't already fixed

    output$svshape.1 <- renderUI({
        conditionalPanel(condition = "input.parset.includes('shape.1') && !input.parameter.includes('shape.1')",
                         numericInput("svshape.1","shape.1 start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) ## set starting value of shape.1 ensure it isn't already fixed
    output$svshape.2 <- renderUI({
        conditionalPanel(condition = "input.parset.includes('shape.2') && !input.parameter.includes('shape.2')",
                         numericInput("svshape.2","shape.2 start value:",value = 1,min = 1,max = 100,step = 1)
                         )              
    }) ## set starting value of shape.2 ensure it isn't already fixed
    
    
    ## Fit model based on inputs of user and output parameter estimates and plots
    
    
    fit <- eventReactive(input$fit,{
        detections <- detections()
        traps <- traps()
        if("bearing" %in% names(detections)){
            validate(need(detections$bearing >= 0 & detections$bearing <= 2*pi |
                          "bd" %in% input$bearing_range,
                          "Your bearing measurements are outside the range of radians, please indicate correct measurement in the sidebar."))
            
            if("bd" %in% input$bearing_range){
                detections$bearing <- (2*pi/360)*detections$bearing
            }
        }
        
        if(trapType() == "single"){
            traps <- as.matrix(cbind(traps$x,traps$y)) 
        }else{
            traps <- split(traps, traps$array)
            traps <- lapply(traps,function(x) cbind(x$x,x$y))  
        }
        mask <- mask()
        validate(need(!is.null(mask), "Please construct mask"))
        nms <- names(detections)
        capt.hist <- capthist()
        validate(need(!is.null(capt.hist), "No capture hstory information"))
        if(trapType() == "multi"){
            validate(need(length(capt.hist) == length(mask), "Please construct mask for your loaded traps"))
        }
        ## fixed values
        param.fix <- input$parameter
        param.fix.value <- list(g0 = input$g0,sigma = input$sigma,z = input$z,lambda0 = input$lambda0,shape = input$shape,
                                scale = input$scale, shape.1 = input$shape.1,shape.2 = input$shape.2)
        idx <- match(param.fix,names(param.fix.value))
        fix <- param.fix.value[idx]
        ## starting values
        param.sv <- input$parset
        param.sv.value <- list(g0 = input$svg0,sigma = input$svsigma,z = input$svz,lambda0 = input$svlambda0,
                               svshape = input$svshape,
                               scale = input$svscale, shape.1 = input$svshape.1,shape.2 = input$svshape.2)
        idsv <- match(param.sv,names(param.sv.value))
        sv <- param.sv.value[idsv]
        fit <- NULL
        disable("downloadSurfPlot")
        disable("downloadContPlot")
        disable("downloadDetPlot")
        disable("downloadbearingPlot")
        disable("downloaddistancePlot")
        disable("downloadMask")
        disable("downloadModel")
        disable("anispeed")
        disable("report")
        disable("fit")
        disable("side-panel")
        shinyjs::show("processing") ## stuff to disable fitting button
        if(!is.null(input$covariate.choose)){
            ihd <- list(model = as.formula(paste("~",
                                                 paste(input$covariate.choose,
                                                       collapse = " + ", sep = " "))),
                        covariates = cov.use())
            fit <-  fit.ascr(capt = capt.hist,traps = traps,mask = mask,detfn =  input$select,
                             fix = fix, sv = sv,trace = TRUE, ihd.opts = ihd)
        }else{
            fit <-  fit.ascr(capt = capt.hist,traps = traps,mask = mask,detfn =  input$select,
                             fix = fix, sv = sv,trace = TRUE)
        }
        enable("fit")
        enable("side-panel")
        enable("downloadSurfPlot")
        enable("downloadContPlot")
        enable("downloadDetPlot")
        enable("downloadbearingPlot")
        enable("downloaddistancePlot")
        enable("anispeed")
        enable("report")
        enable("downloadMask")
        enable("downloadModel")
        hide("processing")
        return(fit)
    })
    ## coefficients
    output$coefs <- renderTable({
        fit <- fit()
        if(class(fit)[1]=="ascr"){
            ci <- confint(fit)
            res <- data.frame(Estimate = summary(fit)$coefs,Std.Error = summary(fit)$coefs.se, "2.5%" = ci[,1], "97.5%" = ci[,2] )
            if(res$Estimate[1] < 0.01){res <- matrix(apply(res,1,formatC, format = "e",digits = 2),ncol = 4, byrow = TRUE)}
            rownames(res) <- names(coef(fit))
            colnames(res) <- c("Estimate", "Std.Error", "2.5% Cl", "97.5% Cl")
            return(res)
        }
    },rownames = TRUE,digits = 3)
    ## AIC and log Likelihood
    output$AIClL <- renderTable({
        fit <- fit()
        if(class(fit)[1]=="ascr"){
            tab <- rbind(AIC = AIC(fit),logLik = fit$loglik)
            colnames(tab) <- "value"
            return(tab)
        }
    },rownames = TRUE)
    ## Detection function plots and location estimate plots
    
    output$detectionsurf <- renderPlot({
        fit <- fit()
        if(class(fit)[1] == "ascr"){
            par(mfrow = c(1,2))
            show.detsurf(fit,session = input$choose_trap)
            show.detsurf(fit,session = input$choose_trap, surface = FALSE)    
        }else{
            plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
            text(1,1,paste("convergence issues try advanced options"),col = "grey")
        }
    })
    output$detfn <- renderPlot({
        fit <- fit()
        if(class(fit)[1]=="ascr"){
            detfn <- fit$args$detfn
            pars <- get.par(fit, pars = fit$detpars, cutoff = fit$fit.types["ss"],as.list = TRUE)
            buffer <- attr(get.mask(fit,session = input$choose_trap), "buffer")
            probs <- ascr:::calc.detfn(buffer, detfn = detfn, pars = pars,ss.link =fit$args$ss.opts$ss.link)
            ascr:::show.detfn(fit)
            if(probs >= 0.1){
                legend("center", bty = "n",paste("The detection probability at the mask buffer of ", buffer, "m is", round(probs,3), "(i.e., non-zero), perhaps increase mask buffer."),cex = 0.7,text.col = "red")
            }
            }else{
                plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
                text(1,1,paste("convergence issues try advanced options"),col = "grey")
            }
    })
    ## When a double-click happens, check if there's a brush on the plot.
    ## If so, zoom to the brush bounds; if not, reset the zoom.
    ranges <- reactiveValues(x = NULL, y = NULL)
    observeEvent(input$locsplot_dblclick, {
        brush <- input$locsplot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
    
    observeEvent(input$reset_locplot,{
        ranges$x <- NULL
        ranges$y <- NULL
    })
                    
    output$locs <- renderPlot({
        fit <- fit()
        mask <- mask()
        if(trapType() == "single"){
            msk <- mask()
        }else{
            msk <- mask[[input$choose_trap]]
        }
        
        if(class(fit)[1]=="ascr"){
            validate(need(input$call.num,"Please provide a call number"))
            validate(need(input$call.num <= nrow(fit$args$capt[[1]]$bincapt),"Please provide a valid call number"))
            if("fine" %in% input$advancedOptions & is.null(ranges$x)){
                validate(need(input$plotmaskspacing,
                              "Please provide a spacing for the (plotting) mask or uncheck this option"))
                validate(need(input$plotmaskspacing > 0,
                              "Cannnot have a spacing of zero meters"))
                validate(need(input$buffer > input$plotmaskspacing,
                              "The mask buffer cannot be less than the (plotting) mask spacing"))
                validate(need(input$plotmaskspacing < input$spacing,
                              "To obtain a smooth plot the (plotting) mask spacing should be finer than the model fit mask "))
                locations(fit,input$call.num,mask = msk)
                legend("top",legend = paste("call",input$call.num,sep = " "),bty = "n")
            }else{
                if(!is.null(ranges$x) & !("fine" %in% input$advancedOptions)){
                    locations(fit,input$call.num,xlim = ranges$x, ylim = ranges$y)
                    legend("top",legend = paste("call",input$call.num,sep = " "),bty = "n")
                }else{
                    if("fine" %in% input$advancedOptions & !is.null(ranges$x)){
                        validate(need(input$plotmaskspacing,
                                      "Please provide a spacing for the (plotting) mask or uncheck this option"))
                        validate(need(input$plotmaskspacing > 0,
                                      "Cannnot have a spacing of zero meters"))
                        validate(need(input$buffer > input$plotmaskspacing,
                                      "The mask buffer cannot be less than the (plotting) mask spacing"))
                        validate(need(input$plotmaskspacing < input$spacing,
                                      "To obtain a smooth plot the (plotting) mask spacing should be finer than the model fit mask ")) 
                        if(trapType() == "single"){
                            locations(fit,input$call.num,mask = msk,xlim = ranges$x, ylim = ranges$y)
                        }else{
                            locations(fit,session = input$choose_trap,input$call.num,mask = msk,xlim = ranges$x, ylim = ranges$y)
                            title(main = paste("array",input$choose_trap))
                        }
                        
                        legend("top",legend = paste("call",input$call.num,sep = " "),bty = "n")
                    }else{
                        if(trapType() == "single"){
                            locations(fit, input$call.num)
                        }else{
                            locations(fit,session = input$choose_trap, input$call.num)
                            title(main = paste("array",input$choose_trap))
                        }
                        
                        legend("top",legend = paste("call",input$call.num,sep = " "),bty = "n")
                    }
                }
            }
        }else{
            plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
            text(1,1,paste("Convergence issues try advanced options"),col = "grey")
        }
    },width = 700,height = 700)
   
        
    
    ## Measurement error plots
    output$bearing_pdf <- renderPlot({
        fit <- fit()
        validate(need(!is.null(fit$args$capt[[1]]$bearing),"No bearing data provided"))
        kappa = fit$coefficients["kappa"]
        if(trapType() == "single"){
            theta = sort(fit$args$capt[[1]]$bearing - pi)
            theta = seq(min(theta), max(theta), length.out = 1000)
            show.dvm(theta = theta, kappa = kappa)
        }else{
            theta = sort(fit$args$capt[[input$choose_trap]]$bearing - pi)
            theta = seq(min(theta), max(theta), length.out = 1000)
            show.dvm(theta = theta, kappa = kappa)
            title(paste("array", input$choose_trap))
            }
    })
    output$distance_pdf <- renderPlot({
        fit <- fit()
        validate(need(!is.null(fit$args$capt[[1]]$dist),"No distance data provided"))
        validate(need(!(input$distD == 0), "Distance cannot be zero"))
        validate(need(!is.null(input$distD), "Please provide distance for measurement error distribution"))
        validate(need(input$distD < max(fit$args$capt[[1]]$dist),"Distance cannot be greater than those observed"))
        d <- input$distD
        shape <- fit$coefficients["alpha"]
        if(trapType() == "single"){
            x <- sort(fit$args$capt[[1]]$dist)
            show.distgam(x = x, shape = shape, d = d)
        }else{
            x <- sort(fit$args$capt[[input$choose_trap]]$dist)
            show.distgam(x = x, shape = shape, d = d)
            title(paste("array", input$choose_trap))
        }
    })
    ## inhomogeneous density plot
    output$density_surface <- renderPlot({
        req(!is.null(input$covariate.choose))
        show.Dsurf(fit(), session = input$choose_trap)
        title(main = paste("array",input$choose_trap))
    })
    ## Downloads
    output$downloaddensity_surfPlot<- downloadHandler(
        filename = "ascr_density_surface.png",
        content = function(file) {
            req(!is.null(input$covariate.choose))
            png(file)
            show.Dsurf(fit(), session = input$choose_trap)
            title(main = paste("array",input$choose_trap))
            dev.off()
        })
    output$downloadMask <- downloadHandler(
        filename = "ascrMask.png",
        content = function(file) {
            png(file)
            traps <- traps()
            mask <- mask()
            if(trapType() == "single"){
                show.mask(mask,traps)
            }else{
                traps <- split(traps, traps$array)
                m.lst <- list()
                for(i in 1:length(traps)){ m.lst[[i]] <- show.mask(mask[[i]], traps = traps[[i]])}
              do.call(grid.arrange, m.lst)
            }
            dev.off()
        })
    output$downloadSurfPlot <- downloadHandler(
        filename = "ascr_detection_surface_plot.png",
        content = function(file) {
            png(file)
            fit <- fit()
            if(class(fit)[1] == "ascr"){
                par(mfrow = c(1,2))
                show.detsurf(fit,session = input$choose_trap)
                show.detsurf(fit,,session = input$choose_trap, surface = TRUE)    
            }else{
                plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
                text(1,1,paste("convergence issues try advanced options"),col = "grey")
            }
            dev.off()
        })
    output$downloadContPlot <- downloadHandler(
        filename = "ascr_detection_contour_plot.png",
        content = function(file) {
            png(file)
            fit <- fit()
            if(class(fit)[1] == "ascr"){
                par(mfrow = c(1,2))
                show.detsurf(fit,session = input$choose_trap)
                show.detsurf(fit,,session = input$choose_trap, surface = FALSE)    
            }else{
                plot(1,1,col="white",axes = FALSE,xlab = "",ylab = "")
                text(1,1,paste("convergence issues try advanced options"),col = "grey")
            }
            dev.off()
        })
    output$downloadDetPlot <- downloadHandler(
        filename = "ascr_detection_function_plot.png",
        content = function(file) {
            png(file)
            fit <- fit()
            if(class(fit)[1]=="ascr"){
                ascr:::show.detfn(fit)
            }else{
                NULL
            }
            dev.off()
        })
    ## deal with bearing and distance plots
    observe({
        fit <- fit()
        if(is.null(fit$args$capt[[1]]$dist)){
            disable("downloaddistancePlot")
            disable("distD")
        }else{
            enable("downloaddistancePlot")
            enable("distD")
        }
        if(is.null(fit$args$capt[[1]]$bearing)){
            disable("downloadbearingPlot")
        }else{
            enable("downloadbearingPlot")
            }
        })
    output$downloadbearingPlot <- downloadHandler(
        filename = "ascr_bearing_distribution_plot.png",
        content = function(file) {
            fit <- fit()
            validate(need(!is.null(fit$args$capt[[1]]$bearing),"No bearing data provided"))
            png(file) 
            if(class(fit)[1]=="ascr"){ 
                kappa = fit$coefficients["kappa"]
                if(trapType() == "single"){
                    theta = sort(fit$args$capt[[1]]$bearing - pi)
                    show.dvm(theta = theta, kappa = kappa)
                }else{
                    theta = sort(fit$args$capt[[input$choose_trap]]$bearing - pi)
                    show.dvm(theta = theta, kappa = kappa)
                    title(paste("array", input$choose_trap))
                }
            }else{
                NULL
            }
            dev.off()
        })
    output$downloaddistancePlot <- downloadHandler(
        filename = "ascr_distance_distribution_plot.png",
        content = function(file) {
            fit <- fit()
            validate(need(!is.null(fit$args$capt[[1]]$dist),"No distance data provided"))
            png(file)
            if(class(fit)[1]=="ascr"){
                d <- input$distD
                shape <- fit$coefficients["alpha"]
                if(trapType() == "single"){
                    x <- sort(fit$args$capt[[1]]$dist)
                    show.distgam(x = x, shape = shape, d = d)
                }else{
                    x <- sort(fit$args$capt[[input$choose_trap]]$dist)
                    show.distgam(x = x, shape = shape, d = d)
                    title(paste("array", input$choose_trap))
                }
            }else{
                NULL
            }
            dev.off()
        })
    
    output$downloadModel <- downloadHandler(
        filename = paste("ascr_",date(),".RData",sep = ""),
        content = function(file){
            fit <- fit()
            save(fit,file = file)
        }
    )
    output$report <- downloadHandler(
        filename = "animation.html",
        content = function(file) {
            disable("downloadSurfPlot")
            disable("downloadContPlot")
            disable("downloadDetPlot")
            disable("downloadMask")
            disable("downloadModel")
            disable("side-panel")
            disable("downloadbearingPlot")
            disable("downloaddistancePlot")
            disable("anispeed")
            disable("report")
            shinyjs::show("proc_report")
            fit <- fit()
            array <- ifelse(!is.null(input$choose_trap), input$choose_trap, 1)
            ## Copy the report file to a temporary directory before processing it, in
            ## case we don't have write permissions to the current working dir (which
            ## can happen when deployed).
            ## go to a temp dir to avoid permission issues
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
            ## Set up parameters to pass to Rmd document
            params <- list(fit = fit(),
                           anispeed = input$anispeed,
                           array = array)
            ## Knit the document, passing in the `params` list, and eval it in a
            ## child of the global environment (this isolates the code in the document
            ## from the code in this app).
            render(tempReport, output_file = file,
                   params = params,
                   envir = new.env(parent = globalenv())
                   )
            enable("downloadSurfPlot")
            enable("downloadContPlot")
            enable("downloadDetPlot")
            enable("side-panel")
            enable("downloadbearingPlot")
            enable("downloaddistancePlot")
            enable("anispeed")
            enable("downloadMask")
            enable("downloadModel")
            enable("report")
            hide("proc_report")
        })
    observeEvent(input$reset_input, {
        updateSliderInput(session, "spacing", max = 1000, value = 250)
        updateSliderInput(session, "buffer", max =10000, value = 1000)
        updateCheckboxInput(session, "example",  value = FALSE)
        reset("side-panel")
    })
    observe({
        if (input$close > 0) {
            stopApp()
            }
    })
    session$onSessionEnded(stopApp)
})
    
