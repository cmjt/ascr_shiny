

withConsoleRedirect <- function(containerId, expr) {
  ## Change type="output" to type="message" to catch stderr
  ## (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "message")
  if (length(txt) > 0) {
    insertUI(paste0("##", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}

shinyServer(function(input, output,session) {
    ## read in input data
     traps <- reactive({
        if(input$example == TRUE){
            
            traps <- shiny_example_traps
            
        }else{
            req(input$file1)
            traps <- read.csv(input$file1$datapath,
                              header = input$header,
                              sep = input$sep,
                              quote = input$quote)
            
            validate(need("x" %in% names(traps) & "y" %in% names(traps) & "post" %in% names(traps),
                          "Trap file must contain columns named x, y, and post"))
            validate(need(!("array" %in% names(traps) & length(table(traps$array)) > 1),
                          "It seems you have multiple arrays, this software currently doesn't support this"))
            validate(need(class(traps$post)=="integer", "Please give post ID as a whole number"))
            validate(need(class(traps$x)%in%c("integer","numeric"), "Please ensure x coordinate is numeric"))
            validate(need(class(traps$y)%in%c("integer","numeric"), "Please ensure y coordinate is numeric"))
            return(traps)
        } 
     })
    detections <- reactive({
        if("simple" %in% input$which_example & input$example == TRUE){
            
            detections <- shiny_example_detections[,1:3]
            disable("bearing_range")
            hide("bearing_range")
            return(detections)
        }else{
            if("bearings" %in% input$which_example & input$example == TRUE){
                
                detections <- shiny_example_detections[,1:4]
                enable("bearing_range")
                shinyjs::show("bearing_range")
                return(detections)
            }else{
                if("distance" %in% input$which_example & input$example == TRUE){
                    
                    detections <- shiny_example_detections[,-4]
                    disable("bearing_range")
                    hide("bearing_range")
                    return(detections)
                }else{
                    if("bd" %in% input$which_example & input$example == TRUE){
                        
                        detections <- shiny_example_detections
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
                        ## checking the same group was not heard on by same trap more than once on the same occasion
                        can1 <- 1/2 * (as.numeric(detections$post) +
                                       detections$group)* (as.numeric(detections$post) +
                                                           detections$group + 1) + detections$group
                        validate(need(
                            length(
                                table(1/2 *(can1 + detections$occasion) *(can1 + detections$occasion + 1) +
                                      detections$occasion)) == nrow(detections),
                            "Detections were made more than once by some traps on the same occasion. CHECK DATA"))
                        validate(need(class(detections$group) == "integer", "Please give group ID as a whole number"))
                        validate(need(class(detections$occasion) == "integer", "Please give occasion ID as a whole number"))
                        validate(need(class(detections$post) == "integer", "Please give post ID as a whole number"))
                        if("bearing" %in% names(detections)){
                            validate(need(class(detections$bearing) == "numeric", "Please make sure all bearings are numeric"))
                            enable("bearing_range")
                            shinyjs::show("bearing_range")
                        }else{
                            disable("bearing_range")
                            hide("bearing_range")
                        }
                        if("distance" %in% names(detections)){
                            validate(need(class(detections$distance) == "numeric", "Please make sure all distances are numeric"))
                            
                        }
                        return(detections)
                    }
                }
            }
        }
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
            enable("which_example")
            shinyjs::show("which_example")
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
            disable("which_example")
            hide("which_example")
        }
        ## code to produce downloadable objects (i.e., plots and report)
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
            plot(traps$x,traps$y,asp = 1,type = "n",xlab = "Longitude",ylab = "Latitude")
            text(traps$x,traps$y,traps$post,lwd = 2)
        }else{
            plot(traps$x,traps$y,asp = 1,pch = 4,cex = 2,lwd = 3,xlab = "Longitude",ylab = "Latitude")
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

    output$capt.hist <- renderTable({
        detections <- detections()
        capt.hist <- get.capt.hist(detections)
        colnames(capt.hist[[1]]) <- names(table(detections$post))
        rownames(capt.hist[[1]]) <- unique(paste("occasion",detections$occasion, "group", detections$group))
        if(input$disp == "head") {
            return(head(capt.hist[[1]]))
        }else{
            return(capt.hist[[1]])
        }
    },striped = TRUE,rownames = TRUE,colnames = TRUE,digits = 0)
    ## chage buffer slider based on trap range
    ## chage spacing slider based on trap range
    observe({
        infile <- traps() ## user input file upload
        if(!is.null(infile)) {
            traps <- traps()
            maxdistance <- diff(range(traps$x,traps$y))/4
            updateSliderInput(session, "spacing", max = maxdistance, value = maxdistance/2)
            maxdistance <- 4*diff(range(traps$x,traps$y))
            updateSliderInput(session, "buffer", max = maxdistance,value = maxdistance/2) 
        }
    })
    ## show all plot for raw data
    output$show <- renderPlot({
        detections <- detections()
        traps <- traps()
        capt.hist <- get.capt.hist(detections)
        validate(need(input$show.call.num,"Please provide a call number"))
        validate(need(input$show.call.num <= nrow(capt.hist$bincapt),"Please provide a valid call number"))
        show.data(traps, capt.hist,id = input$show.call.num)
        legend("top",legend = paste("call",input$show.call.num,sep = " "),bty = "n")
    })
    ## change buffer sliding in advanced increase buffer option chosen
    observe({
        if("inc" %in% input$advancedOptions) {
        maxdistance <- input$incmaskbuffer
        updateSliderInput(session, "buffer", max = maxdistance,value = maxdistance/2)
        }
    })
    ## plot of mask 
    output$maskPlot <- renderPlot({
        
        traps <- traps()
        
        traps <- as.matrix(cbind(traps$x,traps$y))
        validate(need(input$buffer > input$spacing,"The mask buffer cannot be less than the spacing"))
        validate(need(input$buffer/input$spacing < 80, "Infeasibly fine mask"))
        mask <- create.mask(traps,input$buffer,input$spacing)
        show.mask(mask,traps)
        
    },width = 500, height = 500)
    ## print out mask buffet info
    output$maskinfo <- renderText({
        paste("This mask is assuming that a distance of ",input$buffer, "meters is the maximum distance at which a detection is feasibly possible")
    })
    ## chose which parameters of which detection function to fit, conditional numeric input for fixing param values
    output$fixedParamSelection <- renderUI({
        params.fix <- cbind(c("g0","sigma","g0","sigma","z","shape","scale"),
                            c("hn","hn","hr","hr","hr","th","th"))
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
    output$fixedshape <- renderUI({
        conditionalPanel(condition = "input.parameter.includes('shape')",       
                         numericInput("shape","fix shape to:",value = 1,min = 1,max = 100,step = 1)
                         )
    })

    output$startParamSelection <- renderUI({
        params.fix <- cbind(c("g0","sigma","g0","sigma","z","shape","scale"),
                            c("hn","hn","hr","hr","hr","th","th"))
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
        
        traps <- as.matrix(cbind(traps$x,traps$y))
        mask <- create.mask(traps,input$buffer,input$spacing)
        nms <- names(detections)
        
        capt.hist <- get.capt.hist(detections)
        ## fixed values
        param.fix <- input$parameter
        param.fix.value <- list(g0 = input$g0,sigma = input$sigma,z = input$z,shape = input$shape,
                                scale = input$scale, shape.1 = input$shape.1,shape.2 = input$shape.2)
        idx <- match(param.fix,names(param.fix.value))
        fix <- param.fix.value[idx]
        ## starting values
        param.sv <- input$parset
        param.sv.value <- list(g0 = input$svg0,sigma = input$svsigma,z = input$svz,svshape = input$svshape,
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
        withConsoleRedirect("console", {
            fit <-  tryCatch({
                fit.ascr(capt = capt.hist,traps = traps,mask = mask,detfn =  input$select,
                         fix = fix, sv = sv,trace = TRUE) 
            },warning = function(w) print("fit.ascr convergence issues"))
        })
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
            res <- data.frame(Estimate = summary(fit)$coefs,Std.Error = summary(fit)$coefs.se)
            rownames(res) <- names(coef(fit))
            return(res)
        }
    },rownames = TRUE)
    ## density
     output$denst <- renderTable({
        fit <- fit()
        if(class(fit)[1]=="ascr"){
            res <- rbind( fit$coefficients["D"], fit$coefficients["D"]/0.01)
            rownames(res) <- c("per hectare",   "per squared km")
            colnames(res) <- "Call density"
            return(res)
        }
    },rownames = TRUE,colnames = TRUE)
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
            show.detsurf(fit)
            show.detsurf(fit,surface = FALSE)    
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
            buffer <- attr(get.mask(fit), "buffer")
            probs <- calc.detfn(buffer, detfn = detfn, pars = pars,ss.link =fit$args$ss.opts$ss.link)
            show.detfn(fit)
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
        if(class(fit)[1]=="ascr"){
            validate(need(input$call.num,"Please provide a call number"))
            validate(need(input$call.num <= nrow(fit$args$capt$bincapt),"Please provide a valid call number"))
            if("fine" %in% input$advancedOptions & is.null(ranges$x)){
                traps <- traps()
                validate(need(input$plotmaskspacing,
                              "Please provide a spacing for the (plotting) mask or uncheck this option"))
                validate(need(input$plotmaskspacing > 0,
                              "Cannnot have a spacing of zero meters"))
                validate(need(input$buffer > input$plotmaskspacing,
                              "The mask buffer cannot be less than the (plotting) mask spacing"))
                validate(need(input$plotmaskspacing < input$spacing,
                              "To obtain a smooth plot the (plotting) mask spacing should be finer than the model fit mask "))
                msk <- create.mask(traps,input$buffer,input$plotmaskspacing)
                locations(fit,input$call.num,mask = msk)
                legend("top",legend = paste("call",input$call.num,sep = " "),bty = "n")
            }else{
                if(!is.null(ranges$x) & !("fine" %in% input$advancedOptions)){
                    locations(fit,input$call.num,xlim = ranges$x, ylim = ranges$y)
                    legend("top",legend = paste("call",input$call.num,sep = " "),bty = "n")
                }else{
                    if("fine" %in% input$advancedOptions & !is.null(ranges$x)){
                        traps <- traps()
                        validate(need(input$plotmaskspacing,
                                      "Please provide a spacing for the (plotting) mask or uncheck this option"))
                        validate(need(input$plotmaskspacing > 0,
                                      "Cannnot have a spacing of zero meters"))
                        validate(need(input$buffer > input$plotmaskspacing,
                                      "The mask buffer cannot be less than the (plotting) mask spacing"))
                        validate(need(input$plotmaskspacing < input$spacing,
                                      "To obtain a smooth plot the (plotting) mask spacing should be finer than the model fit mask "))
                        msk <- create.mask(traps,input$buffer,input$plotmaskspacing)
                        locations(fit,input$call.num,mask = msk,xlim = ranges$x, ylim = ranges$y)
                        legend("top",legend = paste("call",input$call.num,sep = " "),bty = "n")
                    }else{
                        locations(fit,input$call.num)
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
        validate(need(!is.null(fit$args$capt$bearing),"No bearing data provided"))
        show.dvm(fit)

    })
    output$distance_pdf <- renderPlot({
        fit <- fit()
        validate(need(!is.null(fit$args$capt$dist),"No distance data provided"))
        validate(need(!(input$distD == 0), "Distance cannot be zero"))
        validate(need(!is.null(input$distD), "Please provide distance for measurement error distribution"))
        validate(need(input$distD < max(fit$args$capt$dist),"Distance cannot be greater than those observed"))
        show.distgam(fit, d = input$distD)
    })
        
    output$downloadMask <- downloadHandler(
      filename = "ascrMask.png",
      content = function(file) {
          png(file)
          
          traps <- traps()
          traps <- as.matrix(cbind(traps$x,traps$y))
          mask <- create.mask(traps,input$buffer,input$spacing)
          show.mask(mask,traps)
          dev.off()
      })
    output$downloadSurfPlot <- downloadHandler(
        filename = "ascr_detection_surface_plot.png",
        content = function(file) {
            
            png(file)
            fit <- fit()
            if(class(fit)[1]=="ascr"){
                show.detsurf(fit)
            }else{
                NULL
            }
            dev.off()
        })
    output$downloadContPlot <- downloadHandler(
        filename = "ascr_detection_contour_plot.png",
        content = function(file) {
            png(file)
            fit <- fit()
            if(class(fit)[1]=="ascr"){
                show.detsurf(fit,surface = FALSE)
            }else{
                NULL
            }
            dev.off()
        })
    output$downloadDetPlot <- downloadHandler(
        filename = "ascr_detection_function_plot.png",
        content = function(file) {
            png(file)
            fit <- fit()
            if(class(fit)[1]=="ascr"){
                show.detfn(fit)
            }else{
                NULL
            }
            dev.off()
        })
    ## deal with bearing and distance plots
    observe({
        fit <- fit()
        if(is.null(fit$args$capt$dist)){
            disable("downloaddistancePlot")
            disable("distD")
        }else{
            enable("downloaddistancePlot")
            enable("distD")
        }
        if(is.null(fit$args$capt$bearing)){
            disable("downloadbearingPlot")
        }else{
            enable("downloadbearingPlot")
            }
        })
    output$downloadbearingPlot <- downloadHandler(
        filename = "ascr_bearing_distribution_plot.png",
        content = function(file) {
            png(file)
            fit <- fit()
            if(class(fit)[1]=="ascr"){  
                show.dvm(fit)
            }else{
                NULL
            }
            dev.off()
        })
    output$downloaddistancePlot <- downloadHandler(
        filename = "ascr_distance_distribution_plot.png",
        content = function(file) {
            png(file)
            fit <- fit()
            if(class(fit)[1]=="ascr"){  
               show.distgam(fit, d = input$distD)
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
          
        filename = "report.html",
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
            
            ## Copy the report file to a temporary directory before processing it, in
            ## case we don't have write permissions to the current working dir (which
            ## can happen when deployed).
                             
            tempReport <- file.path(tempdir(), "report.Rmd")
            file.copy("report.Rmd", tempReport, overwrite = TRUE)
                             
            ## Set up parameters to pass to Rmd document
            params <- list(buffer = input$buffer,
                           spacing = input$spacing,
                           fit = fit(),
                           anispeed = input$anispeed,
                           dist = input$distD)
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
    
