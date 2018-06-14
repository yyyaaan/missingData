library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(purrr)
library(matrixcalc)
library(DT)
library(dplyr)

# global var --------------------------------------------------------------

myColors<- setNames(c('#66c2a5','#fc8d62','#8da0cb','#e78ac3','#a6d854','#ffd92f'), 
                    c('Distribution 1', 'Distribution 2', 'Theoretic mu 1',
                      'Theoretic mu 2', 'Trace of mu 1', 'Trace of mu 2'))
orgDF <- readRDS("sampleData.rds"); genDF <- NULL; mainDF <- NULL;
sim <- NULL; init <- NULL; p3d <- NULL


shinyServer(function(input, output, session) {

# generate missing data ---------------------------------------------------

  
  observeEvent({input$missType; input$missPercent},{
    genList <- createData(orgDF, input$missType, input$missPercent / 100)
    genDF <<- genList$generatedData
    
    output$valueBoxA <- renderValueBox({
      valueBox( nrow(genDF), "total observations",
                icon = icon("bars"), color = "red")
    })
    output$valueBoxB <- renderValueBox({
      valueBox( sum(!is.na(genDF$Total)), "complete cases",
                icon = icon("info-circle"), color = "yellow")
    })
    output$valueBoxC <- renderValueBox({
      valueBox( paste(round(mean(genDF$Total, na.rm = T), digits = 2), 
                      round(mean(orgDF$Total), digits = 2), sep = " / "),
                "mean: CC vs. Truth",
                icon = icon("dashboard"), color = "aqua")
    })    
    output$valueBoxD <- renderValueBox({
      valueBox( paste(round(sd(genDF$Total, na.rm = T), digits = 2), 
                      round(sd(orgDF$Total), digits = 2), sep = " / "),
                "sd: CC vs. Truth",
                icon = icon("calculator"), color = "blue")
    })    
  })


# imputation --------------------------------------------------------------
  
  observeEvent(input$runImputation,{
    impList <- imputointi(genDF, input$imputeMethod, input$imputeN)

    output$missPlot <- renderPlot(impList$impPlot)
  
    resultDT <- datatable(round(tableIMP(impList, orgDF), digits = 3),
                          options = list(dom = "t", rownames = FALSE))
    output$missTable <- renderDataTable(resultDT)
    
    # set a global var for plot
    impDF <- impList$impDATA
    missId <- which(is.na(genDF$Total))
    plotDF <- as.data.frame(matrix(nrow = nrow(orgDF), ncol = ncol(impDF)))
    plotDF[missId,1:ncol(impDF)] <- impDF
    plotDF$age <- genDF$Age
    plotDF$days <- genDF$Days
    plotDF$ObsTotal <- genDF$Total
    plotDF$real <- NA
    plotDF$real[missId] <- orgDF$Total[missId]
    mainDF <<- tidyr::gather(plotDF, "key", "total", -age, -days)
    
    updatePickerInput(session, "plotItems", choices = unique(mainDF$key), selected = c("ObsTotal", "real","V1"))
  })
  
  observeEvent({input$showGroup; input$plotItems},{
    if(!is.null(mainDF)){
       plotData <- mainDF %>% filter(key %in% input$plotItems)
       p <- ggplot(plotData, aes(days, total, color = key)) + geom_point()
       print(head(plotData)); print(input$plotItems)
       if(input$showGroup) p <- p + facet_wrap(~age)
       
       output$missMain <- renderPlotly((p))
    }
  })
  

# EM: generate samples ----------------------------------------------------
  
  output$emInfo <- renderUI(p('Note: The initial value is generated automatically by including an error defined by InitValue Error'))

  observeEvent(input$genSim,{
    sigma1 <- matrix(c(input$sigma1.11, input$sigma1.21, input$sigma1.31,
                       input$sigma1.21, input$sigma1.22, input$sigma1.32,
                       input$sigma1.31, input$sigma1.32, input$sigma1.33),
                       ncol = 3, nrow = 3)
    sigma2 <- matrix(c(input$sigma2.11, input$sigma2.21, input$sigma2.31,
                       input$sigma2.21, input$sigma2.22, input$sigma2.32,
                       input$sigma2.31, input$sigma2.32, input$sigma2.33),
                     ncol = 3, nrow = 3)
    mu1 <- c(input$mean1x, input$mean1y, input$mean1z)
    mu2 <- c(input$mean2x, input$mean2y, input$mean2z)
    
    if(is.positive.semi.definite(sigma1) & is.positive.semi.definite(sigma2)){
      
      sim <<- genData(input$sizeN, mu1, mu2, sigma1, sigma2, input$lambda/100)
      
      init <<- randomInit(mu1, mu2, sigma1, sigma2, input$lambda/100, input$initMag)
      print(input$lambda)
      
      p3d <<- plot_ly(sim$df) %>% 
        add_markers(x = ~X1, y= ~X2, z= ~X3, color = ~isFirst, colors = myColors,
                    marker = list(opacity = 0.6, symbol = "circle-open-dot", size = 3)) %>%
        add_markers(x = mu1[1], y = mu1[2], z = mu1[3], color = "Theoretic mu 1",
                    marker = list(opacity =1, symbol = "diamond", size = 5))%>%
        add_markers(x = mu2[1], y = mu2[2], z = mu2[3], color = "Theoretic mu 2",
                    marker = list( symbol = "diamond", size = 5))

      output$plot3d <- renderPlotly(p3d)
      
      output$emPrint <- renderPrint({
        print("Samples Generated and Plotted")
        print(list(mu1 = mu1, mu2 = mu2, sigma1 = sigma1, sigma2 = sigma2))
      })
      
    } else {
      output$emInfo <- renderUI(tagList(tags$b("ERROR: "), p("please change covariance | must be positively semi-definite")))
      output$emPrint <- renderPrint({
        print("covariance matrix error")
        print("nothing updated")
      })
    }
    
  })

# EM: do the algorithm ----------------------------------------------------

  observeEvent(input$runEM, {
    
    if(!is.null(sim)){
      em <- myEM(init, sim$data, maxiter = input$maxiter)
      
      tr.mu11 <- em$all %>% map(1) %>% map_dbl(1)
      tr.mu12 <- em$all %>% map(1) %>% map_dbl(2)
      tr.mu13 <- em$all %>% map(1) %>% map_dbl(3)
      tr.mu21 <- em$all %>% map(2) %>% map_dbl(1)
      tr.mu22 <- em$all %>% map(2) %>% map_dbl(2)
      tr.mu23 <- em$all %>% map(2) %>% map_dbl(3)
      
      p3d <<- p3d %>% 
        add_markers(x = tr.mu11, y = tr.mu12, z = tr.mu13, color = "Trace of mu 1",
                    marker = list( symbol = "cross", size = 5)) %>%
        add_markers(x = tr.mu21, y = tr.mu22, z = tr.mu23, color = "Trace of mu 2",
                    marker = list( symbol = "cross", size = 5))
      
      output$emInfo <- renderUI(tagList(
        p("EM result after", length(tr.mu11), "iterations", br(),
          "Covergence reached?", !em$reachMax, br(),
          "convergence is set to 1e-12")
      ))
      
      output$emPrint <- renderPrint({
        print(em$result)
        print("=========")
        print("initial values are")
        print(init)
      })
      
      output$plot3d <- renderPlotly(p3d)
    } else {
      output$emInfo <- renderUI(tagList(tags$b("ERROR: "), p("please click Generate Data first")))
      output$emPrint <- renderPrint({
        print("no data available")
      })
    }
    
    
  })
})
