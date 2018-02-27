shinyServer(function(input, output) {
 
 output$cbdMap0   <- renderLeaflet(cbdMap0Leaflet(input$myLHJ,  input$myCAUSE, input$myMeasure, input$myYear, input$myCon, input$myGeo                              ))  
 output$cbdMap1   <- renderPlot(   cbdMap0(       input$myLHJX, input$myCAUSE, input$myMeasure, input$myYear, input$myCon, input$myGeo, input$cZoom,input$myLabName ))
 output$rankCause <- renderPlot(   rankCause(     input$myLHJ,                 input$myMeasure, input$myYear,                           input$myN                   ))
 output$rankGeo   <- renderPlot(   rankGeo(       input$myLHJX, input$myCAUSE, input$myMeasure, input$myYear,                           input$gZoom,input$myCI      ))
 output$trend     <- renderPlot(   trend(         input$myLHJ,  input$myCAUSE, input$myMeasure                                                                      ))
 
 output$map_title <- renderUI({
                                HTML(paste("<div style='text-align:center;font-size:18px'>",
                                     names(lMeasures[lMeasures==input$myMeasure])," - ",causeList36[causeList36[,1]==input$myCAUSE,2],
                                     "('zoom' map still in testing)",
                                     "</div>", sep = "") ) })
    })
