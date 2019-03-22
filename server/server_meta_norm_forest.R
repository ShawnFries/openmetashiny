if(input$metric1 == "PLO") {
  forest(res, transf=transf.ilogit, targs=list(ni=vals$data$ni), refline=NA, digits=input$digits, level=conflevel)
} else if (input$metric1 == "PAS") {
  forest(res, transf=transf.isqrt, targs=list(ni=vals$data$ni), refline=NA, digits=input$digits, level=conflevel)
} else if (input$metric1 == "PR") {
  forest(res, refline=NA, digits=input$digits, level=conflevel)
}