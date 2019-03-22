if(input$metric1=="PLO"){
  forest(res, transf=transf.ilogit, targs=list(ni=vals$data$ni), 
         xlim=c(0,1), refline=NA, digits=input$digits, level=conflevel)
}else if(input$metric1=="PAS"){
  forest(res, transf=transf.isqrt, targs=list(ni=vals$data$ni), 
         xlim=c(0,1), refline=NA, digits=input$digits, level=conflevel)
}else if(input$metric1=="PR"){
  forest(res, xlim=c(0,1), refline=NA, digits=input$digits, level=conflevel)
}