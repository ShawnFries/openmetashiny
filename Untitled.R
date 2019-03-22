ui <- function(){
  
  bootstrapPage('',
                
                navbarPage(title = 'Hello'),
                
                tags$style(type = 'text/css', '.navbar { background-color: #262626;
                           font-family: Arial;
                           font-size: 13px;
                           color: #FF0000; }',
                           
                           '.navbar-dropdown { background-color: #262626;
                           font-family: Arial;
                           font-size: 13px;
                           color: #FF0000; }'))
  
}

server <- function(input, output, session){
}


shinyApp(ui = ui, server = server)









##rhandsontable
DF_na = data.frame(integer = c(NA, 2:10), 
                   logical = c(NA, rep(TRUE, 9)), 
                   character = c(NA, LETTERS[1:9]),
                   factor = c(NA, factor(letters[1:9])),
                   date = c(NA, seq(from = Sys.Date(), by = "days", 
                                    length.out = 9)),
                   stringsAsFactors = FALSE)

DF_na$factor_ch = as.character(DF_na$factor)
DF_na$date_ch = c(NA, as.character(seq(from = Sys.Date(), by = "days", 
                                       length.out = 9)))

rhandsontable(DF_na, width = 550, height = 300)










# ###save a png plot
# png(filename="/Users/sibyl/Downloads/p1.png", width=8.27, height=11.69, units ="in", res = 210)
# forest(res, showweight = T, addfit= T, cex = .9) 
# #text(-1.6, 18, "Author(s) (Year)", pos=4)     
# #text( 1.6, 18, "Correlation [95% CI]", pos=2)
# dev.off()



