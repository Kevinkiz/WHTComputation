#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("WHT Computation"),
  
  selectInput("WHT", "Tax Head", 
              c("Sec 83.Tax on international payments by Non-Resident(15%)" = "Sec 83.Tax on international payments by Non-Resident(15%)", 
                "Interest paid by a resident company in respect of debentures[If;1.Issued for Rasing Capital outside Ug.2.widely Issued.3.Paid outside Ug.(0%)" = "Interest paid by a resident company in respect of debentures[If;1.Issued for Rasing Capital outside Ug.2.widely Issued.3.Paid outside Ug.(0%)", 
                "Sec 84.payments to non-resident public entertainers or sports persons(15%)" = "Sec 84.payments to non-resident public entertainers or sports persons(15%)", 
                "Sec 85.payments to non-resident contractors or professionals(15%)" = "Sec 85.payments to non-resident contractors or professionals(15%)",
                "Sec.86.Non-residents providing shipping, air transport or tele-communications services(2%)" = "Sec.86.Non-residents providing shipping, air transport or tele-communications services(2%)",
                "Non-resident carries on business of transmitting messages by cable,radio,optical fibre,satellite,or internet(5%)" = "Non-resident carries on business of transmitting messages by cable,radio,optical fibre,satellite,or internet(5%)",
                "Sec 117,118.Interest & Dividends Paid to resident Person(15%)" = "Sec 117,118.Interest & Dividends Paid to resident Person(15%)",
                "Interest on Government Securities(15%)" = "Interest on Government Securities(15%)",
                "Dividend Payment to Individuals from Listed Companies on the Stock exchange(10%)" = "Dividend Payment to Individuals from Listed Companies on the Stock exchange(10%)",
                "Interest Paid on Govt Securities to Resd. Person(20%)" = "Interest Paid on Govt Securities to Resd. Person(20%)",
                "Sec119.Payments for Goods & services > 1million or Contract & Sec118A.Professional Fees(6%)" = "Sec119.Payments for Goods & services > 1million or Contract & Sec118A.Professional Fees(6%)",
                "Sec118B.Purchase of an Asset(10%)" = "Sec118B.Purchase of an Asset(10%)",
                "Sec118C. Payments for Winnings of Betting or Gaming(15%)" = "Sec118C. Payments for Winnings of Betting or Gaming(15%)",
                "Sec118D. Re-Insurance Premiums(10%)" = "Sec118D. Re-Insurance Premiums(10%)",
                "Agricultural Supplies(0%)" = "Agricultural Supplies(0%)",
                "Sec118F.Commission paid by telecom companies for airtime distribution & Mobile money(10%)" = "Sec118F.Commission paid by telecom companies for airtime distribution & Mobile money(10%)",
                "Sec118G & Sec118H Commision paid to Insurance & Advertising Agent(10%)" = "Sec118G & Sec118H Commision paid to Insurance & Advertising Agent(10%)",
                "opt5" = "opt4",
                "opt5" = "opt5")),
  numericInput("Amount", "Gross Amount", 0),
  
  textOutput("result")
  
)

server <- function(input, output) {
  
  result <- reactive({
    if(input$WHT == "Sec 83.Tax on international payments by Non-Resident(15%)"){
      input$Amount * 0.15
    } else if(input$WHT == "Interest paid by a resident company in respect of debentures[If;1.Issued for Rasing Capital outside Ug.2.widely Issued.3.Paid outside Ug.(0%)"){
      input$Amount * 0.0
    } else if(input$WHT == "Sec 84.payments to non-resident public entertainers or sports persons(15%)"){
      input$Amount * 0.15
    } else if(input$WHT == "Sec 85.payments to non-resident contractors or professionals(15%)"){
      input$Amount * 0.15
    } else if(input$WHT == "Sec.86.Non-residents providing shipping, air transport or tele-communications services(2%)"){
      input$Amount * 0.02
     } else if(input$WHT == "Non-resident carries on business of transmitting messages by cable,radio,optical fibre,satellite,or internet(5%)"){
        input$Amount * 0.05
     } else if(input$WHT == "Sec 117,118.Interest & Dividends Paid to resident Person(15%)"){
       input$Amount * 0.15
     } else if(input$WHT == "Interest on Government Securities(15%)"){
       input$Amount * 0.15
     } else if(input$WHT == "Dividend Payment to Individuals from Listed Companies on the Stock exchange(10%)"){
       input$Amount * 0.1
     } else if(input$WHT == "Interest Paid on Govt Securities to Resd. Person(20%)"){
       input$Amount * 0.2
     } else if(input$WHT == "Sec119.Payments for Goods & services > 1million or Contract & Sec118A.Professional Fees(6%)"){
       input$Amount * 0.06
     } else if(input$WHT == "Sec118B.Purchase of an Asset(10%)"){
       input$Amount * 0.1
     } else if(input$WHT == "Sec118C. Payments for Winnings of Betting or Gaming(15%)"){
       input$Amount * 0.15
     } else if(input$WHT == "Sec118D. Re-Insurance Premiums(10%)"){
       input$Amount * 0.1
     } else if(input$WHT == "Agricultural Supplies(0%)"){
       input$Amount * 0.00
     } else if(input$WHT == "Sec118F.Commission paid by telecom companies for airtime distribution & Mobile money(10%)"){
       input$Amount * 0.1
     } else if(input$WHT == "Sec118G & Sec118H Commision paid to Insurance & Advertising Agent(10%)"){
       input$Amount * 0.1
     } else if(input$WHT == "opt4"){
       input$Amount * 0.15
     } else if(input$WHT == "opt5"){
       input$Amount * 0.15
    }
  })
  
  output$result <- renderText({
    paste("The calculated tax is:","SHS", format(round(result(), 2),scientific=FALSE,big.mark=",",decimal.mark="."))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

