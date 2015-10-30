
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(
  titlePanel("Hi. Using this page you can generate 'Product labels' and 'BUlk Upload Shipment Creation CSV Files' for Amazon FBA and Flipkart FA"),
  sidebarLayout(
    sidebarPanel(
      a( "For Flipkart visit this link. Download the file and change the quantity in the file.", href = "https://seller.flipkart.com/sfx/listings/download?sellerId=ia2tdh26i36gcw7a" ),
      br(),
      a( "For Amazon use the master.csv file you already have. You can also download it from here.", href = "www.google.com" ),
      br(),
      fileInput('file1', 'After making quantity changes to the desired file. Save it and upload it here.',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain',
                  '.csv',
                  '.tsv'
                )
      ),
      br(),
      textInput( "monthImport", label = "Enter The month of Import. Example: 'Oct 2015'" ),
      br(),
      radioButtons("radio", label = "Amazon or Flipkart?",
                   choices = list( "Amazon" = 1, "Flipkart" = 2 ), selected = 1 ),
      br(),
      p("Second: Click on the below button and wait untill you see DONE " ),
      actionButton("begin", "Start Generating labels and shipment creation file"),
      verbatimTextOutput("status"),
      p("Third: Once you see DONE, click on the 'DOWNLOAD PDF' button to download the product labels " ),
      downloadButton('downloadpdf', 'Download Product Labels PDF'),
      p("Click on the 'DOWNLOAD CSV' button to download the CSV. You can upload this file on the respective portal to create the shipment" ),
      downloadButton('downloadcsv', 'Download Shipment Creation CSV'),
      br(),
      a( "Sipment creation link of Flipkart", href = "https://seller.flipkart.com/index.html#dashboard/fa-services?inFrameSrc=%2Fsfx%23%2Fconsignments%2Fsaved%2Fwarehouse%3FwarehouseId%3Dhyderabad_medchal_01" ),
      br(),
      a( "Shipment creation link of AMazon", href = "https://sellercentral.amazon.in/gp/ssof/workflow/upload/upload-create-plan.html/" ),
      br()
    ),
    mainPanel(
      dataTableOutput('contents')
      #dataTableOutput('ex1')
      
    )
  )
))
