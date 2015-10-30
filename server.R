
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

options( shiny.maxRequestSize = 9 * 1024 ^ 2 )

shinyServer(function(input, output, session) {
  output$contents <- renderDataTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1
    if( is.null( inFile ) )
      return( NULL )
    read.csv( inFile$datapath )
  }, options = list( pageLength = 5 ) )
  
  begin <- eventReactive(input$begin, {
    ###### Printing Product labels
    
    # Libraries required
    library( data.table ) # to read csv peacefully
    library( rPython ) # to plot barcode
    library( png ) #to read PNG
    library( httr ) #to get brand logo images from web
    
    #constants required
    code1 <- "import Image\r\nimport ImageDraw\r\nCODE128_CHART = \"\"\"\r\n0 212222 space space 00\r\n1 222122 ! ! 01\r\n2 222221 \" \" 02\r\n3 121223 # # 03\r\n4 121322 $ $ 04\r\n5 131222 % % 05\r\n6 122213 & & 06\r\n7 122312 ' ' 07\r\n8 132212 ( ( 08\r\n9 221213 ) ) 09\r\n10 221312 * * 10\r\n11 231212 + + 11\r\n12 112232 , , 12\r\n13 122132 - - 13\r\n14 122231 . . 14\r\n15 113222 / / 15\r\n16 123122 0 0 16\r\n17 123221 1 1 17\r\n18 223211 2 2 18\r\n19 221132 3 3 19\r\n20 221231 4 4 20\r\n21 213212 5 5 21\r\n22 223112 6 6 22\r\n23 312131 7 7 23\r\n24 311222 8 8 24\r\n25 321122 9 9 25\r\n26 321221 : : 26\r\n27 312212 ; ; 27\r\n28 322112 < < 28\r\n29 322211 = = 29\r\n30 212123 > > 30\r\n31 212321 ? ? 31\r\n32 232121 @ @ 32\r\n33 111323 A A 33\r\n34 131123 B B 34\r\n35 131321 C C 35\r\n36 112313 D D 36\r\n37 132113 E E 37\r\n38 132311 F F 38\r\n39 211313 G G 39\r\n40 231113 H H 40\r\n41 231311 I I 41\r\n42 112133 J J 42\r\n43 112331 K K 43\r\n44 132131 L L 44\r\n45 113123 M M 45\r\n46 113321 N N 46\r\n47 133121 O O 47\r\n48 313121 P P 48\r\n49 211331 Q Q 49\r\n50 231131 R R 50\r\n51 213113 S S 51\r\n52 213311 T T 52\r\n53 213131 U U 53\r\n54 311123 V V 54\r\n55 311321 W W 55\r\n56 331121 X X 56\r\n57 312113 Y Y 57\r\n58 312311 Z Z 58\r\n59 332111 [ [ 59\r\n60 314111 \\ \\ 60\r\n61 221411 ] ] 61\r\n62 431111 ^ ^ 62\r\n63 111224 _ _ 63\r\n64 111422 NUL ` 64\r\n65 121124 SOH a 65\r\n66 121421 STX b 66\r\n67 141122 ETX c 67\r\n68 141221 EOT d 68\r\n69 112214 ENQ e 69\r\n70 112412 ACK f 70\r\n71 122114 BEL g 71\r\n72 122411 BS h 72\r\n73 142112 HT i 73\r\n74 142211 LF j 74\r\n75 241211 VT k 75\r\n76 221114 FF l 76\r\n77 413111 CR m 77\r\n78 241112 SO n 78\r\n79 134111 SI o 79\r\n80 111242 DLE p 80\r\n81 121142 DC1 q 81\r\n82 121241 DC2 r 82\r\n83 114212 DC3 s 83\r\n84 124112 DC4 t 84\r\n85 124211 NAK u 85\r\n86 411212 SYN v 86\r\n87 421112 ETB w 87\r\n88 421211 CAN x 88\r\n89 212141 EM y 89\r\n90 214121 SUB z 90\r\n91 412121 ESC { 91\r\n92 111143 FS | 92\r\n93 111341 GS } 93\r\n94 131141 RS ~ 94\r\n95 114113 US DEL 95\r\n96 114311 FNC3 FNC3 96\r\n97 411113 FNC2 FNC2 97\r\n98 411311 ShiftB ShiftA 98\r\n99 113141 CodeC CodeC 99\r\n100 114131 CodeB FNC4 CodeB\r\n101 311141 FNC4 CodeA CodeA\r\n102 411131 FNC1 FNC1 FNC1\r\n103 211412 StartA StartA StartA\r\n104 211214 StartB StartB StartB\r\n105 211232 StartC StartC StartC\r\n106 2331112 Stop Stop Stop\r\n\"\"\".split()\r\nVALUES   = [int(value) for value in CODE128_CHART[0::5]]\r\nWEIGHTS  = dict(zip(VALUES, CODE128_CHART[1::5]))\r\nCODE128A = dict(zip(CODE128_CHART[2::5], VALUES))\r\nCODE128B = dict(zip(CODE128_CHART[3::5], VALUES))\r\nCODE128C = dict(zip(CODE128_CHART[4::5], VALUES))"
    python.exec( code1 )
    code2 <- "for charset in (CODE128A, CODE128B):\r\n\tcharset[' '] = charset.pop('space')\r\n\r\ndef code128_format(data):\r\n\ttext     = str(data)\r\n\tpos      = 0\r\n\tlength   = len(text)\r\n\t\r\n\t# Start Code\r\n\tif text[:2].isdigit() and length > 1:\r\n\t\tcharset = CODE128C\r\n\t\tcodes   = [charset['StartC']]\r\n\telse:\r\n\t\tcharset = CODE128B\r\n\t\tcodes   = [charset['StartB']]\r\n\t\r\n\t# Data\r\n\twhile pos < length:\r\n\t\tif charset is CODE128C:\r\n\t\t\tif text[pos:pos+2].isdigit() and length - pos > 1:\r\n\t\t\t\t# Encode Code C two characters at a time\r\n\t\t\t\tcodes.append(int(text[pos:pos+2]))\r\n\t\t\t\tpos += 2\r\n\t\t\telse:\r\n\t\t\t\t# Switch to Code B\r\n\t\t\t\tcodes.append(charset['CodeB'])\r\n\t\t\t\tcharset = CODE128B\r\n\t\telif text[pos:pos+4].isdigit() and length - pos >= 4:\r\n\t\t\t# Switch to Code C\r\n\t\t\tcodes.append(charset['CodeC'])\r\n\t\t\tcharset = CODE128C\r\n\t\telse:\r\n\t\t\t# Encode Code B one character at a time\r\n\t\t\tcodes.append(charset[text[pos]])\r\n\t\t\tpos += 1\r\n\t\r\n\t# Checksum\r\n\tchecksum = 0\r\n\tfor weight, code in enumerate(codes):\r\n\t\tchecksum += max(weight, 1) * code\r\n\tcodes.append(checksum % 103)\r\n\t\r\n\t# Stop Code\r\n\tcodes.append(charset['Stop'])\r\n\treturn codes\r\n\r\ndef code128_image(data, height=100, thickness=3, quiet_zone=True):\r\n\tif not data[-1] == CODE128B['Stop']:\r\n\t\tdata = code128_format(data)\r\n\t\r\n\tbarcode_widths = []\r\n\tfor code in data:\r\n\t\tfor weight in WEIGHTS[code]:\r\n\t\t\tbarcode_widths.append(int(weight) * thickness)\r\n\twidth = sum(barcode_widths)\r\n\tx = 0\r\n\r\n\tif quiet_zone:\r\n\t\twidth += 20 * thickness\r\n\t\tx = 10 * thickness\r\n\r\n\t# Monochrome Image\r\n\timg  = Image.new('1', (width, height), 1)\r\n\tdraw = ImageDraw.Draw(img)\r\n\tdraw_bar = True\r\n\tfor width in barcode_widths:\r\n\t\tif draw_bar:\r\n\t\t\tdraw.rectangle(((x, 0), (x + width - 1, height)), fill=0)\r\n\t\tdraw_bar = not draw_bar\r\n\t\tx += width\r\n\r\n\treturn img\r\n\r\n\r\ndef jindsh( data ):\r\n\tcode=code128_format(data)\r\n\timage=code128_image(code)\r\n  \trow = image.size[0] - 1\r\n\tcol = image.size[1] - 1\r\n\tresult = [[0 for x in range( col + 1)] for x in range( row + 1 )] \r\n\tfor i in range( 0, row ):\r\n\t\tfor j in range( 0, col ):\r\n\t\t\tresult[ i ][ j ] = image.getpixel( ( i, j ) )\r\n\treturn result\r\n\r\n"
    python.exec( code2 )
    tempImageName <- "temp.png"
    pythonFunction <- "jindsh"
    
    ####Attributes to be present on product sticker
    # Imported By - Constant
    # Customer Care - Constant
    # Generic Name - From CSV
    # Net quantity ( 01 U ). Space around this is to be taken care of. - Constant
    # MRP - From CSV
    # Month, Year of Import - Taken from User
    # SKu ( jindal trading company SKu ) - From CSv
    # brand Logo - From CSV, web
    
    ####Attributes to be present on stock keeping sticker ( Flipkart / Amazon )
    # store SKU ( AMazon's SKU, Flipkart's SKU) - From CSv
    # Warehouse SKu ( Amazon's FNSKU, Flipkart's FSN ) - From CSV
    # Barcode of Ware house SKU - From CSV
    
    ####Data needed from CSV
    # Name
    # MRP
    # SKU ( jindal Trading company )
    # logo ( brand name )
    # store sku ( Amazon's SKU, Flipkart's SKU )
    # Warehouse SKU ( Amazon's FNSKU, Flipkart's FSN )
    # Amazon FBA quanity, Flipkart's FA quantity.
    
    #Constants
    importedBy <- "Imported by: Jindal Trading Company, 128/A Ravi Colony,\nTrimulgerry, Telangana 500015"
    customerCare <- "Customer Care: Jindal Trading Company, 128/A Ravi Colony,\nTrimulgerry, Telangana 500015\nEmail:jindaltradingcompany00@gmail.com Phone:04040163179"
    netQuantity <- "\nNet Quantity:         01 Unit\n"
    
    #to be taken from user input
    monthInput <- input$monthImport
    month <- paste( "Month of Import:", monthInput )
    
    #to be read from some csv file
    #name <- "Name: dbrand Mobile Skin"
    #mrp <- "MRP: 2999 INR"
    #sku <- "SKU: i6carbonbacksplit"
    #brand <- "JT"
    
    
    ###Function to generate product stickers
    #
    ## Input:
    #
    # importedBy: "Imported by: Jindal Trading Company, 128/A Ravi Colony, Trimulgerry, Telangana 500015"
    # month: "Month of Import: October 2015"
    # customerCare: "Customer Care: Jindal Trading Company, 128/A Ravi Colony, Trimulgerry, Telangana 500015\nEmail: jindaltradingcompany00@gmail.com Phone: 040-40163179"
    # netQuantity: "\nNet Quantity:       01 Unit\n"
    # name: name of the product
    # mrp: mrp of the product
    # sku: sku of the product
    # brand: brand name of the product
    #
    ## Output:
    #
    # Image(rasterized). It  just plots the desired output on a plot.
    generateProductSticker <- function( importedBy, month, customerCare, netQuantity, name, mrp, sku, response ){
      metadata <- paste( name, mrp, sku, month, importedBy, customerCare, netQuantity, sep = "\n"  )
      png( "product.png", width = 420, height = 210, units = "px", bg = "white")
      par( mfrow = c( 1, 1 ), mar = rep( 0.05, 4 ) )
      plot( 1 : 2, type = 'n' , yaxt = 'n', xaxt = 'n', ylab = '', xlab = '', bty = 'n' )#for no border
      rasterImage( readPNG( response$content ), 1.5, 1.67, 2.0, 1.95 )
      text( 0.96, 1.4, metadata, pos = 4, cex = 1.1 )
      dev.off()
      image <- readPNG( "product.png" )
      return( image )
    }
    
    ####Function to generate Bar code Sticker
    #
    ## Input:
    #
    # sku: product sku to pe printed normally
    # barCodeText: FSN, AFSN to generate barcode
    #
    ## Output:
    # 
    # None. Just generated barcode for stock keeping uses.
    #sku <- "i6carbonbacksplit"
    #barCodeText <- "X000EY2VO3"
    generateBarCodeSticker <- function( sku, barCodeText ){
      img <- python.call( pythonFunction, barCodeText )
      output <- matrix( unlist( img ), ncol = length( img ) )
      png( "sticker.png", width = 420, height = 210, units = "px", bg = "white")
      par( mfrow = c( 1, 1 ), mar = rep( 0.05, 4 ) )
      plot( 1 : 2, type = 'n', yaxt = 'n', xaxt = 'n', ylab = '', xlab = '', bty = 'n' )#for no border
      text( 1.5, 1.8, paste( sku, "\n", barCodeText ), cex = 1.5 )
      rasterImage( output[ 1 : ( nrow( output ) - 1 ), 1 : ( ncol( output ) - 1 ) ], 0.95, 1.0, 2.05, 1.6 )
      dev.off()
      image <- readPNG( "sticker.png" )
      return( image )
    }
    data <- data.table( read.csv( input$file1$datapath ) )
    n <- nrow( data )
    flag <- input$radio
    
    pdf( file = "test.pdf", onefile = TRUE, paper = "a4", bg = "white", width = 7.8, height = 11 )
    par( mfrow = c( 8, 3 ), mar = rep( 0.5, 4 ) )
    if( flag == 1 ){ #Amazon https://sellercentral.amazon.in/gp/ssof/reports/search.html/ref=ag_fbafulrpts_cont_fbareports?ie=UTF8&recordType=FBA_MYI_ALL_INVENTORY
      setnames( data, "Quantity", "quantity" )
      setnames( data, "Brand..Wizzit.JT.dbrand.None.", "brand" )
      setnames( data, "product.name", "name" )
      setnames( data, "MRP", "price" )
      setnames( data, "sku", "sku" )
      setnames( data, "fnsku", "barCodeText" )
    }
    if( flag == 2 ){ #Flipkart https://seller.flipkart.com/sfx/listings/download?sellerId=ia2tdh26i36gcw7a
      setnames( data, "QUANTITY", "quantity")
      setnames( data, "BRAND", "brand" )
      setnames( data, "TITLE", "name")
      setnames( data, "SELLING.PRICE", "price" )
      setnames( data, "SKU", "sku")
      setnames( data, "PRODUCT.ID", "barCodeText" )
      data$price <- 2 * data$price
    }
    data <- data[ order( data$brand ) ]
    currentBrand <- ""
    for ( i in 1 : n ){
      if( !is.na( data$quantity[ i ] ) ){
        if( data$quantity[ i ] > 0 ){
          #Amazon https://sellercentral.amazon.in/gp/ssof/reports/search.html/ref=ag_fbafulrpts_cont_fbareports?ie=UTF8&recordType=FBA_MYI_ALL_INVENTORY
          #Flipkart https://seller.flipkart.com/sfx/listings/download?sellerId=ia2tdh26i36gcw7a
          name <- paste( "Name:", data$name[ i ] )
          mrp <- paste( "MRP:", as.character( data$price [ i ] ), "INR" )
          sku <- paste( "SKU:", data$sku[ i ] )
          brand <- data$brand[ i ]
          barCodeText <- data$barCodeText[ i ]
          if( brand != currentBrand ){
            response = GET( paste( "www.dbrand.co.in/img/", brand, ".png", sep= "") )
            currentBrand = brand
          }
          product <- generateProductSticker( importedBy, month, customerCare, netQuantity, name, mrp, sku, response )
          barcode <- generateBarCodeSticker( sku, barCodeText )
          for ( j in 1 : data$quantity[ i ] ){
            plot( 1 : 2, type = 'n', yaxt = 'n', xaxt = 'n', ylab = '', xlab = '', bty = 'n' )
            rasterImage( product, 0.98, 1.0, 2.0, 2.0 )
            plot( 1 : 2, type = 'n', yaxt = 'n', xaxt = 'n', ylab = '', xlab = '', bty = 'n' )
            rasterImage( barcode, 0.98, 1.0, 2.0, 2.0 )
          }
        }
      }
    }
    dev.off()
    "Done. Now you can download the pdf and the csv. Enjoy."
    
  })
  
  output$status <- renderText({
    begin()
  })
  
  output$downloadpdf <- downloadHandler(
    filename = function() { 
      'labels.pdf'
    },
    content = function(file) {
      file.copy( 'test.pdf', file )
    }
  )
  
  
  datasetInput <- reactive({
    library( data.table )
    library( random )
    flag <- input$radio
    data <- data.table( read.csv( input$file1$datapath ) )
    if( flag == 1 ){ # Amazon shipment
      temp <- "PlanName\trandomPlanName\t\r\nShipToCountry\tIN\t\r\nAddressName\tJindal Trading Company\t\r\nAddressFieldOne\t120/A Ravi Colony\t\r\nAddressFieldTwo\tTrimulgerry\t\r\nAddressCity\tSecunderabad\t\r\nAddressCountryCode\tIN\t\r\nAddressStateOrRegion\tTelangana\t\r\nAddressPostalCode\t500015\t\r\nAddressDistrict\t\r\n\t\r\nMerchantSKU\tQuantity\t\r\n"
      con <- textConnection( temp )
      shipmentData <- data.table( read.csv( con, sep = '\t') )
      close( con )
      shipmentData$X <- NULL
      plan <- randomStrings( n = 1, len = 7 )[ 1 ]
      
      data <- data[ order( -data$Quantity ) ]
      m <- nrow( data )
      n <- min( which( is.na( data$Quantity ) | data$Quantity == 0 ), m )
      table <- data.table( "PlanName"= data$sku[ 1 : ( n - 1 ) ], "randomPlanName" = data$Quantity[ 1 : ( n - 1 ) ] )
      shipmentData <- rbind( shipmentData, table )
      
      setnames( shipmentData, "randomPlanName", plan )
    }
    if( flag == 2 ){ # Flipkart Shipment
      data <- data[ order( -data$QUANTITY ) ]
      m <- nrow( data )
      n <- min( which( is.na( data$QUANTITY ) | data$QUANTITY == 0 ), m )
      shipmentData <- data[ 1 : ( n - 1 ) ]
    }
    shipmentData
  })
  
  output$downloadcsv <- downloadHandler(
    filename = function() { 
      flag <- input$radio
      if( flag == 1 ){ name = 'shipment.txt' }
      if( flag == 2 ){ name = 'shipment.csv' }
      paste( name )
    },
    content = function(file) {
      flag <- input$radio
      if( flag == 1 ){ 
        sep = '\t'
        quote = FALSE
      }
      if( flag == 2 ){ 
        sep = ','
        quote = TRUE
      }
      write.table( datasetInput(), file, sep = sep, na = "", row.name = FALSE, quote = quote )
    }
  )
  
})
