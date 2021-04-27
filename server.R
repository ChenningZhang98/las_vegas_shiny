server <- function(input, output, session) {
 
  # code to collapse navbar after selecting a menu item
  observeEvent(
    eventExpr = input$navBar, 
    {
      runjs(
        'var elem = document.getElementsByClassName("navbar-collapse")[0]
       elem.setAttribute("aria-expanded", "false");
       elem.setAttribute("class", "navbar-collapse collapse");'
      )
    }
  )
  
# ***********************---- 
# page - home----
  
  # _home reactive values----
  hm <- reactiveValues(
    i = 1, # i = index of image
    n = 5  # n = # of home images
  )
  
  # _ui home----
  output$home <- renderUI(
    if (input$navBar == '0') {
      fluidRow(
        style = paste0(
          'background-image:url(pg0_', hm$i, '.png);',
          'background-size:cover;',
          'padding-top:25%; padding-bottom:25%;'
        ),
        align = 'center',
        h1(style = 'color:#fdfe02;', 'Las Vegas'),
        actionBttn(
          inputId = 'left',
          label = '<',
          style = 'simple',
          size = 'md'
        ),
        actionBttn(
          inputId = 'right',
          label = '>',
          style = 'simple',
          size = 'md'
        )
      )
    }
  )
  
  # _event left button----
  observeEvent(
    eventExpr = input$left,
    if (hm$i > 1) {
      hm$i <- hm$i - 1
    } else {
      hm$i <- hm$n
    }
  )
  
  # _event right button----
  observeEvent(
    eventExpr = input$right,
    if (hm$i < hm$n) {
      hm$i <- hm$i + 1
    } else {
      hm$i <- 1
    }
  )

# ***********************----
# page - hotels----
  
  # _navigate to hotels page----
  observeEvent(
    eventExpr = input$pg1Bttn,
    updateNavbarPage(
      session, 'navBar',
      selected = 'a'
    )
  )
  
  # _hotels reactive values----
  hotelSel <- reactiveValues(
    id = NULL, nm = NULL, st = NULL, # hotel_id, hotel_name, star_rating
    ad = NULL, ws = NULL,            # address, website
    lt = NULL, ln = NULL,            # latitude, longitude
    p1 = 0, p2 = 0, p3 = 0,          # price per night for 3 rooms
    i = 1, n = 5                     # image index, total images per hotel
  )
  
  # _ui hotels----
  output$hote <- renderUI(
    if (input$navBar == 'a') {
      fluidRow(
        column(
          width = 6,
          wellPanel(
            radioGroupButtons(
              inputId = 'neigh',
              label = '1. Select an area to explore:',
              choices = c(
                'Las Vegas'
              ),
              selected = character(0),
              justified = TRUE
            ),
            pickerInput(
              inputId = 'hotel',
              label = NULL,
              choices = NULL,
              width = '100%',
              options = list(
                title = '2. Select a hotel'
              )
            ),
            hr(),
            uiOutput('hotelInfo')
          )
        ),
        column(
          width = 6,
          tabsetPanel(
            id = 'hTabs',
            type = 'pills',
            # __ui hotel map----
            tabPanel(
              title = 'Hotel Map',
              value = 'h1',
              leafletOutput(
                outputId = 'hotMap',
                height = '700px'
              )
            ),
            # __ui booking form----
            tabPanel(
              title = 'Booking Form',
              value = 'h2',
              uiOutput('book')
            )
          )
        )
      )
    }
  )

  # _event neigh click----
  observeEvent(
    eventExpr = input$neigh,
    {
      h <- hot %>% 
        filter(neigh == input$neigh)
      hotelSel$id <- NULL
      updatePickerInput(
        session = session,
        inputId = 'hotel',
        choices = h$hotel_name
      )
    }
  )
  
 
  
  # _hotel map----
  output$hotMap <- renderLeaflet(
    {
      if (!is.null(input$hotel)) {
        mapData <- hot #%>% 
        #filter(neigh == input$neigh)
        leaflet(
          data = mapData,
          options = leafletOptions(zoomControl = TRUE)
        ) %>%
          addProviderTiles(providers$Esri.WorldImagery) %>%  
          addMarkers(
            lng = ~lng,
            lat = ~lat,
            label = ~hotel_name,
          ) 
      } 
    }
  )
  
  
  # _event hotel picker----
  observeEvent(
    eventExpr = input$hotel,
    if (input$hotel != '') {
      h <- hot %>% 
        filter(hotel_name == input$hotel)
      hotelSel$lt <- h$lat
      hotelSel$ln <- h$lng
      hotelSel$id <- h$hotel_id
      hotelSel$nm <- h$hotel_name
      hotelSel$ad <- h$address
      hotelSel$ws <- h$website
      hotelSel$st <- h$star_rating
      hotelSel$i <- 1
      leafletProxy(
        mapId = 'hotMap',
      ) %>%
        clearMarkers() %>% 
        addMarkers(
          layerId = '0',
          lng = hotelSel$ln,
          lat = hotelSel$lt,
          label = hotelSel$nm
        ) %>% 
        setView(
          lng = hotelSel$ln,
          lat = hotelSel$lt,
          zoom = 15
        )
    }
  )
  
  # _event hotel map marker----
  observeEvent(
    eventExpr = input$hotMap_marker_click,
    {
      hh <- input$hotMap_marker_click
      h <- hot %>% 
        filter(lat == hh$lat & lng == hh$lng)
      hotelSel$lt <- h$lat
      hotelSel$ln <- h$lng
      hotelSel$id <- h$hotel_id
      hotelSel$nm <- h$hotel_name
      hotelSel$ad <- h$address
      hotelSel$ws <- h$website
      hotelSel$st <- h$star_rating
      hotelSel$i <- 1
      updatePickerInput(
        session = session,
        inputId = 'hotel',
        selected = hotelSel$nm
      )
    }
  )
  
  # _ui hotel info----
  output$hotelInfo <- renderUI(
    {
      if (!is.null(hotelSel$id)) {
        wellPanel(
          align = 'center',
          style = 'background-color:#ffffff;
                   padding-top:5px; padding-bottom:0px; 
                   padding-left:10px; padding-right:10px;'
          ,
          # __hotel name----
          h4(
            style = 'text-align:left;', 
            tags$a(
              href = hotelSel$ws,
              target = '_blank',
              hotelSel$nm
            )
          ),
          # __hotel address----
          h6(style = 'text-align:left;', hotelSel$ad),
          fluidRow(
            # __hotel star rating----
            column(
              width = 4,
              align = 'left',
              h5(style = 'color:#01cdfe;', 
                 str_dup('\U2605\U0020', hotelSel$st))
            ),
            # __hotel owner----
            column(
              width = 8,
              align = 'right',
              if (input$hotel != '') {
                h5(
                  paste0(
                    'Owner: ', 
                    dbGetQuery(
                      conn = con,
                      statement = paste0(
                        'SELECT owner_name ',
                        'FROM hotels JOIN owners USING (owner_id) ',
                        'WHERE hotel_name = \'', input$hotel, '\''
                      )
                    )
                  )
                )
              } 
              
            )
          ),
          # __hotel images----
          div(
            style = 'position:relative; overflow:hidden; 
                     border:5px solid #ffffff;
                     background-color:#4e4e4e;',
            img(
              src = paste0(
                'hotels/', hotelSel$id, '_', 
                str_pad(input$imgCtr, 2, 'left', '0'), '.jpeg'
              ),
              width = 'auto',
              height = '370'
            )
          ),
          br(),
          # __hotel image slider----
          sliderInput(
            inputId = 'imgCtr',
            label = NULL,
            min = 1,
            max = hotelSel$n,
            value = hotelSel$i,
            step = 1,
            ticks = FALSE,
            width = '90%'
          )
        )
      }
    }
  )
  
  # _event image slider move----
  observeEvent(
    eventExpr = input$imgCtr,
    {
      hotelSel$i <- as.numeric(input$imgCtr)
    }
  )
  
  # _booking form----
  output$book <- renderUI(
    if (!is.null(hotelSel$id)) {
      wellPanel(
        align = 'center',
        fluidRow(
          column(
            width = 4,
            dateInput(
              inputId = 'bookDate1',
              label = 'From',
              min = Sys.Date(),
              value = Sys.Date()
            )
          ),
          column(
            width = 4,
            dateInput(
              inputId = 'bookDate2',
              label = 'To',
              min = Sys.Date() + 1,
              value = Sys.Date() + 1
            )
          ),
          column(
            width = 4,
            pickerInput(
              inputId = 'gsts',
              label = 'Number of Guests:',
              choices = c(1:4),
              selected = 2
            )
          )
        ),
        hr(),
        actionBttn(
          inputId = 'viewRms',
          label = 'View Available Rooms',
          style = 'fill',
          color = 'success',
          size = 'md'
        ),
        hr(),
        uiOutput('availRms')
      )
    }
  )
  
  # __event view rooms button----
  observeEvent(
    eventExpr = input$viewRms,
    {
      stmt <- paste0(
        'SELECT avg(price_pn) m, stddev(price_pn) s\n',
        'FROM bookings\n',
        'WHERE hotel_id = \'', hotelSel$id, '\'\n',
        'AND guests = ', input$gsts, ';'
      )
      hp <- dbGetQuery(con, stmt)
      hotelSel$p1 <- round(rnorm(1, hp$m, hp$s), 2)
      hotelSel$p2 <- round(hotelSel$p1 * (1 + runif(1, 0, 0.25)), 2)
      hotelSel$p3 <- round(hotelSel$p2 * (1 + runif(1, 0, 0.25)), 2)
    }
  )
  
  # __ui avail rooms----
  output$availRms <- renderUI(
    if (input$viewRms > 0) {
      div(
        wellPanel(
          fluidRow(
            column(
              width = 9,
              div(
                h4('Room Option 1'),
                h3(paste0('Price/Night: ', dollar(hotelSel$p1)))
              )
            ),
            column(
              width = 3,
              actionBttn(
                inputId = 'bookRm1',
                label = 'Book This Room',
                style = 'fill',
                color = 'primary',
                size = 'sm'
              )
            )
          )
        ),
        wellPanel(
          fluidRow(
            column(
              width = 9,
              div(
                h4('Room Option 2'),
                h3(paste0('Price/Night: ', dollar(hotelSel$p2)))
              )
            ),
            column(
              width = 3,
              actionBttn(
                inputId = 'bookRm2',
                label = 'Book This Room',
                style = 'fill',
                color = 'primary',
                size = 'sm'
              )
            )
          )
        ),
        wellPanel(
          fluidRow(
            column(
              width = 9,
              div(
                h4('Room Option 3'),
                h3(paste0('Price/Night: ', dollar(hotelSel$p3)))
              )
            ),
            column(
              width = 3,
              actionBttn(
                inputId = 'bookRm3',
                label = 'Book This Room',
                style = 'fill',
                color = 'primary',
                size = 'sm'
              )
            )
          )
        )
      )
    }
  )
  
  # __event adjust date 2----
  observeEvent(
    eventExpr = input$bookDate1,
    {
      updateDateInput(
        session = session,
        inputId = 'bookDate2',
        value = input$bookDate1 + 1
      )
    }
  )
  
  # __event book room 1 button----
  observeEvent(
    eventExpr = input$bookRm1,
    {
      los <- input$bookDate2 - input$bookDate1
      rmr <- hotelSel$p1 * as.numeric(los)
      txf <- round(0.15 * rmr, 2)
      shinyalert(
        title = 'Enjoy your stay!',
        text = paste0(
          '<hr>Your booking for ',
          los,
          ifelse(los == 1, ' night at ', ' nights at<br>'),
          '<h4>', hotelSel$nm, '</h4>',
          'is confirmed.<hr>Your total cost will be:<br>',
          '<h3>', dollar(rmr + txf), '</h3>',
          '= ', los, ' x ', dollar(hotelSel$p1), ' (room)<br>',
          '+ ', dollar(txf), ' (taxes & fees)'
        ),
        type = 'success',
        html = TRUE
      )
    }
  )
  
  # __event book room 2 button----
  observeEvent(
    eventExpr = input$bookRm2,
    {
      los <- input$bookDate2 - input$bookDate1
      rmr <- hotelSel$p2 * as.numeric(los)
      txf <- round(0.15 * rmr, 2)
      shinyalert(
        title = 'Enjoy your stay!',
        text = paste0(
          '<hr>Your booking for ',
          los,
          ifelse(los == 1, ' night at ', ' nights at<br>'),
          '<h4>', hotelSel$nm, '</h4>',
          'is confirmed.<hr>Your total cost will be:<br>',
          '<h3>', dollar(rmr + txf), '</h3>',
          '= ', los, ' x ', dollar(hotelSel$p2), ' (room)<br>',
          '+ ', dollar(txf), ' (taxes & fees)'
        ),
        type = 'success',
        html = TRUE
      )
    }
  )
  
  # __event book room 3 button----
  observeEvent(
    eventExpr = input$bookRm3,
    {
      los <- input$bookDate2 - input$bookDate1
      rmr <- hotelSel$p3 * as.numeric(los)
      txf <- round(0.15 * rmr, 2)
      shinyalert(
        title = 'Enjoy your stay!',
        text = paste0(
          '<hr>Your booking for ',
          los,
          ifelse(los == 1, ' night at ', ' nights at<br>'),
          '<h4>', hotelSel$nm, '</h4>',
          'is confirmed.<hr>Your total cost will be:<br>',
          '<h3>', dollar(rmr + txf), '</h3>',
          '= ', los, ' x ', dollar(hotelSel$p3), ' (room)<br>',
          '+ ', dollar(txf), ' (taxes & fees)'
        ),
        type = 'success',
        html = TRUE
      )
    }
  )
  
  # ***********************----
  
  # page - attractions----
  
  # _navigate to attractions page----
  observeEvent(
    eventExpr = input$pg2Bttn,
    updateNavbarPage(
      session, 'navBar',
      selected = 'b'
    )
  )
  
  # _attractions reactive values----
  attrSel <- reactiveValues(
    id = NULL, nm = NULL,  # attr_id, attr_name
    ad = NULL, ws = NULL,  # address, website
    lt = NULL, ln = NULL   # latitude, longitude
  )
  
  # _ui attractions----
  output$attr <- renderUI(
    if (input$navBar == 'b') {
      fluidRow(
        column(
          width = 6,
          radioGroupButtons(
            inputId = 'neigh2',
            label = NULL,
            choices = c(
              'Las Vegas',
              'Other'
              
            ),
            selected = character(0),
            justified = TRUE
          ),
          checkboxGroupButtons(
            inputId = 'attType',
            label = NULL,
            choices = sort(unique(att$attr_type)),
            selected = NULL,
            justified = TRUE,
            checkIcon = list(
              yes = icon(
                name = 'ok', 
                lib = 'glyphicon'
              )
            )
          ),
          leafletOutput(
            outputId = 'attMap',
            height = '520px'
          )
        ),
        column(
          width = 6,
          uiOutput('attrInfo')
        )
      )
    }
  )
  
  # _event attr neigh radio----
  observeEvent(
    eventExpr = input$neigh2,
    {
      attrSel$id <- NULL
      updateCheckboxGroupButtons(
        session = session,
        inputId = 'attType',
        selected = character(0)
      )
    }
  )
  
  # _event atttype checkbox----
  observeEvent(
    eventExpr = input$attType,
    if (!is.null(input$neigh2)) {
      leafletProxy(
        mapId = 'attMap',
      ) %>%
        hideGroup(unique(att$attr_type)) %>%
        showGroup(input$attType)
    }
  )
  
  # _attractions map----
  output$attMap <- renderLeaflet(
    {
      if (!is.null(input$neigh2)) {
        mapData <- att %>% 
          filter(neigh == input$neigh2)
        aIcon <- makeIcon(
          iconUrl = paste0('www/a_', str_to_lower(mapData$attr_type), '.png'),
          iconWidth = 50,
          iconHeight = 50,
          iconAnchorX = 25,
          iconAnchorY = 25
        )
        leaflet(
          data = mapData,
          options = leafletOptions(zoomControl = TRUE)
        ) %>%
          addProviderTiles(providers$Stamen.Terrain) %>%  
          addMarkers(
            lng = ~lng,
            lat = ~lat,
            label = ~attr_name,
            icon = aIcon,
            group = ~attr_type
          ) %>% 
          hideGroup(unique(att$attr_type))
      } 
    }
  )
  
  # _event attr map marker----
  observeEvent(
    eventExpr = input$attMap_marker_click,
    {
      aa <- input$attMap_marker_click
      a <- att %>% 
        filter(lat == aa$lat & lng == aa$lng)
      attrSel$lt <- a$lat
      attrSel$ln <- a$lng
      attrSel$id <- a$attr_id
      attrSel$nm <- a$attr_name
      attrSel$ad <- a$address
      attrSel$ws <- a$website
    }
  )
  
  # _ui attr info----
  output$attrInfo <- renderUI(
    {
      if (!is.null(attrSel$id) & length(input$attType) > 0) {
        print(attrSel$id)
        wellPanel(
          align = 'center',
          style = 'background-color:#ffffff;
                   padding:10px;'
          ,
          # __attr name----
          h4(
            style = 'text-align:left;', 
            tags$a(
              href = attrSel$ws,
              target = '_blank',
              attrSel$nm
            )
          ),
          # __attr address----
          h6(style = 'text-align:left;', attrSel$ad),
          # __attr image----
          div(
            style = 'position:relative; overflow:hidden; 
                     border:5px solid #ffffff;
                     background-color:#4e4e4e;',
            img(
              src = paste0(
                'attrs/', attrSel$id, '.jpeg'
              ),
              width = 'auto',
              height = '500px'
            )
          ),
          # __attr dist to hotel----
          h5(
            if (!is.null(hotelSel$id)) {
              paste0(
                'Distance to ', hotelSel$nm, ' = ', 
                dist(hotelSel$lt, hotelSel$ln, attrSel$lt, attrSel$ln), ' km'
              )
            } else {
              'Select a hotel to measure the distance to here.'
            }
          )
        )
      }
    }
  )
  
  # ***********************----
  
  # page - restaurants----
  
  # _navigate to restaurants page----
  observeEvent(
    eventExpr = input$pg3Bttn,
    updateNavbarPage(
      session, 'navBar',
      selected = 'c'
    )
  )
  
  # _restaurants reactive values----
  restSel <- reactiveValues(
    id = NULL, nm = NULL,  # rest_id, rest_name, address
    ad = NULL, ws = NULL,  # address, website
    lt = NULL, ln = NULL   # latitude, longitude
  )
  
  # _ui restaurants----
  output$rest <- renderUI(
    if (input$navBar == 'c') {
      fluidRow(
        column(
          width = 6,
          radioGroupButtons(
            inputId = 'neigh3',
            label = NULL,
            choices = c(
              'Las Vegas'
              
            ),
            selected = character(0),
            justified = TRUE
          ),
          checkboxGroupButtons(
            inputId = 'resType',
            label = NULL,
            choices = sort(unique(res$rest_type)),
            selected = NULL,
            justified = TRUE,
            checkIcon = list(
              yes = icon(
                name = 'ok', 
                lib = 'glyphicon'
              )
            )
          ),
          leafletOutput(
            outputId = 'resMap',
            height = '520px'
          )
        ),
        column(
          width = 6,
          uiOutput('restInfo')
        )
      )
    }
  )
  
  # _event rest neigh radio----
  observeEvent(
    eventExpr = input$neigh3,
    {
      restSel$id <- NULL
      updateCheckboxGroupButtons(
        session = session,
        inputId = 'resType',
        selected = character(0)
      )
    }
  )
  
  # _event restype checkbox----
  observeEvent(
    eventExpr = input$resType,
    if (!is.null(input$neigh3)) {
      leafletProxy(
        mapId = 'resMap',
      ) %>%
        hideGroup(unique(res$rest_type)) %>%
        showGroup(input$resType)
    }
  )
  
  # _restaurants map----
  output$resMap <- renderLeaflet(
    {
      if (!is.null(input$neigh3)) {
        mapData <- res %>% 
          filter(neigh == input$neigh3)
        rIcon <- makeIcon(
          iconUrl = paste0('www/r_', str_to_lower(mapData$rest_type), '.png'),
          iconWidth = 50,
          iconHeight = 50,
          iconAnchorX = 25,
          iconAnchorY = 25
        )
        leaflet(
          data = mapData,
          options = leafletOptions(zoomControl = TRUE)
        ) %>%
          addProviderTiles(providers$Stamen.TonerLite) %>%  
          addMarkers(
            lng = ~lng,
            lat = ~lat,
            label = ~rest_name,
            icon = rIcon,
            group = ~rest_type
          ) %>% 
          hideGroup(unique(res$rest_type))
      } 
    }
  )
  
  # _event rest map marker----
  observeEvent(
    eventExpr = input$resMap_marker_click,
    {
      rr <- input$resMap_marker_click
      r <- res %>% 
        filter(lat == rr$lat & lng == rr$lng)
      restSel$lt <- r$lat
      restSel$ln <- r$lng
      restSel$id <- r$rest_id
      restSel$nm <- r$rest_name
      restSel$ad <- r$address
      restSel$ws <- r$website
    }
  )
  
  # _ui rest info----
  output$restInfo <- renderUI(
    {
      if (!is.null(restSel$id) & length(input$resType) > 0) {
        wellPanel(
          align = 'center',
          style = 'background-color:#ffffff;
                   padding:10px;'
          ,
          # __rest name----
          h4(
            style = 'text-align:left;', 
            tags$a(
              href = restSel$ws,
              target = '_blank',
              restSel$nm
            )
          ),
          # __rest address----
          h6(style = 'text-align:left;', restSel$ad),
          # __rest image----
          div(
            style = 'position:relative; overflow:hidden; 
                     border:5px solid #ffffff;
                     background-color:#4e4e4e;',
            img(
              src = paste0(
                'rests/', restSel$id, '.jpeg'
              ),
              width = 'auto',
              height = '500px'
            )
          ),
          # __rest dist to hotel----
          h5(
            if (!is.null(hotelSel$id)) {
              paste0(
                'Distance to ', hotelSel$nm, ' = ', 
                dist(hotelSel$lt, hotelSel$ln, restSel$lt, restSel$ln), ' km'
              )
            } else {
              'Select a hotel to measure the distance to here.'
            }
          )
        )
      }
    }
  )
  
  
  # ***********************----
  
  # page - insights----
  
  # _insights reactive values----
  ins <- reactiveValues(
    cd1 = NULL, cd2 = NULL, cd3 = NULL, 
    cd4a = NULL, cd4b = NULL,cd5 = NULL,cd6 = NULL, cd7 =NULL, # code for each insight
    i2 = NULL, # best/worst 3 months
    i4m = 0, i4d = 0, # mean for insight 4, sd for insight 4
    i5 = NULL
  )
  
  # _navigate to insights page----
  observeEvent(
    eventExpr = input$pg4Bttn,
    updateNavbarPage(
      session, 'navBar',
      selected = 'd'
    )
  )
  
  # _ui insights----
  output$insi <- renderUI(
    if (input$navBar == 'd') {
      
      if (!is.null(input$insight)) {
        
        # __ui insight 1----
        if (input$insight == 'Insight 1') {
          div(
            style = 'padding-left:20px;',
            fluidRow(
              h3(
                'Number of Guests Per Stay by Hotel'
              ),
              hr(),
              pickerInput(
                inputId = 'hot1',
                label = NULL,
                choices = hot$hotel_name,
                width = '75%',
                options = list(
                  title = '1. Select a hotel'
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                align = 'center',
                sliderInput(
                  inputId = 'los',
                  label = 'Length of Stay (Nights):',
                  value = 3,
                  min = 1,
                  max = 8,
                  step = 1,
                  ticks = FALSE,
                  width = '75%'
                )
              ),
              column(
                width = 8,
                uiOutput('uiIns1b')
              )
            )
          )
          
          # __ui insight 2----  
        } else if (input$insight == 'Insight 2') {
          div(
            style = 'padding-left:20px;',
            fluidRow(
              h3(
                'Best / Worst Months of the Year by Hotel'
              ),
              hr(),
              pickerInput(
                inputId = 'hot2',
                label = NULL,
                choices = hot$hotel_name,
                width = '75%',
                options = list(
                  title = '1. Select a hotel'
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                align = 'center',
                switchInput(
                  inputId = 'beswor',
                  onStatus = 'primary',
                  offStatus = 'warning',
                  onLabel = 'Best',
                  offLabel = 'Worst',
                  value = TRUE
                ),
                uiOutput('uiIns2a')
              ),
              column(
                width = 8,
                uiOutput('uiIns2b')
              )
            )
          )
          
          # __ui insight 3---- 
        } else if (input$insight == 'Insight 3') {
          div(
            fluidRow(
              style = 'padding-left:20px;',
              h3(
                'Top / Bottom Five Hotels by Day of Week & Year of Check-Ins'
              ),
              hr(),
              column(
                width = 4,
                align = 'center',
                switchInput(
                  inputId = 'topbot',
                  onStatus = 'success',
                  offStatus = 'danger',
                  onLabel = 'Top',
                  offLabel = 'Bottom',
                  value = TRUE
                )
              )
            ),
            fluidRow(
              column(
                width = 4,
                checkboxGroupButtons(
                  inputId = 'dow',
                  label = '1. Select Day(s) of Week',
                  choices = c(
                    'Sunday' = 0,
                    'Monday' = 1,
                    'Tuesday' = 2,
                    'Wednesday' = 3,
                    'Thursday' = 4,
                    'Friday' = 5,
                    'Saturday' = 6
                  ),
                  selected = character(0),
                  direction = 'vertical',
                  justified = TRUE,
                  checkIcon = list(
                    yes = icon(
                      name = 'ok', 
                      lib = 'glyphicon'
                    )
                  )
                ),
                checkboxGroupButtons(
                  inputId = 'year',
                  label = '2. Select Year(s)',
                  choices = c(2019:2021),
                  selected = character(0),
                  justified = TRUE,
                  checkIcon = list(
                    yes = icon(
                      name = 'ok', 
                      lib = 'glyphicon'
                    )
                  )
                )
              ),
              column(
                width = 8,
                uiOutput('uiIns3')
              )
            )
          )
          # __ui insight 4----  
        } else if (input$insight == 'Insight 4') {
          div(
            fluidRow(
              style = 'padding-left:20px;',
              h3(
                'Distribution of Lead Time for Bookings'
              ),
              hr()
            ),
            fluidRow(
              column(
                width = 4,
                radioGroupButtons(
                  inputId = 'neigh4',
                  label = '1. Select an area:',
                  choices = c(
                    'Las Vegas'
                  ),
                  selected = character(0),
                  justified = TRUE
                ),
                radioGroupButtons(
                  inputId = 'qtr4',
                  label = '2. Select Quarter of Check-In Date:',
                  choices = c(paste('Qtr', 1:4)),
                  selected = character(0),
                  justified = TRUE
                ),
                uiOutput('uiIns4a')
              ),
              column(
                width = 8,
                uiOutput('uiIns4b')
              )
            )
          )
         # __ui insight 5----  
      } else if (input$insight == 'Insight 5') {
        div(
          style = 'padding-left:20px;',
          fluidRow(
            h3(
              'Guest Traffic Analysis'
            ),
            hr(),
            pickerInput(
              inputId = 'hot5',
              label = NULL,
              choices = hot$hotel_name,
              width = '75%',
              options = list(
                title = '1. Select a hotel'
              )
            )
          ),
          fluidRow(
            column(
              width = 4,
              align = 'center',
              uiOutput('uiIns5a')
            ),
            column(
              width = 8,
              uiOutput('uiIns5b')
            )
          )
        )
        
  # __ui insight 6----
      } else if (input$insight == 'Insight 6') {
                div(
                  style = 'padding-left:20px;',
                  fluidRow(
                    h3(
                      'Average Price Per Month of Hotels'
                    ),
                    hr(),
                    pickerInput(
                      inputId = 'hot6',
                      label = NULL,
                      choices = hot$hotel_name,
                      width = '75%',
                      options = list(
                        title = '1. Select a hotel'
                      )
                    )
                  ),
                  fluidRow(
                    pickerInput(
                      inputId = 'par6',
                      label = NULL,
                      choices = c('Average'),
                      width = '75%',
                      options = list(
                        title = '2. Select a parameter'
                      )
                    ),
                    column(
                      width = 8,
                      uiOutput('uiIns6b')
                    )
                  )
                )
        # __ui insight 7----
      } else if (input$insight == 'Insight 7') {
        div(
          style = 'padding-left:20px;',
          fluidRow(
            h3(
              'Resturants Price Analysis'
            ),
            hr(),
            pickerInput(
              inputId = 'res7',
              label = NULL,
              choices = unique(res$rest_type),
              width = '75%',
              options = list(
                title = '1. Select a resturant type'
              )
            )
          ),
          fluidRow(
            column(
              width = 8,
              uiOutput('uiIns7b')
            )
          )
        )
      }
      }
    }  
  )
  
  # _output insight 1----
  
  # __right side panel----
  output$uiIns1b <- renderUI(
    if (input$hot1 != '') {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Plot',
            plotOutput('ins1b')
          ),
          tabPanel(
            title = 'Code',
            htmlOutput('code1')
          )
        )
      )
    }
  )
  
  # ___plot guests per stay----
  output$ins1b <- renderPlot(
    if (input$hot1 != '') {
      ins$cd1 <- paste0(
        'SELECT guests, count(*) n\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE hotels.hotel_name = \'', input$hot1 , '\'\n',
        'AND nights = ', input$los, '\n',
        'GROUP BY 1\n',
        'ORDER BY 2 DESC;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd1
      )
      nn <- sum(z$n)
      treemap(
        dtf = z %>% 
          mutate(
            ind = paste0(
              guests, 
              ' Guest', 
              ifelse(guests == 1, '', 's'), 
              ' Per Stay\n(',
              percent(n/nn),
              ')'
            )
          ),
        index = 'ind',
        vSize = 'n',
        type = 'index',
        palette = 'Set2',
        title = paste(
          'Distribution of Guests Per Stay for',
          input$los, 
          'Night Stays at',
          input$hot1
        ),
        fontsize.title = 20,
        fontsize.labels = 16,
        border.col = 'grey'
      )
    }
  )
  
  # ___code insight 1----
  output$code1 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd1 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'ORDER' = paste0(str_dup('&nbsp;', 1), 'ORDER')
          )
        ), 
      '</p>'
    )
  )
  
  # _output insight 2----
  
  # __left side panel----
  output$uiIns2a <- renderUI(
    if (input$hot2 != '') {
      wellPanel(
        align = 'center',
        h3(
          paste0(
            ifelse(input$beswor, 'Best', 'Worst'), 
            ' 3 Months'
          )
        ),
        hr(),
        dataTableOutput('ins2a')
      )
    }
  )
  
  # ___datatable insight 2----
  output$ins2a <- renderDataTable(
    if (input$hot2 != '') {
      c <- paste0(
        '<style>th {text-align: center; color:white;}</style>',
        '<table><thead><tr>',
        '<th>Month</th>',
        '<th>Revenue</th>',
        '</tr></thead></table>'
      )
      datatable(
        data = ins$i2,
        class = 'cell-border stripe',
        container = c,
        rownames = FALSE,
        selection = 'single',
        options = list(
          pageLength = 3,
          paging = FALSE,
          searching = FALSE,
          scrollX = TRUE,
          dom = 't', # shows table and nothing else
          columnDefs = list(
            list(className = 'dt-body-center', targets = 0:0),
            list(className = 'dt-body-right', targets = 1:1)
          ),
          order = list(
            list(1, ifelse(input$beswor, 'desc', 'asc'))
          )
        )
      ) %>% 
        formatCurrency(2:2, digits = 2)
    }
  )
  
  # __right side panel----
  output$uiIns2b <- renderUI(
    if (input$hot2 != '') {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Plot',
            plotOutput('ins2b')
          ),
          tabPanel(
            title = 'Code',
            htmlOutput('code2')
          )
        )
      )
    }
  )
  
  # ___plot monthly revenue----
  output$ins2b <- renderPlot(
    bg = 'transparent',
    if (input$hot2 != '') {
      ins$cd2 <- paste0(
        'SELECT mo, rev,\n',
        'CASE\n',
        'WHEN rnk <= 3 THEN 1\n',
        'ELSE 0\n',
        'END top3\n',
        'FROM\n',
        '(\n',
        'SELECT date_part(\'month\', chkin_dt) mo, ',
        'sum(price_pn * nights) rev,\n',
        'rank() OVER (ORDER BY sum(price_pn * nights) ',
        ifelse(input$beswor, 'DESC', 'ASC'), ') rnk\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE hotels.hotel_name = \'', input$hot2, '\'\n',
        'GROUP BY 1\n',
        ') a\n',
        'ORDER BY 1;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd2
      )
      ins$i2 <- z %>% 
        filter(top3 == 1) %>% 
        mutate(mth = month.abb[mo]) %>% 
        select(mth, rev)
      ggplot(z, aes(x = mo, y = rev, fill = top3)) +
        geom_bar(stat = 'identity', color = 'white') +
        geom_hline(yintercept = max(z$rev), 
                   color = 'blue', size = 1) +
        geom_hline(yintercept = min(z$rev), 
                   color = 'orange', size = 1) +
        labs(x = 'Month', y = 'Revenue') + 
        scale_x_discrete(limits = month.abb) +
        scale_y_continuous(labels = dollar_format()) +
        theme_minimal(base_size = 16) +
        theme(
          legend.position = 'none',
          axis.title.x = element_text(colour = 'white'),
          axis.title.y = element_text(colour = 'white'),
          axis.text.x = element_text(colour = 'white'),
          axis.text.y = element_text(colour = 'white'),
          panel.grid.major = element_line(colour = '#f5f7fb'),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(
            fill = '#ffffff',
            colour = '#ffffff',
            size = 0.5, 
            linetype = 'solid'
          )
        )
    }
  )
  
  # ___code insight 2----
  output$code2 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd2 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'OR ' = paste0(str_dup('&nbsp;', 4), 'OR '),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'CASE' = paste0(str_dup('&nbsp;', 7), 'CASE'),
            'WHEN' = paste0(str_dup('&nbsp;', 9), 'WHEN'),
            'ELSE' = paste0(str_dup('&nbsp;', 9), 'ELSE'),
            'END' = paste0(str_dup('&nbsp;', 7), 'END'),
            'rank' = paste0(str_dup('&nbsp;', 7), 'rank')
          )
        ), 
      '</p>'
    )
  )
  
  # _output insight 3----
  
  # __right side panel----
  output$uiIns3 <- renderUI(
    if (!is.null(input$dow) & !is.null(input$year)) {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Results',
            dataTableOutput(
              outputId = 'ins3'
            )
          ),
          tabPanel(
            title = 'Code',
            htmlOutput(
              outputId = 'code3'
            )
          )
        )
      )
    }
  )
  
  # ___datatable insight 3----
  output$ins3 <- renderDataTable(
    if (!is.null(input$dow) & !is.null(input$year)) {
      ins$cd3 <- paste0(
        'SELECT hotels.hotel_name, sum(price_pn * nights) revenue, count(book_id) bookings\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE date_part(\'dow\', chkin_dt) IN (', paste0(input$dow, collapse = ','), ')\n',
        'AND date_part(\'year\', chkin_dt) IN (', paste0(input$year, collapse = ','), ')\n',
        'GROUP BY 1\n',
        'ORDER BY 2 ', ifelse(input$topbot, 'DESC ', 'ASC '), '\n',
        'LIMIT 5;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd3
      )
      c <- paste0(
        '<style>th {text-align: center; color:white;}</style>',
        '<table><thead><tr>',
        '<th>Rank</th>',
        '<th>Hotel</th>',
        '<th>Revenue</th>',
        '<th>Bookings</th>',
        '</tr></thead></table>'
      )
      datatable(
        data = z,
        class = 'cell-border stripe',
        container = c,
        rownames = TRUE,
        selection = 'single',
        options = list(
          pageLength = 10,
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          scrollX = TRUE,
          dom = 't', # shows table and nothing else
          columnDefs = list(
            list(className = 'dt-body-center', targets = 0:1),
            list(className = 'dt-body-right', targets = 2:3)
          )
        )
      ) %>% 
        formatCurrency(2:2, digits = 2)
    }
  )
  
  # ___code insight 3----
  output$code3 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd3 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'OR ' = paste0(str_dup('&nbsp;', 4), 'OR '),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'ORDER' = paste0(str_dup('&nbsp;', 1), 'ORDER'),
            'LIMIT' = paste0(str_dup('&nbsp;', 1), 'LIMIT')
          )
        ), 
      '</p>'
    )
  )
  
  # _output insight 4----
  
  # __left side panel----
  output$uiIns4a <- renderUI(
    if (!is.null(input$neigh4) & !is.null(input$qtr4)) {
      wellPanel(
        align = 'center',
        h3('Lead Time Percentiles'),
        hr(),
        fluidRow(
          column(
            width = 4,
            h3(style = 'color:#ffe700', '25%-ile'),
            h3(
              style = 'color:#ffe700',
              paste0(
                sprintf('%.2f', ins$i4m - 0.6745 * ins$i4d),
                ' days'
              )
            )
          ),
          column(
            width = 4,
            h3(style = 'color:#74ee15', '50%-ile'),
            h3(
              style = 'color:#74ee15',
              paste0(
                sprintf('%.2f', ins$i4m),
                ' days'
              )
            )
          ),
          column(
            width = 4,
            h3(style = 'color:#4deeea', '75%-ile'),
            h3(
              style = 'color:#4deeea',
              paste0(
                sprintf('%.2f', ins$i4m + 0.6745 * 0),
                ' days'
              )
            )
          )
        )
      ) 
    }
  )
  
  # __right side panel----
  output$uiIns4b <- renderUI(
    if (!is.null(input$neigh4) & !is.null(input$qtr4)) {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Plot',
            align = 'center',
            plotOutput('ins4'),
            hr(),
            uiOutput('binSldr')
          ),
          tabPanel(
            title = 'Code',
            htmlOutput('code4a'),
            htmlOutput('code4b')
          )
        )
      )
    }
  )
  
  # ___plot lead time----
  output$ins4 <- renderPlot(
    bg = 'transparent',
    if (!is.null(input$neigh4) & !is.null(input$qtr4)) {
      ins$cd4a <- paste0(
        'SELECT chkin_dt - book_dt lead\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE neigh = \'', input$neigh4, '\'\n',
        'AND date_part(\'qtr\', chkin_dt) = ', 
        as.numeric(str_sub(input$qtr4, -1)), ';'
      )
      ins$cd4b <- paste0(
        'SELECT avg(lead) mean, stddev(lead) sd\n',
        'FROM\n',
        '(\n',
        'SELECT chkin_dt - book_dt lead\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE neigh = \'', input$neigh4, '\'\n',
        'AND date_part(\'qtr\', chkin_dt) = ', 
        as.numeric(str_sub(input$qtr4, -1)), '\n) a;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd4a
      )
      zz <- dbGetQuery(
        conn = con,
        statement = ins$cd4b
      )
      ins$i4m <- zz$mean
      ins$i4d <- zz$sd
      ggplot(z, aes(x = lead)) +
        geom_histogram(
          aes(y = ..density..),
          binwidth = input$bins, 
          color = 'grey', 
          alpha = 0.75
        ) +
        geom_segment(
          aes(x = zz$mean, 
              xend = zz$mean, 
              y = 0, 
              yend = dnorm(zz$mean, zz$mean, zz$sd)),
          color = '#74ee15', 
          size = 1
        ) +
        geom_segment(
          aes(x = zz$mean - 0.6745 * zz$sd, 
              xend = zz$mean - 0.6745 * zz$sd, 
              y = 0, 
              yend = dnorm(zz$mean - 0.6745 * zz$sd, zz$mean, zz$sd)),
          color = '#ffe700', 
          size = 1
        ) +
        geom_segment(
          aes(x = zz$mean + 0.6745 * zz$sd, 
              xend = zz$mean + 0.6745 * zz$sd, 
              y = 0, 
              yend = dnorm(zz$mean + 0.6745 * zz$sd, zz$mean, zz$sd)),
          color = '#4deeea', 
          size = 1
        ) +
        geom_smooth(
          method = 'gam',
          mapping = aes(y = dnorm(lead, zz$mean, zz$sd)),
          color = 'red',
          size = 2
        ) +
        xlim(0, 50) +
        labs(x = 'Lead Time (Days)', y = 'Density') +
        theme_minimal(base_size = 16) +
        theme(
          legend.position = 'none',
          axis.title.x = element_text(colour = 'white'),
          axis.title.y = element_text(colour = 'white'),
          axis.text.x = element_text(colour = 'white'),
          axis.text.y = element_text(colour = 'white'),
          panel.grid.major = element_line(colour = '#000008'),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(
            fill = '#272a30',
            colour = '#272a30',
            size = 0.5, 
            linetype = 'solid'
          )
        )
    }
  )
  
  # ___bin width slider----
  output$binSldr <- renderUI(
    if (!is.null(input$neigh4) & !is.null(input$qtr4)) {
      sliderInput(
        inputId = 'bins',
        label = 'Histogram Bin Width (Days):',
        value = 4,
        min = 1,
        max = 7,
        step = 1,
        ticks = FALSE,
        width = '75%'
      )
    }
  )
  
  # ___code insight 4a----
  output$code4a <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd4a %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'OR ' = paste0(str_dup('&nbsp;', 4), 'OR '),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'ORDER' = paste0(str_dup('&nbsp;', 1), 'ORDER'),
            'LIMIT' = paste0(str_dup('&nbsp;', 1), 'LIMIT')
          )
        ), 
      '</p>'
    )
  )
  
  # ___code insight 4b----
  output$code4b <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd4b %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'OR ' = paste0(str_dup('&nbsp;', 4), 'OR '),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'ORDER' = paste0(str_dup('&nbsp;', 1), 'ORDER'),
            'LIMIT' = paste0(str_dup('&nbsp;', 1), 'LIMIT')
          )
        ), 
      '</p>'
    )
  )
  
  # _output insight 5----
  
  # __left side panel----
  output$uiIns5a <- renderUI(
    if (input$hot5 != '') {
      wellPanel(
        align = 'center',
        h3(
          paste0(
            'Guest Traffic per Quarter'
          )
        ),
        hr(),
        dataTableOutput('ins5a')
      )
    }
  )
  
  # ___datatable insight 5----
  output$ins5a <- renderDataTable(
    if (input$hot5 != '') {
      c <- paste0(
        '<style>th {text-align: center; color:black;}</style>',
        '<table><thead><tr>',
        '<th>Quarter</th>',
        '<th>Number of Guest</th>',
        '</tr></thead></table>'
      )
      datatable(data=ins$i5,
              options = list(paging=FALSE, searching=FALSE))
    }
  )
  
  # __right side panel----
  output$uiIns5b <- renderUI(
    if (input$hot5 != '') {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Plot',
            plotOutput('ins5b')
          ),
          tabPanel(
            title = 'Code',
            htmlOutput('code5')
          )
        )
      )
    }
  )
  
  # ___plot number of guests per quarter----
  output$ins5b <- renderPlot(
    bg = 'transparent',
    if (input$hot5 != '') {
      ins$cd5 <- paste0(
        'SELECT date_part(\'qtr\',chkin_dt)qtr, sum(guests)gue \n',
        'FROM bookings\n',
        'WHERE hotel_name = \'', input$hot5, '\'\n',
        'GROUP BY 1\n',
        'ORDER BY 1;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd5
      )
      ins$i5 <- z %>% 
        select(qtr, gue)
      z$gue = as.numeric(z$gue)
      print(ins$i5)
      ggplot(z, aes(x = qtr, y = gue)) + geom_col(color='cadetblue3',fill='cadetblue3') + ylim(0, max(z$gue)*1.2) + xlim(0.5,4.5) 
    }
  )
  
  # ___code insight 5----
  output$code5 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd5 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'OR ' = paste0(str_dup('&nbsp;', 4), 'OR '),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'CASE' = paste0(str_dup('&nbsp;', 7), 'CASE'),
            'WHEN' = paste0(str_dup('&nbsp;', 9), 'WHEN'),
            'ELSE' = paste0(str_dup('&nbsp;', 9), 'ELSE'),
            'END' = paste0(str_dup('&nbsp;', 7), 'END'),
            'rank' = paste0(str_dup('&nbsp;', 7), 'rank')
          )
        ), 
      '</p>'
    )
  )
  # _output insight 6----
  
  # __right side panel----
  output$uiIns6b <- renderUI(
    if (input$hot6 != '') {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Plot',
            plotOutput('ins6b')
          ),
          tabPanel(
            title = 'Code',
            htmlOutput('code6')
          )
        )
      )
    }
  )
  
  # ___plot guests per stay----
  output$ins6b <- renderPlot(
    if (input$hot6 != '' & input$par6 != '') {
      func <- NA
      if(input$par6 == "Average"){
        func = "avg"
      }
      ins$cd6 <- paste0(
        'SELECT extract(month from book_dt) as month, ',
        func,
        '(price_pn) as num\n',
        'FROM bookings\n',
        'JOIN hotels USING (hotel_id)\n',
        'WHERE hotels.hotel_name = \'', input$hot6 , '\'\n',
        'GROUP BY hotels.hotel_name, extract(month from book_dt) \n',
        'ORDER BY month ASC;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd6
      )
      #nn <- sum(z$n)
      ggplot(z,aes(x = month, y=num)) +
        geom_point(color = "red", size = 3) + 
        geom_line(
          color = 'red',
          size = 0.5
        ) +  
        geom_hline(yintercept = max(z$num), 
                   color = 'blue', size = 1) +
        geom_hline(yintercept = min(z$num), 
                   color = 'orange', size = 1) +
        labs(title="Price Curve") +
        xlab('Month') +
        ylab(paste0(input$par1 , ' Prices')) +
        scale_x_continuous(breaks = seq(1, 12, by = 1)) 
      
    }
  )
  
  # ___code insight 6----
  output$code6 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd6 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'ORDER' = paste0(str_dup('&nbsp;', 1), 'ORDER')
          )
        ), 
      '</p>'
    )
  )
  
  # _output insight 7----
  
  # __right side panel----
  output$uiIns7b <- renderUI(
    if (input$res7 != '') {
      wellPanel(
        tabsetPanel(
          type = 'pills',
          tabPanel(
            title = 'Plot',
            plotOutput('ins7b')
          ),
          tabPanel(
            title = 'Code',
            htmlOutput('code7')
          )
        )
      )
    }
  )
  
  # ___plot guests per stay----
  output$ins7b <- renderPlot(
    if (input$res7 != '') {
      ins$cd7 <- paste0(
        'SELECT rest_cost, count(rest_id)num \n',
        'FROM restaurants\n',
        'WHERE rest_type = \'', input$res7 , '\'\n',
        'GROUP BY 1\n',
        'ORDER BY 1 DESC;'
      )
      z <- dbGetQuery(
        conn = con,
        statement = ins$cd7
      )
      print(z)
      nn <- sum(z$n)
      treemap(
        dtf = z,
        index = 'rest_cost',
        vSize = 'num',
        type = 'index',
        palette = 'Set2',
        title = paste(
          'Distribution of prices of restaurant type',
          input$res7
        ),
        fontsize.title = 20,
        fontsize.labels = 16,
        border.col = 'grey'
      )
    }
  )
  
  # ___code insight 7----
  output$code7 <- renderText(
    expr = paste0(
      '<br><p style = "font-family:courier; color:lime;',
      'font-size: 16px;">', 
      ins$cd7 %>% 
        str_replace_all(
          c(
            '\n' = '<br>', 
            'FROM' = paste0(str_dup('&nbsp;', 2), 'FROM'),
            'JOIN' = paste0(str_dup('&nbsp;', 2), 'JOIN'),
            'WHERE' = paste0(str_dup('&nbsp;', 1), 'WHERE'),
            'AND' = paste0(str_dup('&nbsp;', 3), 'AND'),
            'GROUP' = paste0(str_dup('&nbsp;', 1), 'GROUP'),
            'ORDER' = paste0(str_dup('&nbsp;', 1), 'ORDER')
          )
        ), 
      '</p>'
    )
  )  
  
  
  
  
  
  # ***********************----
  
  # page - about us----
  
  # _navigate to about us page----
  observeEvent(
    eventExpr = input$pg5Bttn,
    updateNavbarPage(
      session, 'navBar',
      selected = 'e'
    )
  )

  # _ui about----
  output$abou <- renderUI(
    if (input$navBar == 'e') {
      h <- '125px'
      p <- '13px'
      fluidRow(
        style = 'padding-top:60px; padding-bottom:50px; margin:0px;
                 background-image:url(team5.jpg); 
                 background-size:cover;
                 background-position:center;',
        h1(style = 'padding-left:25px;color: white;', 'Our Team'),
        hr(),
        fluidRow(
          column(
            width = 2,
            align = 'right',
            img(src = paste0('1.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:none; padding:', p, ';', 
              'border-style:solid; border-width: 0;'
            ),
            h2(style='color: white',own$owner_name[1]),
            h4(own$owner_role[1])
          ),
          column(
            width = 3,
            align = 'right',
            img(src = paste0('2.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:none; padding:', p, ';', 
              'border-style:solid; border-width: 0;'
            ),
            h2(style='color: white',own$owner_name[2]),
            h4(own$owner_role[2])
          )
        ),
        br(),
        fluidRow(
          column(
            width = 2,
            align = 'right',
            img(src = paste0('3.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:none; padding:', p, ';', 
              'border-style:solid; border-width: 0;'
            ),
            h2(style='color: white',own$owner_name[3]),
            h4(own$owner_role[3])
          ),
          column(
            width = 3,
            align = 'right',
            img(src = paste0('4.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:none; padding:', p, ';', 
              'border-style:solid; border-width: 0;'
            ),
            h2(style='color: white',own$owner_name[4]),
            h4(own$owner_role[4])
          )
        ),
        br(),
        fluidRow(
          column(
            width = 2,
            align = 'right',
            img(src = paste0('5.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:none; padding:', p, ';', 
              'border-style:solid; border-width: 0;'
            ),
            h2(style='color: white',own$owner_name[5]),
            h4(own$owner_role[5])
          ),
          column(
            width = 3,
            align = 'right',
            img(src = paste0('6.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:none; padding:', p, ';', 
              'border-style:solid; border-width: 0;'
            ),
            h2(style='color: white',own$owner_name[6]),
            h4(own$owner_role[6])
          )
        ),
        br(),
        fluidRow(
          column(
            width = 2,
            align = 'right',
            img(src = paste0('7.jpeg'), height = h, 
                style = 'border-style:solid; border-width: medium;')
          ),
          column(
            width = 3,
            style = paste0(
              'background-color:none; padding:', p, ';', 
              'border-style:solid; border-width: 0;'
            ),
            h2(style='color: white',own$owner_name[7]),
            h4(own$owner_role[7])
          )
        )  
      )
    }
  )
  
}