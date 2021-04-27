ui <- tagList(
  
  useShinyjs(),
  useShinyalert(),

  # ***********************----
  # navbarpage layout----
  navbarPage(
    id = 'navBar',
    # hyperlink to a website of your choice
    title = tags$a(
      href = 'https://sps.columbia.edu/academics/masters/applied-analytics',
      target = '_blank',
      img(src = 'cu_light.png', height = '25px')
    ),
    windowTitle = 'Las Vegas', #<--what the tab on browser will state
    position = 'fixed-top', #<--fixed-bottom if you want menu bar on bottom
    collapsible = TRUE, #<--collapse navbar tabs when window is shrunk
    theme = shinytheme('lumen'), #<--select shinytheme https://rstudio.github.io/shinythemes/
      # cerulean, cosmo, cyborg, darkly, flatly, journal, lumen, paper
      # readable, sandstone, simplex, slate, spacelab, superhero, united, yeti
    
    # ***********************----
    # home page----
    tabPanel(
      value = '0',
      title = div(
        img(src = 'vlogo.png', height = 25) #<--image for the home page
      ),
      # ui home rendered on server
      uiOutput('home'),
      
      # _hotel section----
      fluidRow(
        style = 'background-image:url(pg1.png); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg1Bttn',
          label = h1('Hotels'),
          style = 'simple'
        )
      ),
      
      # _attractions section----
      fluidRow(
        style = 'background-image:url(pg2.jpg); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg2Bttn',
          label = h1('Attractions'),
          style = 'simple'
        )
      ),
      
      # _restaurants section----
      fluidRow(
        style = 'background-image:url(pg3.jpg); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg3Bttn',
          label = h1('Restaurants'),
          style = 'simple'
        )
      ),
      
      # _insights section----
      fluidRow(
        style = 'background-image:url(pg4.jpg); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg4Bttn',
          label = h1('Insights'),
          style = 'simple'
        )
      ),
      
      # _about us section----
      fluidRow(
        style = 'background-image:url(pg5.jpg); 
                 background-size:cover;
                 padding:20%;',
        align = 'center',
        actionBttn(
          inputId = 'pg5Bttn',
          label = h1('About Us'),
          style = 'simple'
        )
      ),
      
      # _footer section----
      fluidRow(
        wellPanel(
          style = 'padding:20px; background-color:#ffffff;',
          align = 'center',
          h4('Our Partners:'),
          hr(),
          tags$span(
            # __MGM
            tags$a(
              href = 'https://www.mgmresorts.com/',
              target = '_blank',
              img(src = 'MGM.png', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __Hilton
            tags$a(
              href = 'https://www.hilton.com/',
              target = '_blank',
              img(src = 'Hilton.png', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __wynn
            tags$a(
              href = 'https://www.wynnlasvegas.com/',
              target = '_blank',
              img(src = 'wynn1.jpeg', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __trump
            tags$a(
              href = 'https://www.trumphotels.com/',
              target = '_blank',
              img(src = 'trump-logo.jpeg', height = '30px')
            ),
            HTML(str_dup('&nbsp;', 10)),
            # __sheraton
            tags$a(
              href = 'https://sheraton.marriott.com/',
              target = '_blank',
              img(src = 'sheraton.png', height = '30px')
            )
          )
        )
      )
    ),
  
    # ***********************----
    # tabs----
    # _hotels tab----
    tabPanel(
      value = 'a',
      title = div(
        img(src = 'icon_1.png', height = '25px'), 
        HTML('&nbsp;'), 'Hotels'
      ),
      h3(style = 'padding-top:70px;', 'Las Vegas Hotels'),
      hr(),
      uiOutput('hote')
    ),
    
    # _attractions tab----
    tabPanel(
      value = 'b',
      title = div(
        img(src = 'icon_2.png', height = '25px'), 
        HTML('&nbsp;'), 'Attractions'
      ),
      h3(style = 'padding-top:70px;', 'Las Vegas Attractions'),
      hr(),
      uiOutput('attr')
    ),
    
    # _restaurants tab----
    tabPanel(
      value = 'c',
      title = div(
        img(src = 'icon_3.png', height = '25px'), 
        HTML('&nbsp;'), 'Restaurants'
      ),
      h3(style = 'padding-top:70px;', 'Las Vegas Restaurants'),
      hr(),
      uiOutput('rest')
    ),
    
    # _insights tab----
    tabPanel(
      value = 'd',
      title = div(
        img(src = 'icon_4.png', height = '25px'), 
        HTML('&nbsp;'), 'Insights'
      ),
      fluidRow(
        style = 'padding-top:70px; padding-left:10px; padding-right:10px;',
        radioGroupButtons(
          inputId = 'insight',
          label = 'Select an Insight to View:',
          choices = c(paste0('Insight ', 1:7)),
          selected = character(0),
          justified = TRUE
        )
      ),
      uiOutput('insi')
    ),
    
    # _about us tab----
    tabPanel(
      value = 'e',
      title = div(
        img(src = 'icon_5.png', height = '25px'), 
        HTML('&nbsp;'), 'About Us'
      ),
      uiOutput('abou')
    )
  
  )

)
  

