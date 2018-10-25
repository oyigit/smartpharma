function(input, output, session) {
  
  login <- reactiveValues(login = FALSE, user = NULL, role = NULL, email = NULL, department = NULL)
  values <- reactiveValues(products = data_frame(),
                           tl.product.sales = data_frame(),
                           unit_product_sales = data_frame())
  
  # initially display the login modal
  observe({
    composeLoginModal()
  })
  
  observeEvent(input$logout_ok, {
    shiny::removeModal()
    
    # clear the values when logout is confirmed
    login$login <- FALSE
    login$user  <- NULL
    login$role  <- NULL
    login$email <- NULL
    login$department <- NULL
    
    composeLoginModal(
      div(
          id    = "modal-logout-message"
        , style = "margin-bottom: 10px"
        , span(class = "text-muted", "Başarıyla çıkış yapıldı")
      ) #/ modal-logout-message
    ) #/ composeLoginModal
  })
  
  # once a login is attempted, do some checks
  observeEvent(input$login_button, {
    
    # remove the modal while we check
    shiny::removeModal()
    
    # query the database for that user will return NAs if not populated
    stored <- sendUserGetQuery(input$login_user)
    values$products <- sendProductGetQuery(input$login_user)
    values$tl.product.sales <- sendGeneralGetQuery("tl_product_sales") %>% 
      filter(product %in% values$products$product)
    values$unit.product.sales <- sendGeneralGetQuery("unit_product_sales") %>% 
      filter(product %in% values$products$product)
    
    # if any are NA then the record doesn't exist or the record is corrupted
    user_invalid <- stored %>% sapply(is.na) %>% any
    
    # try to login, will automatically handle NULL-y objects
    login$login <- validateLogin(stored$password, input$login_passwd)
    
    # if the login is not successful, toss up another login modal, 
    # this time with a message
    if (isTRUE(user_invalid) | login$login == FALSE) {
      composeLoginModal(
        div(
            id    = "modal-login-message"
          , style = "margin-bottom: 10px"
          , span(style = "color: red; font-weight:bold", "Incorrect Login/Password")
          ) #/ modal-login-message
        ) #/ composeLoginModal
    } else {
      # if the login is successful, populate the known values
      login$user  <- stored$user
      login$role  <- stored$role
      login$email <- stored$email
      login$department <- stored$department
      
      rm(stored)
    } #/ fi
  }) #/ login_button Observer
  
  # close database conncention on exit
  session$onSessionEnded(function() {
    dbDisconnect(db)
  })
  
  observeEvent(input$logout, {
    helpText("Çıkmak istediğinizden emin misiniz?") %>%
      div(style = "margin-bottom: 15px", .) %>%
      showConfirmModal("logout", .)
  })
  
  observeEvent(input$logout_cancel, {
    shiny::removeModal()
  })
  
  output$welcome <- renderUI({
    # wait to render until the login is truthy
    req(login$login)
    
    bootstrapPage(
      fluidRow(
      column(width = 3,
        box(width = NULL,
          title = "Girdiler", status = "primary", solidHeader = TRUE,
        selectInput("product.selector", label = 'Ürün seçimi', choices = values$products$product)
        )
      ),
      column(width = 9,
        box(width = NULL, status = "success", solidHeader = TRUE, title = 'TL Satışlar',
        plotlyOutput("tl.product.sales")
        ),
        box(width = NULL, status = "success", solidHeader = TRUE, title = 'Kutu Satışlar',
        plotlyOutput("unit.product.sales")
        )
      )
      )
      )
    

  })
  
  
  output$sidebar <- renderMenu({
    
    req(login$login)
    
      menuItem("Ürün tahminlemeleri", tabName = "productforecasts", icon = icon("dashboard"))
  })
  
  output$tl.product.sales <- renderPlotly({
    values$tl.product.sales %>%
      mutate(month = as.Date(month)) %>%
      filter(product == input$product.selector) %>%
      plot_ly(x = ~month, y = ~sales, mode = 'lines', type = 'scatter') %>%
      layout(
        xaxis = list(title = '',
          rangeselector = list(
            buttons = list(
              list(
                count = 3,
                label = "3 ay",
                step = "month",
                stepmode = "backward"),
              list(
                count = 6,
                label = "6 ay",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yıl",
                step = "year",
                stepmode = "backward"),
              list(
                count = 1,
                label = "YTD",
                step = "year",
                stepmode = "todate"),
              list(step = "all"))),
          
          rangeslider = list(type = "date")),
        
        yaxis = list(title = "TL"))
  })
  output$unit.product.sales <- renderPlotly({
    values$unit.product.sales %>%
      mutate(month = as.Date(month)) %>%
      filter(product == input$product.selector) %>%
      plot_ly(x = ~month, y = ~sales, type = 'bar') %>%
      layout(
        xaxis = list(title = '',
          rangeselector = list(
            buttons = list(
              list(
                count = 3,
                label = "3 ay",
                step = "month",
                stepmode = "backward"),
              list(
                count = 6,
                label = "6 ay",
                step = "month",
                stepmode = "backward"),
              list(
                count = 1,
                label = "1 yıl",
                step = "year",
                stepmode = "backward"),
              list(
                count = 1,
                label = "YTD",
                step = "year",
                stepmode = "todate"),
              list(step = "all"))),
          
          rangeslider = list(type = "date")),
        
        yaxis = list(title = "Kutu"))
  })
}




