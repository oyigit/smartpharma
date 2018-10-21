
# Libraries ---------------------------------------------------------------

library("shiny")
library("RSQLite")
library("sodium")
library("dtplyr")
library("dplyr")
library("plotly")
library("shinydashboard")

# Global Variables --------------------------------------------------------

app_name <- "Smart Pharma"
database <- "authorization.db"

support_contact <- "ozgurcanyigit@gmail.com"

forgot_password_message <- paste0(
  paste0("mailto:", support_contact, "?")
  , paste0("subject=", app_name, " - Forgot Password&")
  , "body=Hello,%0D%0A%0D%0AI have forgotten my password for this app, would you mind resetting the password for the user associated with this email address?%0D%0A%0D%0AThank you!"
) %>%
  URLencode

request_login_message <- paste0(
  paste0("mailto:", support_contact, "?")
  , paste0("subject=", app_name, " - Access Request&")
  , "body=Hello,%0D%0A%0D%0AI would like access to this app, would you mind initializing a username and password for this email address?%0D%0A%0D%0AThank you!"
) %>%
  URLencode

# Global Functions --------------------------------------------------------

composeUserGetQuery <- function(user) 
  # convenience function to cmopose the query given the table and user
  {
  sprintf('SELECT user, password, email, role, department FROM pw WHERE user = "%s"', user)
}

composeProductGetQuery <- function(user)
  # convenience function to cmopose the query given the table and user
{
  sprintf('SELECT * FROM user_products WHERE user = "%s"', user)
}

sendUserGetQuery <- function(user) 
  # convenience function to compose a query, retreive the results and if 
  # necessary, convert from null atomic vector to a NA value
  {
  query <- composeUserGetQuery(user) %>%
    dbSendQuery(db, .)
  
  response <- dbFetch(query) %>% 
    as.list %>%
    lapply(., function(x) ifelse(length(x) == 0L, NA_character_, x))
  
  dbClearResult(query)
  
  return(response)
}

sendProductGetQuery <- function(user) 
  # convenience function to compose a query, retreive the results and if 
  # necessary, convert from null atomic vector to a NA value
{
  query <- composeProductGetQuery(user) %>%
    dbSendQuery(db, .)
  
  response <- dbFetch(query) 
  return(response)
}

sendGeneralGetQuery <- function(table) {
  query <- dbSendQuery(db, paste0('SELECT * FROM ',table))
  response <- dbFetch(query) 
  return(response)
}

validateLogin <- function(stored, input) 
  # check the input against the stored value, returning a boolean and handling
  # the pesky NA values indicating a corrupted/non-existant login
  {
  if (is.na(stored)) {
    return(FALSE)
  } else {
    return(sodium::password_verify(stored, input))
  }
}

composeLoginModal <- function(...)
  # generate a modal with the inputs for a login a well as initialization
  # and password recovery links
  {
  showModal(
    modalDialog(
        id        = "loginmodal"
      , size      = 's'
      , easyClose = FALSE 
      , div(
          id = "modal-contents"
        , textInput('login_user', 'Kullanıcı')
        , passwordInput('login_passwd', 'Şifre')
        , div(...)
        , actionButton(
            inputId = 'login_button'
          , label   = 'Giriş'
          , class   = 'btn action-button btn-success'
          , icon    = icon('sign-in')
          ) #/ login-button
        ) #/ modal-contents
    , footer = div(id = "modal-footer" 
      , a(id = "forgot-login-link"
          , href = forgot_password_message
          , p("Şifremi unuttum", style = "display: inline;")
          )
      , HTML("&bull;")
      , a(id = "request-login-link"
          , href = request_login_message
          , p("Yeni Kullanıcı", style = "display: inline;")
          )
      ) #/ modal-footer
    ) #/ modalDialog
  ) #/ showModal
}

showConfirmModal <- function(id, ...) {
  showModal(
    modalDialog(
        id        = sprintf("%s-confirm-modal", id)
      , size      = 's'
      , easyClose = TRUE
      , div(...)
      , div(style = "text-align: right"
        , actionButton(sprintf("%s_ok", id), "OK", icon = icon("check"), style = "display: inline;")
        , actionButton(sprintf("%s_cancel", id), "İptal", icon = icon("times"), style = "display: inline;")
      )
      , footer = NULL
    ) #/ modalDialog
  ) #/ showModal
}


# if the database hasn't been initialized, create it
if (!file.exists(database)) 
  source("populate_database.R", local = TRUE)

# initialize a connection for the session -- a server observer will 
# automatically close the connection for us upon exit
db <- dbConnect(SQLite(), dbname = database)
