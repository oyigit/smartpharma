
composeUserAddQuery <- function(user, password, email, role, department) 
  # convenience function to salt and hash a password before composing
  # a query to add it to the database
  {
  sodium::password_store(password) %>%
    sprintf("INSERT INTO pw VALUES('%s', '%s', '%s', '%s', '%s')", user, ., email, role, department)
}

composeGeneralAddQuery <- function(x , ...) {
  
  variable.list <- list(...)
  var.count <- length(variable.list)
  
  text <- paste0("INSERT INTO ",x, " VALUES(")

  product.list <- character(0)
  for(i in 1:var.count) {
    temp <- paste0("'",variable.list[i],"',")
    if(i == var.count) {
      temp <- paste0("'",variable.list[i],"'")
    }
    product.list <- paste0(product.list, temp)
  }
  text <- paste0(text, product.list, ")")
  dbSendQuery(db, text) %>%
    dbClearResult()
}


sendUserAddQuery <- function(user, password, email, role, department) 
  # convenience function to call the composer and send the query
  # returning named booleans indicating the success of each row
  {
  composeUserAddQuery(user, password, email, role, department) %>%
    dbSendQuery(db, .) %>%
    dbClearResult()
}

# initialize a connection, will create a db if not exists
db <- dbConnect(SQLite(), dbname = database)

# create the table for the logins
dbClearResult(dbSendQuery(db, 'DROP TABLE IF EXISTS pw'))
dbClearResult(dbSendQuery(db, 'DROP TABLE IF EXISTS user_products'))
dbClearResult(dbSendQuery(db, 'CREATE TABLE pw (user TEXT, password TEXT, email TEXT, role TEXT, department TEXT)'))
dbClearResult(dbSendQuery(db, 'CREATE TABLE user_products (user TEXT, product TEXT)'))

# initialize a DT of some dummy logins
db_logins <- data.table::data.table(
  user = c('Mert', 'Ozgur', 'Senay', 'Merve'),
  role = c('Principal', 'Data Scientist', 'Marketing Director', 'Product Manager'),
  password = rep("Welcome1", 4),
  department = c('Marketing Excellence', 'Business Intelligence', 'Probiotics', 'Oncologics')
)
db_logins[, email := paste0(user, "@ilacfirmasi.com")]

# Initialize another DT of product - user mappings

initial.product.list <- c('A', 'B', 'C', 'D', 'E')

db_products <- data.table::data.table(
  user = c(rep('Mert', 5), rep('Ozgur', 5), rep('Senay', 3), rep('Merve', 2)),
  product = c(initial.product.list, initial.product.list, 'A', 'B', 'C', 'D', 'E')
)

# perform additions
login.success <- db_logins[, mapply(sendUserAddQuery, user, password, email, role, department)]
userproduct.success <- db_products[, mapply(composeGeneralAddQuery, 'user_products', user, product)]


# check that all are TRUE
stopifnot(all(success))
stopifnot(all(userproduct.success))


dbDisconnect(db)
