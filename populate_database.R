
composeUserAddQuery <- function(user, password, email, role, department) 
  # convenience function to salt and hash a password before composing
  # a query to add it to the database
  {
  sodium::password_store(password) %>%
    sprintf("INSERT INTO pw VALUES('%s', '%s', '%s', '%s', '%s')", user, ., email, role, department)
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
dbClearResult(dbSendQuery(db, 'CREATE TABLE pw (user TEXT, password TEXT, email TEXT, role TEXT, department TEXT)'))

# initialize a DT of some dummy logins
db_logins <- data.table::data.table(
  user = c('Mert', 'Ozgur', 'Senay', 'Merve'),
  role = c('Principal', 'Data Scientist', 'Marketing Director', 'Product Manager'),
  password = rep("Welcome1", 4),
  department = c('Marketing Excellence', 'Business Intelligence', 'Probiotics', 'Oncologics')
)
db_logins[, email := paste0(user, "@ilacfirmasi.com")]

# perform additions
success <- db_logins[, mapply(sendUserAddQuery, user, password, email, role, department)]

# check that all are TRUE
stopifnot(all(success))

dbDisconnect(db)
