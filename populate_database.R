
composeUserAddQuery <- function(user, password, email, role, department) 
  # convenience function to salt and hash a password before composing
  # a query to add it to the database
  {
  sodium::password_store(password) %>%
    sprintf("INSERT INTO pw VALUES('%s', '%s', '%s', '%s', '%s')", user, ., email, role, department)
}

GeneralDBAddQuery <- function(x , y) {

  init <- paste0("INSERT INTO ",x, " VALUES(")

  for(j in 1:nrow(y)){
  paste.list <- character(0)
  for(i in 1:ncol(y)) {
    temp <- paste0("'",y[j,i],"',")
    if(i == ncol(y)) {
      temp <- paste0("'",y[j,i],"'")
    }
    paste.list <- paste0(paste.list, temp)
  }
  text <- paste0(init, paste.list, ")")
  dbSendQuery(db, text) %>%
    dbClearResult()
  }
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

# create the table for the logins, user_product_mappings and product sales
dbClearResult(dbSendQuery(db, 'DROP TABLE IF EXISTS pw'))
dbClearResult(dbSendQuery(db, 'DROP TABLE IF EXISTS user_products'))
dbClearResult(dbSendQuery(db, 'DROP TABLE IF EXISTS tl_product_sales'))
dbClearResult(dbSendQuery(db, 'DROP TABLE IF EXISTS unit_product_sales'))
dbClearResult(dbSendQuery(db, 'CREATE TABLE pw (user TEXT, password TEXT, email TEXT, role TEXT, department TEXT)'))
dbClearResult(dbSendQuery(db, 'CREATE TABLE user_products (user TEXT, product TEXT)'))
dbClearResult(dbSendQuery(db, 'CREATE TABLE tl_product_sales (product TEXT, month TEXT, sales INT)'))
dbClearResult(dbSendQuery(db, 'CREATE TABLE unit_product_sales (product TEXT, month TEXT, sales INT)'))

# initialize a DT of some dummy logins
db_logins <- data.table::data.table(
  user = c('Mert', 'Ozgur', 'Senay', 'Merve'),
  role = c('Principal', 'Data Scientist', 'Marketing Director', 'Product Manager'),
  password = rep("Welcome1", 4),
  department = c('Marketing Excellence', 'Business Intelligence', 'Probiotics', 'Oncologics')
)
db_logins[, email := paste0(user, "@ilacfirmasi.com")]

# Initialize another DT of product - user mappings

# write.csv(tl_sales, "initial_tl_sales_db.csv", row.names = FALSE)
# write.csv(unit_sales, "initial_unit_sales_db.csv", row.names = FALSE)

db_products <- read.csv("initial_productusermappings_db.csv", stringsAsFactors = FALSE)
tl_product_sales <- read.csv("initial_tl_sales_db.csv", stringsAsFactors = FALSE)
unit_product_sales <- read.csv("initial_unit_sales_db.csv", stringsAsFactors = FALSE)

products <- tl_product_sales %>%
  filter(month == '2012-10-01') %>%
  slice(1:50)

db_products <- data_frame(user = c(rep('Mert',20), rep('Ozgur', 10), rep('Senay',20)))

db_products <- db_products %>% bind_cols(products %>% select(1))

tl_product_sales <- tl_product_sales %>%
  filter(prd %in% products$prd)

unit_product_sales <- unit_product_sales  %>%
  filter(prd %in% products$prd)


# perform additions
login.success <- db_logins[, mapply(sendUserAddQuery, user, password, email, role, department)]
# GeneralDBAddQuery('product_sales', db_product_sales, product, year, sales)
GeneralDBAddQuery('user_products', db_products)
GeneralDBAddQuery('tl_product_sales', tl_product_sales)
GeneralDBAddQuery('unit_product_sales', unit_product_sales)


dbDisconnect(db)
