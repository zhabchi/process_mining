library(shinymanager)
# you can use keyring package to set database key
#library(keyring)
#key_set("R-shinymanager-key", "ziadhabchi") # pass 1234


credentials <- data.frame(
  user = c("shiny", "shinymanager"),
  password = c("azerty", "Pass@Shiny1"), # password will automatically be hashed
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)

# Create the database
create_db(
  credentials_data = credentials,
  sqlite_path = "database/users.sqlite", # will be created
  passphrase = "Travelport"
)


