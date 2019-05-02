# Install R packages
# Add the ones you need below

# Import R libraries
# Add the ones you need below
library(RMySQL) # Import data table from LS database 

# Data import from mysql
mydb = dbConnect(MySQL(), user="dae_student", password="GrapeDrop99", dbname="dae", host="192.38.56.104")
rs<-dbSendQuery(mydb, "SELECT * FROM la_baguette")
baguette<- fetch(rs, n=-1);dbClearResult(dbListResults(mydb)[[1]])

View(baguette)

# Analyse your data below 