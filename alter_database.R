library(RMariaDB)

dbConfig <- config::get("database")
conn <- dbConnect(
    RMariaDB::MariaDB(),
    dbname = dbConfig$dbname,
    host = dbConfig$host,
    port = dbConfig$port,
    username = dbConfig$username,
    password = dbConfig$password
)
dbExecute(conn, "ALTER TABLE SiayaHTS ADD month_of_test varchar(255), ADD dayofweek varchar(255);")
dbDisconnect(conn)