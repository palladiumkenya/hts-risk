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
dbExecute(conn, "ALTER TABLE HomaBayHTS ADD LastHIVTest varchar(255), ADD SexLast12Months varchar(255), ADD NumberSexPartner varchar(255), ADD KnowHIVStatusSexPartner varchar(255), ADD HIVStatusSexPartner varchar(255);")
dbDisconnect(conn)