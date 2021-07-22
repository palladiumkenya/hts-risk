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
dbExecute(conn, "ALTER TABLE homabayhts ADD HTSNumber varchar(25);
UPDATE `hts_risk`.`homabayaccess` SET `usernames` = 'Test' WHERE `usernames` = 'Eric';")
dbDisconnect(conn)