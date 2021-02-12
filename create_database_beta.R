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

dbExecute(conn,
          "CREATE TABLE IF NOT EXISTS HomaBayHTS (
        ID integer primary key auto_increment,
        AgeAtTest integer,
        MaritalStatus varchar(255),
        Gender varchar(255),
        EverTestedForHIV varchar(255),
        MonthsSinceLastTest integer,
        ClientTestedAs varchar(255),
        TestingStrategy varchar(255),
        ClientSelfTested varchar(255),
        TBScreening varchar(255),
        Sitecode varchar(255),
        month_of_test varchar(255),
        dayofweek varchar(255),
        KeyPopulationType varchar(255),
        Prediction double,
        TestResult varchar(255),
        TimeofTest varchar(255)
    )"
)

dbExecute(conn,
          "CREATE TABLE IF NOT EXISTS HomaBayAccess (
        ID integer primary key auto_increment,
        usernames varchar(255),
        passwords varchar(255)
    )"
)

users <- dbGetQuery(conn, "SELECT * FROM HomaBayAccess")
if(nrow(users) == 0) {
  dbExecute(conn, "
        INSERT INTO HomaBayAccess(usernames, passwords)
        VALUES ('Laureen', '123'), ('Eric', '456'), ('Evans', '789')
    ")
}

dbDisconnect(conn)

