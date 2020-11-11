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
    "CREATE TABLE IF NOT EXISTS SiayaHTS (
        ID integer primary key auto_increment,
        AgeAtTest integer,
        KeyPopulationType varchar(255),
        MaritalStatus varchar(255),
        Gender varchar(255),
        PatientDisabled varchar(255),
        EverTestedForHIV varchar(255),
        MonthsSinceLastTest integer,
        ClientTestedAs varchar(255),
        EntryPoint varchar(255),
        TestingStrategy varchar(255),
        TBScreening varchar(255),
        ClientSelfTested varchar(255),
        Sitecode varchar(255),
        Prediction double,
        TestResult varchar(255),
        TimeofTest varchar(255),
        month_of_test varchar(255),
        dayofweek varchar(255)
    )"
)

dbExecute(conn,
    "CREATE TABLE IF NOT EXISTS SiayaAccess (
        ID integer primary key auto_increment,
        usernames varchar(255),
        passwords varchar(255)
    )"
)

users <- dbGetQuery(conn, "SELECT * FROM SiayaAccess")
if(nrow(users) == 0) {
    dbExecute(conn, "
        INSERT INTO SiayaAccess(usernames, passwords)
        VALUES ('Laureen', '123'), ('Eric', '456'), ('Evans', '789')
    ")
}

dbDisconnect(conn)