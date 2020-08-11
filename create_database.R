# Load the RSQLite Library
library(RSQLite)

conn <- dbConnect(RSQLite::SQLite(), "HTS.db")

dbGetQuery(conn, "CREATE TABLE NairobiHTS (
           ID integer primary key autoincrement,
           AgeAtTest integer,
           KeyPopulationType varchar,
           MaritalStatus varchar,
           Gender varchar,
           PatientDisabled varchar,
           EverTestedForHIV varchar,
           MonthsSinceLastTest integer,
           ClientTestedAs varchar,
           EntryPoint varchar,
           TestingStrategy varchar,
           TBScreening varchar,
           ClientSelfTested varchar,
           CoupleDiscordant varchar,
           Sitecode varchar,
           Prediction numeric,
           TestResult varchar,
           TimeofTest varchar)")

dbDisconnect(conn)

# Set up table with usernames and passwords
df <- data.frame(
  usernames = c("Laureen", "Eric", "Evans"),
  passwords = c(123, 456, 789)
)

conn <- dbConnect(RSQLite::SQLite(), "HTS.db")

dbWriteTable(conn, "NairobiAccess", df)

dbDisconnect(conn)

# df <- data.frame(ID = NA, dat[20, ], Prediction = .2)
# 
# dbWriteTable(conn, "NairobiHTS", df, append = TRUE)
# 
# dbGetQuery(conn, "SELECT * FROM NairobiHTS")
# 
# dbRemoveTable(conn, "NairobiHTS")
