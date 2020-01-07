#Loading Library
library(RJDBC)

#Loading Jar Files
db2 <- JDBC("com.ibm.db2.jcc.DB2Driver",
            "db2jars/db2jcc.jar")

#Establishing DataBase Connection
conn = dbConnect(db2,
                 "jdbc:db2://ipadress:port/dbname",
                 user="username",
                 password="password")

#Query the Database
xyz <- dbSendQuery(conn, "select * from gvmc_user.compliant_details_t where RECORD_STATUS=1
                   order by com_dt desc fetch first 100 rows only")

#Fetch Data as Per Provided Query
df<-fetch(xyz,-1)

#Display First 5 rows of Reteived Dataframe
print(head(df))
