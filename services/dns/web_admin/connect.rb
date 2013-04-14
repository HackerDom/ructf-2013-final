require 'mysql'

dbh = Mysql.real_connect("localhost", "dns", "default_password", "dns")
dbh.query("DROP TABLE IF EXISTS records")
dbh.query("CREATE TABLE records
              (
                id		varchar(32),
                type	varchar(10),
                key		varchar(40)
              )
            ")
