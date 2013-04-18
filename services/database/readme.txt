Query syntax

SELECT (*|fieldname1, fieldname2, ...) FROM dbname.tablename [ WHERE conditions ]
DROP TABLE dbname.tablename | CREATE TABLE dbname.tablename (fieldname1, fieldname2, ...)
DROP DATABASE dbname | CREATE DATABASE dbname
INSERT INTO dbname.tablename VALUES (literal1, literal2, ...)
DELETE FROM dbname.tablename [ WHERE conditions ]

Conditions

fieldname_or_literal ==|!= fieldname_or_literal [and|or conditions]

Allowed symbols

fieldname, dbname, tablename: [A-Za-z]+
literal: '.*'
