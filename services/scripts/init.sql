CREATE USER 'scripts'@'localhost' IDENTIFIED BY 'scripts__p4Ssw0rd';
CREATE DATABASE IF NOT EXISTS scripts;
GRANT ALL PRIVILEGES ON scripts.* TO 'scripts'@'localhost';