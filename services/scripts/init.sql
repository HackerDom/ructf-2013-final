CREATE USER 'scripts'@'localhost' IDENTIFIED BY 'scripts__p4Ssw0rd';
CREATE DATABASE IF NOT EXISTS scripts;
GRANT ALL PRIVILEGES ON scripts.* TO 'scripts'@'localhost';

USE scripts;
CREATE TABLE IF NOT EXISTS `scripts` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `uid` varchar(30) NOT NULL,
  `name` varchar(30) NOT NULL,
  `script` text NOT NULL,
  PRIMARY KEY (`id`),
  KEY `id` (`id`)
) ENGINE=InnoDB;