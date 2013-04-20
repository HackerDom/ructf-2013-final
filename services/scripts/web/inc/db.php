<?php 

require_once 'inc/common.php';

function connect()
{
  $db_server = ':/home/scripts/mysql/mysql.sock';
  $db_username = 'scripts';
  $db_password = 'scripts__p4Ssw0rd';
  $db_dbname = 'scripts';
  $connection = mysql_pconnect($db_server, $db_username, $db_password);
  if (! $connection)
    error('Can\'t connect to database');
  mysql_select_db($db_dbname);
}

connect();



?>