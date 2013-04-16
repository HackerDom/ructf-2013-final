<?php 

  if (array_key_exists('session', $_COOKIE) && is_logon($_COOKIE['session']))
  {
    require 'create.php';
    exit;
  }

  require 'templates/index.en.html';

?>