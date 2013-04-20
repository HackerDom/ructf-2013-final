<?php 
  require_once 'inc/i18n.php';
  require_once 'inc/users.php';

  if (array_key_exists('session', $_COOKIE) && is_logon($_COOKIE['session']))
  {
    require 'create.php';
    exit;
  }

  i18n_template('index');

?>