<?php
  require_once 'inc/db.php';
  require_once 'inc/common.php';
  require_once 'inc/users.php';

  if (array_key_exists('save', $_POST))
  {
    $params = check_args(array('session', 'name', 'code'));

    if (! is_logon($params['session']))
    {
      require 'index.php';
      exit;
    }

    $user_info = user_info($params['session']);
    $uid = $user_info['uid'];
    $name = addslashes($params['name']);
    $code = addslashes($params['code']);
  
    $query = "INSERT INTO scripts (uid, name, code) VALUES ('$uid', '$name', '$code')";
    mysql_query($query);
  }

  include 'templates/create.en.html';
?>