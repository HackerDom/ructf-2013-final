<?php
  require_once 'inc/db.php';
  require_once 'inc/common.php';
  require_once 'inc/users.php';
  require_once 'inc/i18n.php';
  require_once 'code/lex.php';

  $params = check_args(array('session', 'name', 'code', 'save'));

  if (array_key_exists('save', $params))
  {
    required_args(array('session', 'name', 'code', 'save'));

    if (! is_logon($params['session']))
    {
      require 'index.php';
      exit;
    }

    $user_info = user_info($params['session']);
    $uid = $user_info['uid'];
    $name = addslashes($params['name']);
    $code = $params['code'];
    try
    {
      $code = compile($code);
    }
    catch (Exception $e)
    {
      $error = $e->getMessage();
    }
  
    if (! isset($error))
    {
      $query = "INSERT INTO scripts (uid, name, script) VALUES ('$uid', '$name', '$code')";
      mysql_query($query);
      $name = '';
      $code = '';
      header('Location: scripts.php');
    }
  }

  if (array_key_exists('session', $params) && is_logon($params['session']))
  {
    i18n_template('create');
  }
  else
  {
    i18n_template('index');
  }
?>