<?php
  require_once 'inc/db.php';
  require_once 'inc/common.php';
  require_once 'inc/users.php';
  require_once 'inc/i18n.php';
  require_once 'code/execute.php';

  if (! isset($_COOKIE['session']) || ! is_logon($_COOKIE['session']))
  {
    include 'templates/index.en.html';
    exit;
  }

  if (array_key_exists('code_id', $_POST) && array_key_exists('input', $_POST))
  {
    $code_id = (int) $_POST['code_id'];
    $input = $_POST['input'];
    
    $query = 'SELECT script FROM scripts WHERE id = '.$code_id.' LIMIT 1';
    $script = mysql_fetch_array(mysql_query($query), MYSQL_ASSOC);
    $script = $script['script'];

    $program_input = split("\n", $input); 
    try
    {
      execute_program($script);
    }
    catch (Exception $e)
    {
      $error = $e->getMessage();
    }

    $result = isset($program_output) ? $program_output : '';
  }

  $user_info = user_info($_COOKIE['session']);
  $query = 'SELECT * FROM scripts WHERE uid = \''.$user_info['uid'].'\' ORDER BY id DESC';
  $ans = mysql_query($query);
  $user_codes = array();

  while ($a = mysql_fetch_array($ans, MYSQL_ASSOC))
    $user_codes[] = $a;
  
  i18n_template('scripts');
?>