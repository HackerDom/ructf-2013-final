<?php
  require_once 'inc/db.php';
  require_once 'inc/common.php';
  require_once 'inc/users.php';
  require_once 'inc/i18n.php';
  require_once 'code/execute.php';

  $params = check_args(array('session', 'code_id', 'input'));

  if (! isset($params['session']) || ! is_logon($params['session']))
  {
    i18n_template('index');
    exit;
  }

  if (array_key_exists('code_id', $params) && array_key_exists('input', $params))
  {
    $code_id = (int) $params['code_id'];
    $input = $params['input'];
    
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

  require 'scripts.php';
?>