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

  $user_info = user_info($params['session']);
  $query = 'SELECT * FROM scripts WHERE uid = \''.$user_info['uid'].'\' ORDER BY id DESC';
  $ans = mysql_query($query);
  $user_codes = array();

  while ($a = mysql_fetch_array($ans, MYSQL_ASSOC))
    $user_codes[] = $a;
  
  i18n_template('scripts');
?>