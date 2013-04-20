<?php

require_once 'inc/api.php';

function user_info($session)
{
  return api_request('', 'user', array('session' => $session));
}

function is_logon($session)
{
  $user_info = user_info($session);
  return $user_info['status'] == 'OK';
}

function get_scripts($uid)
{
  $query = 'SELECT * FROM scripts WHERE uid = ';
}

?>