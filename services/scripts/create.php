<?php
  require_once 'inc/db.php';
  require_once 'inc/common.php';
  require_once 'inc/auth.php';

  $params = check_args(array('session', 'name', 'code'));
  
  echo json_encode($params);
  echo user_info($params['session'])
?>