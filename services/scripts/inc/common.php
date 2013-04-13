<?php

function error($error_msg)
{
  echo $error_msg;
  exit;
}

function check_args($args)
{
  $result = array();
  foreach ($args as $arg)
  {
    if (! isset($_GET[$arg]))
      error('Parameter '.$arg.' isn\'t defined');
    $result[$arg] = $_GET[$arg];
  }
  return $result;
}

?>