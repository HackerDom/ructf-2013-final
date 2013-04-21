<?php

require_once 'inc/api.php';

function wrapper_print($what)
{
  global $program_output;
  if (! isset($program_output))
    $program_output = "";
  if (is_array($what))
    $program_output .= json_encode($what);
  else
    $program_output .= $what;
  $program_output .= "\n";
  return 1;
}

function wrapper_input()
{
  global $program_input;
  if (sizeof($program_input) == 0)
    throw new Exception('Error: read attempt from empty input');
  $first = trim($program_input[0]);
  $program_input = array_slice($program_input, 1);
  if (is_numeric($first))
    $first = (int) $first;
  return $first;
}

function wrapper_api($service, $function)
{
  global $services;
  $args = array_slice(func_get_args(), 2);
  $args_names = $services[$service][$function];
  $a = array();
  foreach ($args_names as $idx => $arg_name)
    $a[$arg_name] = $args[$idx];
  return api_request($service, $function, $a);
}

?>