<?php

require_once 'lex.php';
require_once 'wrappers.php';

define('MAX_ADDRESS', 10000);

$memory = array();
$ip = 0;

function check_address($address)
{
  if (! is_numeric($address))
    throw new Exception('Error: invalid address: '.$address);
  if ($address < 0 || $address >= MAX_ADDRESS)
    throw new Exception('Error: invalid address: '.$address);  
}

function get_memory_value($address)
{
  global $memory;
  $address = split('\.', $address);
  check_address($address[0]);
  $result = array_key_exists($address[0], $memory) ? $memory[$address[0]] : null;
  foreach (array_slice($address, 1) as $p)
  {
    if ($result)
      $result = array_key_exists($p, $result) ? $result[$p] : null;
  }
  return $result;
}

function store($address, $value)
{
  global $memory;
  $address = split('\.', $address);
  check_address($address[0]);
  $current = '$memory['.$address[0].']';
  foreach (array_slice($address, 1, sizeof($address) - 2) as $p)
  {
    if (! is_array(eval($current)))
      eval($current.' = array();');
    $current = $current.'['.$p.']';
  }
  if (sizeof($address) > 1)
    $current .= '['.$address[sizeof($address) - 1].']';
/*  echo $current.' = '.$value.';'."\n";*/
  eval($current.' = '.$value.';');
}

function func_equal($p1, $p2) { return (int) ($p1 == $p2); }
function func_less($p1, $p2) { return (int) ($p1 < $p2); }
function func_greater($p1, $p2) { return (int) ($p1 > $p2); }
function func_equal_less($p1, $p2) { return (int) ($p1 <= $p2); }
function func_equal_greater($p1, $p2) { return (int) ($p1 >= $p2); }
function func_not_equal($p1, $p2) { return (int) ($p1 != $p2); }
function func_add($p1, $p2) { return $p1 + $p2; }
function func_sub($p1, $p2) { return $p1 - $p2; }
function func_mul($p1, $p2) { return $p1 * $p2; }
function func_div($p1, $p2) { return $p1 / $p2; }
function func_and($p1, $p2) { return $p1 & $p2; }
function func_or($p1, $p2) { return $p1 | $p2; }
function func_xor($p1, $p2) { return $p1 ^ $p2; }

function execute_command($opcode, $args)
{
  global $ip, $memory, $opcodes, $functions;
  $opcode = $opcodes[$opcode];
  echo $opcode.' '.join(', ', $args)."\n";
  switch ($opcode)
  {
    case 'store':
      store($args[1], $args[0]);
      break;
    case 'copy':
      store($args[1], get_memory_value($args[0]));
      break;
    case 'call':
      $function_name = $functions[$args[0]];
      $result = $args[sizeof($args) - 1];
      $args = array_map('get_memory_value', array_slice($args, 1, sizeof($args) - 2));
      store($result, call_user_func_array('wrapper_'.$function_name, $args));
      break;
    case 'equal':
    case 'less':
    case 'greater':
    case 'equal_less':
    case 'equal_greater':
    case 'not_equal':
    case 'add':
    case 'sub':
    case 'mul':
    case 'div':
    case 'and':
    case 'or':
    case 'xor':
      $param1 = get_memory_value($args[0]);
      $param2 = get_memory_value($args[1]);
      $result = call_user_func('func_'.$opcode, $param1, $param2);
      store($args[2], $result);
      break;
    case 'ifnot':
      $cond = get_memory_value($args[0]);
      if (! $cond)
        $ip += $args[1];
      else
        $ip += 1;
      $not_need_change_ip = true;
      break;
    case 'jump':
      $ip += $args[0];
      $not_need_change_ip = true;
      break;
    default:
      throw new Exception('Error: unknown opcode: '.$opcode);
  }
  if (! isset($not_need_change_ip))
    ++$ip;
}

function execute_program($program)  
{
  global $ip;
  $program = split("\n", base64_decode($program));
  print_r($program);
  $cmds = split('\|', $program[sizeof($program) - 1]);

  $ip = 0;
  while ($ip >= 0 && $ip < sizeof($cmds))
  {
    $cmd = split(' ', $program[$cmds[$ip]]);
    execute_command($cmd[0], array_slice($cmd, 1));
  }
}

execute_program(compile('a <- 1; print(a); if a < 1 then print(a + 1) end'));

?>