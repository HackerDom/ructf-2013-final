<?php

$lexems = array('L_ASSIGN',
                'L_EQUAL', 'L_LESS', 'L_GREATER', 'L_EQUAL_LESS', 'L_EQUAL_GREATER', 'L_NOT_EQUAL',
                'L_PLUS', 'L_MINUS', 'L_MUL', 'L_DIV', 'L_OR', 'L_XOR', 'L_AND',
                'L_KEYWORD',
                'L_OPEN', 'L_CLOSE', 'L_COMMA',
                'L_VARIABLE', 'L_FUNCTION', 'L_NUMBER', 'L_STRING',
                'L_EOS', 'L_END');

$const_lexems = array();

$keywords = array('if', 'then', 'else', 'for', 'end', 'from', 'to');
$whitespaces = array(' ', "\n", "\r", "\t");
$functions = array('input', 'print');
$services = array('ses' => array('identity.add' => array('email'), 'identity.list' => array(), 'identity.del' => array('id'),
                                 'credentials.add' => array(), 'credentials.list' => array(), 'credentials.del' => array('id'),
                                 'mail.send' => array('from', 'to', 'subject', 'message'),
                                 'stats' => array(), 'error' => array('id'))
                 );
$opcodes = array('store', 'copy', 'call', 'add', 'sub', 'mul', 'div', 'and', 'or', 'xor', 'equal', 'less', 'greater', 'equal_less', 'equal_greater', 'not_equal', 'ifnot', 'jump', 'inc');

$current_lexem = 0;
$lexem_value = 0;

$memory_count = 0;
$variables = array();

function init_lexems()
{
  global $lexems;
  foreach ($lexems as $id => $lexem)
    define($lexem, $id);
}

function init_const_lexems()
{
  // TODO move $const_lexems to up with string constants
  global $const_lexems;
  $const_lexems = array('<-' => L_ASSIGN,
                        '=' => L_EQUAL, '<' => L_LESS, '>' => L_GREATER, '<=' => L_EQUAL_LESS, '>=' => L_EQUAL_GREATER, '!=' => L_NOT_EQUAL,
                        '+' => L_PLUS, '-' => L_MINUS, '*' => L_MUL, '/' => L_DIV, '|' => L_OR, '^' => L_XOR, '&' => L_AND,
                        '(' => L_OPEN, ')' => L_CLOSE, ',' => L_COMMA,
                        ';' => L_EOS);
}

function init_keywords()
{
  global $keywords;
  foreach ($keywords as $id => $keyword)
  {
    define('KEYWORD_'.strtoupper($keyword), $id);
    $keywords[$keyword] = $id;
  }
}

function init_functions()
{
  global $services, $functions;
  foreach ($services as $service => $service_functions)
  {
    foreach ($service_functions as $function => $params)
    {
      $functions[] = $service.'.'.$function;
    }
  }
}

init_lexems();
init_const_lexems();
init_keywords();
init_functions();

function hexToStr($hex)
{
  $string = '';
  for ($i = 0; $i < strlen($hex) - 1; $i += 2)
    $string .= chr(hexdec($hex[$i].$hex[$i + 1]));
  return $string;
}

function strToHex($string)
{
  $hex = '';
  $len = strlen($string);
  for ($i = 0; $i < $len; $i++)
    $hex .= dechex(ord($string[$i]));
  return $hex;
}

function is_variable_symbol($ch)
{
  if (ctype_alnum($ch))
    return true;
  if (in_array($ch, array('.', '_')))
    return true;
  return false;
}

function next_lexem(&$string)
{
  global $const_lexems, $whitespaces, $functions;

  while (strlen($string) > 0 && in_array($string[0], $whitespaces))
    $string = substr($string, 1);

  if (strlen($string) == 0)
    return L_END;
  if (strlen($string) >= 2 && array_key_exists(substr($string, 0, 2), $const_lexems))
  {
    $lexem = substr($string, 0, 2);
    $string = substr($string, 2);
    return $const_lexems[$lexem];
  }

  if (array_key_exists($string[0], $const_lexems))
  {
    $lexem = substr($string, 0, 1);
    $string = substr($string, 1);
    return $const_lexems[$lexem];
  }

  // TODO move to function get_piece_by_function(ctype_digit|ctype_alpha)
  if (ctype_digit($string[0]))
  {
    global $lexem_value;
    $lexem_value = '';
    while (strlen($string) > 0 && ctype_digit($string[0]))
    {
      $lexem_value .= $string[0];
      $string = substr($string, 1);
    }
    $lexem_value = intval($lexem_value);
    return L_NUMBER;
  }

  if (ctype_alpha($string[0]))
  {
    global $lexem_value, $keywords;
    $lexem_value = '';
    while (strlen($string) > 0 && is_variable_symbol($string[0]))
    {
      $lexem_value .= $string[0];
      $string = substr($string, 1);
    }
    if (array_key_exists($lexem_value, $keywords))
    {
      $lexem_value = $keywords[$lexem_value];
      return L_KEYWORD;
    }
    if (in_array($lexem_value, $functions))
    {
      $lexem_value = array_search($lexem_value, $functions);
      return L_FUNCTION;
    }
    return L_VARIABLE;
  }

  if ($string[0] == '"')
  {
    global $lexem_value;
    $idx = 1;
    $len = strlen($string);
    while ($idx < $len)
    {
      if ($string[$idx] == '\\')
      {
        $idx += 1;
        continue;
      }
      if ($string[$idx] == '"')
        break;
      ++$idx;
    }
    if ($idx >= $len)
      throw new Exception('Error: unclosed string constant');
    $lexem_value = substr($string, 1, $idx - 1);
    $string = substr($string, $idx + 1);
    return L_STRING;
  }

  throw new Exception('Error: can\'t parse near '.substr($string, 0, 10));
}

function new_memory()
{
  global $memory_count;
  return $memory_count++;
}

function get_memory($variable)
{
  global $variables;
  $name = split('\.', $variable);
  $name = $name[0];
  if (! array_key_exists($name, $variables))
    $variables[$name] = new_memory();
  return $variables[$name].substr($variable, strlen($name));
}

function opcode($name)
{
  global $opcodes;
  $args = array_slice(func_get_args(), 1);
  if (! in_array($name, $opcodes))
    throw new Exception('Internal error: invalid opcode: '.$name);
  foreach ($args as &$arg)
  {
    if ($arg[0] == '"')
      $arg = '0x'.strToHex($arg);
  }
  return array_search($name, $opcodes).' '.join(' ', $args)."\n";
}

function read_function_call(&$string)
{
  global $current_lexem, $lexem_value;
  $function = $lexem_value;
  $current_lexem = next_lexem($string);
  if ($current_lexem != L_OPEN)
    throw new Exception('Error: expected \'(\' near '.substr($string, 0, 10));

  $current_lexem = next_lexem($string);
  $params = array();
  $code = '';
  while ($current_lexem != L_CLOSE)
  {
    $result = read_expression($string);
    $params[] = $result['result'];
    $code .= $result['code'];
    if ($current_lexem != L_COMMA && $current_lexem != L_CLOSE)
      throw new Exception('Error: expected \')\' or \',\' near '.substr($string, 0, 10));
    if ($current_lexem == L_COMMA)
      $current_lexem = next_lexem($string);
  }
  $current_lexem = next_lexem($string);
  $new = new_memory();
  if (sizeof($params) == 0)
    $result = array('code' => $code.opcode('call', $function, $new), 'result' => $new);
  else    
    $result = array('code' => $code.opcode('call', $function, join(' ', $params), $new), 'result' => $new);
  return $result;
}

function read_factor(&$string)
{
  global $current_lexem, $lexem_value;
  switch ($current_lexem)
  {
    case L_NUMBER:
      $new = new_memory();
      $result = array('code' => opcode('store', $lexem_value, $new), 'result' => $new);
      $current_lexem = next_lexem($string);
      return $result;
    case L_VARIABLE:
      $result = array('code' => '', 'result' => get_memory($lexem_value));
      $current_lexem = next_lexem($string);
      return $result;
    case L_FUNCTION:           
      return read_function_call($string);
    case L_OPEN:
      $current_lexem = next_lexem($string);
      $result = read_expression($string);
      if ($current_lexem != L_CLOSE)
        throw new Exception('Error: not-closed bracket near '.substr($string, 0, 10));
      $current_lexem = next_lexem($string);
      return $result;
    case L_STRING:
      $new = new_memory();
      $result = array('code' => opcode('store', $lexem_value, $new), 'result' => $new);
      $current_lexem = next_lexem($string);
      return $result;      
    default:
      throw new Exception('Error: can\'t understand near '.substr($string, 0, 10));
  }
}

function read_summand(&$string)
{
  global $current_lexem;
  $result = read_factor($string);
  while ($current_lexem == L_MUL || $current_lexem == L_DIV)
  {
    $saved_lexem = $current_lexem;
    $current_lexem = next_lexem($string);
    $second = read_factor($string);
    $result['code'] .= $second['code'];
    if ($saved_lexem == L_MUL)
      $code = 'mul';
    elseif ($saved_lexem == L_DIV)
      $code = 'div';
    $new = new_memory();
    $result['code'] .= opcode($code, $result['result'], $second['result'], $new);
    $result['result'] = $new;
  }
  return $result;
}

function read_expression(&$string)
{
  global $current_lexem;
  $result = read_summand($string);
  while ($current_lexem == L_PLUS || $current_lexem == L_MINUS)
  {
    $saved_lexem = $current_lexem;
    $current_lexem = next_lexem($string);
    $second = read_summand($string);
    $result['code'] .= $second['code'];
    if ($saved_lexem == L_PLUS)
      $code = 'add';
    elseif ($saved_lexem == L_MINUS)
      $code = 'sub';
    $new = new_memory();
    $result['code'] .= opcode($code, $result['result'], $second['result'], $new);
    $result['result'] = $new;
  }
  return $result;
}

function read_bool_factor(&$string)
{
  global $current_lexem, $lexem_value;
  switch ($current_lexem)
  {
    case L_OPEN:
      $current_lexem = next_lexem($string);
      $result = read_bool_expression($string);
      if ($current_lexem != L_CLOSE)
        throw new Exception('Error: not-closed bracket near '.substr($string, 0, 10));
      $current_lexem = next_lexem($string);
      return $result;
    default:
      $first = read_expression($string);
      if (! in_array($current_lexem, array(L_EQUAL, L_LESS, L_GREATER, L_EQUAL_LESS, L_EQUAL_GREATER, L_NOT_EQUAL)))
        throw new Exception('Error: can\'t understand near '.substr($string, 0, 10));
      $saved_lexem = $current_lexem;
      $current_lexem = next_lexem($string);
      $second = read_expression($string);
      $new = new_memory();
      $result = array('code' => $first['code'].$second['code'], 'result' => $new);
      switch ($saved_lexem)
      {
        case L_EQUAL: $code = 'equal'; break;
        case L_LESS: $code = 'less'; break;
        case L_GREATER: $code = 'greater'; break;
        case L_EQUAL_LESS: $code = 'equal_less'; break;
        case L_EQUAL_GREATER: $code = 'equal_greater'; break;
        case L_NOT_EQUAL: $code = 'not_equal'; break;
      }
      $result['code'] .= opcode($code, $first['result'], $second['result'], $new);
      return $result;
  }
} 

function read_bool_summand(&$string)
{
  global $current_lexem;
  $result = read_bool_factor($string);
  while ($current_lexem == L_AND)
  {
    $saved_lexem = $current_lexem;
    $current_lexem = next_lexem($string);
    $second = read_bool_factor($string);
    $result['code'] .= $second['code'];
    $new = new_memory();
    $result['code'] .= opcode('and', $result['result'], $second['result'], $new);
    $result['result'] = $new;
  }
  return $result;
}

function read_bool_expression(&$string)
{
  global $current_lexem;
  $result = read_bool_summand($string);
  while ($current_lexem == L_OR || $current_lexem == L_XOR)
  {
    $saved_lexem = $current_lexem;
    $current_lexem = next_lexem($string);
    $second = read_bool_summand($string);
    $result['code'] .= $second['code'];
    if ($saved_lexem == L_OR)
      $code = 'or';
    elseif ($saved_lexem == L_XOR)
      $code = 'xor';
    $new = new_memory();
    $result['code'] .= opcode($code, $result['result'], $second['result']. $new);
    $result['result'] = $new;
  }
  return $result;
}

function read_block(&$string)
{
  global $current_lexem, $lexem_value;
  $result = '';
  while ($current_lexem != L_KEYWORD || $lexem_value != KEYWORD_END)
  {
    $stmt = read_statement($string);
    $result .= $stmt['code'];
    if ($current_lexem == L_EOS)
      $current_lexem = next_lexem($string);
  }
  $current_lexem = next_lexem($string);
  return array('code' => $result, 'result' => -1);
}

function read_statement(&$string)
{
  global $current_lexem, $lexem_value;
  switch ($current_lexem)
  {
    case L_VARIABLE:
      $variable = $lexem_value;
      $memory = get_memory($variable);
      $current_lexem = next_lexem($string);
      if ($current_lexem != L_ASSIGN)
        throw new Exception('Error: can\'t understand near '.substr($string, 0, 10));
      $current_lexem = next_lexem($string);
      $expr = read_expression($string);
      $result = array('code' => $expr['code'].opcode('copy', $expr['result'], $memory), 'result' => $memory);
      return $result;
    case L_KEYWORD:
      switch ($lexem_value)
      {
        case KEYWORD_IF:
          $current_lexem = next_lexem($string);
          $bool_expr = read_bool_expression($string);
          if ($current_lexem != L_KEYWORD || $lexem_value != KEYWORD_THEN)
            throw new Exception('Error: expected `then` near '.substr($string, 0, 10));
          $current_lexem = next_lexem($string);
          $block = read_block($string);
          $else = false;
          if ($current_lexem == L_KEYWORD && $lexem_value == KEYWORD_ELSE)
          {
            $else = true;
            $current_lexem = next_lexem($string);
            $else_block = read_block($string);
          }
          $block_len = substr_count($block['code'], "\n") + 2;
          $else_block_len = $else ? substr_count($else_block['code'], "\n") + 1 : 1;
          $result = array('code' => $bool_expr['code'].
                                    opcode('ifnot', $bool_expr['result'], $block_len).
                                    $block['code'].
                                    opcode('jump', $else_block_len).
                                    ($else ? $else_block['code'] : ''),
                          'result' => $bool_expr['result']);
          return $result;
        case KEYWORD_FOR:   
          $current_lexem = next_lexem($string);
          if ($current_lexem != L_VARIABLE)
            throw new Exception('Error: expected variable near '.substr($string, 0, 10));
          $variable = get_memory($lexem_value);
          $compare = new_memory();
          $current_lexem = next_lexem($string);

          if ($current_lexem != L_KEYWORD || $lexem_value != KEYWORD_FROM)
            throw new Exception('Error: expected `from` near '.substr($string, 0, 10));
          $current_lexem = next_lexem($string);
          $from = read_expression($string);

          if ($current_lexem != L_KEYWORD || $lexem_value != KEYWORD_TO)
            throw new Exception('Error: expected `to` near '.substr($string, 0, 10));
          $current_lexem = next_lexem($string);
          $to = read_expression($string);

          $block = read_block($string);
          /* TODO optimize substr_count */
          $block_len = substr_count($block['code'], "\n");
          $to_len = substr_count($to['code'], "\n");

          $code = $from['code'].
                  opcode('copy', $from['result'], $variable).
                  $to['code'].
                  opcode('equal_less', $variable, $to['result'], $compare).
                  opcode('ifnot', $compare, ($block_len + 3)).
                  $block['code'].
                  opcode('inc', $variable).
                  opcode('jump', - ($block_len + $to_len + 3));

          $result = array('code' => $code, 'result' => $compare);
          return $result;

        default:
          throw new Exception('Error: unexpected keyword near '.substr($string, 0, 10));
      }
    case L_FUNCTION:
      return read_function_call($string);
    default:
      throw new Exception('Error: can\'t understand near '.substr($string, 0, 10));
  }
}

function read_program(&$string)
{
  global $current_lexem;
  $result = '';
  while ($current_lexem != L_END)
  {
    $stmt = read_statement($string);
    $result .= $stmt['code'];
    if ($current_lexem == L_EOS)
      $current_lexem = next_lexem($string);
  }
  return $result;
}

function compile($program)
{
  global $current_lexem;
  $tmp = $program;
  $current_lexem = next_lexem($tmp);
  $code = read_program($tmp);

  $code = split("\n", trim($code));
  $all = array_unique($code);
  shuffle($all);
  foreach ($code as &$c)
    $c = array_search($c, $all);

  return base64_encode(join("\n", $all)."\n".join('|', $code));
}

compile("a.a <- 1;\nprint(a);");

?>