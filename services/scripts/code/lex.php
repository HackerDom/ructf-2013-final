<?php

$lexems = array('L_ASSIGN',
                'L_EQUAL', 'L_LESS', 'L_GREATER', 'L_EQUAL_LESS', 'L_EQUAL_GREATER', 'L_NOT_EQUAL',
                'L_PLUS', 'L_MINUS', 'L_MUL', 'L_DIV', 'L_OR', 'L_XOR', 'L_AND',
                'L_KEYWORD',
                'L_OPEN', 'L_CLOSE',
                'L_VARIABLE', 'L_NUMBER', 'L_STRING',
                'L_EOS', 'L_END');

$const_lexems = array();

$keywords = array('if', 'then', 'else', 'for', 'end');

$whitespaces = array(' ', '\n', '\r', '\t');

$current_lexem = 0;
$lexem_value = 0;

$memory = 0;
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
                        '(' => L_OPEN, ')' => L_CLOSE,
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

init_lexems();
init_const_lexems();
init_keywords();

function next_lexem(&$string)
{
  global $const_lexems, $whitespaces;

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
    while (strlen($string) > 0 && ctype_alpha($string[0]))
    {
      $lexem_value .= $string[0];
      $string = substr($string, 1);
    }
    if (array_key_exists($lexem_value, $keywords))
    {
      $lexem_value = $keywords[$lexem_value];
      return L_KEYWORD;
    }
    return L_VARIABLE;
  }

  throw new Exception('Error: can\'t parse near '.substr($string, 0, 10));
}

function parse_lexems($string)
{
  global $lexem_value;
  while (($lexem = next_lexem($string)) != L_END)
  {
    echo $lexem.($lexem == L_VARIABLE || $lexem == L_NUMBER ? ' '.$lexem_value : '')."\n";
  }
}

function new_memory()
{
  global $memory;
  return $memory++;
}

function get_memory($variable)
{
  global $variables;
  if (! array_key_exists($variable, $variables))
    $variables[$variable] = new_memory();
  return $variables[$variable];
}

function read_factor(&$string)
{
  global $current_lexem, $lexem_value;
  switch ($current_lexem)
  {
    case L_NUMBER:
      $new = new_memory();
      $result = array('code' => 'store '.$lexem_value.' '.$new."\n", 'result' => $new);
      $current_lexem = next_lexem($string);
      return $result;
    case L_VARIABLE:
      $result = array('code' => '', 'result' => get_memory($lexem_value));
      $current_lexem = next_lexem($string);
      return $result;
    case L_OPEN:
      $current_lexem = next_lexem($string);
      $result = read_expression($string);
      if ($current_lexem != L_CLOSE)
        throw new Exception('Error: not-closed bracket near '.substr($string, 0, 10));
      $current_lexem = next_lexem($string);
      return $result;
    default:
      throw new Exception('Error: can\'t understand near '.substr($string, 0, 10));
  }
}

/* TODO copy-parse from read_expression */
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
      $result['code'] .= 'mul';
    elseif ($saved_lexem == L_DIV)
      $result['code'] .= 'div';
    $new = new_memory();
    $result['code'] .= ' '.$result['result'].' '.$second['result'].' '.$new."\n";
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
      $result['code'] .= 'add';
    elseif ($saved_lexem == L_MINUS)
      $result['code'] .= 'sub';
    $new = new_memory();
    $result['code'] .= ' '.$result['result'].' '.$second['result'].' '.$new."\n";
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
      $result['code'] .= $code.' '.$first['result'].' '.$second['result'].' '.$new."\n";
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
    $result['code'] .= 'and';
    $new = new_memory();
    $result['code'] .= ' '.$result['result'].' '.$second['result'].' '.$new."\n";
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
      $result['code'] .= 'or';
    elseif ($saved_lexem == L_XOR)
      $result['code'] .= 'xor';
    $new = new_memory();
    $result['code'] .= ' '.$result['result'].' '.$second['result'].' '.$new."\n";
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
      $result = array('code' => $expr['code'].'copy '.$expr['result'].' '.$memory."\n", 'result' => $memory);
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
                                    'ifnot '.$bool_expr['result'].' '.$block_len."\n".
                                    $block['code'].
                                    'jump '.$else_block_len."\n".
                                    ($else ? $else_block['code'] : ''),
                          'result' => $bool_expr['result']);
          return $result;
        default:
          throw new Exception('Error: unexpected keyword near '.substr($string, 0, 10));          
      }
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

$program = 'a <- 1; if b < 3 & a <= 1 then a <- 10; end else a <- 11; b <- 10; end';
$current_lexem = next_lexem($program);
echo read_program($program);

?>