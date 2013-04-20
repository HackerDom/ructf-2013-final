<?php

require_once 'inc/db.php';

function init_i18n()
{
  global $language;
  if (array_key_exists('HTTP_ACCEPT_CHARSET', $_SERVER))
  {
    $header_charsets = split(',', $_SERVER['HTTP_ACCEPT_CHARSET']);
    foreach ($header_charsets as $charset)
    {
      if (strpos($charset, ';') !== false)
        $charset = substr($charset, 0, strpos($charset, ';'));
      $charset = strtolower($charset);
      $charset = str_replace('-', '', $charset);
      $charset = str_replace('windows', 'cp', $charset);
      mysql_query('SET NAMES '.mysql_escape_string($charset));
      break;
    }

    $header_languages = split(',', $_SERVER['HTTP_ACCEPT_LANGUAGE']);
    foreach ($header_languages as $l)
    {
      if (strpos($l, ';') !== false)
        $l = substr($l, 0, strpos($l, ';'));
      if (strpos($l, '-') !== false)
        $l = substr($l, 0, strpos($l, '-'));
      $l = strtolower($l);
      $language = $language;
      break;
    }
  }
}

function i18n_template($template_name)
{
  global $language;
  extract($GLOBALS, EXTR_REFS | EXTR_SKIP);
  if (! file_exists('template/'.$template_name.'.'.$language.'.html'))
    $language = 'en';
  require 'templates/'.$template_name.'.'.$language.'.html';
}

init_i18n();

?>