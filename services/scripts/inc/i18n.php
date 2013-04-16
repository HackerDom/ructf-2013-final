<?php

function init_i18n()
{
  global $charset;
  if (array_key_exists('HTTP_ACCEPT_CHARSET', $_SERVER))
  {
    $header_charsets = split(',', $_SERVER['HTTP_ACCEPT_CHARSET']);
    foreach ($header_charsets as $charset)
    {
      if (strpos($charset, ';') !== false)
        $charset = substr($charset, 0, strpos($charset, ';'));
    }
  }
}

init_i18n();

?>