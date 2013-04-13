<?php

function user_info($session)
{
  $c = curl_init();
  /* TODO replace 127.0.0.1 to _SERVER['HTTP_HOST'] */
  curl_setopt($c, CURLOPT_URL, "http://127.0.0.1/user"); 
  curl_setopt($c, CURLOPT_POST, 1);
  curl_setopt($c, CURLOPT_RETURNTRANSFER, 1);
  curl_setopt($c, CURLOPT_POSTFIELDS, 'session='.urlencode($session));
  $res = curl_exec($c);
  curl_close($c);

  if (! $res)
  {
  }
  else
  {
    echo $res;
  }
}

?>