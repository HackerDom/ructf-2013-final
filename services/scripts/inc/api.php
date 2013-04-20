<?php

function request($url, $params)
{
  $c = curl_init();
  curl_setopt($c, CURLOPT_URL, $url);
  curl_setopt($c, CURLOPT_POST, 1);
  curl_setopt($c, CURLOPT_RETURNTRANSFER, 1);
  curl_setopt($c, CURLOPT_POSTFIELDS, json_encode($params));
  $http_headers = array('X-Requested-With: XMLHttpRequest', 'Content-Type: application/json');
  if (array_key_exists('session', $_COOKIE))
    $http_headers[] = 'Cookie: session='.str_replace("\n", "", str_replace("\r", "", $_COOKIE['session']));
  curl_setopt($c, CURLOPT_HTTPHEADER, $http_headers);
  $res = curl_exec($c);
  curl_close($c);

  if (! $res)
    return null;

  $res = json_decode($res, true);
  if (! $res)
  {
    throw new Exception('Runtime error: answer of service in\'t in JSON format');
  }
  return $res;
}

function api_request($service, $function, $params)
{                
  $function = str_replace('.', '/', $function);
  $host = $_SERVER['HTTP_HOST'];
  if ($service != '')
    $service .= '.';
  $service_url = 'http://'.$service.join('.', array_slice(split('\.', $host), 1)).'/'.$function;
  return request($service_url, $params);
}

?>