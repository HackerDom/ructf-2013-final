<?php

function request($url, $params)
{
  $c = curl_init();
  curl_setopt($c, CURLOPT_URL, $url);
  curl_setopt($c, CURLOPT_POST, 1);
  curl_setopt($c, CURLOPT_RETURNTRANSFER, 1);
  curl_setopt($c, CURLOPT_POSTFIELDS, json_encode($params));
  curl_setopt($c, CURLOPT_HTTPHEADER, array('X-Requested-With: XMLHttpRequest', 'Content-Type: application/json'));
  $res = curl_exec($c);
  curl_close($c);

  if (! $res)
    return null;

  return json_decode($res, true);
}

function api_request($service, $function, $params)
{                
  $host = $_SERVER['HTTP_HOST'];
  if ($service != '')
    $service .= '.';
  # TODO remove port!!!
  $service_url = 'http://'.$service.join('.', array_slice(split('\.', $host), 1)).':8080/'.$function;
  return request($service_url, $params);
}

?>