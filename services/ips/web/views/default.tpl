<!DOCTYPE html>
<html lang="en">
  <head>
    <title>{{title}}</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link href="/static/bootstrap/css/bootstrap.css" rel="stylesheet">
    <link href="/static/css/style.css" rel="stylesheet">
  </head>
  <body class="ips">
    <div class="page">
    <div id="wrap">
      <div class="navbar navbar-fixed-top">
        <div class="navbar-inner">
          <div class="container">
            <a class="brand" href="http://{{domain}}/">RuCTF 2013</a>
            <div class="nav-collapse collapse">
              <ul class="nav">
                <li><a href="http://ses.{{domain}}/">SES</a></li>
                <li><a href="http://mr.{{domain}}/">MapReduce</a></li>
                <li><a href="http://db.{{domain}}/">DB</a></li>
                <li><a href="http://queue.{{domain}}/">Queue</a></li>
                <li><a href="http://dns.{{domain}}/">DNS</a></li>
                <li class="active"><a href="http://ips.{{domain}}/">IPS</a></li>
                <li><a href="http://scripts.{{domain}}/">ScriptAPI</a></li>
              </ul>
              <div class="pull-right">
                %x()
              </div>
            </div>
          </div>
        </div>
      </div>
      <div id="content">
        <div class="container">
          <div class="header-main">
              RuCTF
          </div>
          <hr>
          %include
        </div>
      </div>
    </div>
    </div>
    <div id="footer">
      <div class="container">
        <div class="pull-right">
          <a href="http://ructf.org">RuCTF</a>
          <a href="http://hackerdom.ru">HackerDom</a>
          <a href="http://ructf.org/2013/ru/devteam">Developers</a>
        </div>
      </div>
    </div>
  </body>
</html>
