# need to set up:
# r_user_name
# r_host
# r_authored
# teamN

add_template = %q{
<!DOCTYPE html>
<html lang="en">
<head>
    <title>Add record</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link href="/static/bootstrap/css/bootstrap.css" rel="stylesheet">
    <link href="/static/css/style.css" rel="stylesheet">
</head>
<body class="dns">
<div id="wrap">
    <div class="navbar navbar-fixed-top">
        <div class="navbar-inner">
            <div class="container">
                <a class="brand" href="#">RuCTF 2013</a>
                <div class="nav-collapse collapse">
                    <ul class="nav">
                        <li><a href="http://ses.<%= teamN %>.ructf/">SES</a></li>
                        <li><a href="http://mr.<%= teamN %>.ructf/">MapReduce</a></li>
                        <li><a href="http://db.<%= teamN %>.ructf/">DB</a></li>
                        <li><a href="http://queue.<%= teamN %>.ructf/">Queue</a></li>
                        <li class="active"><a href="#">DNS</a></li>
                        <li><a href="http://ips.<%= teamN %>.ructf/">IPS</a></li>
                        <li><a href="http://scripts.<%= teamN %>.ructf/">ScriptAPI</a></li>
                    </ul>
                    <div class="pull-right name-pan">
                        <% if r_authored %>
                            <a href="#" class="name"><%= r_user_name %></a>
                            <a class="btn btn-delete" href="http://<%= teamN %>.ructf/login">Logout</a>
                        <% else %>
                            <a class="btn btn-success" href="http://<%= teamN %>.ructf/register">Sign Up</a>
                            <a class="btn" href="http://<%= teamN %>.ructf/login">Sign In</a>
                        <% end %>
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
            <ul class="nav nav-pills">
                <li class="active"><a href="http://<%= r_host %>/add">Add record</a></li>
                <li><a href="http://<%= r_host %>/show">View all records</a></li>
                <li><a href="http://<%= r_host %>/index.html">About</a></li>
            </ul>
            <div class="tab-content">
                    <form method="POST" action="http://<%= r_host %>/add" class="add-record">
                        <fieldset>
                            <select name="type" placeholder="Type">
                                <option disabled selected="selected" class="choose">Choose type</option>
                                <option value="A">A</option>
                                <option value="TXT">TXT</option>
                            </select>
                            <input type="text" name="name" placeholder="Name">
                            <input type="text" name="value" placeholder="Value">
                            <label></label>
                            <button type="submit" class="btn btn-add">Add</button>
                        </fieldset>
                    </form>
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
}
