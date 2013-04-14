# need to set up:
# r_user_name
# r_host
# teamN
# r_dns_records

show_template = %q{
	<!DOCTYPE html>
<html lang="en">
<head>
    <title>DNS records</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link href="bootstrap/css/bootstrap.css" rel="stylesheet">
    <link href="css/style.css" rel="stylesheet">
</head>
<body>
<div class="page">
    <div id="wrap">
        <div class="navbar navbar-fixed-top">
            <div class="navbar-inner">
                <div class="container">
                    <a class="brand" href="#">RuCTF 2013</a>
                    <div class="nav-collapse collapse">
                        <ul class="nav">
                            <li><a href="#">SES</a></li>
                            <li><a href="#">MapReduse</a></li>
                            <li><a href="#">DB</a></li>
                            <li><a href="#">MessageQueue</a></li>
                            <li class="active"><a href="http://<%= r_host %>">DNS</a></li>
                            <li><a href="#">Balanser</a></li>
                            <li><a href="#">ScriptAPI</a></li>
                        </ul>
                        <div class="pull-right name-pan">
                            <a href="#" class="name"><%= r_user_name %></a>
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
                    <li><a href="http://<%= r_host %>/add">Add record</a></li>
                    <li class="active"><a href="http://<%= r_host %>/show">View all records</a></li>
                </ul>
                <div class="tab-content">
                	%% if r_authored do
                	%%	if r_has_records do
                    <table class="table table-striped table-hover">
                        <tbody>
                        	%% r_dns_records.each do |r_record|
		                        <tr>
		                            <td><%= r_record %></td>
		                            <td class="btn-td"><button type="submit" class="btn btn-delete">Delete</button></td>
		                        </tr>
                        	%% end
                        </tbody>
                    </table>
                    %%	else
                    <p> You have no records</p>
                    %%	end
                    %% else
                    <p> Log in, please</p>
                </div>
            </div>
        </div>
    </div>
</div>
<div id="footer">
    <div class="container">
        <div class="pull-right">
            <a href="http://ructf.org">RuCTF</a>
            <a href="http://hackerdom.ru">HackerDom</a>
            <a href="ructf.org/2013/ru/devteam">Developers</a>
        </div>
    </div>
</div>
</body>
</html>
}