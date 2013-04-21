# need to set up:
# r_user_name
# r_host
# teamN
# r_dns_records
# r_has_records
# r_authored

show_template = %q{
	<!DOCTYPE html>
<html lang="en">
<head>
    <title>DNS records</title>
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link href="/static/bootstrap/css/bootstrap.css" rel="stylesheet">
    <link href="/static/css/style.css" rel="stylesheet">

    <script src="/static/js/jquery.js"></script>
    <script> 
function send4del(id){
    $.ajax({
                url: 'http://<%= r_host %>/delete',
                data: JSON.stringify({"id": id}),
                type: 'POST',
                contentType:"application/json; charset=utf-8",
                dataType: 'json',
               success: function () {
                   alert("Deleting record!!");
                   window.location.reload();
                }
            });
}
    </script>
</head>
<body class="dns">
<div class="page">
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
                    <li><a href="http://<%= r_host %>/add">Add record</a></li>
                    <li class="active"><a href="http://<%= r_host %>/show">View all records</a></li>
                    <li><a href="http://<%= r_host %>/index.html">About</a></li>
                </ul>
                <div class="tab-content">
                <% if r_authored %>
                	<% if r_has_records %>
                    <table class="table table-striped table-hover">
                        <tbody>
                        	<% r_dns_records.each do |r_record, r_id| %>
		                        <tr>
		                            <td><%= r_record %></td>
		                            <td class="btn-td"><button type="submit" class="btn btn-delete" id="<%= r_id %>" onclick="send4del(this.id)">Delete</button></td>
		                        </tr>
                        	<% end %>
                        </tbody>
                    </table>
                    <% else %>
                        <p> You have no records</p>
                    <% end %>
                <% else %>
                    <p> Log in, please</p>
                <% end %>
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
            <a href="http://ructf.org/2013/ru/devteam">Developers</a>
        </div>
    </div>
</div>
</body>
</html>
}
