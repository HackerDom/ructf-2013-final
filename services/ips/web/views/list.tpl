<ul class="nav nav-pills">
    <li class="active"><a href="#">Proxy</a></li>
    <li><a href="/">About</a></li>
</ul>
<div class="tab-content">
    <form action="/host/add" method="POST">
        <fieldset>
            <input type="text" name="src_port" placeholder="Source port (>= 60000)">
            <input type="text" name="dst_host" placeholder="Destination host">
            <input type="text" name="dst_port" placeholder="Destination port">
            <textarea rows="5" name="rules" placeholder="Rules (one line one rule)"></textarea>
            <input class="btn" type="submit" value="Create proxy instance">
        </fieldset>
    </form>
    <table class="table table-striped table-hover table-bordered">
        <thead class="table-hd">
            <tr>
                <th>source port</th>
                <th>destination host</th>
                <th>destination port</th>
                <th>delete</th>
            </tr>
        </thead>
        <tboby>
            % for p in proxy:
                % (src_port, dst_host, dst_port) = p.split('-')
                <tr>
                    <form action="/host/del" method="POST">
                        <input type="hidden" name="key" value="{{p}}">
                        <td>{{src_port}}</td>
                        <td>{{dst_host}}</td>
                        <td>{{dst_port}}</td>
                        <td><input class="btn" type="submit" value="&nbsp;-&nbsp;"></td>
                    </form>
                </tr>
            % end
        </tboby>
    </table>
</div>

% def b1():
    <a id="signup" class="btn btn-success" href="http://{{domain}}/register?n=http://ips.{{domain}}/list">Sign Up</a>
    <a id="signin" class="btn" href="http://{{domain}}/login?n=http://ips.{{domain}}/list">Sign In</a>
% end
% def b2():
    <a href="#" class="name">{{user['first_name'] + ' ' + user['last_name']}}</a>
    <a href="http://{{domain}}/logout" class="btn btn-delete">Logout</a>
% end
%rebase default title='Proxy', x = b2 if user else b1, domain=domain