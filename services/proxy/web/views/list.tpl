<ul class="nav nav-pills">
    <li class="active"><a href="#">List</a></li>
    <li><a href="/about">About</a></li>
</ul>
<div class="tab-content">
    <form action="/host/add" method="POST">
        <fieldset>
            <input type="text" name="src_port" placeholder="Source port">
            <input type="text" name="dst_host" placeholder="Destination host">
            <input type="text" name="dst_port" placeholder="Destination port">
            <input class="btn" type="submit" value="Add host">
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
                        <td><input class="btn" type="submit" value="-"></td>
                    </form>
                </tr>
            % end
        </tboby>
    </table>
</div>
%def b1():
    <h1>qwer</h1>
%end
%def b11():
    <h1>qwer1111</h1>
%end
%def b2():
    <h2>test</h2>
%end
% x = b11
%rebase default title='List', b1=x, b2=b2