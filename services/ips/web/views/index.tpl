<ul class="nav nav-pills">
    <li><a href="/list">Proxy</a></li>
    <li class="active"><a href="/">About</a></li>
</ul>
<div class="tab-content">
    <div class="central">
        <div class="about">
            testsdf dsf df sdf 
            testsdaf sadf er
            testewr awer asdf As
            test dsaf afd daf 
            test few fa sdf asdf asdg asdf
        </div>
        <img src="/static/pic/ips.png" class="img-rounded about-img">
    </div>
</div>
% def b1():
    <a id="signup" class="btn btn-success" href="http://{{domain}}/register?n=http://ips.{{domain}}/list">Sign Up</a>
    <a id="signin" class="btn" href="http://{{domain}}/login?n=http://ips.{{domain}}/list">Sign In</a>
% end
% def b2():
    <a href="#" class="name">{{user['first_name'] + ' ' + user['last_name']}}</a>
    <a href="http://{{domain}}/logout" class="btn btn-delete">Logout</a>
% end
%rebase default title='Index', x = b2 if user else b1, domain=domain