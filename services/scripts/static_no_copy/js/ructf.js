function base_domain() {
    var r = /(team\d+\.ructf)$/.exec(location.host);
    return r[1];
}

function url_for(page) {
    var domain = base_domain();
    var url = 'http://' + domain + '/';
    switch (page) {
        case 'signup':
            url += 'register';
            break;
        case 'signin':
            url += 'login';
            break;
        case 'user':
            url += 'user';
            break;
        case 'logout':
            url += 'logout';
            break;
    }
    url += '/?n=' + document.URL;
    return url;
}

function link_to(service) {
    var domain = base_domain();
    return 'http://' + service + '.' + domain + '/';
}

function replace_urls() {
    $('a.brand').attr('href', 'http://' + base_domain());
    $('.navbar ul.nav li:nth-child(1) a').attr('href', link_to('ses'));
    $('.navbar ul.nav li:nth-child(2) a').attr('href', link_to('mp'));
    $('.navbar ul.nav li:nth-child(3) a').attr('href', link_to('db'));
    $('.navbar ul.nav li:nth-child(4) a').attr('href', link_to('queue'));
    $('.navbar ul.nav li:nth-child(5) a').attr('href', link_to('dns'));
    $('.navbar ul.nav li:nth-child(6) a').attr('href', link_to('ips'));
    $('.navbar ul.nav li:nth-child(7) a').attr('href', link_to('scripts'));
}

function user() {
    if ($.cookie('session')) {
        $.ajax(url_for('user'), {
            type: 'POST',
            contentType: 'application/json',
            headers: {'X-Requested-With': 'XMLHttpRequest'},
            data: JSON.stringify({session: $.cookie('session')}),
            dataType: 'json',
            success: function(data) {
                console.log(data);
                if (data.status === 'OK') {
                    $('.navbar-inner .pull-right').html($('#xxx-user').render({name: [data.first_name, data.last_name].join(' '), logout: url_for('logout')}));
                } else {
                    $('.navbar-inner .pull-right').html($('#xxx-login').render({register: url_for('signup'), login: url_for('signin')}));
                }
            }
        });
    } else {
        $('.navbar-inner .pull-right').html($('#xxx-login').render({register: url_for('signup'), login: url_for('signin')}));
    }
}
