function base_domain() {
    var r = /(team\d+\.ructf)$/.exec(location.host);
    var domain = r[1];
    return domain;
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
    return url;
}

function replace_urls() {
    $('#signup').attr('href', url_for('signup'));
    $('#signin').attr('href', url_for('signin'));
}

function user() {
    if ($.cookie('session')) {
        $.ajax(url_for('user'), {
            type: 'POST',
            contentType: 'application/json',
            headers: {'X-Requested-With': 'XMLHttpRequest'},
            data: JSON.stringify({session: $.cookie('session')}),
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
