function ses_domain() {
    return 'http://ses.' + base_domain();
}

function ses_identity_list() {
    var url = ses_domain() + '/identity/list';
    $.post(url, function(data) {
        if (data.status === 'OK') {
            $('#email_list tbody').html($('#identity_row_template').render(data.result));
        }
    })
}

function ses_identity_add() {
    var url = ses_domain() + '/identity/add';
    $.post(url, JSON.stringify({email: $('#email_for_add').first().val()}), function(data) {
        console.log(data);
        if (data.status === 'OK') {
            ses_identity_list();
        }
    })
}

function ses_identity_delete() {
    var url = ses_domain() + '/identity/del';
    $.post(url, JSON.stringify({id: $(this).attr('eid')}), function(data) {
        console.log(data);
        if (data.status === 'OK') {
            ses_identity_list();
        }
    })
}

function ses_credentials_list() {
    var url = ses_domain() + '/credentials/list';
    $.post(url, function(data) {
        if (data.status === 'OK') {
            $('#credentials_list tbody').html($('#credentials_row_template').render(data.result));
        }
    })
}

function ses_credentials_add() {
    var url = ses_domain() + '/credentials/add';
    $.post(url, JSON.stringify({}), function(data) {
        console.log(data);
        if (data.status === 'OK') {
            $('#credential_login').val(data.result.login);
            $('#credential_password').val(data.result.pass);
            ses_credentials_list();
        }
    })
}

function ses_credentials_delete() {
    var url = ses_domain() + '/credentials/del';
    $.post(url, JSON.stringify({id: $(this).attr('cid')}), function(data) {
        console.log(data);
        if (data.status === 'OK') {
            ses_credentials_list();
        }
    })
}

function ses_identity_get() {
    var url = ses_domain() + '/identity/list';
    $.post(url, function(data) {
        if (data.status === 'OK') {
            $('select.mails').html($('#mails_row_template').render(data.result));
        }
    })
}

function ses_mail_send() {
    var url = ses_domain() + '/mail/send'
    $.post(url, JSON.stringify({
        from: $('select[name="from"]').val(),
        to: $('input[name="to"]').val(),
        subject: $('input[name="subject"]').val(),
        message: $('textarea[name="body"]').val()
    }), function(data) {
        console.log(data);
        if (data.status === 'OK') {
            $('input[name="to"]').val('');
            $('input[name="subject"]').val('');
            $('textarea[name="body"]').val('');
            alert('Message send!');
        }
    });
    return false;
}

function ses_stats() {
    var url = ses_domain() + '/stats';
    $.post(url, function(data) {
        if (data.status === 'OK') {
            $('div.stats').html($('#stats').render(data.result));
        }
    })
}
