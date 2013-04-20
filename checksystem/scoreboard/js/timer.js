var url = 'http://172.16.16.100:3001/time_left';
var timeout = 60*1000;
var main_time = 0;
var extra_time = 0;

$(document).ready(Load_Timer);

function Load_Timer()
{
    $("#countdown-status").hide();

	$.ajaxSetup({ cache: false });
	$.getJSON(url, function(data) {
        main_time = parseInt(data.main_time);
        extra_time = parseInt(data.extra_time);

        Update_Main();
        Update_Extra();
        setTimeout(function(){Load_Timer()}, timeout);
    });
}

function Update_Main()
{
	var mt = main_time;
	$("#countdown-main").html('');
    var mainCountdown = new Countdown( {
    	time: mt,
    	width : 300,
		height : 100,
		rangeHi : "hour",
		rangeLo : "second",
    	style: "flip",
    	target : "countdown-main",
    	onComplete : Main_Complete,
    	labels : { color : "#fff" }
    } );
}

function Update_Extra()
{
	var et = main_time>0? 0 : extra_time;
	$("#countdown-extra").html('');
    var extraCountdown = new Countdown( {
    	time: et,
    	width : 300,
		height : 100,
		rangeHi : "hour",
		rangeLo : "second",
    	style: "flip",
    	target : "countdown-extra",
    	onComplete : Extra_Complete,
    	labels : { color : "#fff" }
    } );
}

function Main_Complete()
{
	main_time = 0;
	Update_Extra();
}

function Extra_Complete()
{
	if (main_time == 0) {
        extra_time = 0;
		$("#countdown-status").show();
	}
}