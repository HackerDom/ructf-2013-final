function ClearOutput() {
	$("#output").html("");
	$("#status").html("");
	$("#message").html("");
}

function FillOutput(data) {
	var table = $("#output");
	var html = "";
	console.log(data);
	html += "<tr>";
	for (var i in data.columns) {
		console.log(i);
		html += "<th>" + data.columns[i] + "</th>";
	}
	html += "</tr>";	
	for (var i in data.rows) {
		console.log(data[i]);
		html += "<tr>";
		for (var key in data.columns)
			html += "<td>" + data.rows[i][data.columns[key]] + "</td>";
		html += "</tr>";
	}
	table.html(html);
}
function FillStatus(response) {
	var status = $("#status");
	status.html(response.status);
	if (!response.error)
		return;
	var message = $("#message");
	message.html("error " + response.error.code + ": " + response.error.str);
}

$(function () {
	$("#send").click(function () {
		var query = $("#query").val();
		console.log(query);
		$.ajax({
			url: "http://dqteam.org/Database",
			method: "POST",
			contentType: "application/json",
			data: JSON.stringify({ 
				query: query
			})
		}).done(function (response) {
			console.log(response);
			//json = JSON.parse(response); //not needed if content-type is application/json
			json = response;
			ClearOutput();
			FillStatus(json);
			if (json.status == "OK")
				FillOutput(json.data);
			else
				console.log(json.error);
		})
	});
});
