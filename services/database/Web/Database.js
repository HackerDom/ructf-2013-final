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

$(function () {
	$("#send").click(function () {
		var query = $("#query").val();
		console.log(query);
		$.ajax({
			url: "http://dqteam.org/Database",
			method: "POST",
			data: {
				data: escape(JSON.stringify({ 
					query: query
				}))
			}

		}).done(function (response) {
			console.log(response);
			json = JSON.parse(response);
			if (json.status == "OK")
				FillOutput(json.data);
			else
				console.log(json.error);
		})
	});
});
