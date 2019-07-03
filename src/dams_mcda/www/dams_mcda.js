
let localAuth;

function getAuthHandler(message){
	console.log("getAuth called, now to ajax from django", message);
	// ajax request user data
	$.ajax({
		url: "/api/get_user_session/",
	}).done(function(data){
		console.log("api get user session done");
		Shiny.onInputChange("userSessionString", JSON.stringify(data));

	}).fail(function(response){
		console.log("api get user session Fail", response);

	});

	// sample user data
	let user_info = {
		"username": "wsk4",
		"session": "sdfasf8i1284jhfadfas"
	};
	Shiny.onInputChange("userSessionString", JSON.stringify(user_info));
}

Shiny.addCustomMessageHandler("getAuthHandler", getAuthHandler);
