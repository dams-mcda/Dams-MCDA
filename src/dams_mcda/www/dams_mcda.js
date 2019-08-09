
//let localAuth;
//
//function getAuthHandler(message){
//	console.log("getAuth called, now to ajax from django", message);
//	// ajax request user data
//	$.ajax({
//		url: "/api/get_user_session/",
//	}).done(function(data){
//		//console.log("api get user session done", data);
//		localAuth = data;
//		Shiny.onInputChange("userSessionString", JSON.stringify(data));
//	}).fail(function(response){
//		console.log("api get user session Fail", response);
//	});
//}
//
//function getVerifyAuthHandler(message){
//	console.log("getVerifyAuth called, now to ajax to/from django", message);
//	// ajax request user data
//	$.ajax({
//		url: "/api/verify_user_session/",
//		method: 'POST',
//		data: {
//			'session-id': localAuth["session"],
//			'username': localAuth["user"]["username"],
//			'group': localAuth["user"]["group"]
//		}
//	}).done(function(data){
//		console.log("api get user session done");
//		Shiny.onInputChange("userSessionVerificationString", JSON.stringify(data));
//
//	}).fail(function(response){
//		console.log("api get user session Fail", response);
//
//	});
//}
//
//Shiny.addCustomMessageHandler("getAuthHandler", getAuthHandler);
//Shiny.addCustomMessageHandler("getVerifyAuthHandler", getVerifyAuthHandler);

// method 2

// load context from parent when setContext is called
let cachedContext = {};

// initialize on ready
$(document).ready(function(){
	console.log("frame: ready");
	init();
});


// alert parent that we are awake/ready
function init(){
	console.log("frame: init");
	window.parent.setUpFrame();
	return true;
}


// set session context
function setContext(user, group, csrf, sess){
	console.log("frame: setContext", user, group, csrf, sess);

	$.ajaxSetup({
		headers: {
			"X-CSRFToken": csrf
		}
	});

	cachedContext['user'] = user;
	cachedContext['group'] = group;
	//cachedContext['csrf'] = csrf;
	cachedContext['session'] = sess;
}


// uses django to compare django session and cached context
// useful for debugging and essential for proof of concept
function validateSession(message){
	console.log("validateSession need csrftoken", message);

	// ajax request user data
	$.ajax({
		url: "/api/verify_user_session/",
		method: 'POST',
		data: {
			'session-id': cachedContext["session"],
			'user': cachedContext["user"],
			'group': cachedContext["group"],
		}
	}).done(function(data){
		console.log("Valid Session");
	}).fail(function(response){
		console.log("Invalid Session", response);
		alert('Invalid Session, Please try reloading.');
	});
}
Shiny.addCustomMessageHandler("validateSession", validateSession);
