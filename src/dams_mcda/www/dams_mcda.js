// load context from parent when setContext is called
let cachedContext = {};


/*
 * init
 *
 * alert parent that we are awake/ready
 */
function init(){
	console.log("frame: init");
	window.parent.setUpFrame();
	return true;
}


/*
 * setContext
 *
 * set session context
 */
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


/*
 * validateSession
 *
 * uses django to compare django session and cached context
 * useful for debugging and essential for proof of concept
 */
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


/*
 * saveRawJsonScores
 *
 * given string of json send to django backend to save
 *
 * NOTE: as of now this just tests sending json string from shiny backend to shiny frontend
 */
function saveRawJsonScores(message){

	console.log("saveRawJsonScores: ", message);
	// potential ajax setup below

	// ajax request user data
	$.ajax({
		url: "/api/saveScores/",
		method: 'POST',
		data: {
			'session-id': cachedContext["session"],
			'user': cachedContext["user"],
			'group': cachedContext["group"],
			'scores': JSON.parse(message)
		}
	}).done(function(data){
		console.log("Succesfully Saved!");
	}).fail(function(response){
		console.log("Failed Saving!", response);
		alert('Not Implemented, just for testing');
	});
}


function noFileSelected(message){
	alert("You must select a file for upload before continuing");
}


/*
 * Shiny Event Handlers
 *
 * binds a string handler name to actual function
 */
Shiny.addCustomMessageHandler("validateSession", validateSession);
Shiny.addCustomMessageHandler("saveResultsToDjango", saveRawJsonScores);
Shiny.addCustomMessageHandler("noFileSelected", noFileSelected);


// initialize shiny js on ready
$(document).ready(function(){
	console.log("frame: ready");
	init();
});
