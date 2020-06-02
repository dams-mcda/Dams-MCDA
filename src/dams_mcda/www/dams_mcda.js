// load context from parent when setContext is called
let cachedContext = {};
let damNames = [];
let critNames = [];


/*
 * init
 *
 * alert parent that we are awake/ready
 */
function init(){
	console.log("frame: init");
	cachedContext['django_found'] = false;
	window.parent.setUpFrame();
	return true;
}


/*
 * setContext
 *
 * set session context
 */
function setContext(user, userId, group, groupId, csrf, sess){
	console.log("frame: setContext", user, userId, group, groupId, csrf, sess);

	$.ajaxSetup({
		headers: {
			"X-CSRFToken": csrf
		}
	});

	cachedContext['user'] = user;
	cachedContext['userId'] = parseInt(userId);
	cachedContext['group'] = group;
	cachedContext['groupId'] = parseInt(groupId);
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
	console.log("validateSession: checks for django & csrftoken", message);

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
		console.log("Valid Session - Django available for save/loads");
		cachedContext['django_found'] = true;
	}).fail(function(response){
		console.log("Invalid Session - No Django Server", response);
		alert('Invalid Session. Cannot save or load results. You can continue to use. File uploads will still work.');
	});
}


/*
 * for setting application mode ie: "group", "individual"
 */
function setAppMode(message){
	console.log("APP MODE", message)
	cachedContext['appMode'] = message;
}


/*
 * userHasGroup
 *
 * checks if user has group associated, if false we might have to redirect to group selection
 *
 * we could make a new ajax request to check if the user has been assigned a group since they loaded the page
 * but I am assuming this will not happen and can be solved by reloading the page
 *
 * returns a string of an integer for group_id or false and stores in input$session_user_group
 */
function userHasGroup(message){
	if (cachedContext['group'] != undefined){
		console.log("userHasGroup?: ", cachedContext['group']);
		Shiny.setInputValue('user_group', cachedContext['group'])
	}else{
		//console.log("userHasGroup? False, context: ", cachedContext);
		Shiny.setInputValue('user_group', "false")
	}
}


/*
 * saveRawJsonScores
 *
 * given string of json send to django backend to save
 *
 * NOTE: saved preferences are unique to user-group so
 * the most the db ever saves is two per user
 * one for group and one for individual
 *
 */
function saveRawJsonScores(message){
	//console.log("saveRawJsonScores: ", message);

	// get params
	let group_mode = (cachedContext["appMode"] == "group");
	let getParams; // check if already saved preferences (see note above)
	let params; // params for saving

	if (group_mode){
		getParams = {
			'user': cachedContext["userId"],
			'group': cachedContext["groupId"],
		}
	}else{
		getParams = {
			'user': cachedContext["userId"],
			'group': ''
		}
	}

	// params for update/create
	if (group_mode){
		params = {
			'session-id': cachedContext["session"],
			'user': cachedContext["userId"],
			'group': cachedContext["groupId"],
			'scores': message
		}
	}else{
		params = {
			'session-id': cachedContext["session"],
			'group': "",
			'user': cachedContext["userId"],
			'scores': message
		}
	}

	// check if save for this user/group exists
	$.ajax({
		url: "/core/api/preference/",
		method: 'GET',
		dataType:'json',
		data: getParams
	}).done(function(data){
		if (data[0]!==undefined){
			console.log("Patch existing PreferenceRecord");
			// UPDATE already existing
			$.ajax({
				url: "/core/api/preference/"+ data[0].id +"/",
				method: 'PATCH',
				dataType:'json',
				data: params
			}).done(function(data){
				console.log("PATCH Succesful!");
			}).fail(function(response){
				console.log("Failed PATCHING!", response);
			});
		}else{
			console.log("create new PreferenceRecord");
			// CREATE
			$.ajax({
				url: "/core/api/preference/",
				method: 'POST',
				dataType:'json',
				data: params
			}).done(function(data){
				console.log("Succesfully CREATED!");
			}).fail(function(response){
				console.log("Failed CREATE!", response);
			});
		}
	}).fail(function(response){
		// CREATE
		$.ajax({
			url: "/core/api/preference/",
			method: 'POST',
			dataType:'json',
			data: params
		}).done(function(data){
			console.log("Succesfully CREATED!");
		}).fail(function(response){
			console.log("Failed CREATE!", response);
		});
	});
}


/*
 * bindDamNames
 *
 * message is array of string names (in order of appearance)
 */
function bindDamNames(message){
	damNames = message;
}


/*
 * bindCritNames
 *
 * message is array of string names (in order of appearance)
 */
function bindCritNames(message){
	critNames = message;
}


/*
 * loadScores
 *
 * get preferences from a saved session
 *
 */
function loadScores(input_mode){
	let group_mode = (cachedContext["appMode"] == "group");

	let params;
	if (group_mode){
		params = {
			'group': cachedContext["groupId"]
		}
	}else{
		params = {
			'user': cachedContext["userId"],
		}
	}

	// ajax request user data
	$.ajax({
		url: "/core/api/preference/",
		method: 'GET',
		dataType:'json',
		data: params

	}).done(function(data){
		if (data[0] != undefined){
			if (group_mode){
				//console.log("loadScores group results: ", data);
				// return average of each result
				let num_records = data.length;

				for (let damId in damNames){

					let damIndex = parseInt(damId) + 1; // R index starts at 1
					//console.log("data for dam ", damId, damData);
					for (let critId in critNames){
						let summed_score = 0;
						for (let recordId=0;recordId<num_records;recordId++){
							//console.log("record: ", recordId, " val: ", data[recordId]["scores"][damNames[damId]][critId])
							summed_score += data[recordId]["scores"][damNames[damId]][critId]
						}
						summed_score = (summed_score/num_records)
						//console.log("final summed_score: ", summed_score)
						// fires event on server to update input slider
						Shiny.setInputValue(
							"session_input_update",
							[(critNames[critId] + damIndex.toString()), summed_score],
							{priority: "event"}
						)
					}
				}
				alert("Saved Group Scores Loaded")
			}else{
				//console.log("loadScores indiv results: ", data);
				// return first result
				let scores = data[0]["scores"];
				for (let damId in damNames){
					let damData = scores[damNames[damId]];
					let damIndex = parseInt(damId) + 1; // R index starts at 1
					//console.log("data for dam ", damId, damData);
					for (let critId in critNames){
						//console.log("InputId: ", critNames[critId] + damIndex.toString())
						// fires event on server to update input slider
						Shiny.setInputValue(
							"session_input_update",
							[(critNames[critId] + damIndex.toString()), damData[critId]],
							{priority: "event"}
						)
					}
				}
				alert("Saved Scores Loaded")
			}
			//return data;
		}else{
			// successfull request but there are no data returned
			console.log("first time running, no scores to load");
		}

	}).fail(function(response){
		// failed request
		console.log("loadScores failed: ", response);
	});
}


/*
 * noFileSelected
 *
 * user clicks continue without selecting a file to upload
 */
function noFileSelected(message){
	alert("You must select a file for upload before continuing");
}


/*
 * invalidFileSelected
 *
 * user uploads a file that can't be used
 */
function invalidFileSelected(message){
	alert(message + ". Please verify you are uploading the correct file or try a different file.");
}


/*
 * Shiny Event Handlers
 *
 * binds a string handler name to actual function
 */
Shiny.addCustomMessageHandler("validateSession", validateSession);
Shiny.addCustomMessageHandler("saveResultsToDjango", saveRawJsonScores);
Shiny.addCustomMessageHandler("loadScores", loadScores);
Shiny.addCustomMessageHandler("noFileSelected", noFileSelected);
Shiny.addCustomMessageHandler("invalidFileSelected", invalidFileSelected);
Shiny.addCustomMessageHandler("checkUserHasGroup", userHasGroup);
Shiny.addCustomMessageHandler("setAppMode", setAppMode);
Shiny.addCustomMessageHandler("bindDamNames", bindDamNames);
Shiny.addCustomMessageHandler("bindCritNames", bindCritNames);


// initialize shiny js on ready
$(document).on('shiny:sessioninitialized', function(event){
	console.log("frame: ready");
	init();
});
