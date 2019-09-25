let frame;

// called from the iframe on the page
// notifies wrapper that it frame is ready for interation
function setUpFrame(){
	frame = window.frames['shiny-dams-mcda'].contentWindow;
	console.log("wrapper: setUpFrame", frame);

	// test context
	let user = getUsername();
	let userId = getUserId();
	let group = getGroup();
	let groupId = getGroupId();
	let csrf = getCSRF();
	let sess = getSession();

	frame.setContext(user, userId, group, groupId, csrf, sess);
}

function getUsername(){
	return $('#dams-mcda-user').attr('data-username');
}
function getUserId(){
	return $('#dams-mcda-user').attr('data-user-id');
}

function getGroup(){
	let group_name;
	try{
		group_name = $('#dams-mcda-group').attr('data-groupname');
	}catch(e){
		group_name = null;
	}
	return group_name;
}

function getGroupId(){
	let group_id;
	try{
		group_id = $('#dams-mcda-group').attr('data-group-id');
	}catch(e){
		group_id = null;
	}
	return group_id;
}

function getCSRF(){
	return $('[name="csrfmiddlewaretoken"]').val();
}

function getSession(){
	return $('#dams-mcda-session').attr('data-session');
}
