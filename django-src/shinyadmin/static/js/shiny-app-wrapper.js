let frame;

// called from the iframe on the page
// notifies wrapper that it frame is ready for interation
function setUpFrame(){
	frame = window.frames['shiny-dams-mcda'].contentWindow;
	console.log("wrapper: setUpFrame", frame);

	// test context
	let user = getUsername();
	let group = getGroup();
	let csrf = getCSRF();
	let sess = getSession();

	frame.setContext(user, group, csrf, sess);
}

function getUsername(){
	return $('#dams-mcda-username').attr('data-username');
}

function getGroup(){
	let group_name;
	try{
		group_name = $('#dams-mcda-groupname').attr('data-groupname');
	}catch(e){
		group_name = null;
	}
	return group_name

}

function getCSRF(){
	return $('[name="csrfmiddlewaretoken"]').val();
}

function getSession(){
	return $('#dams-mcda-session').attr('data-session');
}
