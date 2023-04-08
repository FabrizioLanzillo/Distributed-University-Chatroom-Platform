var websocket;

const LOGIN = "LOGIN";
const LOGOUT = "LOGOUT";
const MESSAGE = "MESSAGE";
const UPDATE_ONLINE_USERS = "UPDATE_ONLINE_USERS"

var username = "";
var course = "";

const server_url = "localhost:8080";



function ws_onOpen() {
    // Creazione dell'oggetto JSON
    var data = {
        opcode: LOGIN,
        username: username,
        course: 1
    };
  
    // Convertiamo l'oggetto JSON in una stringa JSON
    var json_string = JSON.stringify(data);
    websocket.send(json_string);
}


function ws_onClose(){
    // Creazione dell'oggetto JSON
    var data_logout = {
        opcode: LOGOUT
    };
  
    // Convertiamo l'oggetto JSON in una stringa JSON
    var json_string = JSON.stringify(data_logout);
    websocket.send(json_string);
}
  

function connect(logging_user){

    username = logging_user;

    var course_selected = document.getElementById("course");
    course = course_selected.options[course_selected.selectedIndex].text;

    websocket = new WebSocket(server_url);
    websocket.onopen = function(){ws_onOpen()};
    websocket.onclose = function(){ws_onClose()};
}

function disconnect(){
    websocket.close();
}
