var websocket;

console.log("ciao");

const LOGIN = "LOGIN";
const LOGOUT = "LOGOUT";
const MESSAGE = "MESSAGE";
const UPDATE_ONLINE_USERS = "UPDATE_ONLINE_USERS"

var username = "";
var course = null;

const server_url = "ws://localhost:8080/";



function ws_onOpen() {
    // Creazione dell'oggetto JSON
    var data = {
        "opcode": LOGIN,
        "username": username,
        "course": course
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

    // var course_selected = document.getElementById("course");
    // course = course_selected.options[course_selected.selectedIndex].text;
    course = 1;

    websocket = new WebSocket(server_url);
    websocket.onopen = function(){ws_onOpen()};
    websocket.onclose = function(){ws_onClose()};
}

function send_message(event){
    
}

function disconnect(){
    websocket.close();
}

