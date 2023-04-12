var websocket;

console.log("ciao");

const LOGIN = "LOGIN";
const LOGOUT = "LOGOUT";
const MESSAGE = "MESSAGE";
const UPDATE_ONLINE_USERS = "UPDATE_ONLINE_USERS"

var username = "";
var course = null;
var id_timer = null;

const server_url = "ws://localhost:8080/";

function print_message(sender_name, message, sendOrReceive) {

    var box = document.getElementById("chatbox");

    var div = document.createElement("div");
    div.setAttribute("class", "chatbox__messages");

    var indMessageDiv = document.createElement("div");

    var p_name = document.createElement("p");
    p_name.setAttribute("class", "name");

    var p_message = document.createElement("p");
    p_message.setAttribute("class", "message");

    if(sendOrReceive == false) {

        indMessageDiv.setAttribute("class", "chatbox__messages__user-message--ind-message__right");

        p_name.textContent = "You:";
        p_message.textContent = message;
    } else {
        p_name.textContent = sender_name + ":";
        p_message.textContent = message;

        indMessageDiv.setAttribute("class", "chatbox__messages__user-message--ind-message__left");
    }

    indMessageDiv.appendChild(p_name);
    indMessageDiv.appendChild(document.createElement("br"));
    indMessageDiv.appendChild(p_message);
    div.appendChild(indMessageDiv);
    box.appendChild(div);
} 


function keep_connection_alive(){
    const timeout = 30000;
    if (websocket.readyState === websocket.OPEN){
        var json_string = {
            "opcode": UPDATE_ONLINE_USERS,
        };
        
        websocket.send(JSON.stringify(json_string));
        id_timer = setTimeout(keep_connection_alive, timeout);
    }
}

function stop_keep_alive(){
    clearTimeout(id_timer);
}

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
    keep_connection_alive();
}


function ws_onClose(){
    // Creazione dell'oggetto JSON
    var data_logout = {
        opcode: LOGOUT
    };
  
    // Convertiamo l'oggetto JSON in una stringa JSON
    var json_string = JSON.stringify(data_logout);
    websocket.send(JSON.stringify(json_string));
    stop_keep_alive();
}

function ws_onMessage(event) {

    console.log("MESSAGE: " + event.data);

    var receivedMessage = JSON.parse(event.data);
    
    if(receivedMessage.opcode === MESSAGE){
        print_message(receivedMessage.sender, receivedMessage.text, true);
    }
    
} 

function connect(logging_user){

    username = logging_user;

    // var course_selected = document.getElementById("course");
    // course = course_selected.options[course_selected.selectedIndex].text;
    course = 1;

    websocket = new WebSocket(server_url);
    websocket.onopen = function(){ws_onOpen()};
    websocket.onclose = function(){ws_onClose()};
    websocket.onmessage = function(event){ws_onMessage(event)};
}

function send_message(event){
    var input_message = document.getElementById("message_text");

    var message_text = input_message.value;
    input_message.value = "";

    var json_string = {
        "opcode": MESSAGE,
        "text": message_text
    };
  
    websocket.send(JSON.stringify(json_string));
    print_message("", message_text, false);
    
}

function disconnect(){
    websocket.close();
}
