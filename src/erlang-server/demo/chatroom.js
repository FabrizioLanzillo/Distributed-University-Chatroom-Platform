var websocket;

console.log("ciao");

const LOGIN = "LOGIN";
const LOGOUT = "LOGOUT";
const MESSAGE = "MESSAGE";
const GET_ONLINE_USERS = "GET_ONLINE_USERS"

var username = "";
var course = null;
var id_timer = null;

const server_url = "ws://localhost:8080/";
// const server_url = "ws://10.2.1.4:8300/";


function keep_connection_alive(){
    const timeout = 10000;
    if (websocket.readyState === websocket.OPEN){
        var json_string = {
            "opcode": GET_ONLINE_USERS,
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
    stop_keep_alive();
}

function ws_onMessage(event) {

    console.log("MESSAGE: " + event.data);

    var receivedMessage = JSON.parse(event.data);
    
    if (receivedMessage.opcode === MESSAGE) {
        print_message(receivedMessage.sender, receivedMessage.text, true);
    }
    else if (receivedMessage.opcode === GET_ONLINE_USERS) {
        update_online_students_list(receivedMessage.list);
    }
    
} 

function connect(logging_user){

    username = logging_user;

    // var course_selected = document.getElementById("course");
    // course = course_selected.options[course_selected.selectedIndex].text;
    course = 1;

    websocket = new WebSocket(server_url);
    websocket.onopen = ws_onOpen;
    websocket.onclose = ws_onClose;
    websocket.onmessage = ws_onMessage;
}

function send_message(){
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


function update_online_students_list(student_list) {
    var list_elem = document.getElementById("online_students");
    // Flush old list from HTML page
    list_elem.innerHTML = "";
    // Remove duplicates from new list
    student_list = [...new Set(student_list)];
    // Build new list
    student_list.forEach(student => {
        var username = document.createElement("p");
        username.setAttribute("class", "username");
        username.textContent = student;
        list_elem.appendChild(username);
    });
}
