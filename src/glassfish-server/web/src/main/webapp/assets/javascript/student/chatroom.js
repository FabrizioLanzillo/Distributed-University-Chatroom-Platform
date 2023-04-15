var websocket;

const LOGIN = "LOGIN";
const LOGOUT = "LOGOUT";
const MESSAGE = "MESSAGE";
const GET_ONLINE_USERS = "GET_ONLINE_USERS"

var username = "";
var course = null;
var id_timer = null;

const server_url = "ws://localhost:8080/";
// const server_url = "ws://10.2.1.4:8300/";

const websocketConfigurationParameters = {

    0: "opcode",
    1: "username",
    2: "course"
};

const messageParameters = {

    0: "opcode",
    1: "text"
};

function sendObjectThroughWebsocket(isWebsocketConfigurationMessage, ...args){

    var json_string = {};

    for( let i = 0; i < args.length; i++ ){
        const parameter = isWebsocketConfigurationMessage ? websocketConfigurationParameters[i]: messageParameters[i];
        json_string[parameter] = args[i];
    }
    websocket.send(JSON.stringify(json_string));
}

function refreshWebsocketConnection(){
    // refresh interval time
    const timeout = 10000;
    if (websocket.readyState === websocket.OPEN){
        // send the refresh message
        sendObjectThroughWebsocket(true, GET_ONLINE_USERS);
        id_timer = setTimeout(refreshWebsocketConnection, timeout);
    }
}

function stopRefreshWebsocketConnection(){
    clearTimeout(id_timer);
}

function openWebsocketConnection(){
    // creation json object
    sendObjectThroughWebsocket(true, LOGIN, username, course);
    // start of the counter for the websocket connection refresh
    refreshWebsocketConnection();
}

function closeWebsocketConnection(){
    // stop of the refresh of the connection
    stopRefreshWebsocketConnection();
}

function receiveObjectThroughWebsocket(event){

    var message = JSON.parse(event.data);
    if(message.opcode === MESSAGE){
        appendMessageToTheChat("You", message_text, false);
    }
    else if(message.opcode == GET_ONLINE_USERS){
        updateOnlineStudentsList(message.list);
    }
    else{
        alert("Unknown Message Recived");
    }

}

function connect(logged_user, course){

    username = logged_user;
    course = course;

    websocket = new WebSocket(server_url);
    websocket.onopen = openWebsocketConnection;
    websocket.onclose = closeWebsocketConnection;
    websocket.onmessage = receiveObjectThroughWebsocket;
}

function disconnect(){
    // the websocket is closed and the onclose method is invoked
    websocket.close();
}

function sendMessage(){

    var input_message = document.getElementById("chatroom-submit-area-input");
    var message_text = input_message.value;
    input_message.value = "";

    // TEST sendObjectThroughWebsocket(true, LOGIN, username, course);
    appendMessageToTheChat("You", message_text, true);
    appendMessageToTheChat("You", message_text, false);

    var list = [
        { username: "Mario" },
        { username: "Luigi" },
        { username: "Giovanni" }
    ];
    updateOnlineStudentsList(list);
}

function appendMessageToTheChat(sender_name, message, isMyMessage){

    var chatContainer = document.getElementById("chatroom-chat");

    var divMessageContainer = document.createElement("div");

    var divStudentProfile = document.createElement("div");
    divStudentProfile.setAttribute("class", "student-profile-image");

    var divMessage = document.createElement("div");
    divMessage.setAttribute("class", "message");

    var divMessageHeader = document.createElement("div");
    divMessageHeader.setAttribute("class", "message-header");

    var divMessageHeaderUsername = document.createElement("div");
    divMessageHeaderUsername.setAttribute("class", "message-header-username");
    divMessageHeaderUsername.textContent = sender_name;

    var divMessageContent = document.createElement("div");
    divMessageContent.setAttribute("class", "message-content");
    divMessageContent.textContent = message;

    if(isMyMessage){
        divMessageContainer.setAttribute("class", "message-container sent-message");
    }
    else{
        divMessageContainer.setAttribute("class", "message-container received-message");
    }

    chatContainer.appendChild(divMessageContainer);
    divMessageContainer.appendChild(divStudentProfile);
    divMessageContainer.appendChild(divMessage);
    divMessage.appendChild(divMessageHeader);
    divMessageHeader.appendChild(divMessageHeaderUsername);
    divMessage.appendChild(divMessageContent);
}

function updateOnlineStudentsList(studentList){
    var ulOnlineStudents = document.getElementById("online-student-list");
    // Flush old list from HTML page
    ulOnlineStudents.innerHTML = "";
    // Remove duplicates from new list

    student_list = [...new Set(studentList)];
    student_list.forEach(student => {
        var li = document.createElement("li");
        li.textContent = student.username;
        ulOnlineStudents.appendChild(li);
    });
}