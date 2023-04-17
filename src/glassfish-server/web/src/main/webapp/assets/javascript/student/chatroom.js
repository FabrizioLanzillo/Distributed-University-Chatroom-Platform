let websocket;

const LOGIN = "LOGIN";
const MESSAGE = "MESSAGE";
const GET_ONLINE_USERS = "GET_ONLINE_USERS"

let username = "";
let course = null;
let id_timer = null;

const server_url = "ws://localhost:8000/";
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



/*
    WEBSOCKET HANDLERS
 */

function sendObjectThroughWebsocket(isWebsocketConfigurationMessage, ...args){
    // Create JSON object
    const json_string = {};
    // Generate right parameters for type of object (websocket configuration vs chatroom message)
    for (let i = 0; i < args.length; i++) {
        const parameter = isWebsocketConfigurationMessage ? websocketConfigurationParameters[i]: messageParameters[i];
        json_string[parameter] = args[i];
    }
    // Send JSON object
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
    // Parse received JSON message and execute necessary action
    const message = JSON.parse(event.data);
    if (message.opcode === MESSAGE) {
        appendMessageToTheChat(message.sender, message.text, false);
    }
    else if (message.opcode === GET_ONLINE_USERS) {
        updateOnlineStudentsList(message.list);
    }
    else {
        alert("Unknown Message Recived");
    }

}

function handleWebsocketError() {
    if (websocket.readyState === 3) {
        // Connection closed
        alert("Error: cannot connect to chatroom server");
    }
}



/*
    METHODS FOR WEBSOCKET CONNECTION
 */

function connect(_logged_user, _course){
    // Initialize username and course
    username = _logged_user;
    course = _course;

    // Create websocket connection
    websocket = new WebSocket(server_url);
    websocket.onopen = openWebsocketConnection;
    websocket.onclose = closeWebsocketConnection;
    websocket.onmessage = receiveObjectThroughWebsocket;
    websocket.onerror = handleWebsocketError;
}

function disconnect(){
    // the websocket is closed and the onclose method is invoked
    websocket.close();
}

function sendMessage(){

    const input_message = document.getElementById("chatroom-submit-area-input");
    const message_text = input_message.value;
    input_message.value = "";

    sendObjectThroughWebsocket(false, MESSAGE, message_text);
    appendMessageToTheChat("You", message_text, true);

}



/*
    METHODS FOR DOM HANDLING
 */

function appendMessageToTheChat(sender_name, message, isMyMessage){

    const chatContainer = document.getElementById("chatroom-chat");

    const divMessageContainer = document.createElement("div");

    const divStudentProfile = document.createElement("div");
    divStudentProfile.setAttribute("class", "student-profile-image");

    const divMessage = document.createElement("div");
    divMessage.setAttribute("class", "message");

    const divMessageHeader = document.createElement("div");
    divMessageHeader.setAttribute("class", "message-header");

    const divMessageHeaderUsername = document.createElement("div");
    divMessageHeaderUsername.setAttribute("class", "message-header-username");
    divMessageHeaderUsername.textContent = sender_name;

    const divMessageContent = document.createElement("div");
    divMessageContent.setAttribute("class", "message-content");
    divMessageContent.textContent = message;

    if (isMyMessage) {
        divMessageContainer.setAttribute("class", "message-container sent-message");
    }
    else {
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
    const ulOnlineStudents = document.getElementById("online-student-list");
    // Flush old list from HTML page
    ulOnlineStudents.innerHTML = "";
    // Remove duplicates from new list
    let student_list = [...new Set(studentList)];
    // Add online students in HTML list
    student_list.forEach(student => {
        const li = document.createElement("li");
        li.textContent = student;
        ulOnlineStudents.appendChild(li);
    });
}