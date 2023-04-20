let websocket;

const LOGIN = "LOGIN";
const MESSAGE = "MESSAGE";
const GET_ONLINE_USERS = "GET_ONLINE_USERS"

let username = "";
let course = null;
let id_timer = null;

// const server_url = "ws://localhost:8300/";
const server_url = "ws://10.2.1.4:8300/";

const websocketConfigurationParameters = {

    0: "opcode",
    1: "username",
    2: "course"
};

const messageParameters = {

    0: "opcode",
    1: "text"
};



/**
 * this function create an object from the args passed and
 * send it through the websocket
 * @param isWebsocketConfigurationMessage is a boolean value that select the right parameters
 *                                        for the object creation
 * @param args  are all the parameters of the object
 */
function sendObjectThroughWebsocket(isWebsocketConfigurationMessage, ...args){

    // create a JSON object
    const json_string = {};
    // generate the params for the type of object websocket vs chat params
    for( let i = 0; i < args.length; i++ ){
        const parameter = isWebsocketConfigurationMessage ? websocketConfigurationParameters[i]: messageParameters[i];
        json_string[parameter] = args[i];
    }
    // send JSON object
    websocket.send(JSON.stringify(json_string));
}

/**
 * this function execute an automatic refresh of the websocket connection every 10 sec
 */
function refreshWebsocketConnection(){
    // refresh interval time
    const timeout = 10000;
    if (websocket.readyState === websocket.OPEN){
        // send the refresh message
        sendObjectThroughWebsocket(true, GET_ONLINE_USERS);
        id_timer = setTimeout(refreshWebsocketConnection, timeout);
    }
}

/**
 * this function stop the automatic refresh of the websocket connection
 */
function stopRefreshWebsocketConnection(){
    clearTimeout(id_timer);
}

/**
 * this function start the connection with websocket, sending the login message
 * and with the start of the automatic refresh of the websocket connection, by the call of the
 * provided function
 */
function openWebsocketConnection(){
    // creation json object
    sendObjectThroughWebsocket(true, LOGIN, username, course);
    // start of the counter for the websocket connection refresh
    refreshWebsocketConnection();
}

/**
 * this function is invoked when the connection with the websocket is closed
 * it stops the automatic refresh of the websocket connection, by the call of the
 * provided function
 */
function closeWebsocketConnection(){
    stopRefreshWebsocketConnection();
    console.error("WebSocket connection closed");
    // Try to connect again to chatroom servers
    setTimeout(function() {
        connect(username, course);
    }, 1000);
}

/**
 * function that parse the JSON object received and execute the action
 * provided by the opcode parameter of the object
 * @param event is the JSON object that has to be parsed
 */
function receiveObjectThroughWebsocket(event){
    // parse of the JSON object received and execute
    const message = JSON.parse(event.data);
    if(message.opcode === MESSAGE){
        appendMessageToTheChat(message.sender, message.text, false);
    }
    else if(message.opcode === GET_ONLINE_USERS){
        updateOnlineStudentsList(message.list);
    }
    else{
        alert("Unknown Message Received");
    }

}

/**
 * function that handle possible errors with the websocket connection
 */
function handleWebsocketError() {
    if (websocket.readyState === 3) {
        // Connection is closed
        alert("Error: cannot connect to chatroom server");
    }
    console.log("Websocket error");
}




/**
 * function that execute the connection with the websocket
 * @param _logged_user is the username of the student
 * @param _course is the current course where we want chat
 */
function connect(_logged_user, _course){
    // Save user information
    username = _logged_user;
    course = _course;
    
    // Setup websocket connection
    websocket = new WebSocket(server_url);
    websocket.onopen = openWebsocketConnection;
    websocket.onclose = closeWebsocketConnection;
    websocket.onmessage = receiveObjectThroughWebsocket;
    websocket.onerror = handleWebsocketError;
}

/**
 * function that execute the disconnect with the websocket
 */
function disconnect(){
    // the websocket is closed and the onclose method is invoked
    websocket.close();
}

/**
 * function that create a JSON object message with the provided function and
 * that append it directly to the chat of the user, because it is one of his messages
 */
function sendMessage(){

    const input_message = document.getElementById("chatroom-submit-area-input");
    const message_text = input_message.value;
    input_message.value = "";
    
    if (websocket.readyState === 3) {
        // Connection closed
        alert("Error: the chatroom is offline. Please wait for reconnection");
    }
    else {
        sendObjectThroughWebsocket(false, MESSAGE, message_text);
        appendMessageToTheChat("You", message_text, true);
    }

}




/**
 * function that append a message to the div of the chat
 * @param sender_name is the name of the sender, if the sender is the user it is
 *                      substituted by the YOU word
 * @param message is the text of the message
 * @param isMyMessage is a boolean, in order to understand if the sender is the user
 *                    or another student
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
    
    // Scroll chat to bottom
    scrollChatToBottom();
}

/**
 * this function update the list of the online students
 * @param studentList is the list of the current online students
 */
function updateOnlineStudentsList(studentList){
    const ulOnlineStudents = document.getElementById("online-student-list");
    // Flush old list from HTML page
    ulOnlineStudents.innerHTML = "";
    // Remove duplicates from new list

    let student_list = [...new Set(studentList)];
    // add of the user to the html list of the online user
    student_list.forEach(student => {
        const li = document.createElement("li");
        li.textContent = student;
        ulOnlineStudents.appendChild(li);
    });
}

/**
 * Scroll chat to bottom
 */
function scrollChatToBottom() {
    const chat = document.getElementById("chatroom-chat");
    chat.scrollTop = chat.scrollHeight;
}
