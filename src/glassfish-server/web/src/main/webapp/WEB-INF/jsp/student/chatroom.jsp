<%@ page import="it.unipi.dsmt.student_platform.dto.*" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.AccessController" %>
<%@ page contentType="text/html;charset=UTF-8" %>

<html>

<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
    if (loggedUserDTO == null) {
        return;
    }
    
    int courseId = Integer.parseInt(request.getParameter("id"));
	
    String courseName;
    if (request.getAttribute("course_name") != null) {
        courseName = request.getAttribute("course_name").toString();
    }
	else {
		return;
    }

%>
    <head>
        <title>Chatroom</title>
        <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/assets/css/student/chatroom.css">
        <script src="${pageContext.request.contextPath}/assets/javascript/student/chatroom.js"></script>
    </head>
    <body onload="connect('<%= loggedUserDTO.getUsername() %>', <%= courseId %>)" onunload="disconnect()">
        <div id="chatroom-page">
            <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

            <div id="chatroom-container">
                <div id="online-student-list-container">
                    <header class="online-student-list-header">
                        <b>Online Students:</b>
                    </header>
                    <div class="online-student-list-content">
                        <ul id="online-student-list">
                            <li>No Online Students</li>
                        </ul>
                    </div>
                </div>

                <div class="chatroom">
                    <header class="chatroom-header">
                        <b><%=courseName%> Chat</b>
                    </header>
                    <main id="chatroom-chat"> </main>

                    <div class="chatroom-submit-area">
                        <input type="text" id="chatroom-submit-area-input" placeholder="Enter your message...">
                        <button type="submit" class="chatroom-submit-area-button" onclick="sendMessage()">
                            Send
                        </button>
                    </div>
                </div>
            </div>
        </div>
    </body>
</html>