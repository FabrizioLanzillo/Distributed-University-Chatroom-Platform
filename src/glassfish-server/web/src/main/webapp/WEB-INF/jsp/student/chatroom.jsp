<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.*" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.AccessController" %>
<%@ page contentType="text/html;charset=UTF-8" %>

<html>

<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
    if (loggedUserDTO == null) {
        return;
    }

    String courseName = "";
    if(request.getAttribute("course_name") != null){
        courseName = request.getAttribute("course_name").toString();
        System.out.println(courseName);
    }
%>
    <head>
        <title>Chatroom</title>
        <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/assets/css/student/chatroom.css">
        <script src="${pageContext.request.contextPath}/assets/javascript/chatroom.js"></script>
    </head>
    <body>
        <div id="chatroom-page">
            <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

            <div id="chatroom-container">
                <div class="online-user-list-container">
                    <header class="online-user-list-header">
                        <b>Online Students:</b>
                    </header>
                    <div class="online-user-list-content">
                        <ul>
                            <li>f.montini</li>
                            <li>f.lanzillo</li>
                            <li>r.sagramoni</li>
                            <li>e.tinghi</li>
                            <li>t.bertini</li>
                            <li>salvatoccia</li>
                            <li>Cindy</li>
                        </ul>
                    </div>
                </div>

                <div class="chatroom">

                    <header class="chatroom-header">
                        <b><%=courseName%></b>
                    </header>
                    <main class="chatroom-chat">
                        <div class="message-container received-message">
                            <div
                                    class="student-profile-image"
                                    style="background-image: url(https://image.flaticon.com/icons/svg/327/327779.svg)">
                            </div>

                            <div class="message">
                                <div class="message-header">
                                    <div class="message-header-username">BOT</div>
                                </div>
                                <div class="message-content">
                                    Hi, welcome to SimpleChat! Go ahead and send me a message. 😄
                                </div>
                            </div>
                        </div>

                        <div class="message-container sent-message">
                            <div
                                    class="student-profile-image"
                                    style="background-image: url(https://image.flaticon.com/icons/svg/145/145867.svg)">
                            </div>

                            <div class="message">
                                <div class="message-header">
                                    <div class="message-header-username">YOU</div>
                                </div>

                                <div class="message-content">
                                    You can change your name in JS secction!nge your name in JS secction!nge your name in JS secction!nge your name in JS secction!
                                </div>
                            </div>
                        </div>
                        <div class="message-container sent-message">
                            <div
                                    class="student-profile-image"
                                    style="background-image: url(https://image.flaticon.com/icons/svg/145/145867.svg)">
                            </div>

                            <div class="message">
                                <div class="message-header">
                                    <div class="message-header-username">YOU</div>
                                </div>

                                <div class="message-content">
                                    You can change your name in JS section!
                                </div>
                            </div>
                        </div>

                    </main>


                    <form class="chatroom-submit-area">
                        <input type="text" class="chatroom-submit-area-input" placeholder="Enter your message...">
                        <button type="submit" class="chatroom-submit-area-button" onclick="showAlert()">Send</button>
                    </form>
                </div>
            </div>
        </div>
    </body>
</html>