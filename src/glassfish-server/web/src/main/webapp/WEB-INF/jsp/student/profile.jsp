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

    List<StudentBookedMeetingDTO> bookedMeeting = (List<StudentBookedMeetingDTO>) request.getAttribute("booked-meeting");

	//	get the result of the delete operation, if it has been made
    String meetingDeleteACK = "";
    if(request.getAttribute("delete_ack") != null){
        meetingDeleteACK = request.getAttribute("delete_ack").toString();
    }
%>

    <head>
        <title>Student Profile</title>
        <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/assets/css/student/profile.css">
        <script src="${pageContext.request.contextPath}/assets/javascript/student/profile.js"></script>
    </head>
    <body>
        <div id="profile-page">
            <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />
            <br>
            <h1>
                Welcome to your student profile, <%= loggedUserDTO.getUsername() %>
            </h1>
            <br>
            <div id="meetings-container">
                <div id="meetings">
                    <script>
                        const meetingsDiv = document.getElementById("meetings");
                        meetingsDiv.innerHTML = "";
<%
                        // check on the result of the delete operation, if it has been made
                        if(!meetingDeleteACK.equals("")){
                            if(meetingDeleteACK.equals("ok")){
%>
                                alert("Meeting correctly deleted");
<%
                            }
                            else{
%>
                                alert("An error occurred while deleting the meeting");
<%
                            }
                            request.removeAttribute("delete_ack");
                        }
%>
                    </script>
<%
                    if(bookedMeeting.isEmpty()){
%>
                        <label>No booked meeting yet!</label>
<%
                    }
                    else{
                        for(StudentBookedMeetingDTO meeting : bookedMeeting){
%>
                            <button type="button" id="<%= meeting.getBookedMeetingId() %>" class="remove_booked_meeting" onclick="showDeleteMeetingAlert('<%= meeting.getBookedMeetingId() %>')">
                                <%=meeting.toString()%>
                            </button>
<%
                        }
                    }
%>
                </div>
            </div>
            <br>
            <%-- hidden form for the course delete --%>
            <div id="delete-meeting-alert" style="display:none;">
                <span class="close-alert-button" onclick="closeDeleteMeetingAlert();">&times;</span>
                <div class="delete-button-container">
                    <strong style="font-size: 1.2vw;">Attention! Are you really sure to delete this meeting?</strong>
                    <form id="delete-meeting-form" action="${pageContext.request.contextPath}/student/profile" method="POST">
                    </form>
                </div>
            </div>
        </div>
    </body>
</html>