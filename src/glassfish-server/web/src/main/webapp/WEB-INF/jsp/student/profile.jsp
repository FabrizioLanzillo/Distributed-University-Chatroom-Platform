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
    </head>
    <body>
        <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

        <h1>
            Welcome to your student profile, <%= loggedUserDTO.getUsername() %>
        </h1>

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

                // function that show the hidden form, that send the post request for the meeting delete
                // it also add the relative button to the selected meeeting, setting up the value
                // to be submitted with the id of the meeting selected
                function showDeleteMeetingAlert(meetingIdToDelete) {
                    // disable of all the buttons of the course until the operation is finished
                    // or the cancel button is clicked
                    var meetingButtons = document.querySelectorAll('.remove_booked_meeting');
                    Array.from(meetingButtons).forEach(function (button){
                        button.disabled = true;
                    });
                    document.getElementById("delete-meeting-alert").style.display = "block";
                    var deleteMeetingForm = document.getElementById("delete-meeting-form");
                    var submitButton = document.createElement("button");
                    submitButton.innerHTML = "Delete";
                    submitButton.setAttribute("type", "submit");
                    submitButton.setAttribute("id", meetingIdToDelete);
                    submitButton.setAttribute("name", "meeting_id");
                    submitButton.setAttribute("value", meetingIdToDelete);
                    deleteMeetingForm.appendChild(submitButton);
                }

                // function that hide the form, if the cancel button is clicked
                function closeDeleteMeetingAlert() {
                    // enable of all the buttons of the course previously disabled
                    var meetingButtons = document.querySelectorAll('.remove_booked_meeting');
                    Array.from(meetingButtons).forEach(function (button){
                        button.disabled = false;
                    });
                    document.getElementById("delete-meeting-alert").style.display = "none";
                    var deleteMeetingForm = document.getElementById("delete-meeting-form");
                    deleteMeetingForm.innerHTML = "";
                }
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
        <%-- hidden form for the course delete --%>
        <div id="delete-meeting-alert" style="display:none;"><br>
            <label>Are you really sure to delete this meeting?</label><br><br>
            <button onclick="closeDeleteMeetingAlert()">Cancel</button>
            <form id="delete-meeting-form" action="${pageContext.request.contextPath}/student/profile" method="POST">
            </form>
        </div>

    </body>
</html>