<%@ page contentType="text/html;charset=UTF-8"%>
<%@ page import="it.unipi.dsmt.student_platform.dto.MeetingDTO" %>
<%@ page import="java.util.ArrayList" %>

<%
    // List of available slots
    ArrayList<MeetingDTO> bookedSlots;
    try{
        bookedSlots = (ArrayList<MeetingDTO>)request.getAttribute("bookedSlots");
    }catch(Exception e){
		bookedSlots = new ArrayList<>();
    }
    // Extract current offset if not set initialize to 0 (current month)
    int offset = request.getParameter("offset")==null ? 0 : Integer.parseInt(request.getParameter("offset"));
%>
<html>
<head>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/professor/meeting.css">
    <title>Meetings</title>
</head>
<body>

<jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

<h1 id="header1"></h1>

<script>
    const monthNames = ["January", "February", "March", "April", "May", "June",
        "July", "August", "September", "October", "November", "December"
    ];

    const d = new Date();
    const month = d.getMonth() + <%=offset%>;
    d.setMonth(month)
    document.getElementById("header1").textContent = "Your meetings for " + monthNames[d.getMonth()] + " " + d.getFullYear();
</script>

<div id="meeting_tab">
    <h2>Booked slots:</h2>
    <form id="formMeetings" name="selected_slot" method="post"
          action="${pageContext.request.contextPath}/professor/meeting?offset=<%=offset%>">
        <%
            // If any, show booked meeting in the selected month
            if(bookedSlots.isEmpty()){
                %>
                <div class="alert"> No booked meeting yet! </div>
                <%
            }
            else{
                int i=0;
                for(MeetingDTO bDTO : bookedSlots){%>

                        <%=bDTO.toString()%>
                        <button class="remove" name="timeslot" type="submit" value=<%=i%>> Delete this meeting </button>
                        <br>
                <%
                    i++;
                }
            }
        %>
    </form>
    <div id="offsetDiv">
        <form class="offsetForm" method="post" action="${pageContext.request.contextPath}/professor/meeting?action=offsetChange&offset=<%=offset - 1%>">
            <%
                if(offset <=0 ){%>
            <button class="offset" disabled="disabled"><-</button>
            <%}
            else{%>
            <button type="submit"><-</button>
            <%}%>
        </form>
        <form class="offsetForm" method="post" action="${pageContext.request.contextPath}/professor/meeting?action=offsetChange&offset=<%=offset + 1%>">
            <button class="offset" type="submit">-></button>
        </form>
    </div>
</div>

<script>
    <%
    String rParam = request.getParameter("r");
    // check on the result of the delete operation, if it has been made
    if(rParam != null && rParam.equals("error")){
    %>
    alert("Error during meeting deletion");
    <%
    }
    else if(rParam!= null && rParam.equals("success")){
    %>
    alert("Meeting deleted successfully");
    <%
    }
%>
</script>


</body>
</html>