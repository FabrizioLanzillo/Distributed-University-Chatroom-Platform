<%@ page contentType="text/html;charset=UTF-8"%>
<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.MeetingDTO" %>
<%@ page import="java.util.ArrayList" %>

<%
    // List of available slots
    ArrayList<MeetingDTO> bookedSlots;
    try{
        bookedSlots = (ArrayList<MeetingDTO>)request.getAttribute("bookedSlots");
    }catch(Exception e){
        System.out.println(e.getMessage());
		bookedSlots = new ArrayList<>();
    }

    int offset = Integer.parseInt(request.getParameter("offset"));
%>
<html>
<head>
    <title>StudentChat</title>
</head>
<body>

<jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

<h1>Sign up to StudentChat</h1>

<div>
    <h2>All available slots:</h2>
    <form name="selected_slot" method="post"
          action="${pageContext.request.contextPath}/professor/meeting?offset=<%=offset%>">
        <%
            if(bookedSlots.isEmpty()){
                %>
                <div class="alert"> No booked meeting yet! </div>
                <%
            }
            else{
                int i=0;
                for(MeetingDTO bDTO : bookedSlots){%>

                        <%=bDTO.toString()%>
                        <input class="remove" name="timeslot" type="submit" value=<%=i%>> Delete this meeting </input>
                        <br>
                <%
                    i++;
                }
            }
        %>
    </form>
    <div name="response">
        <%
            // Check if the user failed the login
            String rParam = request.getParameter("r");
            if (rParam != null && rParam.equals("error")) {
        %>
        <div name="errorResponse">Error during your booking removal, try again later!</div>
        <%
        }
        else if (rParam != null && rParam.equals("success")) {
        %>
        <div name="successResponse">Removal successful!</div>
        <%
            }
        %>
    </div>
</div>

</body>
</html>