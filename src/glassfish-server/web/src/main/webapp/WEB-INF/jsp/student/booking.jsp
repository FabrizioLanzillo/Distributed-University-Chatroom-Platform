<%@ page contentType="text/html;charset=UTF-8"%>
<%@ page import="it.unipi.dsmt.student_platform.dto.BookingDTO" %>
<%@ page import="java.util.ArrayList" %>

<%
    // List of available slots
    ArrayList<BookingDTO> bookingDTOS = (ArrayList<BookingDTO>)request.getAttribute("slots");
    int id = Integer.parseInt(request.getParameter("id"));
    int offset = Integer.parseInt(request.getParameter("offset"));
%>
<html>
<head>
    <title>StudentChat</title>
</head>
<body>
<h1>Sign up to StudentChat</h1>

<div>
    <h2>All available slots:</h2>
    <form name="selected_slot" method="post"
          action="${pageContext.request.contextPath}/student/booking?id=<%=id%>&offset=<%=offset%>">
        <%
            int i=0;
            for(BookingDTO bDTO : bookingDTOS){%>
                <input type="submit" class="timeslotbox" name="timeslot" value=<%=i%>><%=bDTO.toString()%></input>
                <br>
                <%
                i++;
            }
        %>
    </form>
    <div name="response">
        <%
            // Check if the user failed the login
            String rParam = request.getParameter("r");
            if (rParam != null && rParam.equals("error")) {
        %>
        <div name="errorResponse">Error during your booking, try again later!</div>
        <%
            }
            else if (rParam != null && rParam.equals("success")) {
        %>
        <div name="successResponse">Booking successful!</div>
        <%
            }
        %>
    </div>
</div>

</body>
</html>