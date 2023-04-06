<%@ page contentType="text/html;charset=UTF-8"%>
<%@ page import="it.unipi.dsmt.student_platform.dto.BookingDTO" %>
<%@ page import="java.util.List" %>
<%@ page import="java.util.Optional" %>
<%@ page import="java.util.ArrayList" %>

<%
    // Get list of available slots from request attributes
    List<BookingDTO> bookingDTOS = Optional.ofNullable((List<BookingDTO>) request.getAttribute("slots"))
            .orElse(new ArrayList<>());
    
	// Get course id from GET parameters
	String idStr = request.getParameter("id");
	if (idStr == null || idStr.isEmpty()) {
	    throw new RuntimeException("No id provided");
    }
	int id = Integer.parseInt(idStr);
	
	// Get offset from GET parameters
    String offsetStr = request.getParameter("offset");
    int offset = offsetStr == null ? 0 : Integer.parseInt(offsetStr);
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
          action="${pageContext.request.contextPath}/student/booking?id=<%=id%>&offset=<%=offset%>">
        <%
            int i=0;
			//Create a button for each available slot
            for(BookingDTO bDTO : bookingDTOS){
        %>
                <input type="submit" class="timeslot" name="timeslot" value=<%=i%>><%=bDTO.toString()%>
                <br>
        <%
                i++;
            }
        %>
    </form>

    <form method="post" action="${pageContext.request.contextPath}/student/booking?action=offsetChange&id=<%=id%>&offset=<%=offset - 1%>">
        <%
        // Create buttons to change month, backward is enabled only if we are looking for at least a month in the future
        if(offset <=0 ){
        %>
            <button disabled="disabled"><-</button>
        <%}
        else{
        %>
            <button type="submit"><-</button>
        <%}
        %>
    </form>
    <form method="post" action="${pageContext.request.contextPath}/student/booking?action=offsetChange&id=<%=id%>&offset=<%=offset + 1%>">
        <button type="submit">-></button>
    </form>

    <div id="response">
        <script>
            <%
                // Check if we have been redirected here after a booking request, in that case "r" parameter is set
                String rParam = request.getParameter("r");
                if (rParam != null && rParam.equals("error")) {
            %>
                alert("Booking failed");
            <%
                }
                else if (rParam != null && rParam.equals("success")) {
            %>
                alert("Booking successful");
            <%
                }
            %>
        </script>
    </div>
</div>

</body>
</html>