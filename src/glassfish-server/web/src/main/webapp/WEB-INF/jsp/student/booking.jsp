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
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/student/booking.css">
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
    document.getElementById("header1").textContent = "Available meetings for " + monthNames[d.getMonth()] + " " + d.getFullYear();
</script>

<div id="booking_tab">
    <h2>All available slots:</h2>
    <form id="formBookings" name="selected_slot" method="post"
          action="${pageContext.request.contextPath}/student/booking?id=<%=id%>&offset=<%=offset%>">
        <%
            int i=0;
			//Create a button for each available slot
            for(BookingDTO bDTO : bookingDTOS){
        %>
        <button type="submit" class="timeslot" name="timeslot" value=<%=i%>><%=bDTO.toString()%></button>
                <br>
        <%
                i++;
            }
        %>
    </form>
    <div id="offsetDiv">
        <form class="offsetForm" method="post" action="${pageContext.request.contextPath}/student/booking?action=offsetChange&id=<%=id%>&offset=<%=offset - 1%>">
            <%
            // Create buttons to change month, backward is enabled only if we are looking for at least a month in the future
            if(offset <=0 ){
            %>
                <button  class="offset" disabled="disabled"><-</button>
            <%}
            else{
            %>
                <button  class="offset" type="submit"><-</button>
            <%}
            %>
        </form>
        <form method="post" action="${pageContext.request.contextPath}/student/booking?action=offsetChange&id=<%=id%>&offset=<%=offset + 1%>">
            <button class="offset" type="submit">-></button>
        </form>
    </div>

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