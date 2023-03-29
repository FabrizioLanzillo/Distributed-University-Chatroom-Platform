<%@ page import="it.unipi.dsmt.student_platform.dto.CourseDTO" %>
<%@ page contentType="text/html;charset=UTF-8" %>
<html>

<%
CourseDTO courseDTO = (CourseDTO) request.getAttribute("course");
if (courseDTO == null) {
    // Error message
%>

    <head>
        <title>COURSE NOT FOUND</title>
    </head>
    <body>
        <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />
        
        The selected course doesn't exist :(
    </body>

<%
}
else {
%>

    <head>
        <title> <%= courseDTO.getName() %> </title>
    </head>

    <body>
        <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

        <h1><%=courseDTO.getName()%></h1>

        <div id="professor">
            This course is held by prof. <%= courseDTO.getProfessor().getFullName() %>
        </div>

        <div id="details">
            <h2>Details</h2>
            <%= courseDTO.getDescription() %>
        </div>

        <!-- Buttons for chatroom and booking -->
        <a href="${pageContext.request.contextPath}/student/chatroom?id=<%= courseDTO.getId() %>">
            <button>Go to chatroom</button>
        </a>
        <a href="${pageContext.request.contextPath}/student/booking?id=<%= courseDTO.getId() %>">
            <button>Book a meeting</button>
        </a>

    </body>
<%
}
%>

</html>
