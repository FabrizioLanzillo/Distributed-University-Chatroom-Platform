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
        The selected course doesn't exist :(
    </body>

<%
}
else {
%>

    <head>
        <title> <%= courseDTO.getName() %> </title>
        
        <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.6.1/jquery.min.js"></script>
        <script type="text/javascript">
            function starCourse(){
                const params = {
                    action: "star",
                    courseId: <%= courseDTO.getId() %>
                };
                
                $.post(
                    "${pageContext.request.contextPath}/student/course",
                    params,
                    function () {
                        alert("Course starred");
                    }
                ).fail(
                    function() {
                        alert('Error');
                    }
                ).always(
                    function() {
                        location.reload();
                    }
                );
            }

            function unstarCourse(){
                const params = {
                    action: "unstar",
                    courseId: <%= courseDTO.getId() %>
                };

                $.post(
                    "${pageContext.request.contextPath}/student/course",
                    params,
                    function () {
                        alert("Course unstarred");
                    }
                ).fail(
                    function() {
                        alert("Error");
                    }
                ).always(
                    function() {
                        location.reload();
                    }
                );
            }
            
        </script>
        
    </head>

    <body>

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
        
<%
    if (courseDTO.isStarred()) {
%>
        <button onclick="unstarCourse()">Unstar this course</button>
<%
    }
    else {
%>
        <button onclick="starCourse()">Star this course</button>
<%
    }
%>
    </body>
<%
}
%>

</html>
