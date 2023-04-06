<%--
    Page for showing the details of a course
--%>

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
        
        <script src="${pageContext.request.contextPath}/assets/libs/jquery/js/jquery-3.6.4.min.js"></script>
        <script type="text/javascript">
            
            function sendPostToCourseServlet(params, successMessage) {
                $.post(
                    "${pageContext.request.contextPath}/student/course",
                    params,
                    function () {
                        alert(successMessage);
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
            
            function starCourse(){
                const params = {
                    action: "star",
                    courseId: <%= courseDTO.getId() %>
                };
                
                sendPostToCourseServlet(params, "Course starred")
            }

            function unstarCourse(){
                const params = {
                    action: "unstar",
                    courseId: <%= courseDTO.getId() %>
                };

                sendPostToCourseServlet(params, "Course unstarred")
            }
            
        </script>
        
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
        <a href="${pageContext.request.contextPath}/student/booking?id=<%= courseDTO.getId() %>&offset=0">
            <button>Book a meeting</button>
        </a>
        
<%
    // Insert button to star or unstar a course (depending on from the current state)
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
