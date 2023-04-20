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
        
        <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/student/course.css">
        
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
        
        <div class="vertical-container">
            <div>
                <h1><%=courseDTO.getName()%></h1>
        
                <div class="course-professor">
                    This course is held by prof. <%= courseDTO.getProfessor().getFullName() %>
                </div>
        
                <div class="course-details">
                    <h2>Details</h2>
                    <span><%= courseDTO.getDescription() %></span>
                </div>
                
                <div class="buttons">
                    <!-- Buttons for chatroom and booking -->
                    <button onclick='location.href="${pageContext.request.contextPath}/student/chatroom?id=<%= courseDTO.getId() %>"'>
                        Go to chatroom
                    </button>
                    <button onclick='location.href="${pageContext.request.contextPath}/student/booking?id=<%= courseDTO.getId() %>&offset=0"'>
                        Book a meeting
                    </button>
                
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
                </div>
            </div>

        </div>
    </body>
<%
}
%>

</html>
