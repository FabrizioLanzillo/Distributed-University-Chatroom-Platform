<%@ page import="it.unipi.dsmt.student_platform.dto.StudentDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.CourseDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.ProfessorDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.LoggedUserDTO" %>
<%@ page import="java.util.List" %>
<%@ page import="java.util.ArrayList" %>
<%@ page contentType="text/html;charset=UTF-8" %>

<html>

<%
    LoggedUserDTO loggedUserDTO = (LoggedUserDTO) request.getSession().getAttribute("logged_user");

    List<CourseDTO> courses = (List<CourseDTO>) request.getAttribute("courses");

    String isStarredView = "";

	if(request.getAttribute("starred") != null){
        isStarredView = request.getAttribute("starred").toString();
		System.out.println(isStarredView);
    }
%>

    <head>
        <title>Student Portal</title>
    </head>
    <body>
        <h1>
            Welcome to your student portal, <%= loggedUserDTO.getUsername() %>
        </h1>
        <h1>University Courses</h1>

        <form action="${pageContext.request.contextPath}/student/portal" method="get">
            <label for="searchInput">Search Courses:</label>
            <input type="text" id="searchInput" name="search_input" placeholder="Type the course name...">
            <button type="submit">Search</button>
        </form>

        <form action="${pageContext.request.contextPath}/student/portal" method="get">
            <input type="hidden" name="student" value="<%= loggedUserDTO.getUsername() %>">
<%
                if(isStarredView.equals("true")){
%>
                    <button type="submit" name="starred" value="false">See Other Courses</button>
<%
                }
				else{
%>
                    <button type="submit" name="starred" value="true">See Starred Courses</button>
<%
                }
%>
        </form>
        <div id="courses">
            <script>
                const coursesDiv = document.getElementById("courses");
                coursesDiv.innerHTML = "";
            </script>
<%
                for (CourseDTO course : courses) {
%>
                    <button type="button" id="<%= course.getName() %>" class="normal_courses">
                            <%= course.getName() %>
                    </button>
<%
				}
%>
        </div>
    </body>
</html>