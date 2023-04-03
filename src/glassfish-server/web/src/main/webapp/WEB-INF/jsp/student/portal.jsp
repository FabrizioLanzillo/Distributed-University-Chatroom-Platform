<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.*" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.AccessController" %>
<%@ page contentType="text/html;charset=UTF-8" %>

<html>

<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
	if (loggedUserDTO == null) {
		return;
    }

    List<MinimalCourseDTO> courses = (List<MinimalCourseDTO>) request.getAttribute("courses");

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
        <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />
        
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
                for (MinimalCourseDTO course : courses) {
%>
                    <button type="button" id="<%= course.getName() %>" class="normal_courses"
                            onclick="location.href = '${pageContext.request.contextPath}/student/course?id=<%= course.getId() %>'">
                            <%= course.getName() %>
                    </button>
<%
				}
%>
        </div>
    </body>
</html>