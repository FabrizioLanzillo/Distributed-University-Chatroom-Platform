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
        <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/student/portal.css">
    </head>
    <body>
        <div id="student-portal-container">
            <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

            <br>
            <h1>University Courses Research</h1>
            <br>
            <div id="course-selection-container">
                <div id="course-selection">
                    <div class="course-search">
                        <form class="search-form" action="${pageContext.request.contextPath}/student/portal" method="get">
                            <label class="search-input-label">Search Courses:</label>
                            <input type="text" id="search-input" name="search_input" placeholder="Type the course name or the professor surname ...">
                            <button type="submit" class="portal-button">Search</button>
                        </form>
                    </div>
                    <div id="course-type">
                        <form class="search-form" action="${pageContext.request.contextPath}/student/portal" method="get">
<%
                            if(isStarredView.equals("true")){
%>
                                <button type="submit" name="starred" class="student-platform-button" value="false">
                                    See Other Courses
                                </button>
<%
                            }
                            else{
%>
                                <button type="submit" name="starred" class="student-platform-button" value="true">
                                    See Starred Courses
                                </button>
<%
                            }
%>
                        </form>
                    </div>
                </div>
                <hr class="hr-style">
                <div id="courses">
                    <script>
                        const coursesDiv = document.getElementById("courses");
                        coursesDiv.innerHTML = "";
                    </script>
<%
                        int counter = 0;
                        for (MinimalCourseDTO course : courses) {
							if(counter == 0){
%>
                                <div class="course-row-buttons">
<%
                            }
%>
                                    <button type="button" id="<%= course.getName() %>" class="selected-courses"
                                            onclick="location.href = '${pageContext.request.contextPath}/student/course?id=<%= course.getId() %>'">
                                            <%= course.getName() %>
                                    </button>
<%
                            if(counter == 2 || courses.indexOf(course) == courses.size() - 1){
								counter = 0;
%>
                                </div>
<%
                            }
							else{
								counter++;
                            }
                        }
%>
                </div>
            </div>
        </div>
    </body>
</html>