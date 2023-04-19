<%@ page import="it.unipi.dsmt.student_platform.dto.LoggedUserDTO" %>
<%@ page import="java.util.List" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.MinimalCourseDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.AccessController" %>
<html>

<%
    // get the attribute for the user and for the courses
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
	if (loggedUserDTO == null) {
		return;
    }
    List<MinimalCourseDTO> courses = (List<MinimalCourseDTO>) request.getAttribute("courses");

    //	get the result of the delete operation, if it has been made
	String courseDeleteACK = "";
    if(request.getAttribute("deleteAck") != null){
        courseDeleteACK = request.getAttribute("deleteAck").toString();
    }
%>

    <head>
        <title>Delete Course</title>
        <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/assets/css/professor/delete-course.css">
        <script src="${pageContext.request.contextPath}/assets/javascript/professor/deleteCourse.js"></script>
    </head>
    <body>
        <div id="professor-delete-course-container">
            <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

            <br>
            <h1>
                Welcome to the Delete Course Page, <%= loggedUserDTO.getUsername() %>
            </h1>
            <br>
            <div id="course-to-delete-selection-container">
                <div id="course-to-delete-selection">
                    <div class="course-to-delete-search">
                        <%-- Form for the course search --%>
                        <form class="search-courses-form" action="${pageContext.request.contextPath}/professor/delete-course" method="get">
                            <label class="search-input-label">Search Courses:</label>
                            <input type="text" id="search-delete-course-input" name="search_input" placeholder="Type the course name...">
                            <button type="submit" class="delete-course-button">Search</button>
                        </form>
                    </div>
                </div>
                <hr class="hr-style">
                <div id="courses-to-delete">
                    <script>
                        const coursesDiv = document.getElementById("courses-to-delete");
                        coursesDiv.innerHTML = "";
<%
                        // check on the result of the delete operation, if it has been made
                        if(!courseDeleteACK.equals("")){
                            if(courseDeleteACK.equals("ok")){
%>
                                alert("Course correctly deleted");
<%
                            }
                            else{
%>
                                alert("An error occurred while deleting the course");
<%
                            }
                            request.removeAttribute("deleteAck");
                        }
%>
                    </script>
<%
                    // load of the courses
                    int counter = 0;
                    for (MinimalCourseDTO course : courses) {
                        if(counter == 0){
%>
                            <div class="course-row-buttons">
<%
                        }
%>
                                <button type="button" id="<%= course.getName() %>" class="selected-courses" onclick="showDeleteAlert('<%= course.getId() %>')">
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
            <br>
            <%-- hidden form for the course delete --%>
            <div id="delete-course-alert" style="display:none;">
                <span class="close-alert-button" onclick="closeDeleteAlert();">&times;</span>
                <div class="delete-button-container">
                    <strong style="font-size: 1.2vw;">Attention! Are you really sure to delete this meeting?</strong>
                    <form id="delete-course-form" action="${pageContext.request.contextPath}/professor/delete-course" method="POST">
                    </form>
                </div>
            </div>
        </div>
    </body>
</html>