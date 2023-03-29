<%@ page import="it.unipi.dsmt.student_platform.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.CourseDTO" %>
<%@ page import="java.util.List" %>
<html>

<%
    // get the attribute for the user and for the courses
    LoggedUserDTO loggedUserDTO = (LoggedUserDTO) request.getSession().getAttribute("logged_user");
    List<CourseDTO> courses = (List<CourseDTO>) request.getAttribute("courses");

    //	get the result of the delete operation, if it has been made
	String courseDeleteACK = "";
    if(request.getAttribute("deleteAck") != null){
        courseDeleteACK = request.getAttribute("deleteAck").toString();
    }
%>

    <head>
        <title>Delete Course</title>
    </head>
    <body>
        <h1>
            Welcome to the delete course Section, <%= loggedUserDTO.getUsername() %>
        </h1>
        <h1>University Courses</h1>

        <%-- Form for the course search --%>
        <form action="${pageContext.request.contextPath}/professor/delete-course" method="get">
            <label for="searchInput">Search Courses:</label>
            <input type="text" id="searchInput" name="search_input" placeholder="Type the course name...">
            <button type="submit">Search</button>
        </form>

        <div id="courses">
            <script>
                const coursesDiv = document.getElementById("courses");
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

                // function that show the hidden form, that send the post request for the course delete
                // it also add the relative button to the selected course, setting up the value
                // to be submitted with the id of the course selected
                function showDeleteAlert(courseIdToDelete) {
                    document.getElementById("delete-alert").style.display = "block";
                    var deleteCourseForm = document.getElementById("delete-course-form");
                    var submitButton = document.createElement("button");
                    submitButton.innerHTML = "Delete";
                    submitButton.setAttribute("type", "submit");
                    submitButton.setAttribute("name", "courseId");
                    submitButton.setAttribute("value", courseIdToDelete);
                    deleteCourseForm.appendChild(submitButton);
                }

                // function that hide the form, if the cancel button is clicked
                function closeDeleteAlert() {
                    document.getElementById("delete-alert").style.display = "none";
                    var deleteCourseForm = document.getElementById("delete-course-form");
                    deleteCourseForm.innerHTML = "";
                }
            </script>
<%
                // load of the courses
                for (CourseDTO course : courses) {
%>
                    <button type="button" id="<%= course.getName() %>" class="normal_courses" onclick="showDeleteAlert('<%= course.getId() %>')">
                        <%= course.getName() %>
                    </button>
<%
                }
%>
            <%-- hidden form for the course delete --%>
            <div id="delete-alert" style="display:none;"><br>
                <label>Are you really sure to delete this course?</label><br><br>
                <button onclick="closeDeleteAlert()">Cancel</button>
                <form id="delete-course-form" action="${pageContext.request.contextPath}/professor/delete-course" method="POST">
                </form>
            </div>
        </div>
    </body>
</html>