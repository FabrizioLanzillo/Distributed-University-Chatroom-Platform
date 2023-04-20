<%@ page import="it.unipi.dsmt.student_platform.servlets.professor.ProfessorCreateCourseServlet" %><%--
    Website page for creating a new course.
--%>

<html>

<head>
    <title>Create course</title>
    
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/libs/jquery/css/jquery.timepicker.min.css">
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/professor/create-course.css">
    
    <script src="${pageContext.request.contextPath}/assets/libs/jquery/js/jquery-3.6.4.min.js"></script>
    <script src="${pageContext.request.contextPath}/assets/libs/jquery/js/jquery.timepicker.min.js"></script>
    <script src="${pageContext.request.contextPath}/assets/javascript/professor/create-course.js"></script>
</head>

<body>
    <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

<%
    Boolean successful = (Boolean) request.getAttribute(ProfessorCreateCourseServlet.attributeSuccessfulCreation);
	if (successful == Boolean.TRUE) {
%>
    <script>
        alert("Course successfully created")
    </script>
<%
    }
	else if (successful == Boolean.FALSE) {
%>
    <script>
        alert("Course creation failed because a duplicate course was found")
    </script>
<%
    }
	// Reset attribute
	request.setAttribute("successful-creation", null);
%>
    
    <div class="form-container">
        <div>
            <h1>Create a course</h1>
            
            <form method="post" action="${pageContext.request.contextPath}/professor/create-course">
                <label>
                    Name of the course:
                    <input type="text" name="name" required />
                </label>
                <br>
                
                
                <div>Meeting hours:</div>
                <div class="meeting_pickers">
                    <label>
                        Weekday:
                        <select id ="weekday" name="weekday" required></select>
                    </label>
                    <label>
                        From:
                        <input id="start-time" name="start-time" class="timepicker" required>
                    </label>
                    <label>
                        To:
                        <input id="end-time" name="end-time" class="timepicker" required disabled>
                    </label>
                </div>
                
                
                <label>
                    Description:
                    <textarea name="description" required></textarea>
                </label>
                <br>
                
                <button type="submit">CREATE</button>
            </form>
        </div>
    </div>

</body>
</html>