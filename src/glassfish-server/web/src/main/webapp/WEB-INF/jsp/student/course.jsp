<%@ page import="it.unipi.dsmt.student_platform.dto.CourseDTO" %>
<html>

<head>
    <title>Course</title>
</head>

<body>

<div id="details">
    <h2>Details</h2>

<%
    CourseDTO courseDTO = (CourseDTO) request.getAttribute("course");
	if (courseDTO != null) {
%>
    COURSE!!
<%
    }
%>

</div>


</body>

</html>