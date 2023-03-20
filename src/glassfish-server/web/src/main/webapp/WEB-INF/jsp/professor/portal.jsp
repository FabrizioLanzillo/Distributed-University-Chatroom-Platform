<%@ page import="it.unipi.dsmt.student_platform.dto.LoggedUserDTO" %>
<%@ page contentType="text/html;charset=UTF-8" %>
<html>

<head>
    <title>Professor portal</title>
</head>

<body>

<%
    LoggedUserDTO loggedUser = (LoggedUserDTO) request.getSession().getAttribute("logged_user");
%>

    <h1>Welcome to your portal, <%= loggedUser.getUsername() %>!</h1>

    <a href="${pageContext.request.contextPath}/professor/create-course">
        <button>Create a course</button>
    </a>

    <a href="${pageContext.request.contextPath}/professor/delete-course">
        <button>Delete a course</button>
    </a>

    <a href="${pageContext.request.contextPath}/professor/meeting">
        <button>Check your future meetings</button>
    </a>

</body>
</html>
