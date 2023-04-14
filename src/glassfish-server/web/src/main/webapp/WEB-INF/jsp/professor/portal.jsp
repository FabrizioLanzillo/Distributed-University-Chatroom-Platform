<%--
    Portal for professors
--%>

<%@ page import="it.unipi.dsmt.student_platform.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.AccessController" %>
<%@ page contentType="text/html;charset=UTF-8" %>
<html>

<head>
    <title>Professor portal</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/professor/portal.css">
</head>

<body>

<jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

<%
    LoggedUserDTO loggedUser = AccessController.getLoggedUserWithRedirect(request, response);
	if(loggedUser == null) {
		return;
    }
%>
    <div class="vertical-container">
        <h1>Welcome to your portal, <%= loggedUser.getUsername() %>!</h1>
        
        <div class="buttons">
            <a href="${pageContext.request.contextPath}/professor/create-course">
                <button onl>Create a course</button>
            </a>
        
            <a href="${pageContext.request.contextPath}/professor/delete-course">
                <button>Delete a course</button>
            </a>
        
            <a href="${pageContext.request.contextPath}/professor/meeting">
                <button>Check your future meetings</button>
            </a>
        </div>
    </div>

</body>
</html>
