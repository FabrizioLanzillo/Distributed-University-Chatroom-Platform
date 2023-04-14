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
        <div>
            <h1>Welcome to your portal, <%= loggedUser.getUsername() %>!</h1>
            
            <div class="buttons">
                <button onclick="location.href = '${pageContext.request.contextPath}/professor/create-course'">
                    Create a course
                </button>
                
                <button onclick="location.href = '${pageContext.request.contextPath}/professor/delete-course'">
                    Delete a course
                </button>
                
                <button onclick="location.href = '${pageContext.request.contextPath}/professor/meeting'">
                    Check your future meetings
                </button>
            </div>
        </div>
    </div>

</body>
</html>
