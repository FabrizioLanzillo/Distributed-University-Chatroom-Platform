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
%>

    <head>
        <title>Admin Portal</title>
    </head>
    <body>
        <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

        <h1>
            Welcome to your admin portal, <%= loggedUserDTO.getUsername() %>
        </h1>

        <div>
            <button type="button" class="admin_portal_button"
                    onclick="location.href = '${pageContext.request.contextPath}/admin/users'">
                    Browse Users
            </button>

            <button type="button" class="admin_portal_button"
                    onclick="location.href = '${pageContext.request.contextPath}/admin/create-professor'">
                Create a new professor account
            </button>
        </div>

    </body>
</html>