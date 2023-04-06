<%@ page import="it.unipi.dsmt.student_platform.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.AccessController" %>
<%@ page contentType="text/html;charset=UTF-8" %>

<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
	if (loggedUserDTO == null) {
		return;
    }
%>

<div id="top-bar" class="top-bar">
    
    <button onclick="location.href = '${pageContext.request.contextPath}/<%= loggedUserDTO.getRole().name() %>/portal'">
        Portal
    </button>
    
    <button onclick="location.href = '${pageContext.request.contextPath}/<%= loggedUserDTO.getRole().name() %>/profile'">
        Profile
    </button>
    
    <button onclick="location.href = '${pageContext.request.contextPath}/logout'">
        Logout
    </button>

</div>

