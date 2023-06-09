<%--
    JSP file containing the topbar of the website.
    It should be included in all the website pages after user's login.
--%>
<%@ page import="it.unipi.dsmt.student_platform.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.AccessController" %>
<%@ page import="it.unipi.dsmt.student_platform.enums.UserRole" %>
<%@ page contentType="text/html;charset=UTF-8" %>

<%
    LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
    if (loggedUserDTO == null) {
        return;
    }
%>

<link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/common/top-bar.css">

<div class="topnav">
    
    <div id="logo">
        <img src="${pageContext.request.contextPath}/assets/img/logo.svg" alt="Logo"/>
    </div>
    
    <div id="left-container">
        <button onclick="location.href = '${pageContext.request.contextPath}/<%= loggedUserDTO.getRole().name() %>/portal'">
            Portal
        </button>

<%
        if (loggedUserDTO.getRole() == UserRole.student) {
%>
            <button onclick="location.href = '${pageContext.request.contextPath}/<%= loggedUserDTO.getRole().name() %>/profile'">
                Profile
            </button>
<%
        }
%>
    </div>
    <div id="flex-container"></div>
    <div id="right-container">
        <button onclick="location.href = '${pageContext.request.contextPath}/logout'">
            Logout
        </button>
    </div>

</div>
