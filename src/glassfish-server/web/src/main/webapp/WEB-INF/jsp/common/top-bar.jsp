<%--
    JSP file containing the topbar of the website.
    It should be included in all the website pages after user's login.
--%>
<%@ page contentType="text/html;charset=UTF-8" %>
<div id="top-bar" class="top-bar">

    <button onclick="location.href = '${pageContext.request.contextPath}/student/profile'">
        Profile
    </button>
    
    <button onclick="location.href = '${pageContext.request.contextPath}/logout'">
        Logout
    </button>

</div>

