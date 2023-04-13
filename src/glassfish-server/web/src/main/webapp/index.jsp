<%--
    Webapp login page
--%>
<%@ page import="it.unipi.dsmt.student_platform.utility.ClientRedirector" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.LoggedUserDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.AccessController" %>
<%@ page contentType="text/html;charset=UTF-8" %>
<html>
<head>
    <title>StudentPlatform</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/login.css">
</head>
<body>
<div class="login-container">
    <h1>Welcome to StudentPlatform!</h1>
    
    <div>
        <h2>Login</h2>
        
        <form method="post" action="${pageContext.request.contextPath}/login">
            <label>
                Username:
                <input type="text" name="username" placeholder="Username" required />
            </label>
            <br>
            <label>
                Password:
                <input type="password" name="password" placeholder="Password" required />
            </label>
            <br>
            <label>
                Role:
                <select name="role">
                    <option value="student">Student</option>
                    <option value="professor">Professor</option>
                    <option value="admin">Admin</option>
                </select>
            </label>
            <br>
            <button type="submit" class="submit-button">LOGIN</button>
        </form>
    </div>
    
    <button onclick="location.href = '${pageContext.request.contextPath}/student/signup'" class="signup-button">
        You don't own an account? Signup now!
    </button>
</div>


<%
    // Check if the user failed the login
	String rParam = request.getParameter("r");
    if (rParam != null && rParam.equals("error")) {
%>
    <div>Error: failed login</div>
<%
    }

	// Redirect user is already logged
    LoggedUserDTO logged_user = AccessController.getLoggedUser(request);
	if (logged_user != null) {
        ClientRedirector.redirectToPortalPage(request, response, logged_user.getRole());
    }

%>

</body>
</html>
