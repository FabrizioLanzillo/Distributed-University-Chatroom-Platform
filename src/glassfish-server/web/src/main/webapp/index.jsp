<%@ page import="it.unipi.dsmt.student_platform.dto.UserDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.UserRedirection" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>PlaceholderName</title>
    <!-- TODO css + icon -->
</head>
<body>
<h1>Welcome to PlaceholderName!</h1>

<div>
    <h2>Login</h2>
    <div class="form">
        <form method="post" action="${pageContext.request.contextPath}/login">
            <label>
                Username:
                <input type="text" name="username" placeholder="username" required />
            </label>
            <br>
            <label>
                Password:
                <input type="password" name="password" placeholder="password" required />
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
            <button type="submit">LOGIN</button>
        </form>
    </div>
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
    UserDTO logged_user = (UserDTO) request.getSession().getAttribute("logged_user");
	if (logged_user != null) {
        UserRedirection.redirectUser(request, response, logged_user.getRole());
    }

%>

</body>
</html>
