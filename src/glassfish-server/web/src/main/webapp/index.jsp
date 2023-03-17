<%@ page import="it.unipi.dsmt.student_platform.dto.UserDTO" %>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>PlaceholderName</title>
    <!-- TODO css + -->
</head>
<body>
<h1>Welcome to PlaceholderName!</h1>

<div>
    <h2>Login</h2>
    <div class="form">
        <form method="post" action="${pageContext.request.contextPath}/login">
            <label>
                <input type="text" name="username" placeholder="username" required />
            </label>
            <label>
                <input type="password" name="password" placeholder="password" required />
            </label>
            <button type="submit">login</button>
        </form>
    </div>
</div>

<%
    HttpSession httpSession = request.getSession();
	UserDTO logged_user = (UserDTO) httpSession.getAttribute("logged_user");
    if (logged_user == null && tried_login) {
%>
    <div>Login failed!</div>
<%
    }
	else if (logged_user != null) {
		response.sendRedirect();
    }
%>

</body>
</html>