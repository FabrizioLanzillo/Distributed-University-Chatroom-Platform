<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.LoginInformationDTO"%>
<%@ page import="it.unipi.dsmt.student_platform.utility.ClientRedirector" %>

<html>
<head>
    <title>StudentChat</title>
</head>
<body>
<h1>Sign up to StudentChat</h1>

<div>
    <h2>Sign up today!</h2>
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
                email:
                <input type="text" name="email" placeholder="email" required />
            </label>
            <br>
            <label>
                name:
                <input type="text" name="name" placeholder="name" required />
            </label>
            <br>
            <label>
                surname:
                <input type="text" name="surname" placeholder="surname" required />
            </label>
            <br>
            <label>
                degree:
                <input type="text" name="degree" placeholder="degree" required />
            </label>
            <br>
            <label>
                language:
                <input type="text" name="language" placeholder="language" required />
            </label>
            <br>
            <button type="submit" name="signupButton">Join us!</button>
        </form>
    </div>
</div>

<%
    // Check if the user failed the login
    String rParam = request.getParameter("r");
    if (rParam != null && rParam.equals("error")) {
%>
        <div name="errorResponse">Error during your sign up, our service may be unavailable, try again later!</div>
<%
    }
    LoginInformationDTO logged_user = (LoginInformationDTO) request.getSession().getAttribute("logged_user");
    if (logged_user != null) {
        ClientRedirector.redirectToPortalPage(request, response, logged_user.getRole());
    }
%>
</body>
</html>