<%@ page contentType="text/html;charset=UTF-8" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.LoginInformationDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.ClientRedirector" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.AccessController" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.LoggedUserDTO" %>

<html>
<head>
    <title>StudentChat</title>
</head>
<body>

<%--TODO: change website name--%>
<h1>Sign up to StudentChat</h1>

<div>
    <h2>Sign up today!</h2>
    <form name="try_signup" method="post"
          action="${pageContext.request.contextPath}/student/signup">
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
        <button type="submit">Join us!</button>
    </form>
</div>

<br>

<button onclick="location.href = '${pageContext.request.contextPath}/'">
    You already own an account? Login!
</button>

<%
    // Check if the user failed the login
    String rParam = request.getParameter("r");
    if (rParam != null && rParam.equals("error")) {
%>
        <div id="errorResponse">Error during your sign up, our service may be unavailable, try again later!</div>
<%
    }
	
	// Check if user is already logged in, then redirect it to its portal page
    LoggedUserDTO loggedUser = AccessController.getLoggedUser(request);
    if (loggedUser != null) {
        ClientRedirector.redirectToPortalPage(request, response, loggedUser.getRole());
		return;
    }
%>
</body>
</html>
