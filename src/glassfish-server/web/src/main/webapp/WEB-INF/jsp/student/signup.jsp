<%@ page contentType="text/html;charset=UTF-8" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.ClientRedirector" %>
<%@ page import="it.unipi.dsmt.student_platform.utility.AccessController" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.LoggedUserDTO" %>

<html>
<head>
    <title>StudentChat</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/student/signup.css">
</head>
<body>
    <h1 class = "title">Sign up to StudentChat</h1>
    
    <div id="maincontainer">
        <h2>Sign up today!</h2>
        <form name="try_signup" id="signupForm" method="post"
              action="${pageContext.request.contextPath}/student/signup">
            <div class="inputField">
                <label>Username:</label>
                <input type="text" name="username" placeholder="username" required />
            </div>
            <div class="inputField">
                <label>Password:</label>
                <input type="password" name="password" placeholder="password" required />
            </div>
            <div class="inputField">
                <label>email:</label>
                <input type="text" name="email" placeholder="email" required />
            </div>
            <div class="inputField">
                <label>name:</label>
                <input type="text" name="name" placeholder="name" required />
            </div>
            <div class="inputField">
                <label>surname:</label>
                <input type="text" name="surname" placeholder="surname" required />
            </div>
            <div class="inputField">
                <label>degree:</label>
                <input type="text" name="degree" placeholder="degree" required />
            </div>
            <div class="inputField">
                <label>language:</label>
                <input type="text" name="language" placeholder="language" required />
            </div>
            <button type="submit" class="buttonClass" id="submit">Join us!</button>
            <button class="buttonClass" onclick="location.href = '${pageContext.request.contextPath}/'">
                You already own an account? Login!
            </button>
        </form>
    </div>
    <script>
    <%
    // Check if something went wrong during login
    String rParam = request.getParameter("r");
    if (rParam != null && rParam.equals("error")) {
    %>
        alert("Error during signup!");
    <%
    }
		%>
    </script>
    <%
    // Check if user is already logged in, then redirect it to its portal page
    LoggedUserDTO loggedUser = AccessController.getLoggedUser(request);
    if (loggedUser != null) {
        ClientRedirector.redirectToPortalPage(request, response, loggedUser.getRole());
        return;
    }
    %>

</body>
</html>
