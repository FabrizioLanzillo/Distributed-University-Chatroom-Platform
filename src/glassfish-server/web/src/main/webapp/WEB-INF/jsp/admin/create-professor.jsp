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

    //	get the result of the delete operation, if it has been made
    String professorAccountCreationACK = "";
    if(request.getAttribute("insertAck") != null){
        professorAccountCreationACK = request.getAttribute("insertAck").toString();
    }
%>

    <head>
        <title>Admin Create Professor Account</title>
    </head>
    <body>
        <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

        <script>
<%
            // check on the result of the delete operation, if it has been made
            if(!professorAccountCreationACK.equals("")){
                if(professorAccountCreationACK.equals("ok")){
%>
                    alert("Professor Account correctly created");
<%
                }
                else{
%>
                    alert("An error occurred while creating the professor account");
<%
                }
                request.removeAttribute("insertAck");
            }
%>
        </script>

        <h1>
            Welcome to the professor account creation page, <%= loggedUserDTO.getUsername() %>
        </h1>

        <form name="create_professor_account" method="post"
              action="${pageContext.request.contextPath}/admin/create-professor">

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
                email:
                <input type="text" name="email" placeholder="email" required />
            </label>
            <br>
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
            <button type="submit">Create Account!</button>
        </form>

    </body>
</html>