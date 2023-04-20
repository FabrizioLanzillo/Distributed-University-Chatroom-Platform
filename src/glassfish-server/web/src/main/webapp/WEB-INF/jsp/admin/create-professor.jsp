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
        <link rel="stylesheet" type="text/css" href="${pageContext.request.contextPath}/assets/css/admin/create-professor.css">
    </head>
    <body>
        <div id="create-professor-page">
            <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />
            <br>
            <h1>
                Welcome to the professor creation page
            </h1>
            <br>
            <div id="form-container">
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

                <form class="professor-creation" name="create_professor_account" method="post"
                      action="${pageContext.request.contextPath}/admin/create-professor">

                    <div class="insert-field">
                        <label for="name">Name:</label>
                        <input type="text" id="name" class="input-field" name="name" placeholder="Insert the Name" required />
                    </div>

                    <div class="insert-field">
                        <label for="surname">Surname:</label>
                        <input type="text" id="surname" class="input-field" name="surname" placeholder="Insert the Surname" required />
                    </div>

                    <div class="insert-field">
                        <label for="email">Email:</label>
                        <input type="text" id="email" class="input-field" name="email" placeholder="Insert the Email" required />
                    </div>

                    <div class="insert-field">
                        <label for="username">Username:</label>
                        <input type="text" id="username" class="input-field" name="username" placeholder="Insert the Username" required />
                    </div>

                    <div class="insert-field">
                        <label for="password">Password:</label>
                        <input type="text" id="password" class="input-field" name="password" placeholder="Insert the Password" required />
                    </div>
                    <br>
                    <button type="submit" class="create_account_button">Create Account!</button>
                </form>
            </div>
        </div>
    </body>
</html>