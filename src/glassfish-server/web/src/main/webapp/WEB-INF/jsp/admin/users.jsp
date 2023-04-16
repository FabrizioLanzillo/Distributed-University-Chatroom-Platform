<%@ page import="java.util.ArrayList" %>
<%@ page import="it.unipi.dsmt.student_platform.dto.GeneralUserDTO" %>
<%@ page import="it.unipi.dsmt.student_platform.enums.UserRole" %>
<%@ page contentType="text/html;charset=UTF-8" %>

<%
    // Get the users list
    ArrayList<GeneralUserDTO> userList = (ArrayList<GeneralUserDTO>) request.getAttribute("userList");
    if(userList == null){
        userList = new ArrayList<>();
    }

	// Check if we've been redirected here after a ban request
    String banACK = "";
    if(request.getAttribute("banACK") != null){
        banACK = request.getAttribute("banACK").toString();
    }

	// Check if we've been redirected here after a search request
	UserRole role = (UserRole) request.getAttribute("role_searched");
	if(role == null){
        role = UserRole.student;
    }

	//Extract the offset otherwise initialize to 0 (Current month)
	int offset = request.getParameter("offset") == null? 0 : Integer.parseInt(request.getParameter("offset"));

%>

<html>
<head>
    <title>Admin search Portal</title>
    <link rel="stylesheet" href="${pageContext.request.contextPath}/assets/css/admin/users.css">
</head>
<body>
    <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

    <div id="mainContainer">
        <h1>Search for users:</h1>
        <div id="searchbar">
            <form id="inputForm" action="${pageContext.request.contextPath}/admin/users?action=search&search=<%=role.toString()%>" method="post">
                <input type="text" id="searchInput" name="search_input" placeholder="Type the username...">
                <button class = "searchButton" type="submit">Search</button>
            </form>

            <form name="action" action="${pageContext.request.contextPath}/admin/users?action=switch" method="post">
                <%
                    // Display role switch button coherently
                    if(role == UserRole.student){

                        %>
                <button class = "searchButton" id="switch" type="submit" name="switch" value="false">See professors</button>
                        <%
                    }
                    else{
                %>
                <button class = "searchButton" id="switch" type="submit" name="switch" value="true">See students</button>
                <%
                    }%>

            </form>
        </div>
        <form action="${pageContext.request.contextPath}/admin/users?action=delete&search=<%=role.toString()%>" method="post">
            <div id="usersDiv">
                <script>
                    const coursesDiv = document.getElementById("users");
                    coursesDiv.innerHTML = "";
                </script>
                <%
                    // Create a button for each user
                    int i=0;
                    for (GeneralUserDTO user : userList) {
                %>
                        <button class = "user" type="submit" name="userButton" value="<%=user.getId()%>">
                            <%= user.toString() %>
                        </button>
                <%
                        i++;
                    }
                %>
            </div>
        </form>
        <div id="offsetDiv">
            <form class="offsetForm" method="post" action="${pageContext.request.contextPath}/admin/users?action=offsetChange&offset=<%=offset - 1%>&search=<%=role.toString()%>">
                <%
                    // Create buttons to change the offset, backward one is available only if offset >0
                    if(offset <=0 ){%>
                <button class="offset" disabled="disabled"><-</button>
                <%}
                else{%>
                <button class="offset" type="submit"><-</button>
                <%}%>
            </form>
            <form class="offsetForm" method="post" action="${pageContext.request.contextPath}/admin/users?action=offsetChange&offset=<%=offset + 1%>&search=<%=role.toString()%>">
                <button class="offset" type="submit">-></button>
            </form>
        </div>
        <script>
        <%
        // check on the result of the delete operation, if it has been made
        if(!banACK.equals("")){
            if(banACK.equals("ok")){
            %>
                alert("user correctly deleted");
            <%
            }
            else{
            %>
                alert("An error occurred while user deletion");
            <%
            }
            request.removeAttribute("banACK");
        }
        %>
        </script>
    </div>
</body>
</html>