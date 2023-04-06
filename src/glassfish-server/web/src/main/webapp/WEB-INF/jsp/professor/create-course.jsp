<%--
    Website page for creating a new course.
--%>

<html>

<head>
    <title>Create course</title>
</head>

<body>
    <jsp:include page="/WEB-INF/jsp/common/top-bar.jsp" />

    <h1>Create a course</h1>

<%
    Boolean successful = (Boolean) request.getAttribute("successful-creation");
	if (successful == Boolean.TRUE) {
%>
    <div>
        Course created with success!
    </div>
<%
    }
	else if (successful == Boolean.FALSE) {
%>
    <div>
        Course creation failed...
    </div>
<%
    }
%>

    <div class="form">
        <form method="post" action="${pageContext.request.contextPath}/professor/create-course">
            <label>
                Name of the course:
                <input type="text" name="name" required />
            </label>
            <br>
            <label>
                Description:
                <textarea name="description" required></textarea>
            </label>
            <br>
            <button type="submit">CREATE</button>
        </form>
    </div>

</body>
</html>