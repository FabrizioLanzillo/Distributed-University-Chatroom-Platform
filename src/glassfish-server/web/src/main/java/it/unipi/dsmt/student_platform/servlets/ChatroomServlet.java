package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.dto.MinimalCourseDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.utility.AccessController;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.List;

/**
 * Servlet which redirects the user to the chatroom JSP page.
 */
@WebServlet(name = "ChatroomServlet", value = "/student/chatroom")
public class ChatroomServlet extends HttpServlet {

	private void handleRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {


		int courseId =  Integer.parseInt(request.getParameter("id"));
		String courseName = "corso pippo";

		request.setAttribute("course_name", courseName);

		String targetJSP = "/WEB-INF/jsp/student/chatroom.jsp";
		RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
		requestDispatcher.forward(request, response);
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		if (AccessController.checkAccess(request, response, UserRole.student) == null) {
			return;
		}

		// TODO ? get information about chatroom
		// TODO check if course exists

		handleRequest(request, response);
	}
	
}
