package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.CourseEJB;
import it.unipi.dsmt.student_platform.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

/**
 * Servlet which redirects the user to the chatroom JSP page.
 */
@WebServlet(name = "ChatroomServlet", value = "/student/chatroom")
public class ChatroomServlet extends HttpServlet {

	@EJB
	private CourseEJB courseEJB;

	/**
	 * function invoked by get and post request to handle them
	 * in order to retrieve and load the data of the page
	 * @param request HttpServletRequest object
	 * @param response HttpServletRequest object
	 * @throws ServletException if forwarding fails
	 * @throws IOException if forwarding fails
	 */
	private void handleRequest(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		int courseId =  Integer.parseInt(request.getParameter("id"));
		String courseName = courseEJB.getCourseName(courseId);

		if (courseName == null) {
			throw new RuntimeException(String.format("Course %d does not exist", courseId));
		}

		request.setAttribute("course_name", courseName);

		String targetJSP = "/WEB-INF/jsp/student/chatroom.jsp";
		RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
		requestDispatcher.forward(request, response);
	}

	/**
	 * Redefinition of the doGet, through the handleRequest invocation
	 * @param request HttpServletRequest object
	 * @param response HttpServletRequest object
	 * @throws ServletException if forwarding fails
	 * @throws IOException if forwarding fails
	 */
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {

		if (AccessController.checkAccess(request, response, UserRole.student) == null) {
			return;
		}

		handleRequest(request, response);
	}
	
}
