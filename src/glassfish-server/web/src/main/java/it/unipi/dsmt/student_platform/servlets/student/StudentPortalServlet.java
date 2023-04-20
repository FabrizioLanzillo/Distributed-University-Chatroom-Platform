package it.unipi.dsmt.student_platform.servlets.student;

import it.unipi.dsmt.student_platform.dto.MinimalCourseDTO;
import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
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
import java.util.List;

@WebServlet(name = "StudentPortalServlet", value = "/student/portal")
public class StudentPortalServlet extends HttpServlet {

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
	private void handleRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		List<MinimalCourseDTO> courses;

		LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
		if (loggedUserDTO == null) {
			return;
		}
		String starred =  request.getParameter("starred");
		String searchInput = request.getParameter("search_input");

		if (searchInput != null) {
			request.setAttribute("search_input", searchInput);
			courses = courseEJB.searchCourses(searchInput);
		}
		else if ((starred != null) && (starred.equals("true"))) {
			courses = courseEJB.getStarredCourses(loggedUserDTO.getId());
			request.setAttribute("starred", starred);
		}
		else {
			courses = courseEJB.getAllCourses();
		}
		request.setAttribute("courses", courses);

		String targetJSP = "/WEB-INF/jsp/student/portal.jsp";
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
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		if (AccessController.checkAccess(request, response, UserRole.student) == null) {
			return;
		}
		handleRequest(request, response);
	}
}
