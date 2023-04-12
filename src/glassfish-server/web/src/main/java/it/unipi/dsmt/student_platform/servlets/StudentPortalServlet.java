package it.unipi.dsmt.student_platform.servlets;

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
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		if (AccessController.checkAccess(request, response, UserRole.student) == null) {
			return;
		}
		handleRequest(request, response);
	}
}
