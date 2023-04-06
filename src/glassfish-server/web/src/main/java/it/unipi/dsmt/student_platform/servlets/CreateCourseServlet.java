package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.CourseCreationDTO;
import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.CourseEJB;
import it.unipi.dsmt.student_platform.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

/**
 * Servlet handling GET and POST requests for the webpage for creating a new course.
 */
@WebServlet(name = "CreateCourseServlet", value = "/professor/create-course")
public class CreateCourseServlet extends HttpServlet {
	
	public static final String attributeSuccessfulCreation = "successful-creation";
	
	@EJB
	CourseEJB courseEJB;
	
	/**
	 * Forward request to JSP page
	 * @param request HttpServletRequest object
	 * @param response HttpServletResponse object
	 * @throws ServletException if forwarding fails
	 * @throws IOException if forwarding fails
	 */
	private void redirectToJsp (HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		request.getRequestDispatcher("/WEB-INF/jsp/professor/create-course.jsp")
				.forward(request, response);
	}
	
	/**
	 * Check user privileges and forward request to JSP page.
	 * @param request HttpServletRequest object
	 * @param response HttpServletResponse object
	 * @throws ServletException if forwarding fails
	 * @throws IOException if forwarding fails
	 */
	@Override
	public void doGet (HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		// Check if logged user is a professor
		if (AccessController.checkAccess(request, response, UserRole.professor) == null) {
			return;
		}
		redirectToJsp(request, response);
	}
	
	/**
	 * Handle a POST request, i.e. create a new course.
	 * @param request HttpServletRequest object
	 * @param response HttpServletResponse object
	 * @throws ServletException if forwarding fails
	 * @throws IOException if forwarding or redirection fails
	 */
	@Override
	public void doPost (HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		// Check if logged user is a professor
		LoggedUserDTO loggedUser = AccessController.checkAccess(request, response, UserRole.professor);
		if (loggedUser == null) {
			return;
		}
		
		// Fetch POST parameters
		String _name = request.getParameter("name");
		String _description = request.getParameter("description");
		if (_name == null || _description == null) {
			request.setAttribute(attributeSuccessfulCreation, false);
			redirectToJsp(request, response);
			return;
		}
		String _professorId = loggedUser.getId();
		if (_professorId == null) {
			request.setAttribute(attributeSuccessfulCreation, false);
			redirectToJsp(request, response);
			return;
		}
		
		// Create course
		boolean successful = courseEJB.createCourse(
				new CourseCreationDTO(
						_name,
						_professorId,
						_description
				)
		);
		
		// Return to jsp page and notify operation result
		request.setAttribute(attributeSuccessfulCreation, successful);
		redirectToJsp(request, response);
	}
	
}
