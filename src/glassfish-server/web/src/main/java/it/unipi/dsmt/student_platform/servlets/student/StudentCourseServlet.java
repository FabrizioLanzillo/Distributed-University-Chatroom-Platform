package it.unipi.dsmt.student_platform.servlets.student;

import it.unipi.dsmt.student_platform.dto.CourseDTO;
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
import java.util.Optional;

/**
 * Servlet handling GET and POST request for the webpage
 * which shows the details of a course.
 */
@WebServlet(name = "StudentCourseServlet", value = "/student/course")
public class StudentCourseServlet extends HttpServlet {
	
	@EJB
	CourseEJB courseEJB;
	
	/**
	 * Fetch course's data from database and dispatch request to JSP page.
	 * @param request HttpServletRequest object
	 * @param response HttpServletResponse object
	 * @throws ServletException if page forwarding fails
	 * @throws IOException if redirection or page forwarding fails
	 */
	@Override
	protected void doGet (HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		// Check if logged user is a student
		if (AccessController.checkAccess(request, response, UserRole.student) == null) {
			return;
		}
		
		// Get course id from GET parameters and convert it to integer
		int id;
		try {
			String stringId = request.getParameter("id");
			if (stringId == null) {
				throw new RuntimeException("course id not set");
			}
			id = Integer.parseInt(stringId);
		} catch (NumberFormatException e) {
			throw new RuntimeException("course id is not a number");
		}
		
		// Get currently logged user
		LoggedUserDTO loggedUser = AccessController.getLoggedUserWithRedirect(request, response);
		if (loggedUser == null) {
			// User not logged
			return;
		}
		
		// Get course's data and dispatch request to JSP page
		CourseDTO course = courseEJB.getCourseDetails(id, loggedUser.getId());
		request.setAttribute("course", course);
		request.getRequestDispatcher("/WEB-INF/jsp/student/course.jsp")
				.forward(request, response);
	}
	
	
	/**
	 * Handles a POST request, i.e. the user asked to star/unstar the course
	 * @param request HttpServletRequest instance
	 * @param response HttpServletResponse instance
	 * @throws IOException if redirection fails
	 */
	@Override
	protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException {
		// Check if logged user is a student
        if (AccessController.checkAccess(request, response, UserRole.student) == null) {
            return;
        }
        
		// Get action type from POST parameters
        String action = Optional.ofNullable(request.getParameter("action"))
		        .orElse("");
		
		// Get course id from POST parameters and parse it to integer
		int courseId;
		try {
			String stringId = request.getParameter("courseId");
			if (stringId == null) {
				throw new RuntimeException("course id not set");
			}
			courseId = Integer.parseInt(stringId);
		} catch (NumberFormatException e) {
			throw new RuntimeException("course id is not a number");
		}
		
		// Get currently logged user
		LoggedUserDTO loggedUser = AccessController.getLoggedUserWithRedirect(request, response);
		if (loggedUser == null) {
			// User not logged
			return;
        }
		
		// Execute requested action (add star or remove star for course)
		boolean result;
		switch (action) {
			case "star":
				result = courseEJB.addStarredCourse(loggedUser.getId(), courseId);
                break;
            case "unstar":
				result = courseEJB.removeStarredCourse(loggedUser.getId(), courseId);
				break;
			default:
				throw new RuntimeException("unknown action");
		}
		
		// Check if query was successful
		if (!result) {
			throw new RuntimeException("failed query");
		}
	}
	
}
