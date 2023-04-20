package it.unipi.dsmt.student_platform.servlets.professor;

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
import java.time.LocalTime;

/**
 * Servlet handling GET and POST requests for the webpage for creating a new course.
 */
@WebServlet(name = "ProfessorCreateCourseServlet", value = "/professor/create-course")
public class ProfessorCreateCourseServlet extends HttpServlet {
	
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
	 * Notify the failure of the course creation to the user and forward the request to the JSP page.
	 * @param request HttpServletRequest instance
	 * @param response HttpServletResponse instance
	 * @throws ServletException if forwarding fails
	 * @throws IOException if forwarding fails
	 */
	private void notifyFailedCreation (HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		request.setAttribute(attributeSuccessfulCreation, false);
		redirectToJsp(request, response);
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
		
		// Get professor id
		String _professorId = loggedUser.getId();
		if (_professorId == null) {
			notifyFailedCreation(request, response);
			return;
		}
		
		// Fetch POST parameters
		String _name = request.getParameter("name");
		String _description = request.getParameter("description");
		String _weekdayString = request.getParameter("weekday");
		String _startTimeString = request.getParameter("start-time");
		String _endTimeString = request.getParameter("end-time");
		if (_name == null || _description == null || _weekdayString == null
				|| _startTimeString == null || _endTimeString == null)
		{
			notifyFailedCreation(request, response);
			return;
		}
		
		// Convert weekday to integer
		int _weekday = Integer.parseInt(_weekdayString);
		if (_weekday < 1 || _weekday > 5) { // from Monday to Friday
			// Not valid weekday
			notifyFailedCreation(request, response);
			return;
		}
		
		// Convert start and end time
		LocalTime _startTime = LocalTime.parse(_startTimeString);
		LocalTime _endTime = LocalTime.parse(_endTimeString);
		if (!_startTime.isBefore(_endTime)) {
			// Inconsistent start and end time
			notifyFailedCreation(request, response);
			return;
		}
		
		
		// Create course
		boolean successful = courseEJB.createCourse(
				new CourseCreationDTO(
						_name,
						_professorId,
						_description,
						_weekday,
						_startTime,
						_endTime
				)
		);
		
		// Return to jsp page and notify operation result
		request.setAttribute(attributeSuccessfulCreation, successful);
		redirectToJsp(request, response);
	}
	
}
