package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.utility.AccessController;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

/**
 * Servlet handling GET and POST requests for the professor's portal webpage.
 */
@WebServlet(name = "ProfessorPortalServlet", value = "/professor/portal")
public class ProfessorPortalServlet extends HttpServlet {
	
	public void doGet (HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		// Check if logged user is a professor
		if (AccessController.checkAccess(request, response, UserRole.professor) == null) {
			return;
		}
		
		// Forward request to JSP page
		request.getRequestDispatcher("/WEB-INF/jsp/professor/portal.jsp")
				.forward(request, response);
	}
	
}
