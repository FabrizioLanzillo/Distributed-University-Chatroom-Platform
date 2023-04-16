package it.unipi.dsmt.student_platform.servlets.common;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.dto.LoginInformationDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
import it.unipi.dsmt.student_platform.utility.AccessController;
import it.unipi.dsmt.student_platform.utility.ClientRedirector;
import jakarta.ejb.EJB;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import java.io.IOException;
import java.util.Optional;

/**
 * Servlet handling POST requests for login.
 */
@WebServlet(name = "LoginServlet", value = "/login")
public class LoginServlet extends HttpServlet {
	
	public static final String errorAttribute = "error";
	
	@EJB
	private UserEJB userEJB;
	
	/**
	 * Handle a POST request by executing the login procedure.
	 * @param request HttpServletRequest instance
	 * @param response HttpServletResponse instance
	 * @throws IOException if redirection fails
	 */
	@Override
	protected void doPost (HttpServletRequest request, HttpServletResponse response) throws IOException	{
		// Extract parameters for login
		String username = Optional.ofNullable(request.getParameter("username"))
				.orElse("");
		String password = Optional.ofNullable(request.getParameter("password"))
				.orElse("");
		UserRole role = request.getParameter("role") != null
				? UserRole.valueOf(request.getParameter("role")) : UserRole.invalid;
		
		// Execute login and set session variable
		LoggedUserDTO loggedUser = userEJB.login(
				new LoginInformationDTO(
						username,
						password,
						role
				)
		);
		
		
		// a) Failed login
		if (loggedUser == null) {
			response.sendRedirect(request.getContextPath() + "/index.jsp?r=error");
			return;
		}
		
		// b) Successful login
		// Add user information as session variable
		AccessController.setLoggedUser(request, loggedUser);
		
		// Redirect to user's portal
		ClientRedirector.redirectToPortalPage(request, response, role);
	}
	
}
