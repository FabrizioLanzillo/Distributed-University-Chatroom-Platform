package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.UserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
import it.unipi.dsmt.student_platform.utility.UserRedirection;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import java.io.IOException;
import java.util.Optional;

@WebServlet(name = "LoginServlet", value = "/login")
public class LoginServlet extends HttpServlet {
	
	@EJB
	private UserEJB userEJB;
	
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException	{
		// Extract parameters for login
		String username = Optional.ofNullable(request.getParameter("username"))
				.orElse("");
		String password = Optional.ofNullable(request.getParameter("password"))
				.orElse("");
		UserRole role = request.getParameter("role") != null
				? UserRole.valueOf(request.getParameter("role")) : UserRole.invalid;
		
		// Execute login and set session variable
		UserDTO loggedUser = userEJB.login(username, password, role);
		
		
		// a) Failed login
		if (loggedUser == null) {
			response.sendRedirect(request.getContextPath() + "/index.jsp?r=error");
			return;
		}
		
		// b) Successful login
		// Add user information as sessione variable
		request.getSession().setAttribute("logged_user", loggedUser);
		
		// Redirect to correct webpage
		UserRedirection.redirectUser(request, response, role);
	}
}
