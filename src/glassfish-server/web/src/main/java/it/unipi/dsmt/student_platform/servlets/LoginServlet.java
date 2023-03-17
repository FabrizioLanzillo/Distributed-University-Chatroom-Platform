package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.UserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
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
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
		// Extract parameters for login
		String username = Optional.ofNullable(request.getParameter("username"))
				.orElse("");
		String password = Optional.ofNullable(request.getParameter("password"))
				.orElse("");
		UserRole role;
		try {
			role = UserRole.valueOf(request.getParameter("role"));
		} catch (IllegalArgumentException exception) {
			role = UserRole.invalid;
		}
		
		// Execute login and set session variable
		UserDTO loggedUser = userEJB.login(username, password, role);
		request.getSession().setAttribute("logged_user", loggedUser);
		
		// Failed login
		if (loggedUser == null) {
			request.setAttribute("login_failed", true);
			request.getRequestDispatcher(request.getContextPath() + "/index.jsp")
					.forward(request, response);
			return;
		}
		
		// Successful login
		switch (role) {
			case student:
				response.sendRedirect(request.getContextPath() + "/student/portal.jsp");
				return;
			case professor:
				response.sendRedirect(request.getContextPath() + "/professor/portal.jsp");
				return;
			case admin:
				response.sendRedirect(request.getContextPath() + "/admin/portal.jsp");
				return;
			default:
				request.setAttribute("login_failed", true);
				request.getRequestDispatcher(request.getContextPath() + "/index.jsp")
						.forward(request, response);
		}
	}
}
