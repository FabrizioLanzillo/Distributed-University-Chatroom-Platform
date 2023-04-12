package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.utility.ClientRedirector;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

/**
 * Servlet which executes logout request.
 */
@WebServlet(name = "LogoutServlet", value = "/logout")
public class LogoutServlet extends HttpServlet {
	
	private void handleRequest(HttpServletRequest request, HttpServletResponse response) throws IOException {
		// Invalidate the session
		request.getSession().invalidate();
		// Redirect to login page
		ClientRedirector.redirectToLogin(request, response);
	}
	
	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
		handleRequest(request, response);
	}
	
	@Override
	public void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        handleRequest(request, response);
    }

}
