package it.unipi.dsmt.student_platform.servlets.admin;

import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.utility.AccessController;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

@WebServlet(name = "AdminPortalServlet", value = "/admin/portal")
public class AdminPortalServlet extends HttpServlet {

	/**
	 * function invoked by get and post request to handle them
	 * in order to retrieve and load the data of the page
	 * @param request HttpServletRequest object
	 * @param response HttpServletRequest object
	 * @throws ServletException if forwarding fails
	 * @throws IOException if forwarding fails
	 */
	private void handleRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		String targetJSP = "/WEB-INF/jsp/admin/portal.jsp";
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

		if (AccessController.checkAccess(request, response, UserRole.admin) == null) {
			return;
		}
		handleRequest(request, response);
	}
}

