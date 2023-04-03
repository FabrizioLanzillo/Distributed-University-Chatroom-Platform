package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.dto.MinimalCourseDTO;
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

@WebServlet(name = "AdminPortalServlet", value = "/admin/portal")
public class AdminPortalServlet extends HttpServlet {

	private void handleRequest(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		String targetJSP = "/WEB-INF/jsp/admin/portal.jsp";
		RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
		requestDispatcher.forward(request, response);
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		if (AccessController.checkAccess(request, response, UserRole.admin) == null) {
			return;
		}
		handleRequest(request, response);
	}
}

