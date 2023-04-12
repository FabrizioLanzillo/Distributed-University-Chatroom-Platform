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
 * Servlet which redirects the user to the chatroom JSP page.
 */
@WebServlet(name = "ChatroomServlet", value = "/student/chatroom")
public class ChatroomServlet extends HttpServlet {
	
	public void doGet (HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		if (AccessController.checkAccess(request, response, UserRole.student) == null) {
			return;
		}
		
		// TODO ? get information about chatroom
		// TODO check if course exists
		
		// Redirect to portal jsp page
		request.getRequestDispatcher("/WEB-INF/jsp/student/chatroom.jsp")
				.forward(request, response);
	}
	
}
