package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.CourseDTO;
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

@WebServlet(name = "CourseServlet", value = "/student/course")
public class CourseServlet extends HttpServlet {
	
	@EJB
	CourseEJB courseEJB;
	
	@Override
	protected void doGet (HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		// Check if logged user is a student
		if (!AccessController.checkAccess(request, response, UserRole.student)) {
			return;
		}
		
		int id;
		try {
			// Get course id from GET parameters
			String stringId = request.getParameter("id");
			if (stringId == null) {
				throw new RuntimeException("course id not set");
			}
			id = Integer.parseInt(stringId);
		} catch (NumberFormatException e) {
			throw new RuntimeException("course id is not a number");
		}
		
		// Get course's data
		CourseDTO course = courseEJB.getCourse(id);
		request.setAttribute("course", course);
		request.getRequestDispatcher("/WEB-INF/jsp/student/course.jsp")
				.forward(request, response);
	}
	
}

