package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.CourseDTO;
import it.unipi.dsmt.student_platform.interfaces.CourseEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.RequestDispatcher;
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
		// Get course id from GET parameters
		String id = request.getParameter("id");
		if (id == null) {
			throw new RuntimeException("course id not set");
		}
		
		// Get course's data
		CourseDTO course = courseEJB.getCourse(id);
		request.setAttribute("course", course);
		RequestDispatcher requestDispatcher = request.getRequestDispatcher("/WEB-INF/jsp/student/course.jsp");
		requestDispatcher.forward(request, response);
	}
	
}

