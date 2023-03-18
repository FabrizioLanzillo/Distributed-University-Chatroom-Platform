package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.CourseCreationDTO;
import it.unipi.dsmt.student_platform.interfaces.CourseEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

@WebServlet(name = "CreateCourseServlet", value = "/professor/create-course")
public class CreateCourseServlet extends HttpServlet {
	
	@EJB
	CourseEJB courseEJB;
	
	private void redirectToJsp (HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		request.getRequestDispatcher("/WEB-INF/jsp/professor/create-course.jsp")
				.forward(request, response);
	}
	
	@Override
	public void doGet (HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		redirectToJsp(request, response);
	}
	
	@Override
	public void doPost (HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException
	{
		// Fetch GET parameters
		String _name = request.getParameter("name");
		String _professorId = request.getParameter("professorId");
		String _description = request.getParameter("description");
		if (_name == null || _professorId == null || _description == null) {
			request.setAttribute("create-successful", false);
			redirectToJsp(request, response);
			return;
		}
		
		// Create course
		boolean successful = courseEJB.createCourse(
				new CourseCreationDTO(
						_name,
						_professorId,
						_description
				)
		);
		
		// Return to jsp page and notify operation result
		request.setAttribute("create-successful", successful);
		redirectToJsp(request, response);
	}
	
}
