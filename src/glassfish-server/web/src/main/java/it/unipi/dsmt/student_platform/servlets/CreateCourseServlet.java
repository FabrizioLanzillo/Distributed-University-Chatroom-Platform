package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.CourseCreationDTO;
import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
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
		String _description = request.getParameter("description");
		LoggedUserDTO loggedUser = (LoggedUserDTO) request.getSession().getAttribute("logged_user");
		if (_name == null || _description == null || loggedUser == null) {
			request.setAttribute("successful-creation", false);
			redirectToJsp(request, response);
			return;
		}
		String _professorId = loggedUser.getId();
		if (_professorId == null) {
			request.setAttribute("successful-creation", false);
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
		request.setAttribute("successful-creation", successful);
		redirectToJsp(request, response);
	}
	
}
