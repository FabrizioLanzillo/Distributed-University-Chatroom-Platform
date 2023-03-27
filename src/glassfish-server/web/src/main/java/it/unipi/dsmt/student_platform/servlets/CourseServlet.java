package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.CourseDTO;
import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.CourseEJB;
import it.unipi.dsmt.student_platform.utility.AccessController;
import it.unipi.dsmt.student_platform.utility.ClientRedirector;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.util.Optional;

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
	
	
	@Override
	protected void doPost (HttpServletRequest request, HttpServletResponse response)
			throws IOException
	{
		// Check if logged user is a student
        if (!AccessController.checkAccess(request, response, UserRole.student)) {
            return;
        }
        
		// Get action from POST parameters
        String action = Optional.ofNullable(request.getParameter("action"))
		        .orElse("");
		
		// Get course id from POST parameters
		int courseId;
		try {
			String stringId = request.getParameter("courseId");
			if (stringId == null) {
				throw new RuntimeException("course id not set");
			}
			courseId = Integer.parseInt(stringId);
		} catch (NumberFormatException e) {
			throw new RuntimeException("course id is not a number");
		}
		
		// Get currently logged user
		LoggedUserDTO loggedUser = (LoggedUserDTO) request.getSession().getAttribute("logged_user");
		if (loggedUser == null) {
			// User not logged
			ClientRedirector.redirectToLogin(request, response);
			return;
        }
		
		// Execute requested action (add star or remove star for course)
		boolean result;
		switch (action) {
			case "star":
				result = courseEJB.addStarredCourse(loggedUser.getId(), courseId);
                break;
            case "unstar":
				result = courseEJB.removeStarredCourse(loggedUser.getId(), courseId);
				break;
			default:
				throw new RuntimeException("unknown action");
		}
		
		// Check if query was successful
		if (!result) {
			throw new RuntimeException("failed query");
		}
	}
	
}

