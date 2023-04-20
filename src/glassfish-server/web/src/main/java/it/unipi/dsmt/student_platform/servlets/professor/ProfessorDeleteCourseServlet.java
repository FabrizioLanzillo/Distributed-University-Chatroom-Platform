package it.unipi.dsmt.student_platform.servlets.professor;

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

@WebServlet(name = "ProfessorDeleteCourseServlet", value = "/professor/delete-course")
public class ProfessorDeleteCourseServlet extends HttpServlet {

	@EJB
	private CourseEJB courseEJB;

	/**
	 * function invoked by get and post request to handle them
	 * in order to retrieve and load the data of the page
	 * @param request HttpServletRequest object
	 * @param response HttpServletRequest object
	 * @param deleteAlert boolean variable to notify that delete has been made
	 * @throws ServletException if forwarding fails
	 * @throws IOException if forwarding fails
	 */
	private void handleRequest(HttpServletRequest request, HttpServletResponse response, Boolean deleteAlert) throws ServletException, IOException {

		LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
		if (loggedUserDTO == null) {
			return;
		}
		
		List<MinimalCourseDTO> courses;
		
		// check if a search has been made, and in the case load of the searched courses
		String searchInput = request.getParameter("search_input");
		if (searchInput != null) {
			request.setAttribute("search_input", searchInput);
			courses = courseEJB.searchCoursesForProfessor(searchInput, loggedUserDTO.getId());
		}
		// load of all the courses
		else {
			courses = courseEJB.getAllCoursesForProfessor(loggedUserDTO.getId());
		}
		// passing the courses via get, with the set of an attribute
		request.setAttribute("courses", courses);

		// check if delete has been made, and in the case check the response
		if(deleteAlert != null){
			if(deleteAlert == Boolean.TRUE){
				request.setAttribute("deleteAck", "ok");
			}
			else{
				request.setAttribute("deleteAck", "error");
			}
		}

		String targetJSP = "/WEB-INF/jsp/professor/delete-course.jsp";
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

		if (AccessController.checkAccess(request, response, UserRole.professor) == null) {
			return;
		}
		handleRequest(request, response, null);
	}

	/**
	 * Redefinition of the doPost, through the handleRequest invocation
	 * @param request HttpServletRequest object
	 * @param response HttpServletRequest object
	 * @throws ServletException if forwarding fails
	 * @throws IOException if forwarding fails
	 */
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		if (AccessController.checkAccess(request, response, UserRole.professor) == null) {
			return;
		}

		Boolean successful = null;

		try{
			successful =  courseEJB.deleteCourse(Integer.parseInt(request.getParameter("courseId")));
		}
		catch(Exception error){
			error.printStackTrace();
		}

		handleRequest(request, response, successful);
	}
}