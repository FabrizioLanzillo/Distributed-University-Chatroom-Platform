package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.CourseDTO;
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
import java.util.ArrayList;
import java.util.List;

@WebServlet(name = "ProfessorDeleteCourseServlet", value = "/professor/delete-course")
public class ProfessorDeleteCourseServlet extends HttpServlet {

	@EJB
	private CourseEJB courseEJB;

	private void handleRequest(HttpServletRequest request, HttpServletResponse response, int deleteAlert) throws ServletException, IOException {

		List<CourseDTO> courses;

		// check if a search has been made, and in the case load of the searched courses
		String searchInput = request.getParameter("search_input");
		if (searchInput != null) {
			request.setAttribute("search_input", searchInput);
			courses = courseEJB.getCourse(searchInput);
		}
		// load of all the courses
		else {
			courses = courseEJB.getAllCourses();
		}
		// passing the courses via get, with the set of an attribute
		request.setAttribute("courses", courses);

		// check if a delete has been made, and in the case check the response
		if(deleteAlert != -1){
			if(deleteAlert == 1){
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
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		if (!AccessController.checkAccess(request, response, UserRole.professor)) {
			return;
		}
		handleRequest(request, response, -1);
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		if (!AccessController.checkAccess(request, response, UserRole.professor)) {
			return;
		}

		String _id = request.getParameter("courseId");
		int id = Integer.parseInt(_id);

		int queryError = -1;

		try{
			queryError = courseEJB.deleteCourse(id);
		}
		catch(Exception error){
			error.printStackTrace();
		}

		handleRequest(request, response, queryError);
	}
}