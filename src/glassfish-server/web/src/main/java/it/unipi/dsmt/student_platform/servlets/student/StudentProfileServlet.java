package it.unipi.dsmt.student_platform.servlets.student;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.dto.StudentBookedMeetingDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.BookedMeetingEJB;
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

@WebServlet(name = "StudentProfileServlet", value = "/student/profile")
public class StudentProfileServlet extends HttpServlet {

	@EJB
	private BookedMeetingEJB bookedMeetingEJB;

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

		List<StudentBookedMeetingDTO> bookedMeeting;

		LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
		if (loggedUserDTO == null) {
			return;
		}

		// check if delete has been made, and in the case check the response
		if(deleteAlert != null){
			if(deleteAlert == Boolean.TRUE){
				request.setAttribute("delete_ack", "ok");
			}
			else{
				request.setAttribute("delete_ack", "error");
			}
		}

		bookedMeeting = bookedMeetingEJB.getStudentMeetings(loggedUserDTO.getId());
		request.setAttribute("booked-meeting", bookedMeeting);

		String targetJSP = "/WEB-INF/jsp/student/profile.jsp";
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

		if (AccessController.checkAccess(request, response, UserRole.student) == null) {
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

		if (AccessController.checkAccess(request, response, UserRole.student) == null) {
			return;
		}

		Boolean successful = null;

		try{
			successful = bookedMeetingEJB.removeSlot(request.getParameter("meeting_id"));
		}
		catch(Exception error){
			error.printStackTrace();
		}

		handleRequest(request, response, successful);
	}

}