package it.unipi.dsmt.student_platform.servlets.admin;

import it.unipi.dsmt.student_platform.dto.CreateProfessorDTO;
import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
import it.unipi.dsmt.student_platform.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.RequestDispatcher;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

@WebServlet(name = "AdminCreateProfessorServlet", value="/admin/create-professor")
public class AdminCreateProfessorServlet extends HttpServlet {

	@EJB
	private UserEJB userEJB;
	private void handleRequest(HttpServletRequest request, HttpServletResponse response, Boolean insertAlert) throws ServletException, IOException {

		LoggedUserDTO loggedUserDTO = AccessController.getLoggedUserWithRedirect(request, response);
		if (loggedUserDTO == null) {
			return;
		}

		if(insertAlert != null){
			if(insertAlert == Boolean.TRUE){
				request.setAttribute("insertAck", "ok");
			}
			else{
				request.setAttribute("insertAck", "error");
			}
		}

		String targetJSP = "/WEB-INF/jsp/admin/create-professor.jsp";
		RequestDispatcher requestDispatcher = request.getRequestDispatcher(targetJSP);
		requestDispatcher.forward(request, response);
	}
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		if (AccessController.checkAccess(request, response, UserRole.admin) == null) {
			return;
		}
		handleRequest(request, response, null);
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

		if (AccessController.checkAccess(request, response, UserRole.admin) == null) {
			return;
		}

		CreateProfessorDTO createProfessorDTO = new CreateProfessorDTO(
			request.getParameter("username"),
			request.getParameter("password"),
			request.getParameter("email"),
			request.getParameter("name"),
			request.getParameter("surname")
		);

		Boolean successful =  userEJB.createProfessorAccount(createProfessorDTO);
		handleRequest(request, response, successful);
	}
}
