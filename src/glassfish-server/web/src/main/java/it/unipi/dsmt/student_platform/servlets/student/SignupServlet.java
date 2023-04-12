package it.unipi.dsmt.student_platform.servlets.student;

import it.unipi.dsmt.student_platform.dto.SignupDTO;
import it.unipi.dsmt.student_platform.interfaces.SignupEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import java.io.IOException;


@WebServlet(name = "SignUpServlet", value="/student/signup")
public class SignupServlet extends HttpServlet {
	@EJB
	private SignupEJB signupEJB;
	
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws IOException {
		String _username = request.getParameter("username");
		String _password = request.getParameter("password");
		String _email = request.getParameter("email");
		String _name = request.getParameter("name");
		String _surname = request.getParameter("surname");
		String _degree = request.getParameter("degree");
		String _language = request.getParameter("language");
		
		boolean r = false;
		
		SignupDTO dto = new SignupDTO(
				_username,
				_password,
				_email,
				_name,
				_surname,
				_degree,
				_language);
		
		
		//Some SQL call to store the data (remember to hash the pwd (?))
		try{
			r = signupEJB.signup(dto);
		}
		catch(Exception error){
			error.printStackTrace();
		}
		//Let's proceed to login page if signup succeeded, otherwise show an error
		if(!r) {
			response.sendRedirect(request.getContextPath() + "/student/signup?r=error");
			return;
		}
		response.sendRedirect(request.getContextPath() + "/");
	}
	
	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.getRequestDispatcher("/WEB-INF/jsp/student/signup.jsp")
				.forward(request, response);
	}
}
