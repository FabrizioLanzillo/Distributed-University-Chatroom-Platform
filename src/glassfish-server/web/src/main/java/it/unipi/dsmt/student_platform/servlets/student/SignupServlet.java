package it.unipi.dsmt.student_platform.servlets.student;

import it.unipi.dsmt.student_platform.dto.SignupDTO;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import java.io.IOException;

@WebServlet(name = "SignUpServlet", value="/student/signup")
public class SignupServlet extends HttpServlet {
	@EJB
	private UserEJB userEJB;
	
	/**
	 * Redefinition of doPost method for signup page. Accessed when the user clicks on the signup button after
	 * inserting the data in the form.
	 *
	 * @param request
	 * @param response
	 * @throws IOException
	 */
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response)
			throws IOException {
		
		//Extracting the data from the form
		String _username = request.getParameter("username");
		String _password = request.getParameter("password");
		String _email = request.getParameter("email");
		String _name = request.getParameter("name");
		String _surname = request.getParameter("surname");
		String _degree = request.getParameter("degree");
		String _language = request.getParameter("language");
		
		boolean r = false;
		// Creating a new signup object
		SignupDTO dto = new SignupDTO(
				_username,
				_password,
				_email,
				_name,
				_surname,
				_degree,
				_language);
		
		// TODO Hash & salt the password
		// Try to store the data, if the entry is duplicated (same username) the query will fail
		r = userEJB.signup(dto);

		
		//Let's proceed to login page if signup succeeded, otherwise show an error
		if(!r) {
			response.sendRedirect(request.getContextPath() + "/student/signup?r=error");
			return;
		}
		response.sendRedirect(request.getContextPath() + "/");
	}
	
	/***
	 * Redefinition of doGet method for signup page. Accessed when the user access it from the login page.
	 * @param request
	 * @param response
	 * @throws ServletException
	 * @throws IOException
	 */
	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		request.getRequestDispatcher("/WEB-INF/jsp/student/signup.jsp")
				.forward(request, response);
	}
}
