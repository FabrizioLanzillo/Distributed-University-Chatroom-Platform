package it.unipi.dsmt.student_platform.utility;

import it.unipi.dsmt.student_platform.enums.UserRole;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;

public class UserRedirection {
	public static void redirectUser (HttpServletRequest request,
	                                 HttpServletResponse response,
	                                 UserRole role) throws IOException 	{
		switch (role) {
			case student:
				response.sendRedirect(request.getContextPath() + "/student/portal.jsp");
				return;
			case professor:
				response.sendRedirect(request.getContextPath() + "/professor/portal.jsp");
				return;
			case admin:
				response.sendRedirect(request.getContextPath() + "/admin/portal.jsp");
				return;
			default:
				response.sendRedirect(request.getContextPath() + "/index.jsp?r=error");
		}
	}
}
