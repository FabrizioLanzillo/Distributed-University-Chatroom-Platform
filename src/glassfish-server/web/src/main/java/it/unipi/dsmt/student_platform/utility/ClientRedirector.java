package it.unipi.dsmt.student_platform.utility;

import it.unipi.dsmt.student_platform.enums.UserRole;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

/**
 * Static class providing methods to redirect users' browsers to specific locations.
 */
public class ClientRedirector {
	
	/**
	 * Redirects the user to its portal page.
	 * @param request HttpServletRequest instance
	 * @param response HttpServletResponse instance
	 * @param role user role
	 * @throws IOException if redirection fails
	 */
	public static void redirectToPortalPage (@NotNull HttpServletRequest request,
	                                         @NotNull HttpServletResponse response,
	                                         @NotNull UserRole role) throws IOException
	{
		switch (role) {
			case student:
				response.sendRedirect(request.getContextPath() + "/student/portal");
				return;
			case professor:
				response.sendRedirect(request.getContextPath() + "/professor/portal");
				return;
			case admin:
				response.sendRedirect(request.getContextPath() + "/admin/portal");
				return;
			default:
				response.sendRedirect(request.getContextPath() + "/?r=error");
		}
	}
	
	/**
	 * Redirects the user to the login page.
	 * @param request HttpServletRequest instance
	 * @param response HttpServletResponse instance
	 * @throws IOException if redirection fails
	 */
	public static void redirectToLogin (@NotNull HttpServletRequest request,
	                                    @NotNull HttpServletResponse response) throws IOException
	{
		response.sendRedirect(request.getContextPath() + "/");
	}
	
}
