package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.GeneralUserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
import it.unipi.dsmt.student_platform.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;

import java.io.IOException;
import java.util.ArrayList;


@WebServlet(name = "UsersServlet", value="/admin/users")
public class UsersServlet extends HttpServlet {

    @EJB
    private UserEJB userEJB;
    
    /**
     * Method accessed after a POST request in which the user tries to find a specific user by a string clicking "search"
     * button.
     * @param request
     * @param response
     */
    private void searchWithStringByOffset(HttpServletRequest request, HttpServletResponse response) {
        // Extract the research input and the user type to search
        String switch_ = request.getParameter("switch");
        String searchInput = request.getParameter("search_input");
        
        if(searchInput == null)
            searchInput = "";
        
        UserRole role_to_search;
    
        // Set up the role
        if(switch_ == null){
            String role = request.getParameter("search");
            if(role.equals("student"))
                role_to_search = UserRole.student;
            else
                role_to_search = UserRole.professor;
        }
        else if (switch_.equals("false"))
            role_to_search = UserRole.professor;
        else
            role_to_search = UserRole.student;
        
        // Update the page with requested "search input"
        try {
            update(request, response, role_to_search, searchInput);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    
    }
    
    /**
     * Method accessed after a POST request triggered by clicking on switch button. it switches showed results type
     * between "professor" and "student".
     * @param request
     * @param response
     */
    private void switchUserType(HttpServletRequest request, HttpServletResponse response) {
        String switch_ = request.getParameter("switch");
    
        UserRole role_to_search;
        
        // Set up new role
        if(switch_ == null){
            String role = request.getParameter("search");
            if(role.equals("student"))
                role_to_search = UserRole.student;
            else
                role_to_search = UserRole.professor;
        }
        else if (switch_.equals("false"))
            role_to_search = UserRole.professor;
        else
            role_to_search = UserRole.student;
    
        // Update the page with requested results, in this case search input get reset
        try {
            update(request, response, role_to_search, "");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    
    }
    
    /**
     * method accessed after a POST request triggered by clicking on user button in the list.
     * It manages admin ban requests
     * @param request
     * @param response
     */
    private void deleteUser(HttpServletRequest request, HttpServletResponse response){
        // Get the role of the user to be banned
        String switch_ = request.getParameter("switch");
        UserRole role_to_search;
        
        if(switch_ == null){
            String role_ = request.getParameter("search");
            if(role_.equals("student"))
                role_to_search = UserRole.student;
            else
                role_to_search = UserRole.professor;
        }
        else if (switch_.equals("false"))
            role_to_search = UserRole.professor;
        else
            role_to_search = UserRole.student;
        
        // Get user ID
        String button = request.getParameter("userButton");
        String userID = button.replace("button", "");

        // Perform DELETE query
        boolean result = false;
        if(!userID.equals("")){
            result = userEJB.banUser(userID, role_to_search);
        }
        
        // Check result and show to the user
        if(result)
            request.setAttribute("banACK", "ok");
        else{
            request.setAttribute("banACK", "error");
        }
        
        // Refresh the page to default state
        try {
            update(request, response, role_to_search, "");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    
    }
    
    /**
     * Generic method invoked after a GET or POST request. It initializes the page with default values if called by the
     * GET otherwise it allow to specify parameters of the user research, i.e. specific offset, search input or role
     * @param request
     * @param response
     * @param role
     * @param searchInput
     * @throws IOException
     * @throws ServletException
     */
    private void update (HttpServletRequest request, HttpServletResponse response, UserRole role, String searchInput) throws IOException, ServletException {
        // Extract offset
        int offset =  request.getParameter("offset")==null ? 0 : Integer.parseInt(request.getParameter("offset"));
        request.setAttribute("offset", offset);
        
        // Get requested user list
        ArrayList<GeneralUserDTO> users = (ArrayList<GeneralUserDTO>) userEJB.searchUsers(searchInput, role, offset);
        request.setAttribute("userList", users);
        
        // Set researched user role
        if(role == UserRole.student) {
            request.setAttribute("role_searched", UserRole.student);
        }
        else if(role == UserRole.professor) {
            request.setAttribute("role_searched", UserRole.professor);
        }
    
        //Forward response
        request.getRequestDispatcher("/WEB-INF/jsp/admin/users.jsp")
                .forward(request, response);
    
    }
    
    /**
     * Redefinition of doGET
     * @param request
     * @param response
     * @throws IOException
     * @throws ServletException
     */
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        if (AccessController.checkAccess(request, response, UserRole.admin) == null) {
            return;
        }
        // Load page with default values
        update(request, response, UserRole.student, "");
        
    }
    
    
    /**
     * Redefinition of doPOST. Select correct request and redirect to the corresponding handle function.
     * @param request
     * @param response
     * @throws IOException
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        
        if (AccessController.checkAccess(request, response, UserRole.admin) == null) {
            return;
        }
        String action = request.getParameter("action");
        
        switch (action) {
            case "search":
            case "offsetChange":
                searchWithStringByOffset(request, response);
                break;
            case "delete":
                deleteUser(request, response);
                break;
            case "switch":
                switchUserType(request, response);
                break;
            
        }
    }
    
}
