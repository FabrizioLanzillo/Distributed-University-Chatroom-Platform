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
     * Method that handles the POST request in which the user tries to find a specific user by a string.
     * @param request
     * @param response
     */
    private void searchWithString(HttpServletRequest request, HttpServletResponse response) {
        //
        String switch_ = request.getParameter("switch");
        String searchInput = request.getParameter("search_input");
    
        if(searchInput == null)
            searchInput = "";
    
        UserRole role_to_search;
    
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
        
        try {
            update(request, response, role_to_search, searchInput);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    
    }
    
    private void switchUserType(HttpServletRequest request, HttpServletResponse response) {
        String switch_ = request.getParameter("switch");
    
        UserRole role_to_search;
    
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
    
        try {
            update(request, response, role_to_search, "");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    
    }
    
    private void deleteUser(HttpServletRequest request, HttpServletResponse response){
        String switch_ = request.getParameter("switch");
        String searchInput = request.getParameter("search_input");
    
        if(searchInput == null)
            searchInput = "";
        
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
        
        String button = request.getParameter("userButton");
        String userID = button.replace("button", "");

        boolean result = false;
        if(!userID.equals("")){
            result = userEJB.banUser(userID, role_to_search);
        }
        
        if(result)
            request.setAttribute("banACK", "ok");
        else{
            request.setAttribute("banACK", "error");
        }
        
        try {
            update(request, response, role_to_search, "");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    
    }
    
    private void update (HttpServletRequest request, HttpServletResponse response, UserRole role, String searchInput) throws IOException, ServletException {
        String index_str =  request.getParameter("offset");
        int index = 0;
        System.out.println("offset taken with value: " + index_str);
        if(index_str != null)
            index = Integer.parseInt(index_str);
        System.out.println("index parsed");
        request.setAttribute("offset", index);
        
        ArrayList<GeneralUserDTO> users = (ArrayList<GeneralUserDTO>) userEJB.searchUsers(searchInput, role, index);
        request.setAttribute("userList", users);
        
        if(role == UserRole.student) {
            request.setAttribute("role_searched", UserRole.student);
        }
        else if(role == UserRole.professor) {
            request.setAttribute("role_searched", UserRole.professor);
        }
    
        request.getRequestDispatcher("/WEB-INF/jsp/admin/users.jsp")
                .forward(request, response);
    
    }
    
    private void changeOffset(HttpServletRequest request, HttpServletResponse response) {
        String switch_ = request.getParameter("switch");
        String searchInput = request.getParameter("search_input");
    
        if(searchInput == null)
            searchInput = "";
    
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
        
        try {
            update(request, response, role_to_search, searchInput);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        if (AccessController.checkAccess(request, response, UserRole.admin) == null) {
            return;
        }
        System.out.println("Entering Update function");
        update(request, response, UserRole.student, "");
        
    }
    
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        
        if (AccessController.checkAccess(request, response, UserRole.admin) == null) {
            return;
        }
        String action = request.getParameter("action");
        
        switch (action) {
            case "search":
                searchWithString(request, response);
                break;
            case "delete":
                deleteUser(request, response);
                break;
            case "switch":
                switchUserType(request, response);
                break;
            case "offsetChange":
                changeOffset(request, response);
                break;
                
        }
    }
    
}
