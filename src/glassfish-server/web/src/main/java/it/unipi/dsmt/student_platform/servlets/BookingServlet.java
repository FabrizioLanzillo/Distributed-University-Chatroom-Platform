package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.BookingDTO;
import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.BookingEJB;
import it.unipi.dsmt.student_platform.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;


import java.io.IOException;
import java.util.List;

@WebServlet(name = "BookingServlet", value = "/student/booking")
public class BookingServlet extends HttpServlet {

    @EJB
    private BookingEJB bookingEJB;
    
    /**
     * Redefinition of doPost method for booking page. This method is called when the user tries to book a slot showed
     * in the booking page. The method after retrieving the data from the request insert the requested booking inside
     * the database.
     *
     * @param request
     * @param response
     * @throws IOException
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        // Check access and get the logged user information
        LoggedUserDTO loggedUser = AccessController.checkAccess(request, response, UserRole.professor);
        if (loggedUser == null) {
            return;
        }
        
        // Get the course id from the request
        int id;
        try{
            id  = Integer.parseInt(request.getParameter("id"));
        }
        catch(IllegalArgumentException e) {
            System.out.println("Error: course ID not set");
            e.printStackTrace();
            return;
        }
        
        // Get the offset in order to understand in which month the user is browsing
        int offset = request.getParameter("offset")==null ? 0 : Integer.parseInt(request.getParameter("offset"));
        
        /* Get the action the user wants to perform, there are 2 possible actions:
        * 1: book a slot, in this case the action parameter is not specified
        * 2: change offset, both forward or backward to browse different months
         */
        String action = request.getParameter("action") == null ? null : request.getParameter("action");
        
        //Check if the action is specified, if it is update the offset and perform a new get with the new offset
        if(action != null && action.equals("offsetChange")){
            response.sendRedirect(request.getContextPath() + "/student/booking?id=" + id + "&offset=" + offset);
            return;
        }

        /* If the action is not specified, get the booking from the database with the current offset since the user is
        * trying to book a slot
         */
        List<BookingDTO> bDTOs = bookingEJB.getSlots(id, offset);

        // Understand which slot has been selected by the user
        int iterator = request.getParameter("timeslot").isEmpty() ? -1 : Integer.parseInt(request.getParameter("timeslot"));
        // Manage some illegal arguments
        if(iterator == -1){
            response.sendRedirect(request.getContextPath() + "/student/booking?id=" + id + "&r=error");
            System.out.println("Error: no timeslot selected");
            return;
        }
        
        // Everything is set now, we can perform the DB update to store the newly booked slot
        boolean ret = bookingEJB.bookSlot(loggedUser.getId(), id, bDTOs.get(iterator), offset);
        // Check query response
        if(!ret){
            response.sendRedirect(request.getContextPath() + "/student/booking?id=" + id + "&r=error&offset=" + offset);
            return;
        }
        response.sendRedirect(request.getContextPath() + "/student/booking?id=" + id + "&r=success&offset=" + offset);
    }
    
    /**
     * Redefinition of doGet method for booking page. Called when accessing the page from the course page or if the user
     * changes the offset through a POST request.
     * @param request
     * @param response
     * @throws IOException
     * @throws ServletException
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        // Check if logged user is a student
        if (AccessController.checkAccess(request, response, UserRole.student) == null)
           return;
        
        // Extract courseID
        int id;
        try {
            id  = Integer.parseInt(request.getParameter("id"));
        }
        catch(Exception e){
            System.out.println("Error: course ID not set");
            e.printStackTrace();
            return;
        }
        // Extract current offset, if it's not specified, set it to 0 (current month)
        int offset = request.getParameter("offset") == null ? 0 : Integer.parseInt(request.getParameter("offset"));
        
        // Get available slots of the selected month and return them to the client
        List<BookingDTO> bDTOs = bookingEJB.getSlots(id, offset);
        
        request.setAttribute("slots", bDTOs);
        request.getRequestDispatcher("/WEB-INF/jsp/student/booking.jsp")
                .forward(request, response);

    }
}