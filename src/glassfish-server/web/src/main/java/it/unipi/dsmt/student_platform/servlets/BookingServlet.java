package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.BookingDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.BookingEJB;
import it.unipi.dsmt.student_platform.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;


import java.io.IOException;
import java.util.ArrayList;

@WebServlet(name = "BookingServlet", value = "/student/booking")
public class BookingServlet extends HttpServlet {

    @EJB
    private BookingEJB bookingEJB;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        HttpSession session = request.getSession();
        int id = request.getParameter("id").isEmpty()? 0 : Integer.parseInt(request.getParameter("id"));
        int offset = request.getParameter("offset").isEmpty()? 0 : Integer.parseInt(request.getParameter("offset"));

        // List of available slots
        ArrayList<BookingDTO> bDTOs = bookingEJB.getSlots(id, offset);

        int iterator = request.getParameter("timeslot").isEmpty() ? -1 : Integer.parseInt(request.getParameter("timeslot"));
        if(iterator == -1){
            response.sendRedirect(request.getContextPath() + "/student/booking?id=" + id + "&r=error");
            System.out.println("Error: no timeslot selected");
        }

        // Send the query
        boolean ret = bookingEJB.bookASlot(id, bDTOs.get(iterator));

        if(!ret){
            response.sendRedirect(request.getContextPath() + "/student/booking?id=" + id + "&r=error&offset=" + offset);
        }
        response.sendRedirect(request.getContextPath() + "/student/booking?id=" + id + "&r=success&offset=" + offset);
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        // Check if logged user is a student
        int id;
        // if (!AccessController.checkAccess(request, response, UserRole.student)) {
        //    return;
        //}

        try {
            // Get course id from GET parameters
            String stringId = request.getParameter("id");
            if (stringId == null) {
                throw new RuntimeException("course id not set");
                }
            id = Integer.parseInt(stringId);
            }
        catch (NumberFormatException e) {
            throw new RuntimeException("course id is not a number");
            }

        // Get the offset to load the right page
        int offset = 0;
        String offsetString = request.getParameter("offset");

        if(offsetString != null) {
            offset = Integer.parseInt(offsetString);
        }

        ArrayList<BookingDTO> bDTOs = bookingEJB.getSlots(id, offset);
        request.setAttribute("slots", bDTOs);

        System.out.println(bDTOs.get(0).toString());
        System.out.println(bDTOs.get(0).getDayOfWeek());

        request.getRequestDispatcher("/WEB-INF/jsp/student/booking.jsp")
                .forward(request, response);

    }
}