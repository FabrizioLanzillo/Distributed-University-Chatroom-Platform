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
        String course_id = request.getParameter("id");

        // List of available slots
        ArrayList<BookingDTO> bookingDTOS = (ArrayList<BookingDTO>)request.getAttribute("slots");

        int id = Integer.parseInt(request.getParameter("selected_slot"));

        // Get selected slot DTO
        BookingDTO bookingDTO = bookingDTOS.get(id);
        // Send the query
        boolean ret = bookingEJB.bookASlot(id, bookingDTO);

        if(!ret){
            response.sendRedirect(request.getContextPath() + "/student/booking?r=error");
        }
        response.sendRedirect(request.getContextPath() + "/student/booking?r=success");

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

        request.getRequestDispatcher("/WEB-INF/jsp/student/booking.jsp")
                .forward(request, response);

    }
}