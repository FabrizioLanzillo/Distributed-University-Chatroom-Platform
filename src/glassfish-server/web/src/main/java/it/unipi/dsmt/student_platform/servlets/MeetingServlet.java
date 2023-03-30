package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.dto.MeetingDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.MeetingEJB;
import it.unipi.dsmt.student_platform.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;


import java.io.IOException;
import java.util.List;

@WebServlet(name = "MeetingServlet", value = "/professor/meeting")
public class MeetingServlet extends HttpServlet {

    @EJB
    private MeetingEJB meetingEJB;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        LoggedUserDTO loggedUser = AccessController.checkAccess(request, response, UserRole.professor);
        if (loggedUser == null)
            return;
        
        LoggedUserDTO user = (LoggedUserDTO)request.getSession().getAttribute("logged_user");
        int offset = request.getParameter("offset")==null ? 0 : Integer.parseInt(request.getParameter("offset"));
    
        String action = request.getParameter("action") == null? "none" : request.getParameter("action");
    
        if(action != null && action.equals("offsetChange")){
            response.sendRedirect(request.getContextPath() + "/professor/meeting?offset=" + offset);
            return;
        }

        // List of available slots
        List<MeetingDTO> mDTOs = meetingEJB.getSlots(user.getId(), offset);

        int iterator = request.getParameter("timeslot").isEmpty() ? -1 : Integer.parseInt(request.getParameter("timeslot"));
        if(iterator == -1){
            response.sendRedirect(request.getContextPath() + "/professor/meeting?r=error&offset=" + offset);
            System.out.println("Error: no timeslot selected");
            return;
        }

        MeetingDTO meetingDTO = mDTOs.get(iterator);
        boolean ret = meetingEJB.removeSlot(meetingDTO);

        if(!ret){
            response.sendRedirect(request.getContextPath() + "/professor/meeting?r=error&offset=" + offset);
            return;
        }
        response.sendRedirect(request.getContextPath() + "/professor/meeting?r=success&offset=" + offset);

    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        // Check if logged user is a professor
        LoggedUserDTO loggedUser = AccessController.checkAccess(request, response, UserRole.professor);
        if (loggedUser == null)
            return;

        // Get the offset to load the right page
        int offset = 0;
        String offsetString = request.getParameter("offset");
        if(offsetString != null) {
            offset = Integer.parseInt(offsetString);
        }

        List<MeetingDTO> bDTOs = meetingEJB.getSlots(loggedUser.getId(), offset);
        request.setAttribute("bookedSlots", bDTOs);

        request.getRequestDispatcher("/WEB-INF/jsp/professor/meeting.jsp")
                .forward(request, response);

    }
}