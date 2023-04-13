package it.unipi.dsmt.student_platform.servlets.professor;

import it.unipi.dsmt.student_platform.dto.LoggedUserDTO;
import it.unipi.dsmt.student_platform.dto.MeetingDTO;
import it.unipi.dsmt.student_platform.enums.UserRole;
import it.unipi.dsmt.student_platform.interfaces.BookedMeetingEJB;
import it.unipi.dsmt.student_platform.utility.AccessController;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;


import java.io.IOException;
import java.util.List;

@WebServlet(name = "ProfessorMeetingServlet", value = "/professor/meeting")
public class ProfessorMeetingServlet extends HttpServlet {

    @EJB
    private BookedMeetingEJB bookedMeetingEJB;
    
    /**
     * Redefinition of doPOST. Invoked when the user (professor in this case) tries to delete a booked meeting
     * @param request
     * @param response
     * @throws IOException
     */
    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        // Check access and obtain user infos
        LoggedUserDTO loggedUser = AccessController.checkAccess(request, response, UserRole.professor);
        if (loggedUser == null)
            return;
        
        // Parameters extraction
        int offset = request.getParameter("offset")==null ? 0 : Integer.parseInt(request.getParameter("offset"));
        String action = request.getParameter("action") == null? "none" : request.getParameter("action");
    
        // If the user tries to switch month handle and redirect
        if(action != null && action.equals("offsetChange")){
            response.sendRedirect(request.getContextPath() + "/professor/meeting?offset=" + offset);
            return;
        }

        // Extract the list of booked slots
        List<MeetingDTO> mDTOs = bookedMeetingEJB.getProfessorMeetings(loggedUser.getId(), offset);
        
        // Get the iterator to understand which of the list haas been clicked
        int iterator = request.getParameter("timeslot").isEmpty() ? -1 : Integer.parseInt(request.getParameter("timeslot"));
        if(iterator == -1){
            response.sendRedirect(request.getContextPath() + "/professor/meeting?r=error&offset=" + offset);
            System.out.println("Error: no timeslot selected");
            return;
        }

        // Delete selected booked slot
        MeetingDTO meetingDTO = mDTOs.get(iterator);
        boolean ret = bookedMeetingEJB.removeSlot(meetingDTO.getMeetingId());

        if(!ret){
            response.sendRedirect(request.getContextPath() + "/professor/meeting?r=error&offset=" + offset);
            return;
        }
        response.sendRedirect(request.getContextPath() + "/professor/meeting?r=success&offset=" + offset);

    }
    
    /**
     * Redefinition of doGET method
     * @param request
     * @param response
     * @throws IOException
     * @throws ServletException
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        // Check if logged user is a professor
        LoggedUserDTO loggedUser = AccessController.checkAccess(request, response, UserRole.professor);
        if (loggedUser == null)
            return;

        // Extract the offset
        int offset = request.getParameter("offset") == null ? 0 : Integer.parseInt(request.getParameter("offset"));

        // Extract the slots for that specific offset
        List<MeetingDTO> bDTOs = bookedMeetingEJB.getProfessorMeetings(loggedUser.getId(), offset);
        request.setAttribute("bookedSlots", bDTOs);

        request.getRequestDispatcher("/WEB-INF/jsp/professor/meeting.jsp")
                .forward(request, response);

    }
}