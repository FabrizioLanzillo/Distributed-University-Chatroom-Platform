package it.unipi.dsmt.student_platform.servlets;

import it.unipi.dsmt.student_platform.dto.SignupDTO;
import it.unipi.dsmt.student_platform.interfaces.SignupEJB;
import it.unipi.dsmt.student_platform.interfaces.UserEJB;
import jakarta.ejb.EJB;
import jakarta.servlet.ServletException;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.*;


import java.io.IOException;


@WebServlet(name = "UsersServlet", value="/admin/users")
public class UsersServlet extends HttpServlet {

    @EJB
    private UserEJB userEJB;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response){

    }

    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response){

    }
}
