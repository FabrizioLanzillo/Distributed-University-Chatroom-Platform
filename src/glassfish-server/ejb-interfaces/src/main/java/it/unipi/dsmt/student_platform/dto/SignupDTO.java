package it.unipi.dsmt.student_platform.dto;

import it.unipi.dsmt.student_platform.enums.UserRole;

public class SignupDTO {
    private String username;
    private String password;
    private String email;
    private String name;
    private String surname;
    private String degree;
    private String language;
    private UserRole role;


    public SignupDTO(String username, String password, String email, String name, String surname, String degree, String language) {
        this.username = username;
        this.password = password;
        this.email = email;
        this.name = name;
        this.surname = surname;
        this.degree = degree;
        this.language = language;
        this.role = UserRole.student; //Default, only students can sign up
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public UserRole getRole() {
        return role;
    }

    public void setRole(UserRole role) {
        this.role = role;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getSurname() {
        return surname;
    }

    public void setSurname(String surname) {
        this.surname = surname;
    }

    public String getDegree() {
        return degree;
    }

    public void setDegree(String degree) {
        this.degree = degree;
    }

    public String getLanguage() {
        return language;
    }

    public void setLanguage(String language) {
        this.language = language;
    }
}
