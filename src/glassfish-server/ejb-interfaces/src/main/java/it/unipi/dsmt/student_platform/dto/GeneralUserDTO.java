package it.unipi.dsmt.student_platform.dto;

import java.io.Serializable;

public class GeneralUserDTO implements Serializable {
    private String id;
    private String username;
    private String email;
    private String name;
    private String surname;

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
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
    
    public String toString(){
        return this.name + " " + this.surname + "\n" +
                this.username + " - " + this.email + "\n" +
                " - id: " + this.id;
    }
}
