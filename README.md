# Distributed-University-Chatroom-Platform

![Java](https://img.shields.io/badge/Java-%23ED8B00.svg?style=flat-square&logo=openjdk&logoColor=white) ![Maven](https://img.shields.io/badge/-Maven-C71A36?logo=apachemaven&logoColor=white&style=flat-square) ![Glassfish](https://img.shields.io/badge/Glassfish-%23ED8B00.svg?style=flat-square&logoColor=white) ![MySQL](https://img.shields.io/badge/MySQL-4479A1?logo=mysql&style=flat-square&logoColor=white) ![NGINX](https://img.shields.io/badge/-NGINX-009639?logo=nginx&logoColor=white&style=flat-square) ![Erlang](https://img.shields.io/badge/-Erlang-A90533?logo=erlang&logoColor=white&style=flat-square) ![MesiaDB](https://img.shields.io/badge/-MnesiaDB-A90533?logo=erlang&logoColor=white&style=flat-square) 

University project for **Distributed Systems and Middleware Technologies** course (MSc Computer Engineering at University of Pisa, A.Y. 2022-23)

**Student University Platform** is a **distributed web-app** aimed at providing various functionalities to students and professors, which are: 
- provide **chatroom functionality** among students where they can share opinions and suggestions 
- allow **booking** and managing of meetings 

Both students and professors have a dedicated page where they can view scheduled meetings and cancel them. 
Within the course page, on the other hand, it is possible to access the dedicated chatroom or look at the list of available slots and choose the meeting
to reserve. 

# System Architecture

![System Architecture](./documentation/system_architecture.png)

The web application, as we can see in figure, consists of a **Client Application**, a **Jakarta Application Server**, and **multiple Erlang Servers**.  
- The **Jakarta EE** Application Server, which implements most of the functionality of the application, uses as its reference implementation, **Glassfish 6.2.5** and also communicates with a **MySql relational database** located on a different machine.  
- The **Erlang** Servers handle the Chatrooms of the application through an **HTTP websocket communication**. They are managed by a **Load Balancer** and configured by a **Master Node** and both are on another machine. We chose **Nginx** for the Load Balancer since it natively supports the WebSocket protocol both as **reverse proxy** and **load balancer**.
- the client application was implemented using the combination of **html**, **css** and **javascript**

# Synchronization and communication

- Multiple clients **exchange messages** with each other in chat rooms **concurrently**. The chat of each client node must be synchronized.
- When a **student enters or leave** the chatroom each client **node must be synchronized** in order to see a consistent list of students within the chat (i.e. ”online students”).
- When a student **books a meeting** in an available time slot or remove a previously booked meeting, each client **node must be synchronized to view the same consistent state** (both for students and professors).
- When a professor creates or delete a course, the server must show the course to all the connected client in the ”browse”, ”search” sections and, in case of deletion, inside the course page.

# Structure of the repository 

```
Distributed-University-Chatroom-Platform
|
├── erlang-server
│   ├── chat_server
│   ├── master_node
│   └── nginx
|
├── glassfish-server
│   ├── ejb
│   │   └── src
│   |       ├── dao
|   |       └── ejb   
│   ├── ejb-interfaces
│   │   └── src
│   |       ├── dto
│   |       ├── enums
|   |       └── interfaces   
│   └── web
│       └── src
│           ├── java/it/unipi/dsmt/student_platform
│           |   ├── servlets
|           |   └── utility  
│           └── webapp    
│               ├── assets
|               |   ├── css
|               |   ├── javascript
|               |   ├── img
|               |   └── libs 
|               └── WEB-INF 
|                   └── jsp  
└── mysql-server 

```

## Authors
- [Fabrizio Lanzillo](https://github.com/FabrizioLanzillo)
- [Federico Montini](https://github.com/FedericoMontini98)
- [Riccardo Sagramoni](https://github.com/RiccardoSagramoni)