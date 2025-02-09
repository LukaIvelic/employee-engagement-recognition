module org.example.employeeengagementrecognition {
    requires javafx.controls;
    requires javafx.fxml;
    requires java.desktop;
    requires java.sql;
    requires org.mongodb.driver.core;
    requires org.mongodb.driver.sync.client;
    requires org.mongodb.bson;

    opens controllers to javafx.fxml;
    opens launch to javafx.fxml;
    exports controllers;
    exports launch;
}