module org.example.employeeengagementrecognition {
    requires javafx.controls;
    requires javafx.fxml;
    requires java.desktop;

    opens controllers to javafx.fxml;
    opens launch to javafx.fxml;
    exports controllers;
    exports launch;
}