module org.example.employeeengagementrecognition {
    requires javafx.controls;
    requires javafx.fxml;

    opens launch to javafx.fxml;
    exports launch;
}