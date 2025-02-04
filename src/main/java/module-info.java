module org.example.employeeengagementrecognition {
    requires javafx.controls;
    requires javafx.fxml;


    opens org.example.employeeengagementrecognition to javafx.fxml;
    exports org.example.employeeengagementrecognition;
}