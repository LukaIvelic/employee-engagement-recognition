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
    exports controllers.create;
    opens controllers.create to javafx.fxml;
    exports controllers.update;
    opens controllers.update to javafx.fxml;
    exports controllers.delete;
    opens controllers.delete to javafx.fxml;
    exports controllers.preview;
    opens controllers.preview to javafx.fxml;
    exports controllers.overview to javafx.fxml;
    opens controllers.overview to javafx.fxml;
    exports controllers.engagement to javafx.fxml;
    opens controllers.engagement to javafx.fxml;
    exports controllers.leaderboard to javafx.fxml;
    opens controllers.leaderboard to javafx.fxml;
    exports controllers.login to javafx.fxml;
    opens controllers.login to javafx.fxml;
}