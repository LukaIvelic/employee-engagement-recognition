/**
 * Represents a Menu Controller which holds the logic for choosing the right filename from resource directory based on the name of the menu item shown in the JavaFX GUI
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-06
 */

package controllers;

import handlers.ResourceManager;
import javafx.scene.control.MenuItem;
import javafx.event.ActionEvent;
import javafx.scene.layout.Pane;
import launch.EmployeeEngagementRecognition;

public class MenuController {
    /**
     * Provides a modified {@code String} following a file naming convention
     * @param rawTitle — the menu item name
     */
    private static String createFilename(String rawTitle){
        return "/scenes/" + rawTitle.replace(" ", ".").toLowerCase() + ".scene.fxml";
    }

    /**
     * Changes the content based on which sender of the event has called the method
     * @param event — is used for getting the sender of the event
     */
    public void changeContentProviderContent(ActionEvent event) {
        MenuItem sender = (MenuItem) event.getSource();
        String filename = createFilename(sender.getText());
        EmployeeEngagementRecognition.primaryStage.setTitle("Employee Engagement Recognition | " + sender.getText());
        Pane rootPane = (Pane)EmployeeEngagementRecognition.primaryStage.getScene().getRoot();
        Pane contentPane =  (Pane)rootPane.getChildren().getLast();
        ResourceManager.loadContent(filename, this).ifPresent(content -> {
            contentPane.getChildren().clear();
            contentPane.getChildren().add(content);
        });
    }
}
