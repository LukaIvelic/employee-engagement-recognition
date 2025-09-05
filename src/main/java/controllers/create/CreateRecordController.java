/**
 * Represents a CreateRecordController that takes care loading other create controllers to GUI
 * This class provides methods load {@code Employee & Engagement} create controllers to GUI
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-11
 */

package controllers.create;

import database.DatabaseManager;
import database.enums.Databases;
import handlers.ResourceManager;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.layout.Pane;
import logging.ErrorLogger;
import logging.Logger;

public class CreateRecordController {
    @FXML
    private ComboBox<String> recordComboBox;
    @FXML
    public Pane recordContentPane;

    /**
     * Prepares recordComboBox with collections in the database ready for selection
     */
    public void initialize() {
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        Thread myThread = new Thread(()->{
            try {
                DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
                ObservableList<String> creators = FXCollections.observableArrayList(databaseManager.getAllCollections());
                recordComboBox.setItems(creators);
                databaseManager.mongoClient.close();
            } catch (Exception e) {
                errorLogger.log(e.getMessage());
            }
        });
        myThread.start();
    }

    /**
     * Loads the Login controller to handle login and loading to GUI
     */
    public void loadCreator(){
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        if(recordComboBox.getSelectionModel().getSelectedItem() == null){
            return;
        }

        try {
            ResourceManager.loadContent("/scenes/login.scene.fxml", this).ifPresent(content ->{
                recordContentPane.getChildren().clear();
                recordContentPane.getChildren().add(content);
            });
        } catch (Exception e) {
            errorLogger.log(e.getMessage());
        }
    }
}