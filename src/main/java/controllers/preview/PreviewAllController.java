/**
 * Represents a PreviewAllEmployeeController which is used to display all the employees in the database
 * This class provides methods load employee information into JavaFX GUI
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-09
 */

package controllers.preview;

import database.DatabaseManager;
import database.enums.DatabaseInfo;
import database.enums.Databases;
import handlers.ResourceManager;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.*;
import javafx.scene.layout.Pane;
import logging.ErrorLogger;
import logging.InfoLogger;
import logging.Logger;

public class PreviewAllController {
    @FXML
    private ComboBox<String> collectionComboBox;
    @FXML
    private Pane previewContentPane;
    static final String INFOLOGGER_PATH = "./logs/info.log.ser";

    /**
     * Loads the employees into the JavaFX GUI upon initializing
     */
    public void initialize() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("initialize() method called");
        fillCollectionComboBox();
    }

    public void fillCollectionComboBox(){
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        infoLogger.log("fillCollectionComboBox() method called");
        Thread myThread = new Thread(()->{
            DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
            try {
                ObservableList<String> list = FXCollections.observableArrayList(databaseManager.getAllCollections());
                Platform.runLater(()-> collectionComboBox.setItems(list));
                databaseManager.mongoClient.close();
            } catch (Exception e) {
                databaseManager.databaseCondition = DatabaseInfo.ERROR;
                databaseManager.mongoClient.close();
                errorLogger.log(e.getMessage());
            } finally {
                databaseManager.databaseCondition = DatabaseInfo.AVAILABLE;
                try{
                    databaseManager.mongoClient.close();
                } catch (Exception e){
                    errorLogger.log(e.getMessage());
                }

            }
        });
        myThread.start();
    }

    public void fetchRecords(){
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        infoLogger.log("fetchRecords() method called");
        if(collectionComboBox.getSelectionModel().getSelectedItem() == null) {
            return;
        }
        try{
            ResourceManager.loadContent(filename(collectionComboBox.getSelectionModel().getSelectedItem()), this).ifPresent(previewContentPane.getChildren()::add);
        } catch (Exception e){
            errorLogger.log(e.getMessage());
        }

    }

    private String filename(String rawTitle){
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("filename() method called");
        return String.format("/scenes/preview.%s.records.scene.fxml", rawTitle);
    }
}