package controllers.update;

import database.DatabaseManager;
import database.enums.Databases;
import handlers.ResourceManager;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.layout.Pane;
import logging.ErrorLogger;
import logging.InfoLogger;
import logging.Logger;

public class UpdateRecordController {
    @FXML
    private ComboBox<String> recordComboBox;
    @FXML
    public Pane recordContentPane;

    public void initialize() {
        Logger infoLogger = new InfoLogger("./logs/info.log.ser");
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        infoLogger.log("initialize() method called");
        Thread myThread = new Thread(()->{
            try{
                DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
                ObservableList<String> creators = FXCollections.observableArrayList(databaseManager.getAllCollections());
                recordComboBox.setItems(creators);
                databaseManager.mongoClient.close();
            } catch (Exception e){
                errorLogger.log(e.getMessage());
            }

        });
        myThread.start();
    }

    public void loadCreator(){
        Logger infoLogger = new InfoLogger("./logs/info.log.ser");
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        infoLogger.log("loadCreator() method called");
        if(recordComboBox.getSelectionModel().getSelectedItem() == null){
            return;
        }
        try{
            ResourceManager.loadContent("/scenes/login.scene.fxml", this).ifPresent(content ->{
                recordContentPane.getChildren().clear();
                recordContentPane.getChildren().add(content);
            });
        } catch (Exception e){
            errorLogger.log(e.getMessage());
        }

    }

}