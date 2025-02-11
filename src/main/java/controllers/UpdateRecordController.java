package controllers;

import database.DatabaseManager;
import database.enums.Databases;
import handlers.ResourceManager;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.layout.Pane;

public class UpdateRecordController {
    @FXML
    private ComboBox<String> recordComboBox;
    @FXML
    public Pane recordContentPane;

    public void initialize() {
        Thread myThread = new Thread(()->{
            DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
            ObservableList<String> creators = FXCollections.observableArrayList(databaseManager.getAllCollections());
            recordComboBox.setItems(creators);
            databaseManager.mongoClient.close();
        });
        myThread.start();
    }

    public void loadCreator(){
        if(recordComboBox.getSelectionModel().getSelectedItem() == null){
            return;
        }

        String title = recordComboBox.getSelectionModel().getSelectedItem();

        ResourceManager.loadContent(filename(title), this).ifPresent(content->{
            recordContentPane.getChildren().clear();
            recordContentPane.getChildren().add(content);
        });
    }

    private String filename(String rawTitle){
        return String.format("/scenes/update.%s.record.scene.fxml", rawTitle);
    }
}
