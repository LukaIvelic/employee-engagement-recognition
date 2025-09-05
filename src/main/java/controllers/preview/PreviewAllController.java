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

public class PreviewAllController {
    @FXML
    private ComboBox<String> collectionComboBox;
    @FXML
    private Pane previewContentPane;

    /**
     * Loads the employees into the JavaFX GUI upon initializing
     */
    public void initialize() {
        fillCollectionComboBox();
    }

    public void fillCollectionComboBox(){
        Thread myThread = new Thread(()->{
            DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
            try {
                ObservableList<String> list = FXCollections.observableArrayList(databaseManager.getAllCollections());
                Platform.runLater(()-> collectionComboBox.setItems(list));
                databaseManager.mongoClient.close();
            } catch (Exception e) {
                databaseManager.databaseCondition = DatabaseInfo.ERROR;
                databaseManager.mongoClient.close();
            } finally {
                databaseManager.databaseCondition = DatabaseInfo.AVAILABLE;
                databaseManager.mongoClient.close();
            }
        });
        myThread.start();
    }

    public void fetchRecords(){
        if(collectionComboBox.getSelectionModel().getSelectedItem() == null) {
            return;
        }
        ResourceManager.loadContent(filename(collectionComboBox.getSelectionModel().getSelectedItem()), this).ifPresent(previewContentPane.getChildren()::add);
    }

    private String filename(String rawTitle){
        return String.format("/scenes/preview.%s.records.scene.fxml", rawTitle);
    }
}
