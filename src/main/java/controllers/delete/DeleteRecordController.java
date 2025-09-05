/**
 * Represents a DeleteRecordController that takes care of deleting records from the database
 * This class provides methods that take care of transforming data into queries from JavaFX GUI
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-12
 */

package controllers.delete;

import com.mongodb.client.MongoCollection;
import database.DatabaseManager;
import database.enums.DatabaseInfo;
import database.enums.Databases;
import handlers.ResourceManager;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import logging.ErrorLogger;
import logging.Logger;
import org.bson.Document;
import org.bson.types.ObjectId;

import java.util.ArrayList;
import java.util.List;

public class DeleteRecordController {
    @FXML
    private ComboBox<String> collectionComboBox;
    @FXML
    private TextField objectIdTextField;
    @FXML
    private Label previewLabel;
    @FXML
    private Label errorLabel;

    static final String ERRORLOGGER_PATH = "./logs/error.log.ser";

    /**
     * Calls the fillCollectionComboBox method
     */
    public void initialize() {
        fillCollectionComboBox();
    }

    /**
     * Loads data from the database and displays it in JSON format in the JavaFX GUI
     */
    public void previewObject(){
        Logger errorLogger = new ErrorLogger(ERRORLOGGER_PATH);
        DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
        try{
            databaseManager.databaseCondition = DatabaseInfo.UNAVAILABLE;


            String selectedCollection = collectionComboBox.getSelectionModel().getSelectedItem();
            List<Document> documents = new ArrayList<>();

            if(collectionComboBox.getSelectionModel().getSelectedItem() == null){
                previewLabel.setText("");
                errorLabel.setText("You haven't selected a collection");
                return;
            }else{
                errorLabel.setText("");
            }

            databaseManager.getCollection(selectedCollection).find().into(documents);

            boolean hasDocument = false;
            for(Document document: documents){
                if(document.get("_id").toString().equals(objectIdTextField.getText())){
                    previewLabel.setText(document.toJson());
                    hasDocument = true;
                }
            }
            if(Boolean.FALSE.equals(hasDocument)){
                previewLabel.setText("");
                errorLabel.setText("There isn't no such record in the database");
            }

            databaseManager.mongoClient.close();
        } catch (Exception e) {
            databaseManager.databaseCondition = DatabaseInfo.ERROR;
            databaseManager.mongoClient.close();
            errorLabel.setText("An error occurred");
            errorLogger.log(e.getMessage());
        } finally {
            databaseManager.databaseCondition = DatabaseInfo.AVAILABLE;
            try{
                databaseManager.mongoClient.close();
            } catch (Exception e){
                errorLogger.log(e.getMessage());
            }

        }
    }

    /**
     * Fills the collectionComboBox with the names of all the collections in the database
     */
    public void fillCollectionComboBox() {
        Logger errorLogger = new ErrorLogger(ERRORLOGGER_PATH);
        Thread myThread = new Thread(()->{
            DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
            try {
                databaseManager.databaseCondition = DatabaseInfo.UNAVAILABLE;
                ObservableList<String> list = FXCollections.observableArrayList(databaseManager.getAllCollections());
                collectionComboBox.setItems(list);
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

    /**
     * Deletes a record from the collection based on the ID given
     */
    public void deleteRecord() {
        Logger errorLogger = new ErrorLogger(ERRORLOGGER_PATH);
        if(collectionComboBox.getSelectionModel().getSelectedItem() == null){
            errorLabel.setText("You haven't selected a collection");
            return;
        }
        if(objectIdTextField.getText().isEmpty()){
            errorLabel.setText("You haven't entered a valid ID");
            return;
        }
        errorLabel.setText("");
        Thread myThread = new Thread(()->{
            DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
            databaseManager.databaseCondition = DatabaseInfo.UNAVAILABLE;
            try{
                MongoCollection<Document> collection = databaseManager.getCollection(collectionComboBox.getSelectionModel().getSelectedItem());
                collection.deleteOne(new Document("_id", new ObjectId(objectIdTextField.getText())));
            } catch (Exception e){
                errorLogger.log(e.getMessage());
            }

        });
        myThread.start();
        ResourceManager.loadOverviewContent(this);
    }

    /**
     * Loads overview.scene.fxml file and displays it after clicking the Cancel button
     */
    public void cancelRecordDelete() {
        ResourceManager.loadOverviewContent(this);
    }
}