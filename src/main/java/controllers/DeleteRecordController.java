package controllers;

import com.mongodb.client.MongoCollection;
import database.DatabaseManager;
import database.enums.DatabaseInfo;
import database.enums.Databases;
import handlers.ResourceManager;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
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

    public void initialize() {
        fillCollectionComboBox();
    }

    public void previewObject(){
        DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
        try{
            databaseManager.databaseCondition = DatabaseInfo.UNAVAILABLE;


            String selectedCollection = collectionComboBox.getSelectionModel().getSelectedItem();
            List<Document> documents = new ArrayList<>();

            if(collectionComboBox.getSelectionModel().getSelectedItem() == null){
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
                errorLabel.setText("There isn't no such record in the database");
            }

            databaseManager.mongoClient.close();
        } catch (Exception e) {
            databaseManager.databaseCondition = DatabaseInfo.ERROR;
            databaseManager.mongoClient.close();
            errorLabel.setText("An error occurred");
        } finally {
            databaseManager.databaseCondition = DatabaseInfo.AVAILABLE;
            databaseManager.mongoClient.close();
        }
    }

    public void fillCollectionComboBox() {
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
            } finally {
                databaseManager.databaseCondition = DatabaseInfo.AVAILABLE;
                databaseManager.mongoClient.close();
            }
        });
        myThread.start();
    }

    public void deleteRecord() {
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
            MongoCollection<Document> collection = databaseManager.getCollection(collectionComboBox.getSelectionModel().getSelectedItem());
            collection.deleteOne(new Document("_id", new ObjectId(objectIdTextField.getText())));
        });
        myThread.start();
        ResourceManager.loadOverviewContent(this);
    }

    public void cancelRecordDelete() {
        ResourceManager.loadOverviewContent(this);
    }
}
