package controllers.update;

import database.DatabaseManager;
import database.enums.DatabaseInfo;
import database.enums.Databases;
import generics.RecordManager;
import handlers.InformationManager;
import handlers.ResourceManager;
import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;
import logging.ErrorLogger;
import logging.InfoLogger;
import logging.Logger;
import org.bson.Document;
import org.bson.types.ObjectId;
import records.Engagement;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.List;

public class UpdateEngagementRecordController {
    @FXML
    private TextField personIdField;
    @FXML
    private TextField hoursSpentField;
    @FXML
    private Label errorLabel;
    @FXML
    private VBox previewVBoxLeft;
    @FXML
    private VBox previewVBoxRight;
    @FXML
    private Label personIdPreviewLabel;
    @FXML
    private Label hoursSpentPreviewLabel;
    @FXML
    private TextField objectIdField;
    @FXML
    private Label previewWrittenLabel;

    static final String INFOLOGGER_PATH = "./logs/info.log.ser";
    static final String ERRORLOGGER_PATH = "./logs/error.log.ser";

    /**
     * Cancels creating record GUI and returns to overview content Pane
     */
    public void cancelCreateRecord() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("cancelCreateRecord() method called");
        ResourceManager.loadOverviewContent(this);
    }

    /**
     * Gets the values from JavaFX GUI and transforms it into an object that's readable to the backend
     * @return Engagement a default record for all engagements
     */
    private Engagement getEngagement() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("getEngagement() method called");
        return new Engagement(
                "mongo_id",
                personIdField.getText(),
                Duration.of(Long.parseLong(hoursSpentField.getText()), ChronoUnit.HOURS)
        );
    }

    public synchronized void previewWrittenObject(){
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger(ERRORLOGGER_PATH);
        infoLogger.log("previewWrittenObject() method called");
        DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
        try{
            databaseManager.databaseCondition = DatabaseInfo.UNAVAILABLE;
            String writtenID = objectIdField.getText();
            List<Document> documents = new ArrayList<>();
            if(objectIdField.getText().isEmpty()){
                errorLabel.setText("You haven't entered an ID");
                return;
            }else{
                errorLabel.setText("");
            }
            databaseManager.getCollection(Databases.Collections.ENGAGEMENT.toString()).find().into(documents);
            boolean hasDocument = false;
            for(Document document: documents){
                if(document.getObjectId("_id").equals(new ObjectId(writtenID))){
                    previewWrittenLabel.setText(document.toJson());
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
     * Used to preview what the record will look like when it is written in the database
     */
    public void previewRecord() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger(ERRORLOGGER_PATH);
        infoLogger.log("previewRecord() method called");
        Thread myThread = new Thread(() -> {
            try{
                previewVBoxLeft.setVisible(true);
                previewVBoxRight.setVisible(true);
                Platform.runLater(() -> {
                    personIdPreviewLabel.setText("\""+personIdField.getText()+"\"");
                    hoursSpentPreviewLabel.setText("\""+hoursSpentField.getText()+"\"");
                });
            } catch (Exception e){
                errorLogger.log(e.getMessage());
            }

        });
        myThread.start();
    }

    /**
     * Creates record and writes it to the database
     */
    public void updateRecord() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger(ERRORLOGGER_PATH);
        infoLogger.log("updateRecord() method called");
        Thread myThread = new Thread(()->{
            DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
            Boolean isValid = InformationManager.checkEngagementRecordInformationValidity(List.of(
                    personIdField, hoursSpentField
            ));
            if(Boolean.FALSE.equals(isValid)) {
                Platform.runLater(()->{
                    errorLabel.setText("");
                    errorLabel.setText("Some of the information you have given is incorrect");
                });
                return;
            }
            if(Boolean.TRUE.equals(RecordManager.checkRecords(getEngagement(), Databases.Collections.ENGAGEMENT.toString()))){
                Platform.runLater(()->{
                    errorLabel.setText("");
                    errorLabel.setText("There's already an employee with the exact information");
                });
                return;
            }
            Platform.runLater(()-> errorLabel.setText(""));
            try{
                Document documentToWrite = getEngagement().getDocument();
                databaseManager.updateDocument(objectIdField.getText(), documentToWrite, Databases.Collections.ENGAGEMENT.toString());
                databaseManager.mongoClient.close();
            } catch (Exception e){
                errorLogger.log(e.getMessage());
            }

        });
        myThread.start();
    }
}