/**
 * Represents a CreateEngagementRecordController that takes care of connecting JavaFX GUI with the backend
 * This class provides methods fetch {@code Engagement} values from GUI and save it in a MongoDB database
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-12
 */

package controllers.create;

import database.DatabaseManager;
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
import records.Engagement;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.List;

public class CreateEngagementRecordController {
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
    private Label previewLabel;

    static final String INFOLOGGER_PATH = "./logs/info.log.ser";

    /**
     * Cancels creating record GUI and returns to overview content Pane
     */
    public void cancelCreateRecord() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("Cancelled engagement record creation");
        ResourceManager.loadOverviewContent(this);
    }

    /**
     * Gets the values from JavaFX GUI and transforms it into an object that's readable to the backend
     * @return Engagement a default record for all engagements
     */
    private Engagement getEngagement() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("Creating engagement object from input fields");
        return new Engagement(
                "mongo_id",
                personIdField.getText(),
                Duration.of(Long.parseLong(hoursSpentField.getText()), ChronoUnit.HOURS)
        );
    }

    /**
     * Used to preview what the record will look like when it is written in the database
     */
    public void previewRecord() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        infoLogger.log("Previewing engagement record");
        previewLabel.setVisible(true);
        previewVBoxLeft.setVisible(true);
        previewVBoxRight.setVisible(true);

        Thread myThread = new Thread(() -> {
            try {
                Platform.runLater(() -> {
                    personIdPreviewLabel.setText("\""+personIdField.getText()+"\"");
                    hoursSpentPreviewLabel.setText("\""+hoursSpentField.getText()+"\"");
                });
            } catch (Exception e) {
                errorLogger.log(e.getMessage());
            }
        });
        myThread.start();
    }

    /**
     * Creates record and writes it to the database
     */
    public void createRecord() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        infoLogger.log("Attempting to create engagement record");
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
            if(Boolean.TRUE.equals(RecordManager.checkRecords(getEngagement(), "engagement"))){
                Platform.runLater(()->{
                    errorLabel.setText("");
                    errorLabel.setText("There's already an employee with the exact information");
                });
                return;
            }
            Platform.runLater(()-> errorLabel.setText(""));
            try {
                Engagement engagement = getEngagement();
                databaseManager.insertDocument(engagement.getDocument(), "engagement");
                databaseManager.mongoClient.close();
            } catch (Exception e) {
                errorLogger.log(e.getMessage());
            }
        });
        myThread.start();
    }
}