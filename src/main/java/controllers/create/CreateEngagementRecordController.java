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

    /**
     * Cancels creating record GUI and returns to overview content Pane
     */
    public void cancelCreateRecord() {
        ResourceManager.loadOverviewContent(this);
    }

    /**
     * Gets the values from JavaFX GUI and transforms it into an object that's readable to the backend
     * @return Engagement a default record for all engagements
     */
    private Engagement getEngagement() {
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
        previewLabel.setVisible(true);
        previewVBoxLeft.setVisible(true);
        previewVBoxRight.setVisible(true);

        Thread myThread = new Thread(() -> Platform.runLater(() -> {
            personIdPreviewLabel.setText("\""+personIdField.getText()+"\"");
            hoursSpentPreviewLabel.setText("\""+hoursSpentField.getText()+"\"");
        }));
        myThread.start();
    }

    /**
     * Creates record and writes it to the database
     */
    public void createRecord() {
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
            Engagement engagement = getEngagement();
            databaseManager.insertDocument(engagement.getDocument(), "engagement");
            databaseManager.mongoClient.close();
        });
        myThread.start();
    }
}
