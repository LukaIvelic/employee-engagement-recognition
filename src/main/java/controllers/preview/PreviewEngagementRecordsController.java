package controllers.preview;

import com.mongodb.client.MongoCollection;
import database.DatabaseManager;
import database.enums.DatabaseInfo;
import database.enums.Databases;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import logging.ErrorLogger;
import logging.InfoLogger;
import logging.Logger;
import org.bson.Document;
import records.Engagement;
import java.time.Duration;
import java.time.temporal.ChronoUnit;

public class PreviewEngagementRecordsController {

    @FXML
    private TableView<Engagement> engagementTable;
    @FXML
    private TableColumn<Engagement, String> idColumn;
    @FXML
    private TableColumn<Engagement, String> personIdColumn;
    @FXML
    private TableColumn<Engagement, String> timeAtWorkColumn;

    public void initialize() {
        Logger infoLogger = new InfoLogger("./logs/info.log.ser");
        infoLogger.log("initialize() method called");
        idColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().objectId()));
        personIdColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().personId()));
        timeAtWorkColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().timeAtWork().toHours() + " Hours"));
        loadAllRecords();
    }

    /**
     * Loads the employees into the JavaFX GUI
     */
    public void loadAllRecords() {
        Logger infoLogger = new InfoLogger("./logs/info.log.ser");
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        infoLogger.log("loadAllRecords() method called");
        Thread myThread = new Thread(() -> {
            DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
            try {
                databaseManager.databaseCondition = DatabaseInfo.UNAVAILABLE;
                MongoCollection<Document> collection = databaseManager.getCollection("engagement");
                ObservableList<Engagement> engagements = FXCollections.observableArrayList();
                for(Document document: collection.find()) {
                    Engagement gottenEngagement = new Engagement(
                            document.get("_id").toString(),
                            document.get("personId").toString(),
                            Duration.of(document.getLong("timeWorking"), ChronoUnit.HOURS)
                    );
                    engagements.add(gottenEngagement);
                }
                engagementTable.setItems(engagements);
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
}