package controllers;

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
import org.bson.Document;
import records.Engagement;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.time.temporal.TemporalUnit;

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
        idColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().objectId()));
        personIdColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().personId()));
        timeAtWorkColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().timeAtWork().toString()));
        loadAllRecords();
    }

    /**
     * Loads the employees into the JavaFX GUI
     */
    public void loadAllRecords() {
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
                            Duration.of(document.getInteger("timeAtWork"), ChronoUnit.HOURS)
                    );
                    engagements.add(gottenEngagement);
                }

                engagementTable.setItems(engagements);
                databaseManager.mongoClient.close();
            } catch (Exception e) {
                e.printStackTrace();
                System.out.println(e.getMessage());
                databaseManager.databaseCondition = DatabaseInfo.ERROR;
                databaseManager.mongoClient.close();
            } finally {
                databaseManager.databaseCondition = DatabaseInfo.AVAILABLE;
                databaseManager.mongoClient.close();
            }
        });
        myThread.start();
    }
}
