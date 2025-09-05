/**
 * Represents a Leaderboard that shows sorted data to display employees with top hours spent working
 * This class provides methods to load data from the database to the JavaFX GUI
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-12
 */

package controllers.leaderboard;

import com.mongodb.client.AggregateIterable;
import com.mongodb.client.MongoCollection;
import controllers.overview.OverviewSceneController;
import controllers.overview.OverviewSceneResources;
import database.DatabaseManager;
import database.enums.Databases;
import javafx.application.Platform;
import javafx.beans.property.SimpleLongProperty;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import org.bson.Document;
import org.bson.conversions.Bson;

import java.util.List;

public class LeaderboardSceneController {
    @FXML
    private TableView<OverviewSceneController.EmployeeAndEngagement> table;
    @FXML
    private TableColumn<OverviewSceneController.EmployeeAndEngagement, String> firstName;
    @FXML
    private TableColumn<OverviewSceneController.EmployeeAndEngagement, String> lastName;
    @FXML
    private TableColumn<OverviewSceneController.EmployeeAndEngagement, Long> hoursSpent;

    /**
     * Initializes TableColumns and calls the fillLeaderboardTable method
     */
    public void initialize(){
        firstName.setCellValueFactory(cellData->new SimpleStringProperty(cellData.getValue().firstName()));
        lastName.setCellValueFactory(cellData->new SimpleStringProperty(cellData.getValue().lastName()));
        hoursSpent.setCellValueFactory(cellData->new SimpleLongProperty(Long.parseLong(cellData.getValue().timeWorking())).asObject());

        fillLeaderboardTable();
    }

    private AggregateIterable<Document> getEmployeeCollection(){
        DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
        MongoCollection<Document> employeeCollection = databaseManager.getCollection(Databases.Collections.EMPLOYEE.toString());
        Bson employeeGroup = new Document("$group", new Document("_id", "$_id")
                .append("firstName", new Document("$first", "$firstName"))
                .append("lastName", new Document("$first", "$lastName"))
        );
        return employeeCollection.aggregate(List.of(employeeGroup));
    }

    private AggregateIterable<Document> getEngagementCollection(){
        DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
        MongoCollection<Document> engagementCollection = databaseManager.getCollection(Databases.Collections.ENGAGEMENT.toString());
        Bson engagementGroup = new Document("$group", new Document("_id", "$personId")
                .append("timeWorking", new Document("$sum", "$timeWorking"))
        );
        return engagementCollection.aggregate(List.of(engagementGroup));
    }

    /**
     * Fills the leaderboard table with the data from the database
     */
    public void fillLeaderboardTable(){
        Thread myThread = new Thread(() -> {
            ObservableList<OverviewSceneController.EmployeeAndEngagement> list = FXCollections.observableArrayList(OverviewSceneResources.collectData(getEngagementCollection(), getEmployeeCollection()));
            Platform.runLater(()->{
                table.setItems(FXCollections.observableArrayList(list));
                table.getSortOrder().add(hoursSpent);
                hoursSpent.setSortType(TableColumn.SortType.DESCENDING);
            });
        });
        myThread.start();
    }
}
