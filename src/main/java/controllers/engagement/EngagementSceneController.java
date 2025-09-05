/**
 * Represents a EngagementSceneController that takes care of charting data and providing info based on charted data
 * This class provides methods to display a PieChart and demotion zone employees
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-12
 */

package controllers.engagement;

import com.mongodb.client.AggregateIterable;
import com.mongodb.client.MongoCollection;
import controllers.overview.OverviewSceneController;
import controllers.overview.OverviewSceneResources;
import database.DatabaseManager;
import database.enums.Databases;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.chart.PieChart;
import javafx.scene.control.Label;
import logging.ErrorLogger;
import logging.Logger;
import org.bson.Document;
import org.bson.conversions.Bson;
import java.util.List;

public class EngagementSceneController {
    @FXML
    private PieChart pieChart;
    @FXML
    private Label demotionZoneLabel;

    /**
     * Calls the fillPieChart method
     */
    public void initialize() {
        fillPieChart();
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
     * Collects data from database and stores it in a pie chart
     */
    public void fillPieChart() {
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        Thread myThread = new Thread(()->{
            try{
                List<OverviewSceneController.EmployeeAndEngagement> employeeAndEngagements = OverviewSceneResources.collectData(getEngagementCollection(), getEmployeeCollection());

                int countUnder0r100 = 0;
                int counter100r200 = 0;
                int counter200over = 0;

                for (OverviewSceneController.EmployeeAndEngagement employeeAndEngagement : employeeAndEngagements) {
                    int timeWorkingValue = Integer.parseInt(employeeAndEngagement.timeAtWork());
                    if(timeWorkingValue <= 100) {
                        countUnder0r100++;
                        Platform.runLater(()->demotionZoneLabel.setText(demotionZoneLabel.getText() + " / " + employeeAndEngagement.firstName() + " " + employeeAndEngagement.lastName()));
                    }
                    else if(timeWorkingValue <= 200) counter100r200++;
                    else counter200over++;
                }

                ObservableList<PieChart.Data> pieChartData = FXCollections.observableArrayList(
                        new PieChart.Data("Employees Under 100", countUnder0r100),
                        new PieChart.Data("Employees Between 100 and 200", counter100r200),
                        new PieChart.Data("Employees Over 200", counter200over)
                );

                Platform.runLater(()->{
                    pieChart.setData(pieChartData);
                    pieChart.setTitle("Employee Hours Spent");
                });
            } catch (Exception e){
                errorLogger.log(e.getMessage());
            }

        });
        myThread.start();
    }
}