/**
 * Represents a OverviewSceneController that displays basic information
 * This class provides methods that take care of transforming data into queries from JavaFX GUI
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-12
 */

package controllers.overview;

import com.mongodb.client.AggregateIterable;
import com.mongodb.client.MongoCollection;
import database.DatabaseManager;
import database.enums.Databases;
import javafx.application.Platform;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.chart.BarChart;
import javafx.scene.chart.XYChart;
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.ToggleButton;
import org.bson.Document;
import org.bson.conversions.Bson;
import java.util.List;

public class OverviewSceneController {

    public record EmployeeAndEngagement(String firstName, String lastName, String timeWorking) {
        public String timeAtWork() {
            return this.timeWorking;
        }
    }

    @FXML
    private TableView<EmployeeAndEngagement> topEmployeeTable;
    @FXML
    private TableColumn<EmployeeAndEngagement, String> firstNameColumnTopEmployees;
    @FXML
    private TableColumn<EmployeeAndEngagement, String> lastNameColumnTopEmployees;
    @FXML
    private TableColumn<EmployeeAndEngagement, String> hoursSpentColumnTopEmployees;
    @FXML
    private TableView<EmployeeAndEngagement> burnoutEmployeeTable;
    @FXML
    private TableColumn<EmployeeAndEngagement, String> firstNameColumnBurnouts;
    @FXML
    private TableColumn<EmployeeAndEngagement, String> lastNameColumnBurnouts;
    @FXML
    private TableColumn<EmployeeAndEngagement, String> hoursSpentColumnBurnouts;
    @FXML
    private Label totalWorkHours;
    @FXML
    private BarChart<String, Long> workPerEmployeeChart;

    public void initialize(){
        getTopEmployees();
        getBurnoutEmployees();
        calculateAverageMonthlyWorkHours();
        fillBarChart();
        Thread myThread = new Thread(() -> Platform.runLater(() -> {
            firstNameColumnTopEmployees.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().firstName()));
            lastNameColumnTopEmployees.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().lastName()));
            hoursSpentColumnTopEmployees.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().timeAtWork()));

            firstNameColumnBurnouts.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().firstName()));
            lastNameColumnBurnouts.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().lastName()));
            hoursSpentColumnBurnouts.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().timeAtWork()));
        }));
        myThread.start();
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

    public void getTopEmployees() {
        Thread myThread = new Thread(() -> {
            DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
            AggregateIterable<Document> engagements = getEngagementCollection();
            AggregateIterable<Document> employees = getEmployeeCollection();
            ObservableList<EmployeeAndEngagement> list = FXCollections.observableArrayList(OverviewSceneResources.getTopEmployees(engagements, employees));
            Platform.runLater(()-> topEmployeeTable.setItems(list));
            databaseManager.mongoClient.close();
        });
        myThread.start();
    }

    public void getBurnoutEmployees() {
        Thread myThread = new Thread(() -> {
            DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
            AggregateIterable<Document> engagements = getEngagementCollection();
            AggregateIterable<Document> employees = getEmployeeCollection();
            ObservableList<EmployeeAndEngagement> list = FXCollections.observableArrayList(OverviewSceneResources.getBurnoutEmployees(engagements, employees));
            Platform.runLater(()-> burnoutEmployeeTable.setItems(list));
            databaseManager.mongoClient.close();
        });
        myThread.start();
    }

    public void autoRefresh(ActionEvent event) {
        Thread myThread = new Thread(() -> {
            ToggleButton sender = (ToggleButton)event.getSource();
            while (Boolean.TRUE.equals(sender.isSelected())){
                try{
                    getTopEmployees();
                    getBurnoutEmployees();
                    calculateAverageMonthlyWorkHours();
                    synchronized(this){
                        wait(1000);
                    }
                }catch (InterruptedException e){
                    Thread.currentThread().interrupt();
                }
            }
        });
        myThread.start();
    }

    public void calculateAverageMonthlyWorkHours() {
        Thread myThread = new Thread(() -> {
           AggregateIterable<Document> engagements = getEngagementCollection();
           Long sum = 0L;
           long counter = 0L;
           for(Document engagement : engagements) {
               sum+=engagement.getLong("timeWorking");
               counter++;
           }
           long average = sum/counter;
           Platform.runLater(()-> totalWorkHours.setText(average + " hours / month"));
        });
        myThread.start();
    }

    public void fillBarChart(){
        workPerEmployeeChart.getData().clear();
        Thread myThread = new Thread(() -> {
            List<EmployeeAndEngagement> barChartData = OverviewSceneResources.collectData(getEngagementCollection(), getEmployeeCollection());
            XYChart.Series<String, Long> series = new XYChart.Series<>();
            series.setName("Hours Spent");
            for(EmployeeAndEngagement employeeAndEngagement : barChartData){
                series.getData().add(new XYChart.Data<>(employeeAndEngagement.lastName, Long.parseLong(employeeAndEngagement.timeWorking)));
            }
            Platform.runLater(()->{
                workPerEmployeeChart.getData().clear();
                workPerEmployeeChart.getData().add(series);
            });
        });
        myThread.start();
    }
}