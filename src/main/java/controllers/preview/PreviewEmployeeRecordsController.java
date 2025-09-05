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
import records.Employee;
import java.math.BigDecimal;
import java.time.ZoneId;

public class PreviewEmployeeRecordsController {

    @FXML
    private TableView<Employee> employeeTable;
    @FXML
    private TableColumn<Employee, String> idColumn;
    @FXML
    private TableColumn<Employee, String> firstNameColumn;
    @FXML
    private TableColumn<Employee, String> lastNameColumn;
    @FXML
    private TableColumn<Employee, String> genderColumn;
    @FXML
    private TableColumn<Employee, String> dateOfBirthColumn;
    @FXML
    private TableColumn<Employee, String> salaryColumn;
    @FXML
    private TableColumn<Employee, String> professionColumn;

    /**
     * Loads the employees into the JavaFX GUI upon initializing
     */
    public void initialize() {
        Logger infoLogger = new InfoLogger("./logs/info.log.ser");
        infoLogger.log("initialize() method called");
        idColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().employeeId()));
        firstNameColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().firstName()));
        lastNameColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().lastName()));
        genderColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().gender().toString()));
        dateOfBirthColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().dateOfBirth().toString()));
        salaryColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().salary().toString()));
        professionColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().profession()));
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
                MongoCollection<Document> collection = databaseManager.getCollection("employee");
                ObservableList<Employee> employees = FXCollections.observableArrayList();
                for(Document document: collection.find()) {
                    Employee gottenEmployee = new Employee(
                            document.get("_id").toString(),
                            document.get("firstName").toString(),
                            document.get("lastName").toString(),
                            document.get("gender").toString().toCharArray()[0],
                            document.getDate("dateOfBirth").toInstant().atZone(ZoneId.systemDefault()).toLocalDate(),
                            new BigDecimal(document.get("salary").toString()),
                            document.get("profession").toString()
                    );
                    employees.add(gottenEmployee);
                }
                employeeTable.setItems(employees);
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