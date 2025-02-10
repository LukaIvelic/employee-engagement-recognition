/**
 * Represents a PreviewAllEmployeeController which is used to display all the employees in the database
 * This class provides methods load employee information into JavaFX GUI
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-09
 */

package controllers;

import com.mongodb.client.MongoCollection;
import database.DatabaseManager;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import org.bson.Document;
import records.Employee;

import java.math.BigDecimal;
import java.time.ZoneId;

public class PreviewAllEmployeesController {
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
    @FXML
    private Label employeeCountLabel;

    /**
     * Loads the employees into the JavaFX GUI upon initializing
     */
    public void initialize() {
        loadAllEmployees();
    }

    /**
     * Loads the employees into the JavaFX GUI
     */
    public void loadAllEmployees() {
        idColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().employeeId()));
        firstNameColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().firstName()));
        lastNameColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().lastName()));
        genderColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().gender().toString()));
        dateOfBirthColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().dateOfBirth().toString()));
        salaryColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().salary().toString()));
        professionColumn.setCellValueFactory(cellData -> new SimpleStringProperty(cellData.getValue().profession()));

        DatabaseManager databaseManager = new DatabaseManager("employee_engagement_recognition");
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
        employeeCountLabel.setText(String.valueOf(employees.size()));
        databaseManager.mongoClient.close();
    }
}
