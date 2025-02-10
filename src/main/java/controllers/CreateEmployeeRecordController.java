/**
 * Represents a CreateEmployeeRecordController that takes care of connecting JavaFX GUI with the backend
 * This class provides methods fetch {@code Employee} values from GUI and save it in a MongoDB database
 * @author Luka Ivelić
 * @version 1.0.1
 * @since 2025-02-09
 */

package controllers;

import com.mongodb.client.MongoCollection;
import database.DatabaseManager;
import handlers.InformationManager;
import handlers.ResourceManager;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;
import org.bson.Document;
import records.Employee;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

public class CreateEmployeeRecordController {

    private final ObservableList<String> genderList = FXCollections.observableArrayList("Male", "Female", "Other");
    private final DatabaseManager databaseManager = new DatabaseManager("employee_engagement_recognition");

    @FXML
    private TextField firstNameField;
    @FXML
    private TextField lastNameField;
    @FXML
    private TextField salaryField;
    @FXML
    private TextField professionField;
    @FXML
    private DatePicker dateOfBirthField;
    @FXML
    private ComboBox<String> genderComboBox;
    @FXML
    private Label errorLabel;
    @FXML
    private Label previewLabel;
    @FXML
    private VBox previewVBoxLeft;
    @FXML
    private VBox previewVBoxRight;
    @FXML
    private Label firstNamePreviewLabel;
    @FXML
    private Label lastNamePreviewLabel;
    @FXML
    private Label genderPreviewLabel;
    @FXML
    private Label dateOfBirthPreviewLabel;
    @FXML
    private Label salaryPreviewLabel;
    @FXML
    private Label professionPreviewLabel;

    /**
     * Sets the genderComboBox items with predefined values from genderList list variable
     */
    public void initialize() {
        genderComboBox.setItems(genderList);
    }

    /**
     * Cancels creating record GUI and returns to overview content Pane
     */
    public void cancelCreateRecord() {
        ResourceManager.loadOverviewContent(this);
    }

    /**
     * Gets the values from JavaFX GUI and transforms it into an object that's readable to the backend
     * @return Employee a default record for all the employees
     */
    private Employee getEmployee() {
        Character gender = genderComboBox.getSelectionModel().getSelectedItem().substring(0, 1).toUpperCase().toCharArray()[0];
        LocalDate dateOfBirth = dateOfBirthField.getValue();
        BigDecimal salary = new BigDecimal(salaryField.getText());

        return new Employee(
                "mongo_id",
                firstNameField.getText(),
                lastNameField.getText(),
                gender,
                dateOfBirth,
                salary,
                professionField.getText()
        );
    }

    /**
     * Checks the database if there's already an existing record written
     * @return Boolean - true if there is more than 1 record that matches the employee given and false if there is not
     */
    private Boolean checkRecords(Employee employee) {
        MongoCollection<Document> collection = databaseManager.getCollection("employee");
        return collection.countDocuments(employee.getDocument()) > 0;
    }

    /**
     * Used to preview what the record will look like when it is written in the database
     */
    public void previewRecord() {
        previewLabel.setVisible(true);
        previewVBoxLeft.setVisible(true);
        previewVBoxRight.setVisible(true);

        try{
            firstNamePreviewLabel.setText("\""+firstNameField.getText()+"\"");
            lastNamePreviewLabel.setText("\""+lastNameField.getText()+"\"");
            salaryPreviewLabel.setText(salaryField.getText());
            professionPreviewLabel.setText("\""+professionField.getText()+"\"");
            dateOfBirthPreviewLabel.setText(dateOfBirthField.getValue() == null ? LocalDate.now().toString() : dateOfBirthField.getValue().toString());
            genderPreviewLabel.setText("\""+genderComboBox.getSelectionModel().getSelectedItem().substring(0, 1).toUpperCase()+"\"");
        }catch (Exception e){

        }
    }

    /**
     * Creates record and writes it to the database
     */
    public void createRecord() {
        Boolean isValid = InformationManager.checkEmployeeRecordInformationValidity(List.of(
            firstNameField, lastNameField, salaryField, professionField, dateOfBirthField, genderComboBox
        ));

        if(Boolean.FALSE.equals(isValid)) {
            errorLabel.setText("");
            errorLabel.setText("Some of the information you have given is incorrect");
            return;
        }
        if(Boolean.TRUE.equals(checkRecords(getEmployee()))){
            errorLabel.setText("");
            errorLabel.setText("There's already an employee with the exact information");
            return;
        }

        errorLabel.setText("");

        Employee employee = getEmployee();
        databaseManager.insertDocument(employee.getDocument(), "employee");
        databaseManager.mongoClient.close();
    }
}