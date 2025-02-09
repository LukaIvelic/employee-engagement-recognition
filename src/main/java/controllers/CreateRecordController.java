package controllers;

import database.DatabaseManager;
import handlers.ResourceManager;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.DatePicker;
import javafx.scene.control.TextField;
import records.Employee;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;

public class CreateRecordController {

    private final ObservableList<String> genderList = FXCollections.observableArrayList("Male", "Female", "Other");
    private final DatabaseManager databaseManager = new DatabaseManager("employee_engagement_recognition");

    @FXML
    TextField firstNameField;
    @FXML
    TextField lastNameField;
    @FXML
    TextField salaryField;
    @FXML
    TextField professionField;
    @FXML
    DatePicker dateOfBirthField;
    @FXML
    ComboBox<String> genderComboBox;

    public void initialize() {
        genderComboBox.setItems(genderList);
    }

    public void cancelCreateRecord() {
        ResourceManager.loadOverviewContent(this);
    }

    public void createRecord() {
        Character gender = genderComboBox.getSelectionModel().getSelectedItem().substring(0, 1).toUpperCase().toCharArray()[0];
        LocalDate dateOfBirth = dateOfBirthField.getValue();
        BigDecimal salary = new BigDecimal(salaryField.getText());

        Employee employee = new Employee(
                BigInteger.valueOf(1),
                firstNameField.getText(),
                lastNameField.getText(),
                gender,
                dateOfBirth,
                salary,
                professionField.getText()
        );

        databaseManager.insertDocument(employee.insertDocument(), "employee");
    }
}
