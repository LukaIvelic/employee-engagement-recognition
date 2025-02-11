package controllers;

import com.mongodb.client.MongoCollection;
import database.DatabaseManager;
import database.enums.DatabaseInfo;
import database.enums.Databases;
import handlers.InformationManager;
import handlers.ResourceManager;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.scene.control.ComboBox;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.layout.VBox;
import org.bson.Document;
import org.bson.types.ObjectId;
import records.Employee;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

public class UpdateEmployeeRecordController {
    private final ObservableList<String> genderList = FXCollections.observableArrayList("Male", "Female", "Other");
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
    @FXML
    private TextField objectIdField;
    @FXML
    private Label previewWrittenLabel;

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
        return new Employee("mongo_id", firstNameField.getText(), lastNameField.getText(), gender, dateOfBirth, salary, professionField.getText());
    }

    /**
     * Checks the database if there's already an existing record written
     * @return Boolean - true if there is more than 1 record that matches the employee given and false if there is not
     */
    private Boolean checkRecords(Employee employee) {
        DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
        MongoCollection<Document> collection = databaseManager.getCollection("employee");
        boolean doesExist = collection.countDocuments(employee.getDocument()) > 0;
        databaseManager.mongoClient.close();
        return doesExist;
    }

    public synchronized void previewWrittenObject(){
        DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
        try{
            databaseManager.databaseCondition = DatabaseInfo.UNAVAILABLE;
            String writtenID = objectIdField.getText();
            List<Document> documents = new ArrayList<>();
            if(objectIdField.getText().isEmpty()){
                errorLabel.setText("You haven't entered an ID");
                return;
            }else{
                errorLabel.setText("");
            }
            databaseManager.getCollection("employee").find().into(documents);
            boolean hasDocument = false;
            for(Document document: documents){
                if(document.getObjectId("_id").equals(new ObjectId(writtenID))){
                    previewWrittenLabel.setText(document.toJson());
                    hasDocument = true;
                }
            }
            if(Boolean.FALSE.equals(hasDocument)){
                errorLabel.setText("There isn't no such record in the database");
            }
            databaseManager.mongoClient.close();
        } catch (Exception e) {
            databaseManager.databaseCondition = DatabaseInfo.ERROR;
            databaseManager.mongoClient.close();
            errorLabel.setText("An error occurred");
        } finally {
            databaseManager.databaseCondition = DatabaseInfo.AVAILABLE;
            databaseManager.mongoClient.close();
        }
    }

    /**
     * Used to preview what the record will look like when it is written in the database
     */
    public void previewRecord() {
        Thread myThread = new Thread(() -> {
            previewVBoxLeft.setVisible(true);
            previewVBoxRight.setVisible(true);
            try{
                Platform.runLater(() -> {
                    firstNamePreviewLabel.setText("\""+firstNameField.getText()+"\"");
                    lastNamePreviewLabel.setText("\""+lastNameField.getText()+"\"");
                    salaryPreviewLabel.setText(salaryField.getText());
                    professionPreviewLabel.setText("\""+professionField.getText()+"\"");
                    dateOfBirthPreviewLabel.setText(dateOfBirthField.getValue() == null ? LocalDate.now().toString() : dateOfBirthField.getValue().toString());
                    genderPreviewLabel.setText("\""+genderComboBox.getSelectionModel().getSelectedItem().substring(0, 1).toUpperCase()+"\"");
                });
            }catch (Exception e){

            }
        });
        myThread.start();
    }

    /**
     * Creates record and writes it to the database
     */
    public void updateRecord() {
        Thread myThread = new Thread(()->{
            DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
            Boolean isValid = InformationManager.checkEmployeeRecordInformationValidity(List.of(
                    firstNameField, lastNameField, salaryField, professionField, dateOfBirthField, genderComboBox
            ));
            if(Boolean.FALSE.equals(isValid)) {
                Platform.runLater(()->{
                    errorLabel.setText("");
                    errorLabel.setText("Some of the information you have given is incorrect");
                });
                return;
            }
            if(Boolean.TRUE.equals(checkRecords(getEmployee()))){
                Platform.runLater(()->{
                    errorLabel.setText("");
                    errorLabel.setText("There's already an employee with the exact information");
                });
                return;
            }
            Platform.runLater(()-> errorLabel.setText(""));
            Document documentToWrite = getEmployee().getDocument();
            databaseManager.updateDocument(objectIdField.getText(), documentToWrite, "employee");
            databaseManager.mongoClient.close();
        });
        myThread.start();
    }
}