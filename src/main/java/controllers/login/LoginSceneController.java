/**
 * Represents a LoginSceneController that takes care of verifying login input data and displaying controllers
 * This class provides methods load properties and provide logic for displaying controllers
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-12
 */

package controllers.login;

import handlers.ResourceManager;
import javafx.fxml.FXML;
import javafx.scene.Node;
import javafx.scene.control.ComboBox;
import javafx.scene.control.TextField;
import javafx.scene.layout.Pane;
import logging.ErrorLogger;
import logging.InfoLogger;
import logging.Logger;

import java.util.Properties;

public class LoginSceneController {
    @FXML
    private TextField usernameField;
    @FXML
    private TextField passwordField;
    @FXML
    private Pane rootPane;

    static final String INFOLOGGER_PATH = "./logs/info.log.ser";

    /**
     * Gets GUI data and compares it to backend information
     */
    public void login(){
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("login() method called");

        Properties properties =  ResourceManager.loadProperties("/properties/login.properties", this);

        String adminUsername = properties.getProperty("username_admin");
        String adminPassword = properties.getProperty("password_admin");
        final String adminPass = adminUsername+adminPassword;

        String usernameEntered = usernameField.getText();
        String passwordEntered = passwordField.getText();
        final String enteredPass = usernameEntered+passwordEntered;

        if(adminPass.equals(enteredPass)){
            loadContent();
        }
    }

    private String filename(String manage, String rawTitle){
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("filename() method called");
        return String.format("/scenes/%s.%s.record.scene.fxml", manage, rawTitle);
    }

    /**
     * Loads content for managing collections and records: create / update / delete
     */
    private void loadContent(){
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        infoLogger.log("loadContent() method called");
        Pane parentPane = (Pane) rootPane.getParent().getParent();
        Pane recordContentPane = (Pane) rootPane.getParent();

        String manage = "";

        switch(parentPane.getId()){
            case "createRecordPane":
                manage = "create";
                break;
            case "updateRecordPane":
                manage = "update";
                break;
            case "deleteRecordPane":
                manage = "delete";
                break;
            default:
                break;
        }

        for (Node child : parentPane.getChildren()) {
            if(child instanceof ComboBox<?> comboBox){
                String title = comboBox.getSelectionModel().getSelectedItem().toString();
                try{
                    ResourceManager.loadContent(filename(manage, title), this).ifPresent(content->{
                        recordContentPane.getChildren().clear();
                        recordContentPane.getChildren().add(content);
                    });
                } catch (Exception e){
                    errorLogger.log(e.getMessage());
                }

            }
        }
    }
}