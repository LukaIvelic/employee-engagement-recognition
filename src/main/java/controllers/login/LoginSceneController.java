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

import java.util.Properties;

public class LoginSceneController {
    @FXML
    private TextField usernameField;
    @FXML
    private TextField passwordField;
    @FXML
    private Pane rootPane;

    /**
     * Gets GUI data and compares it to backend information
     */
    public void login(){
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
        return String.format("/scenes/%s.%s.record.scene.fxml", manage, rawTitle);
    }

    /**
     * Loads content for managing collections and records: create / update / delete
     */
    private void loadContent(){
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
                ResourceManager.loadContent(filename(manage, title), this).ifPresent(content->{
                    recordContentPane.getChildren().clear();
                    recordContentPane.getChildren().add(content);
                });
            }
        }
    }
}
