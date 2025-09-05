/**
 * Represents a Resource Manager that takes care of getting resources
 * This class provides methods to retrieve an {@code Optional Stage} value to load
 * <p>Example usage:</p>
 * <pre>
 * ResourceManager manager = new ResourceManager("/scenes/default.scene.fxml", this, stage);
 * manager.loadStage().ifPresent(Stage::show);
 * </pre>
 * @author Luka Ivelić
 * @version 1.0.2
 * @since 2025-02-04
 */

package handlers;

import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;
import launch.EmployeeEngagementRecognition;
import logging.ErrorLogger;
import logging.InfoLogger;
import logging.Logger;

import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Objects;
import java.util.Optional;
import java.util.Properties;

public class ResourceManager{

    static final String INFOLOGGER_PATH = "./logs/info.log.ser";
    static final String ERRORLOGGER_PATH = "./logs/error.log.ser";


    /**
     * Constructs a new {@code ResourceManager} with specified resourceName, sender and stage.
     * @param resourceName the relative path to the resource including the filename
     * @param sender the object instance which calls the ResourceManager constructor
     * @param stage the JavaFX Stage object instance
     */
    public ResourceManager(String resourceName, Object sender, Stage stage) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("ResourceManager() constructor called");
        setResourceName(resourceName);
        setSender(sender);
        setStage(stage);
    }

    private String resourceName;
    /**
     * Saves the relative path of a resource
     * @param resourceName the relative path to the resource including the filename
     */
    private void setResourceName(String resourceName) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("setResourceName() method called");
        this.resourceName = resourceName;
    }

    private Object sender;
    /**
     * Saves the object instance which calls the {@code setSender} method
     * @param sender the relative path to the resource including the filename
     */
    private void setSender(Object sender) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("setSender() method called");
        this.sender = sender;
    }

    private Stage stage;
    /**
     * Saves the stage instance which is used to load the JavaFX application
     * @param stage the stage instance
     */
    private void setStage(Stage stage) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("setStage() method called");
        this.stage = stage;
    }

    /**
     * Returns a modified string, converting {@code /scenes/default.scene.fxml} to {@code Default Scene}
     * @return a modified string after applying regex
     */
    private static String createTitle(String rawTitle){
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("createTitle() method called");
        rawTitle = rawTitle
                .replaceAll("/controllers/|/scenes/|\\.fxml", "")
                .replace(".", " ");
        String[] rawTitleParts = rawTitle.split(" ");
        for(int i = 0; i < rawTitleParts.length; i++){
            rawTitleParts[i] = rawTitleParts[i].substring(0, 1).toUpperCase() + rawTitleParts[i].substring(1);
        }
        return String.join(" ", rawTitleParts);
    }

    /**
     * Returns an {@code Optional<Stage>} which has a loaded object ready for use or {@code Optional.empty()}
     * @return an optional stage object to use
     */
    public static Optional<Stage> loadStage (String resourceName, Object sender, Stage stage) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger(ERRORLOGGER_PATH);
        infoLogger.log("loadStage() method called");
        try{
            FXMLLoader fxmlLoader = new FXMLLoader(sender.getClass().getResource(resourceName));
            Scene scene = new Scene(fxmlLoader.load());
            stage.setTitle(createTitle(resourceName));
            stage.setScene(scene);
            return Optional.of(stage);
        }catch(IOException e){
            errorLogger.log(e.getMessage());
            return Optional.empty();
        }
    }

    /**
     * Returns an {@code Optional<Stage>} which has a loaded object ready for use or {@code Optional.empty()}
     * @return an optional stage object to use
     */
    public Optional<Stage> loadStage () {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("loadStage() method called");
        return loadStage(resourceName, sender, stage);
    }

    /**
     * Returns an {@code Optional<Pane>} which has a loaded object ready for use or {@code Optional.empty()}
     * @return an optional pane object to use
     */
    public static Optional<Pane> loadContent (String resourceName, Object sender) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger(ERRORLOGGER_PATH);
        infoLogger.log("loadContent() method called");
        try{
            FXMLLoader fxmlLoader = new FXMLLoader(sender.getClass().getResource(resourceName));
            Pane root = fxmlLoader.load();
            return Optional.of(root);
        }catch(IOException e){
            errorLogger.log(e.getMessage());
            return Optional.empty();
        }
    }

    /**
     * Uses {@code loadContent} method to load overview content Pane, created to reduce boilerplate code
     */
    public static void loadOverviewContent (Object sender) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger(ERRORLOGGER_PATH);
        infoLogger.log("loadOverviewContent() method called");
        try{
            loadContent("/scenes/overview.scene.fxml", sender).ifPresent(content -> {
                EmployeeEngagementRecognition.primaryStage.setTitle("Employee Engagement Recognition | Overview");
                Pane rootPane = (Pane)EmployeeEngagementRecognition.primaryStage.getScene().getRoot();
                Pane contentPane =  (Pane)rootPane.getChildren().getLast();
                contentPane.getChildren().clear();
                contentPane.getChildren().add(content);
            });
        } catch (Exception e){
            errorLogger.log(e.getMessage());
        }

    }

    /**
     * Loads properties by passing property name to the method
     * @return Properties
     */
    public static Properties loadProperties(String propertiesName, Object sender) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        Logger errorLogger = new ErrorLogger(ERRORLOGGER_PATH);
        infoLogger.log("loadProperties() method called");
        URL resourceURL = sender.getClass().getResource(propertiesName);
        Objects.requireNonNull(resourceURL);

        try(FileInputStream fis = new FileInputStream(resourceURL.getFile())){
            Properties properties = new Properties();
            properties.load(fis);
            return properties;
        }catch(IOException | NullPointerException e){
            errorLogger.log(e.getMessage());
            return new Properties();
        }
    }
}