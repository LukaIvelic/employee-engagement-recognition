/**
 * Represents a Resource Manager that takes care of getting resources
 * This class provides methods to retrieve an {@code Optional Stage} value to load
 * <p>Example usage:</p>
 * <pre>
 * ResourceManager manager = new ResourceManager("/scenes/default.scene.fxml", this, stage);
 * manager.loadStage().ifPresent(Stage::show);
 * </pre>
 * @author Luka Ivelić
 * @version 1.0.1
 * @since 2025-02-04
 */
package handlers;

import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.Pane;
import javafx.stage.Stage;
import java.io.IOException;
import java.util.Optional;

public class ResourceManager{

    /**
     * Constructs a new {@code ResourceManager} with specified resourceName, sender and stage.
     * @param resourceName the relative path to the resource including the filename
     * @param sender the object instance which calls the ResourceManager constructor
     * @param stage the JavaFX Stage object instance
     */
    public ResourceManager(String resourceName, Object sender, Stage stage) {
        setResourceName(resourceName);
        setSender(sender);
        setStage(stage);
    }

    private String resourceName;
    /**
     * Saves the relative path of a resource
     * @param resourceName the relative path to the resource including the filename
     */
    private void setResourceName(String resourceName) {this.resourceName = resourceName;}

    private Object sender;
    /**
     * Saves the object instance which calls the {@code setSender} method
     * @param sender the relative path to the resource including the filename
     */
    private void setSender(Object sender) {this.sender = sender;}

    private Stage stage;
    /**
     * Saves the stage instance which is used to load the JavaFX application
     * @param stage the stage instance
     */
    private void setStage(Stage stage) {this.stage = stage;}

    /**
     * Returns a modified string, converting {@code /scenes/default.scene.fxml} to {@code Default Scene}
     * @return a modified string after applying regex
     */
    private static String createTitle(String rawTitle){
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
        try{
            FXMLLoader fxmlLoader = new FXMLLoader(sender.getClass().getResource(resourceName));
            Scene scene = new Scene(fxmlLoader.load());
            stage.setTitle(createTitle(resourceName));
            stage.setScene(scene);
            return Optional.of(stage);
        }catch(IOException e){
            return Optional.empty();
        }
    }

    /**
     * Returns an {@code Optional<Stage>} which has a loaded object ready for use or {@code Optional.empty()}
     * @return an optional stage object to use
     */
    public Optional<Stage> loadStage () {
        return loadStage(resourceName, sender, stage);
    }

    /**
     * Returns an {@code Optional<Pane>} which has a loaded object ready for use or {@code Optional.empty()}
     * @return an optional pane object to use
     */
    public static Optional<Pane> loadContent (String resourceName, Object sender) {
        try{
            FXMLLoader fxmlLoader = new FXMLLoader(sender.getClass().getResource(resourceName));
            Pane root = fxmlLoader.load();
            return Optional.of(root);
        }catch(IOException e){
            return Optional.empty();
        }
    }

    /**
     * Returns an {@code Optional<Pane>} which has a loaded object ready for use or {@code Optional.empty()}
     * @return an optional pane object to use
     */
    public Optional<Pane> loadContent () {
        return loadContent(resourceName, sender);
    }
}