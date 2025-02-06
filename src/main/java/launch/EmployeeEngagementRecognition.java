/**
 * Represents an Employee Engagement Recognition Application that is used for tracking productivity and skill assessment
 * This class provides methods to load the application
 * <p>Example usage:</p>
 * <pre>
 * public void start(Stage stage) {
 *    ResourceManager manager = new ResourceManager("/scenes/default.scene.fxml", this, stage);
 *    manager.loadStage().ifPresent(Stage::show);
 * }
 * </pre>
 * @author Luka Ivelić
 * @version 1.0.1
 * @since 2025-02-04
 */

package launch;

import handlers.ResourceManager;
import javafx.application.Application;
import javafx.stage.Stage;

public class EmployeeEngagementRecognition extends Application {

    public static Stage primaryStage; //NOSONAR

    /**
     * Provides a {@code Stage} instance which is used to load the application
     * @param stage the stage instance
     */
    @Override
    public void start(Stage stage) {
        primaryStage = stage; //NOSONAR
        ResourceManager manager = new ResourceManager("/scenes/default.scene.fxml", this, stage);
        manager.loadStage().ifPresent(Stage::show);
    }

    /**
     * Provides an execution point for the application
     * @param args the application arguments after startup
     */
    public static void main(String[] args) { launch(); }
}
