package launch;

import handlers.ResourceManager;
import javafx.application.Application;
import javafx.stage.Stage;

public class EmployeeEngagementRecognition extends Application {
    @Override
    public void start(Stage stage) {
        ResourceManager manager = new ResourceManager("/scenes/default.scene.fxml", this, stage);
        manager.loadStage().ifPresent(Stage::show);
    }

    public static void main(String[] args) { launch(); }
}
