package handlers;

import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.Optional;

public class ResourceManager{
    public ResourceManager() {}

    public ResourceManager(String resourceName, Object sender, Stage stage) {
        setResourceName(resourceName);
        setSender(sender);
        setStage(stage);
    }

    private String resourceName;
    private void setResourceName(String resourceName) {this.resourceName = resourceName;}

    private Object sender;
    private void setSender(Object sender) {this.sender = sender;}

    private Stage stage;
    private void setStage(Stage stage) {this.stage = stage;}

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

    public Optional<Stage> loadStage () {
        return loadStage(resourceName, sender, stage);
    }
}
