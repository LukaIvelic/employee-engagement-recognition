/**
 * Represents a Scene Controller which provides a Pane element for loading content into it
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-06
 */

package controllers;

import handlers.ResourceManager;
import javafx.fxml.FXML;
import javafx.scene.layout.Pane;

public class DefaultSceneController {
    @FXML
    public Pane contentProvider;

    public void initialize() {
        contentProvider.getChildren().clear();
        ResourceManager.loadContent("/scenes/overview.scene.fxml", this).ifPresent(contentProvider.getChildren()::add);
    }
}
