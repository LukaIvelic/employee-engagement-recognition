/**
 * Represents a Scene Controller which provides a Pane element for loading content into it
 * @author Luka Ivelić
 * @version 1.0.1
 * @since 2025-02-09
 */

package controllers;

import handlers.ResourceManager;
import javafx.fxml.FXML;
import javafx.scene.layout.Pane;

public class DefaultSceneController {
    @FXML
    public Pane contentProvider;

    /**
     * Initializes the default GUI with an overview content Pane
     */
    public void initialize() {
        contentProvider.getChildren().clear();
        ResourceManager.loadContent("/scenes/overview.scene.fxml", this).ifPresent(contentProvider.getChildren()::add);
    }
}
