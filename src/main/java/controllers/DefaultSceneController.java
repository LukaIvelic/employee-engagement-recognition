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
import logging.ErrorLogger;
import logging.InfoLogger;
import logging.Logger;

public class DefaultSceneController {
    @FXML
    public Pane contentProvider;

    /**
     * Initializes the default GUI with an overview content Pane
     */
    public void initialize() {
        Logger infoLogger = new InfoLogger("./logs/info.log.ser");
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        infoLogger.log("initialize() method called");
        contentProvider.getChildren().clear();
        try{
            ResourceManager.loadContent("/scenes/overview.scene.fxml", this).ifPresent(contentProvider.getChildren()::add);
        } catch (Exception e){
            errorLogger.log(e.getMessage());
        }

    }
}