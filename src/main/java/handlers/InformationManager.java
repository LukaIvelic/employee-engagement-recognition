/**
 * Represents an Information Manager that takes care of validating passed information
 * This class provides methods to check JavaFX component values
 * <p>Example usage:</p>
 * <pre>
 *   Boolean isValid = InformationManager.checkEmployeeRecordInformationValidity(List.of(
 *      firstNameField, lastNameField, salaryField, professionField, dateOfBirthField, genderComboBox
 *   ));
 *  if(Boolean.FALSE.equals(isValid)) {
 *      errorLabel.setText("");
 *      errorLabel.setText("Some of the information you have given is incorrect");
 *      return;
 *  }
 * </pre>
 * @author Luka Ivelić
 * @version 1.0.0
 * @since 2025-02-09
 */

package handlers;

import javafx.scene.Node;
import javafx.scene.control.ComboBox;
import javafx.scene.control.DatePicker;
import javafx.scene.control.TextField;
import logging.InfoLogger;
import logging.Logger;

import java.util.List;

public class InformationManager {
    static final String INFOLOGGER_PATH = "./logs/info.log.ser";

    private InformationManager(){}

    /**
     * Checks the validity of employee records
     * @param information a list with all the nodes to check
     */
    public static Boolean checkEmployeeRecordInformationValidity(List<Node> information) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("checkEmployeeRecordInformationValidity() method called");
        boolean isValid = true;
        for(Node node : information) {
            if(node instanceof TextField textField && Boolean.FALSE.equals(isTextFieldValid(textField, false))) {
                isValid = false;
            }
            if(node instanceof ComboBox<?> comboBox && Boolean.FALSE.equals(isComboBoxValid(comboBox))) {
                isValid = false;
            }
            if(node instanceof DatePicker datePicker && Boolean.FALSE.equals(isDatePickerValid(datePicker))) {
                isValid = false;
            }
        }
        return isValid;
    }

    /**
     * Checks the validity of the text
     * @param textField a JavaFX component with text
     */
    public static Boolean isTextFieldValid(TextField textField, Boolean allowMixed) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("isTextFieldValid() method called");
        String text = textField.getText();
        try {
            if(Boolean.TRUE.equals(allowMixed)) {
                return text.matches("^\\s*[\\p{L}\\d,.\\s]+\\s*$");
            }else{
                return text.matches("^\\s*(?:[\\p{L},.\\s]+|\\d+[,. ]*)\\s*$");
            }
        } catch (Exception _) {
            return Boolean.FALSE;
        }
    }

    /**
     * Checks the validity of the selected item
     * @param comboBox a JavaFX component with options
     */
    public static Boolean isComboBoxValid(ComboBox<?> comboBox) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("isComboBoxValid() method called");
        try {
            return comboBox.getSelectionModel().getSelectedItem() != null;
        } catch (Exception _) {
            return Boolean.FALSE;
        }
    }

    /**
     * Checks the validity of the selected date
     * @param datePicker a JavaFX component with date selection
     */
    public static Boolean isDatePickerValid(DatePicker datePicker) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("isDatePickerValid() method called");
        try {
            return datePicker.getValue().toString().matches("^\\d{4}-\\d{2}-\\d{2}$");
        } catch (Exception _) {
            return Boolean.FALSE;
        }
    }

    public static Boolean checkEngagementRecordInformationValidity(List<TextField> information) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("checkEngagementRecordInformationValidity() method called");
        boolean isValid = true;
        for(TextField textField : information) {
            if(Boolean.FALSE.equals(isTextFieldValid(textField, true))) {
                isValid = false;
            }
        }
        return isValid;
    }
}