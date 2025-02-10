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
import java.util.List;

public class InformationManager {

    private InformationManager(){}

    /**
     * Checks the validity of employee records
     * @param information a list with all the nodes to check
     */
    public static Boolean checkEmployeeRecordInformationValidity(List<Node> information) {
        boolean isValid = true;
        for(Node node : information) {
            if(node instanceof TextField textField && Boolean.FALSE.equals(isTextFieldValid(textField))) {
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
    public static Boolean isTextFieldValid(TextField textField) {
        String text = textField.getText();
        try {
            return text.matches("^\\s*(?:[\\p{L},.\\s]+|\\d+[,. ]*)\\s*$");
        } catch (Exception e) {
            return Boolean.FALSE;
        }
    }

    /**
     * Checks the validity of the selected item
     * @param comboBox a JavaFX component with options
     */
    public static Boolean isComboBoxValid(ComboBox<?> comboBox) {
        try {
            return comboBox.getSelectionModel().getSelectedItem() != null;
        } catch (Exception e) {
            return Boolean.FALSE;
        }
    }

    /**
     * Checks the validity of the selected date
     * @param datePicker a JavaFX component with date selection
     */
    public static Boolean isDatePickerValid(DatePicker datePicker) {
        try {
            return datePicker.getValue().toString().matches("^\\d{4}-\\d{2}-\\d{2}$");
        } catch (Exception e) {
            return Boolean.FALSE;
        }
    }
}