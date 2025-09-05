/**
 * Represents an Employee class that represents data for storing into the database
 * This class provides methods to manipulate database documents
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-09
 */

package records;

import logging.InfoLogger;
import logging.Logger;
import records.interfaces.DefaultDataStructure;
import org.bson.Document;
import java.math.BigDecimal;
import java.time.LocalDate;

public record Employee(String employeeId, String firstName, String lastName, Character gender, LocalDate dateOfBirth, BigDecimal salary, String profession) implements DefaultDataStructure {

    private static final String firstNameField = "firstName";
    private static final String lastNameField = "lastName";
    private static final String genderField = "gender";
    private static final String dateOfBirthField = "dateOfBirth";
    private static final String salaryField = "salary";
    private static final String professionField = "profession";

    static final String INFOLOGGER_PATH = "./logs/info.log.ser";

    private Document toDocument() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("toDocument() method called");
        return new Document()
                .append(firstNameField, firstName())
                .append(lastNameField, lastName())
                .append(genderField, gender())
                .append(dateOfBirthField, dateOfBirth())
                .append(salaryField, salary())
                .append(professionField, profession());
    }

    @Override
    public Document getDocument() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("getDocument() method called");
        return toDocument();
    }

    @Override
    public Document updateDocument(String condition) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("updateDocument() method called");
        Document update = new Document().append("$set", new Document()
                .append(firstNameField, firstName())
                .append(lastNameField, lastName())
                .append(genderField, gender())
                .append(dateOfBirthField, dateOfBirth())
                .append(salaryField, salary())
                .append(professionField, profession()));

        Document query = Document.parse(condition);
        return new Document("query", query).append("update", update);
    }

    @Override
    public Document deleteDocument(String condition) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("deleteDocument() method called");
        return Document.parse(condition);
    }

    @Override
    public String toString() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("toString() method called");
        return "Employee[" +
                firstName + ", " +
                lastName + ", " +
                gender + ", " +
                dateOfBirth + ", " +
                salary + ", " +
                profession + "]";
    }
}