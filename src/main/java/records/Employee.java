package records;

import records.interfaces.DefaultDataStructure;
import org.bson.Document;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.LocalDate;

public record Employee(BigInteger employeeId, String firstName, String lastName, Character gender, LocalDate dateOfBirth, BigDecimal salary, String profession) implements DefaultDataStructure {

    private static final String firstNameField = "firstName";
    private static final String lastNameField = "lastName";
    private static final String genderField = "gender";
    private static final String dateOfBirthField = "dateOfBirth";
    private static final String salaryField = "salary";
    private static final String professionField = "profession";

    private Document toDocument() {
        return new Document()
                .append(firstNameField, firstName())
                .append(lastNameField, lastName())
                .append(genderField, gender())
                .append(dateOfBirthField, dateOfBirth())
                .append(salaryField, salary())
                .append(professionField, profession());
    }

    @Override
    public Document insertDocument() {
        return toDocument();
    }

    @Override
    public Document updateDocument(String condition) {
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
        return Document.parse(condition);
    }
}
