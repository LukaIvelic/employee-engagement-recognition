package records;

import org.bson.Document;
import records.interfaces.DefaultDataStructure;

import java.time.Duration;

public record Engagement(String objectId, String personId, Duration timeAtWork) implements DefaultDataStructure {

    private static final String timeAtWorkField = "timeAtWork";

    @Override
    public Document getDocument() {
        return new Document()
                .append(timeAtWorkField, timeAtWork);
    }

    @Override
    public Document updateDocument(String condition) {
        Document update = new Document()
                .append("$set", new Document()
                .append(timeAtWorkField, timeAtWork)
        );

        Document query = Document.parse(condition);
        return new Document("query", query).append("update", update);
    }

    @Override
    public Document deleteDocument(String condition) {
        return Document.parse(condition);
    }

    @Override
    public String toString() {
        return "Engagement[" + objectId + ", " +  personId + ", " + timeAtWork + "]";
    }
}
