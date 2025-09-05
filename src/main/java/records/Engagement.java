package records;

import logging.InfoLogger;
import logging.Logger;
import org.bson.Document;
import records.interfaces.DefaultDataStructure;

import java.time.Duration;

public record Engagement(String objectId, String personId, Duration timeAtWork) implements DefaultDataStructure {

    private static final String personIdField = "personId";
    private static final String timeAtWorkField = "timeWorking";
    static final String INFOLOGGER_PATH = "./logs/info.log.ser";
    @Override
    public Document getDocument() {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("getDocument() method called");
        return new Document()
                .append(personIdField, personId)
                .append(timeAtWorkField, timeAtWork.toHours());
    }

    @Override
    public Document updateDocument(String condition) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("updateDocument() method called");
        Document update = new Document()
                .append("$set", new Document()
                        .append(timeAtWorkField, timeAtWork)
                );

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
        return "Engagement[" + objectId + ", " +  personId + ", " + timeAtWork + "]";
    }
}