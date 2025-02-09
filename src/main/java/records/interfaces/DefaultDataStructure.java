package records.interfaces;

import org.bson.Document;

public interface DefaultDataStructure {
    Document insertDocument();
    Document updateDocument(String condition);
    Document deleteDocument(String condition);
}
