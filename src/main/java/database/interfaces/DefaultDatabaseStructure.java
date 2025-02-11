package database.interfaces;

import org.bson.Document;

public interface DefaultDatabaseStructure {
    void insertDocument(Document document, String collectionName);
    void updateDocument(String objectId, Document update, String collectionName);
    void deleteDocument(Document query, String collectionName);
}
