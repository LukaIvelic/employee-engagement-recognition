package database.interfaces;

import org.bson.Document;

public interface DefaultDatabaseStructure {
    void insertDocument(Document document, String collectionName);
    void updateDocument(Document query, Document update, String collectionName);
    void deleteDocument(Document query, String collectionName);
}
