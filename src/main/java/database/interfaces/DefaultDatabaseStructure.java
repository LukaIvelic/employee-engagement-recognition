package database.interfaces;

import database.DatabaseManager;
import org.bson.Document;

public sealed interface DefaultDatabaseStructure permits DatabaseManager {
    void insertDocument(Document document, String collectionName);
    void updateDocument(String objectId, Document update, String collectionName);
    void deleteDocument(Document query, String collectionName);
}
