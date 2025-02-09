package database;

import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import database.enums.DatabaseInfo;
import database.interfaces.DefaultDatabaseStructure;
import org.bson.Document;

public class DatabaseManager implements DefaultDatabaseStructure {

    public DatabaseInfo databaseCondition = DatabaseInfo.AVAILABLE; //NOSONAR
    private MongoDatabase database;

    public DatabaseManager(String databaseName) {
        try {
            MongoClient mongoClient = new MongoClientConnection().getMongoClient();
            database = mongoClient.getDatabase(databaseName);
        } catch (Exception e) {
            databaseCondition = DatabaseInfo.ERROR;
        } finally {
            databaseCondition = DatabaseInfo.AVAILABLE;
        }
    }

    public void insertDocument(Document document, String collectionName) {
        databaseCondition = DatabaseInfo.UNAVAILABLE;
        MongoCollection<Document> collection = database.getCollection(collectionName);
        collection.insertOne(document);
    }

    public void updateDocument(Document query, Document update, String collectionName) {
        databaseCondition = DatabaseInfo.UNAVAILABLE;
        MongoCollection<Document> collection = database.getCollection(collectionName);
        collection.updateOne(query, update);
    }

    public void deleteDocument(Document query, String collectionName) {
        databaseCondition = DatabaseInfo.UNAVAILABLE;
        MongoCollection<Document> collection = database.getCollection(collectionName);
        collection.deleteOne(query);
    }
}
