/**
 * Represents a Database Manager that takes care of handling calls to the MongoDB database
 * This class provides methods manipulate and list out information
 * <p>Example usage:</p>
 * <pre>
 * databaseManager.insertDocument(employee.getDocument(), "employee");
 * </pre>
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-09
 */

package database;

import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.model.Filters;
import database.enums.DatabaseInfo;
import database.interfaces.DefaultDatabaseStructure;
import org.bson.Document;
import org.bson.types.ObjectId;

import java.util.ArrayList;
import java.util.List;

public class DatabaseManager implements DefaultDatabaseStructure {

    public DatabaseInfo databaseCondition; //NOSONAR
    public MongoDatabase database; //NOSONAR
    public MongoClient mongoClient; //NOSONAR

    /**
     * Initializes the DatabaseManager class with MongoClient if the databaseName is provided
     */
    public DatabaseManager(String databaseName) {
        try {
            mongoClient = new MongoClientConnection().getMongoClient();
            database = mongoClient.getDatabase(databaseName);
        } catch (Exception e) {
            databaseCondition = DatabaseInfo.ERROR;
        } finally {
            databaseCondition = DatabaseInfo.AVAILABLE;
        }
    }

    /**
     * Inserts a document into the collection if there is a given document and a collection name
     */
    public void insertDocument(Document document, String collectionName) {
        databaseCondition = DatabaseInfo.UNAVAILABLE;
        MongoCollection<Document> collection = database.getCollection(collectionName);
        collection.insertOne(document);
    }

    /**
     * Updates an existing document in the collection if there is a given document, update document and a collection name
     */
    public void updateDocument(String objectID, Document update, String collectionName) {
        databaseCondition = DatabaseInfo.UNAVAILABLE;
        MongoCollection<Document> collection = database.getCollection(collectionName);

        Document updateDocument = new Document("$set", update);

        collection.updateOne(
                Filters.eq("_id", new ObjectId(objectID)),
                updateDocument
        );
    }


    /**
     * Deletes a document from the collection if there is a given document and a collection name
     */
    public void deleteDocument(Document toDelete, String collectionName) {
        databaseCondition = DatabaseInfo.UNAVAILABLE;
        MongoCollection<Document> collection = database.getCollection(collectionName);
        collection.deleteOne(toDelete);
    }

    /**
     * Gets the entire collection of documents if there is a given collection name
     */
    public MongoCollection<Document> getCollection(String collectionName) {
        databaseCondition = DatabaseInfo.UNAVAILABLE;
        return database.getCollection(collectionName);
    }

    public List<String> getAllCollections() {
        List<String> collections = null;
        try {
            databaseCondition = DatabaseInfo.UNAVAILABLE;
            collections = new ArrayList<>();
            database.listCollectionNames().into(collections);
        } catch (Exception e) {
            databaseCondition = DatabaseInfo.ERROR;
        } finally {
            databaseCondition = DatabaseInfo.AVAILABLE;
        }
        return collections;
    }
}
