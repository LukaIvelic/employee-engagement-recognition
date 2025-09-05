package generics;

import com.mongodb.client.MongoCollection;
import database.DatabaseManager;
import database.enums.Databases;
import org.bson.Document;
import records.interfaces.DefaultDataStructure;

public class RecordManager {

    private RecordManager(){}

    /**
     * Checks the database if there's already an existing record written
     * @return Boolean - true if there is more than 1 record that matches the DefaultDataStructure given and false if there is not
     */
    public static <T extends DefaultDataStructure> Boolean checkRecords(T object, String collectionName) {
        DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
        MongoCollection<Document> collection = databaseManager.getCollection(collectionName);
        boolean doesExist = collection.countDocuments(object.getDocument()) > 0;
        databaseManager.mongoClient.close();
        return doesExist;
    }
}
