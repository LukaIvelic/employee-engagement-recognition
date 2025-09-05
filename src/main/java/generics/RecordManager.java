package generics;

import com.mongodb.client.MongoCollection;
import database.DatabaseManager;
import database.enums.Databases;
import logging.ErrorLogger;
import logging.InfoLogger;
import logging.Logger;
import org.bson.Document;
import records.interfaces.DefaultDataStructure;

public class RecordManager {

    private RecordManager(){}

    /**
     * Checks the database if there's already an existing record written
     * @return Boolean - true if there is more than 1 record that matches the DefaultDataStructure given and false if there is not
     */
    public static <T extends DefaultDataStructure> Boolean checkRecords(T object, String collectionName) {
        Logger infoLogger = new InfoLogger("./logs/info.log.ser");
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        infoLogger.log("checkRecords() method called");
        DatabaseManager databaseManager = new DatabaseManager(Databases.EMPLOYEE_ENGAGEMENT_RECOGNITION.toString());
        MongoCollection<Document> collection = databaseManager.getCollection(collectionName);
        boolean doesExist = false;
        try{
            doesExist = collection.countDocuments(object.getDocument()) > 0;
        } catch (Exception e){
            errorLogger.log(e.getMessage());
        } finally {
            databaseManager.mongoClient.close();
        }

        return doesExist;
    }
}