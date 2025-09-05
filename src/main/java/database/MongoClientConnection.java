/**
 * Represents a MongoClientConnection class which is used to establish a connection with the MongoDB with given parameters in the resource folder
 * This class provides a method to get the MongoClient object
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-09
 */

package database;

import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.ServerApi;
import com.mongodb.ServerApiVersion;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import handlers.ResourceManager;
import logging.ErrorLogger;
import logging.InfoLogger;
import logging.Logger;

import java.util.Properties;

public class MongoClientConnection {
    /**
     * Provides a MongoClient to establish a connection with the MongoDB database
     */
    public MongoClient getMongoClient() throws NullPointerException {
        Logger infoLogger = new InfoLogger("./logs/info.log.ser");
        Logger errorLogger = new ErrorLogger("./logs/error.log.ser");
        infoLogger.log("getMongoClient() method called");
        Properties properties =  ResourceManager.loadProperties("/properties/database.properties", this);

        String connectionString = String.format(
                "mongodb+srv://%s:%s@%s",
                properties.getProperty("username"),
                properties.getProperty("password"),
                properties.getProperty("databaseUrl")
        );

        ServerApi serverApi = ServerApi.builder()
                .version(ServerApiVersion.V1)
                .build();
        MongoClient mongoClient = null;
        try{
            MongoClientSettings settings = MongoClientSettings.builder()
                    .applyConnectionString(new ConnectionString(connectionString))
                    .serverApi(serverApi)
                    .build();

            mongoClient = MongoClients.create(settings);
        } catch (Exception e){
            errorLogger.log(e.getMessage());
        }

        return mongoClient;
    }
}