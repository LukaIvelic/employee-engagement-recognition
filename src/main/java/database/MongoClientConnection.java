package database;

import com.mongodb.ConnectionString;
import com.mongodb.MongoClientSettings;
import com.mongodb.ServerApi;
import com.mongodb.ServerApiVersion;
import com.mongodb.client.MongoClient;
import com.mongodb.client.MongoClients;
import handlers.ResourceManager;

import java.util.Properties;

public class MongoClientConnection {
    public MongoClient getMongoClient() throws NullPointerException {
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

        MongoClientSettings settings = MongoClientSettings.builder()
                .applyConnectionString(new ConnectionString(connectionString))
                .serverApi(serverApi)
                .build();

        return MongoClients.create(settings);
    }
}
