package controllers.overview;

import com.mongodb.client.AggregateIterable;
import logging.InfoLogger;
import logging.Logger;
import org.bson.Document;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import static controllers.overview.OverviewSceneController.EmployeeAndEngagement;

public class OverviewSceneResources {

    static final String INFOLOGGER_PATH = "./logs/info.log.ser";


    private OverviewSceneResources(){}

    public static synchronized List<EmployeeAndEngagement> collectData(AggregateIterable<Document> engagements, AggregateIterable<Document> employees) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("collectData() method called");
        List<EmployeeAndEngagement> employeeAndEngagementListToSort = new ArrayList<>();
        for(Document engagement : engagements) {
            for(Document employee : employees) {
                String engagementId = engagement.get("_id").toString();
                String employeeId = employee.get("_id").toString();
                if(engagementId.equals(employeeId)) {
                    employeeAndEngagementListToSort.add(new EmployeeAndEngagement(
                            employee.get("firstName").toString(),
                            employee.get("lastName").toString(),
                            engagement.get("timeWorking").toString()
                    ));
                }
            }
        }
        return employeeAndEngagementListToSort;
    }

    public static synchronized List<EmployeeAndEngagement> getTopEmployees(AggregateIterable<Document> engagements, AggregateIterable<Document> employees) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("getTopEmployees() method called");
        return collectData(engagements, employees).stream()
                .sorted(Comparator.comparing(a -> Integer.valueOf(((EmployeeAndEngagement)a).timeAtWork())).reversed())
                .limit(5)
                .toList();
    }

    public static synchronized List<EmployeeAndEngagement> getBurnoutEmployees(AggregateIterable<Document> engagements, AggregateIterable<Document> employees) {
        Logger infoLogger = new InfoLogger(INFOLOGGER_PATH);
        infoLogger.log("getBurnoutEmployees() method called");
        return collectData(engagements, employees).stream()
                .sorted(Comparator.comparing(a -> Integer.valueOf(((EmployeeAndEngagement)a).timeAtWork())).reversed())
                .filter(value -> Integer.parseInt(value.timeWorking())>= 150)
                .toList();
    }
}