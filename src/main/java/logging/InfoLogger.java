package logging;

import exceptions.LogFileAccessException;
import exceptions.LogWriteException;

public class InfoLogger extends Logger {
    public InfoLogger(String filePath) {
        super(filePath);
    }

    @Override
    public void log(String message) {
        try {
            writeLog(new LogEntry("INFO", message));
        }catch(LogFileAccessException | LogWriteException e){
            System.out.println("Failed to log: " + e);  //NOSONAR
        }
    }
}