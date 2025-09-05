package logging;

import exceptions.LogFileAccessException;
import exceptions.LogWriteException;

public class ErrorLogger extends Logger {
    public ErrorLogger(String filePath) {
        super(filePath);
    }

    @Override
    public void log(String message) {
        try{
            writeLog(new LogEntry("ERROR", message));
        }catch(LogFileAccessException | LogWriteException e){
            System.out.println("Failed to log: " + e); //NOSONAR
        }
    }
}