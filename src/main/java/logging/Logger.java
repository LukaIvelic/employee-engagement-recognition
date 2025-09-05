package logging;

import exceptions.LogFileAccessException;
import exceptions.LogWriteException;

import java.io.*;

public abstract class Logger implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;
    protected String filePath;

    protected Logger(String filePath) {
        this.filePath = filePath;
    }

    public abstract void log(String message);

    protected void writeLog(LogEntry entry) throws LogWriteException, LogFileAccessException {
        File file = new File(filePath);

        if(!file.exists() || !file.canWrite()){
            throw new LogFileAccessException("Cannot access or write to file: " + filePath);
        }

        try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(filePath, true) /*NOSONAR*/)) {
            oos.writeObject(entry);
        } catch (IOException e) {
            throw new LogWriteException("Failed to write log entry to file: " + filePath, e);
        }
    }
}

