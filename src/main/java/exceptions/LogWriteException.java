package exceptions;

public class LogWriteException extends Exception {
    public LogWriteException(String message, Throwable cause) {
        super(message, cause);
    }
}