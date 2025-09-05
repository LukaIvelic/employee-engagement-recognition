package logging;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;

public class LogEntry implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    private final String level;
    private final LocalDateTime timestamp;
    private final String message;

    public LogEntry(String level, String message) {
        this.level = level;
        this.timestamp = LocalDateTime.now();
        this.message = message;
    }

    @Override
    public String toString() {
        return "[" + level + " " + timestamp + "] " + message;
    }
}
