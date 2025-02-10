/**
 * Represents Database Information that is used to tell other Threads trying to connect its status
 * <p>Example usage:</p>
 * <pre>
 * DatabaseInfo databaseConnection = DatabaseInfo.AVAILABLE
 * </pre>
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-09
 */

package database.enums;

public enum DatabaseInfo {
    AVAILABLE,
    UNAVAILABLE,
    ERROR
}
