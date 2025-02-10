/**
 * Represents a Resource Manager that takes care of getting and manipulating resources from the database
 * This interface provides methods to manipulate database documents
 * @author Luka Ivelić
 * @version 1.0
 * @since 2025-02-09
 */

package records.interfaces;

import org.bson.Document;

public interface DefaultDataStructure {
    /**
     * Forces the implementer class or interface to have a method to convert record to document
     */
    Document getDocument();
    /**
     * Forces the implementer class or interface to have a method to update a document
     */
    Document updateDocument(String condition);
    /**
     * Forces the implementer class or interface to have a method to delete a document
     */
    Document deleteDocument(String condition);
}
