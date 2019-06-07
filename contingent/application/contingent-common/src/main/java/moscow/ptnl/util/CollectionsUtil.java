package moscow.ptnl.util;

import java.util.Collection;

/**
 *
 * @author m.kachalov
 */
public class CollectionsUtil {
    
    private CollectionsUtil(){}
    
    public static boolean isNullOrEmpty(Collection col) {
        return col == null || col.isEmpty();
    }
    
    /**
     * Длина коллекции и 0 если коллекция null.
     * 
     * @param col
     * @return 
     */
    public static int size(Collection col) {
        return col != null ? col.size() : 0;
    }
    
}
