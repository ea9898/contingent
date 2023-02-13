package moscow.ptnl.util;

import java.util.Collection;
import java.util.List;

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
    
    public static boolean equals(List a, List b) {
        if (a == null || b == null) 
            throw new IllegalArgumentException("аргумент не может быть null");
        if (a.isEmpty() && b.isEmpty())
            return true;
        return a.containsAll(b) && b.containsAll(a);
    }
    
}
