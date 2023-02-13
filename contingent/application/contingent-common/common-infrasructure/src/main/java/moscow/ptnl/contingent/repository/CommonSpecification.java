package moscow.ptnl.contingent.repository;

import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import jakarta.persistence.criteria.CriteriaBuilder;
import jakarta.persistence.criteria.CriteriaQuery;
import jakarta.persistence.criteria.Predicate;
import jakarta.persistence.criteria.Root;
import jakarta.persistence.metamodel.SingularAttribute;
import org.springframework.data.jpa.domain.Specification;

/**
 *
 * @author m.kachalov
 */
public final class CommonSpecification {
    
    /** Максимальное количество записей в IN запросе */
    public static final int MAX_RECORDS_IN_CLAUSE = 1000;
    
    private CommonSpecification(){}
    
    public static <E, K> Specification<E> in(Class<E> entityType, String keyFieldName, List<K> keysList) {
        Objects.requireNonNull(keysList);
        if (keysList.size() > MAX_RECORDS_IN_CLAUSE) {
            throw new IllegalArgumentException("Количество элементов в коллекции больше " + MAX_RECORDS_IN_CLAUSE);
        }
        
        Set<K> keys = new HashSet<>(keysList);
        
        return new Specification<E>() {
            @Override
            public Predicate toPredicate(Root<E> root, CriteriaQuery<?> cq, CriteriaBuilder builder) {
                return builder.in(root.get(keyFieldName)).value(keys);
            }        
        };
    }
    
    public static <E, K> Specification<E> in(SingularAttribute<E, K> attribute, List<K> keysList) {
        return in(attribute.getDeclaringType().getJavaType(), attribute.getName(), keysList);
    }
    
}
