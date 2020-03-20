package moscow.ptnl.contingent.domain.area;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.NoRepositoryBean;
import org.springframework.data.repository.support.Repositories;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Service
@Lazy
public class RepositoryService {

    @Autowired
    @Lazy
    private Repositories repositories;

    public CrudRepository getRepository(Class<?> clazz) {
        return (CrudRepository) repositories.getRepositoryFor(clazz)
                .orElseThrow(() -> new RuntimeException("Репозиторий для сущности '" + clazz.getSimpleName() + "' не найден"));
    }
    
    @Transactional
    public <T> T get(Object entityId, Class<T> entityType) {
        if (entityId == null) return null;
        CrudRepository repository = getRepository(entityType);
        return (T) repository.findById(entityId).orElse(null);
    }
    
    @Transactional
    public <T> Iterable<T> getAll(Class<T> entityType) {
        CrudRepository repository = getRepository(entityType);
        return repository.findAll();
    }
}
