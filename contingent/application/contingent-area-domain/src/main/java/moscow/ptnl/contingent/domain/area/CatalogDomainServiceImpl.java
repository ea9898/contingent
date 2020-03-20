package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component("catalogDomainService")
public class CatalogDomainServiceImpl implements CatalogDomainService {

    @Autowired
    private RepositoryService repositoryService;

    @Autowired
    private MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Override
    public <T> Optional<T> get(Long id, Class<T> clazz) {
        T result = repositoryService.get(id, clazz);
        return (result != null) ? Optional.of(result) : Optional.empty();
    }

}
