package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.domain.area.CatalogDomainService;
import moscow.ptnl.contingent.domain.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.repository.area.MoAvailableAreaTypesRepository;
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

    @Override
    public List<MoAvailableAreaTypes> findAreaTypesByMoId(long moId) {
        return moAvailableAreaTypesRepository.findAreaTypes(moId);
    }
}
