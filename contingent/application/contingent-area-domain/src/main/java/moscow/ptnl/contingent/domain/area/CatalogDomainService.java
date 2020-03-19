package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.area.entity.area.MoAvailableAreaTypes;

import java.util.List;
import java.util.Optional;

public interface CatalogDomainService {

    <T> Optional<T> get(Long id, Class<T> clazz);

    List<MoAvailableAreaTypes> findAreaTypesByMoId(long moId);
}
