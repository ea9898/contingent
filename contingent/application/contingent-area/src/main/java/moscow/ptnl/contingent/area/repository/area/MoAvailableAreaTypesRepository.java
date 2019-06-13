package moscow.ptnl.contingent.area.repository.area;

import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface MoAvailableAreaTypesRepository {

    List<MoAvailableAreaTypes> findAreaTypes(long moId);
}
