package moscow.ptnl.contingent.domain.area.methods;

import moscow.ptnl.contingent.domain.area.CatalogDomainService;
import moscow.ptnl.contingent.domain.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.model.params.MoAvailableAreaTypesParams;
import moscow.ptnl.contingent.error.ContingentException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import javax.transaction.Transactional;
import java.util.List;

@Component
@Transactional
public class AreaFactoryMethodImpl implements AreaFactoryMethod {

    @Autowired
    private CatalogDomainService catalogDomainService;

    @Override
    public List<MoAvailableAreaTypes> addMoAvailableAreaTypes(MoAvailableAreaTypesParams params) throws ContingentException {
        return new AddMoAvailableAreaTypesMethod(params).init(catalogDomainService).execute();
    }
}

