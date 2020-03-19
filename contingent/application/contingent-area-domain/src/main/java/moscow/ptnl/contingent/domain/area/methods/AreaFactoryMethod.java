package moscow.ptnl.contingent.domain.area.methods;

import moscow.ptnl.contingent.domain.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.model.params.MoAvailableAreaTypesParams;
import moscow.ptnl.contingent.error.ContingentException;

import java.util.List;

public interface AreaFactoryMethod {

    List<MoAvailableAreaTypes> addMoAvailableAreaTypes(MoAvailableAreaTypesParams params) throws ContingentException;
}
