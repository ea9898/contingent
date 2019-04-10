package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.nsi.AreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.MUProfileTemplates;
import moscow.ptnl.contingent.area.error.AreaErrorReason;
import moscow.ptnl.contingent.area.error.Validation;
import moscow.ptnl.contingent.area.error.ValidationParameter;
import moscow.ptnl.contingent.area.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.area.repository.nsi.MuProfileTemplatesRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Optional;

@Component
public class AreaChecker {

    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private MuProfileTemplatesRepository muProfileTemplatesRepository;

    public void checkAreaTypesExist(List<String> areaTypes, Validation validation, String parameterCode) {
        areaTypes.forEach(a -> {
            Optional<AreaTypes> areaType = areaTypesCRUDRepository.findById(a);

            if (!areaType.isPresent() || !Boolean.TRUE.equals(areaType.get().getActual())) {
                validation.error(AreaErrorReason.AREA_TYPE_NOT_FOUND, new ValidationParameter(parameterCode, a));
            }
        });
    }

    public void checkMuProfileChangePossible(int muTypeId, String areaType, Validation validation, String parameterCode) {
        MUProfileTemplates template = muProfileTemplatesRepository.findMuProfileTemplate(muTypeId, areaType);

        if (template == null || !Boolean.TRUE.equals(template.getAvailableToCreate())) {
            validation.error(AreaErrorReason.CANT_CHANGE_AREA_TYPE, new ValidationParameter(parameterCode, areaType));
        }
    }
}
