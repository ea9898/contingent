package moscow.ptnl.contingent.domain.area;

import moscow.ptnl.contingent.domain.area.entity.MoAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.repository.MoAvailableAreaTypesRepository;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Сервис для работы с медицинскими организациями.
 */

@Service
@Transactional
public class MoServiceImpl implements MoService {

    @Autowired
    private MoAvailableAreaTypesRepository moAvailableAreaTypesRepository;

    @Autowired
    private AreaHelper areaHelper;

    @Override
    public void addMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException {
        areaTypeCodes = areaTypeCodes.stream().distinct().collect(Collectors.toList());
        Validation validation = new Validation();
        List<AreaType> areaTypes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation);
        areaHelper.checkAreaTypesExistInMO(moId, areaTypes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        areaTypeCodes.forEach(a -> {
            MoAvailableAreaTypes availableAreaType = new MoAvailableAreaTypes();
            areaTypes.stream()
                    .filter(t -> Objects.equals(t.getCode(), a))
                    .findFirst()
                    .ifPresent(availableAreaType::setAreaType);
            availableAreaType.setMoId(moId);
            availableAreaType.setCreateDate(LocalDateTime.now());
            moAvailableAreaTypesRepository.save(availableAreaType);
        });
    }

    @Override
    public void delMoAvailableAreaTypes(long moId, List<Long> areaTypeCodes) throws ContingentException {
        areaTypeCodes = areaTypeCodes.stream().distinct().collect(Collectors.toList());
        Validation validation = new Validation();
        areaTypeCodes = areaHelper.checkAndGetAreaTypesExist(areaTypeCodes, validation).stream()
                .map(AreaType::getCode)
                .collect(Collectors.toList());
        List<MoAvailableAreaTypes> moAvailableAreaTypes = areaHelper.checkAndGetAreaTypesNotExistInMO(moId, areaTypeCodes, validation);
        areaHelper.checkAndGetAreaTypesNotExistInMU(moAvailableAreaTypes, areaTypeCodes, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }
        // TODO перееписать с deleteAll
        moAvailableAreaTypes.forEach(a -> moAvailableAreaTypesRepository.delete(a));
    }

    @Override
    public List<AreaType> getMoAvailableAreaTypes(long moId) throws ContingentException {
        List<MoAvailableAreaTypes> moAvailableAreaTypes = moAvailableAreaTypesRepository.findAreaTypes(moId);
        return moAvailableAreaTypes.stream().map(MoAvailableAreaTypes::getAreaType).collect(Collectors.toList());
    }
}
