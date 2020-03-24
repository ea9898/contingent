package moscow.ptnl.contingent.domain.area;


import moscow.ptnl.contingent.domain.AreaErrorReason;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.domain.area.model.area.AddressArea;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.domain.area.repository.AreaAddressRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaMedicalEmployeeRepository;
import moscow.ptnl.contingent.domain.area.repository.AreaRepository;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
@Transactional
public class AreaServiceImpl implements AreaService {


    @Autowired
    private AreaAddressRepository areaAddressRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private AreaHelper areaHelper;

    @Autowired
    private AreaMedicalEmployeeRepository areaMedicalEmployeeRepository;

    @Override
    public AreaInfo getAreaById(Long areaId) throws ContingentException {
        // 1.
        Optional<Area> areaOptional = areaRepository.findById(areaId);
        if (!areaOptional.isPresent()) {
            throw new ContingentException(new Validation().error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", areaId)));
        }
        Area area = areaOptional.get();

        // 2.
        List<AreaMedicalEmployees> mainMedicalEmployees = areaMedicalEmployeeRepository.
                getEmployeesMainActualByAreaId(area.getId());

        // 3.
        List<AreaMedicalEmployees> replacementMedicalEmployees = areaMedicalEmployeeRepository.
                getEmployeesReplacementActualByAreaId(area.getId());

        return new AreaInfo(area, mainMedicalEmployees, replacementMedicalEmployees);
    }

    @Override
    public Page<AreaAddress> getAreaAddress(Long moId, List<Long> areaIds, PageRequest paging) throws ContingentException {
        Validation validation = new Validation();

        // 2.
        areaHelper.checkMaxPage(paging, validation);

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        List<Area> areas = areaRepository.findAllById(areaIds);
        // 3.
        if (areaIds.size() != areas.size()) {
            List<Long> foundAreasIds = areas.stream().map(Area::getId).collect(Collectors.toList());
            areaIds.stream().filter(aIn -> !foundAreasIds.contains(aIn)).forEach(aIn ->
                    validation.error(AreaErrorReason.AREA_NOT_FOUND, new ValidationParameter("areaId", aIn)));

            if (!validation.isSuccess()) { throw new ContingentException(validation); }
        }

        // 4.
        List<Long> areaIdsNotInMo = areas.stream().filter(area -> !area.getMoId().equals(moId))
                .map(Area::getId).collect(Collectors.toList());
        if (!areaIdsNotInMo.isEmpty())  {
            validation.error(AreaErrorReason.AREAS_NOT_IN_MO,
                    new ValidationParameter("areaIds", areaIdsNotInMo.stream().map(Object::toString)
                            .collect(Collectors.joining(","))),
                    new ValidationParameter("moId", moId));
        }
        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        // 5.
        Page<AreaAddress> areaAddresses = areaAddressRepository.findAreaAddressesByAreaId(moId, areaIds, paging);

        return new PageImpl<>(new ArrayList<>(areaAddresses.getContent()),
                areaAddresses.getPageable(), areaAddresses.getTotalElements());
    }

    @Override
    public Long getNewAreaId() throws ContingentException {
        return areaRepository.getNextAreaId();
    }

    @Override
    public Page<Area> searchDnArea(Long moId, List<Long> muIds, List<Long> areaTypeCodes, List<Long> specializationCodes, List<Long> areaIds, PageRequest paging) throws ContingentException {
        //2
        areaHelper.checkSearchDnParameters(moId, muIds, areaTypeCodes, specializationCodes, areaIds);
        //3, 4, 5
        return areaRepository.findAreas(moId, muIds, areaTypeCodes, specializationCodes, areaIds, paging);

    }
}
