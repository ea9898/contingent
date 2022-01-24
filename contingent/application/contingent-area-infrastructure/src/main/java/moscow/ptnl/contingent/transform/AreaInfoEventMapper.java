package moscow.ptnl.contingent.transform;

import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.AreaMedicalEmployees;
import moscow.ptnl.contingent.util.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.area.info.AreaInfoEvent;
import moscow.ptnl.util.CollectionsUtil;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

@Component
public class AreaInfoEventMapper implements Transform<AreaInfoEvent, moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent> {

    @Autowired
    private AreaRestrictionMapper areaRestrictionMapper;

    @Autowired
    private MainEmployeesMapper mainEmployeesMapper;

    @Autowired
    private ReplacementEmployeesMapper replacementEmployeesMapper;

    @Autowired
    private AddressesMapper addressesMapper;

    @Autowired
    private SettingService settingService;

    @Override
    public AreaInfoEvent entityToDtoTransform(moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent entity) {
        LocalDate now = LocalDate.now();
        AreaInfoEvent event = new AreaInfoEvent();
        Area area = entity.getArea();
        //1
        event.setOperationDate(XMLGregorianCalendarMapper.entityToDtoTransform(entity.getOperationDate()));
        event.setOperationType(entity.getOperationType());
        event.setAreaId(area.getId());
        event.setMuId(area.getMuId() == null ? area.getMoId() : area.getMuId());
        event.setAreaType(area.getAreaType().getCode());
        event.setArchive(area.getArchived());
        event.setNumber(area.getNumber());
        event.setName(area.getDescription());
        event.setAutoAssignForAttachment(Boolean.TRUE.equals(area.getAutoAssignForAttach()));
        event.setResidentsBindRate((area.getAreaType().getResidentsBindRate() != null) ? area.getAreaType().getResidentsBindRate().longValue() : null);
        event.setAreaRestriction(areaRestrictionMapper.entityToDtoTransform(area));
        //2
        if (settingService.getPar43().contains(event.getAreaType())) {
            //медицинские работники, находящиеся на участках с типами, указанными в системном параметре PAR_43 всегда указываются как основные
            Set<AreaMedicalEmployees> areaMedicalEmployeesMain = area.getActualMedicalEmployees().stream()
                    .filter(buildEmployeesPredicate(now))
                    .collect(Collectors.toSet());
            if (!CollectionsUtil.isNullOrEmpty(areaMedicalEmployeesMain)) {
                event.setMainEmployees(mainEmployeesMapper.entityToDtoTransform(areaMedicalEmployeesMain));
            }
        } else {
            Set<AreaMedicalEmployees> areaMedicalEmployeesMain = area.getActualMainMedicalEmployees().stream()
                    .filter(buildEmployeesPredicate(now))
                    .collect(Collectors.toSet());
            if (!CollectionsUtil.isNullOrEmpty(areaMedicalEmployeesMain)) {
                event.setMainEmployees(mainEmployeesMapper.entityToDtoTransform(areaMedicalEmployeesMain));
            }
            Set<AreaMedicalEmployees> areaMedicalEmployeesReplacement = area.getActualReplacementMedicalEmployees().stream()
                    .filter(buildEmployeesPredicate(now))
                    .collect(Collectors.toSet());
            if (!CollectionsUtil.isNullOrEmpty(areaMedicalEmployeesReplacement)) {
                event.setReplacementEmployees(replacementEmployeesMapper.entityToDtoTransform(areaMedicalEmployeesReplacement));
            }
        }
        //3
        Set<AreaAddress> areaAddressSet = area.getActualAreaAddresses();
        if (!CollectionsUtil.isNullOrEmpty(areaAddressSet)) {
            event.setAddresses(addressesMapper.entityToDtoTransform(areaAddressSet));
        }

        return event;
    }

    private Predicate<AreaMedicalEmployees> buildEmployeesPredicate(LocalDate now) {
        return (e) -> (e.getError() == null || !e.getError()) &&
                (e.getStartDate() == null || e.getStartDate().isBefore(now) || e.getStartDate().equals(now));
    }

    @Override
    public moscow.ptnl.contingent.domain.esu.event.AreaInfoEvent dtoToEntityTransform(AreaInfoEvent dtoObject) {
        return null;
    }
}
