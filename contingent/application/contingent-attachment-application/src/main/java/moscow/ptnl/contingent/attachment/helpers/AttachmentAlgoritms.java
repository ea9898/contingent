package moscow.ptnl.contingent.attachment.helpers;

import moscow.ptnl.contingent.domain.area.entity.area.Area;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKindEnum;
import moscow.ptnl.contingent.repository.area.AreaMedicalEmployeeRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.stream.Collectors;

@Component
public class AttachmentAlgoritms {

    @Autowired
    AreaMedicalEmployeeRepository areaMedicalEmployeeRepository;

    // A_UP_1
    public List<Area> findPersonalAreasByJobId(long job) {
        List<Area> areaList = areaMedicalEmployeeRepository.findAreasByEmployee(job);
        return areaList.stream()
                .filter(area -> area.isActual() && area.getAreaType().getAreaTypeKind().getCode().equals(AreaTypeKindEnum.PERSONAL.getCode()))
                .collect(Collectors.toList());
    }
}
