package moscow.ptnl.contingent.area.service;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent.area.entity.area.AreaToAreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.model.esu.AreaCreateEvent;
import moscow.ptnl.contingent.area.repository.area.AreaRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

@Component
public class EsuHelperService {

    @Autowired
    private AreaChecker areaChecker;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private EsuService esuService;

    public void trySendPrimaryAreaChange(Area area) {
        //Подготовка к отправке в ЕСУ
        List<Area> areas = areaRepository.findAreas(area.getMuId() == null ? area.getMoId() : null, area.getMuId(),
                (Long) null, null, true).stream()
                .filter(a -> a.getPrimaryAreaTypes() != null &&
                        a.getPrimaryAreaTypes().stream()
                                .anyMatch(t -> Objects.equals(t.getAreaType(), area.getAreaType())))
                .collect(Collectors.toList());
        areas.forEach(a -> {
            if (area.getPrimaryAreaTypes() != null && !area.getPrimaryAreaTypes().isEmpty()) {
                esuService.saveAndPublishToESU(new AreaCreateEvent(a, a.getPrimaryAreaTypes().stream()
                        .filter(t -> Objects.equals(t.getAreaType(), area.getAreaType()))
                        .collect(Collectors.toList()))
                );
            }
        });
    }

    public void trySendDependentAreaChange(Area area) {
        //Подготовка к отправке в ЕСУ
        if (area.getPrimaryAreaTypes() != null && !area.getPrimaryAreaTypes().isEmpty()) {
            List<Area> areas = areaRepository.findAreas(area.getMuId() == null ? area.getMoId() : null, area.getMuId(),
                    area.getPrimaryAreaTypes().stream()
                            .map(AreaToAreaType::getAreaType)
                            .map(AreaType::getCode)
                            .distinct()
                            .collect(Collectors.toList()),
                    null, true);
            if (!areas.isEmpty()) {
                esuService.saveAndPublishToESU(new AreaCreateEvent(area, new ArrayList<>(area.getPrimaryAreaTypes())));
            }
        }
    }
}
