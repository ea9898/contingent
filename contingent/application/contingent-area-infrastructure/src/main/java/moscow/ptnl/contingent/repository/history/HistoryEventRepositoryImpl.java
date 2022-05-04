package moscow.ptnl.contingent.repository.history;

import moscow.ptnl.contingent.domain.area.model.area.AreaFullHistory;
import moscow.ptnl.contingent.domain.area.repository.HistoryEventRepository;
import moscow.ptnl.contingent.repository.BaseRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class HistoryEventRepositoryImpl extends BaseRepository implements HistoryEventRepository {

    @Autowired
    private HistoryEventCRUDRepository historyEventCRUDRepository;

    @Override
    public List<AreaFullHistory.MedicalEmployeeEvent> medicalEmployeeEvents(long areaId) {
        return historyEventCRUDRepository.findMedicalEmployeeEvents(areaId);
    }

    @Override
    public List<AreaFullHistory.AreaEvent> areaEvents(long areaId) {
        return historyEventCRUDRepository.findAreaEvents(areaId);
    }
}
