package moscow.ptnl.contingent.repository.history;

import moscow.ptnl.contingent.domain.area.model.area.AreaOrEmployeeEvent;
import moscow.ptnl.contingent.domain.area.repository.HistoryEventRepository;
import moscow.ptnl.contingent.repository.BaseRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class HistoryEventRepositoryImpl extends BaseRepository implements HistoryEventRepository {

    @Autowired
    private HistoryEventCRUDRepository historyEventCRUDRepository;

    @Override
    public Page<AreaOrEmployeeEvent> findAreaAndEmployeeEvents(long areaId, PageRequest paging) {
        Sort.Direction mainDirection = paging.getSort().get().findFirst().map(Sort.Order::getDirection).orElse(Sort.Direction.DESC);

        if (paging.getSort().isSorted()) {
            paging = PageRequest.of(paging.getPageNumber(), paging.getPageSize(), paging.getSort().and(Sort.by(mainDirection, "id")));
        } else {
            paging = PageRequest.of(paging.getPageNumber(), paging.getPageSize(), Sort.by(mainDirection, "change_date", "id"));
        }
        return historyEventCRUDRepository.findAreaAndEmployeeEvents(areaId, paging);
    }
}
