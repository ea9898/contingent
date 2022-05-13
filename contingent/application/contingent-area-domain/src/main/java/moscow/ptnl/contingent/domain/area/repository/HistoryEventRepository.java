package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.model.area.AreaOrEmployeeEvent;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.repository.NoRepositoryBean;

@NoRepositoryBean
public interface HistoryEventRepository {

	Page<AreaOrEmployeeEvent> findAreaAndEmployeeEvents(long areaId, PageRequest paging);
}
