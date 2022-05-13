package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.model.area.AreaOrEmployeeEvent;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface HistoryEventRepository {

	List<AreaOrEmployeeEvent> findAreaAndEmployeeEvents(long areaId, PageRequest paging);
}
