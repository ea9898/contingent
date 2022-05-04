package moscow.ptnl.contingent.domain.area.repository;

import moscow.ptnl.contingent.domain.area.model.area.AreaFullHistory;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.List;

@NoRepositoryBean
public interface HistoryEventRepository {

	List<AreaFullHistory.MedicalEmployeeEvent> medicalEmployeeEvents(long areaId);

	List<AreaFullHistory.AreaEvent> areaEvents(long areaId);
}
