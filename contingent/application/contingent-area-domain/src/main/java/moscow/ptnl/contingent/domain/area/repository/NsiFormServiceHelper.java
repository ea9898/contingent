package moscow.ptnl.contingent.domain.area.repository;

import org.springframework.data.repository.NoRepositoryBean;
import org.w3c.dom.Document;
import ru.mos.emias.system.v1.usercontext.UserContext;

@NoRepositoryBean
public interface NsiFormServiceHelper {

    Document searchByGlobalId(long formId, long globalId, UserContext userContext);
}
