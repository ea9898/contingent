package moscow.ptnl.contingent.area.repository.esu;

import moscow.ptnl.contingent.area.entity.esu.EsuOutput;
import org.springframework.data.repository.NoRepositoryBean;

import java.time.LocalDateTime;
import java.util.List;

@NoRepositoryBean
public interface EsuOutputRepository {

	List<EsuOutput> findEsuOutputsToResend(LocalDateTime olderThan);
}
