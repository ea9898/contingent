package moscow.ptnl.contingent.repository.esu;

import org.springframework.data.repository.NoRepositoryBean;

import java.time.LocalDateTime;
import java.util.List;
import moscow.ptnl.contingent.domain.esu.EsuOutput;

@NoRepositoryBean
public interface EsuOutputRepository {

	List<EsuOutput> findEsuOutputsToResend(LocalDateTime olderThan);
}
