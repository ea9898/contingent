package moscow.ptnl.contingent.area.repository.esu;

import moscow.ptnl.contingent.area.entity.esu.EsuOutput;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.Date;
import java.util.List;

@NoRepositoryBean
public interface EsuOutputRepository {

	List<EsuOutput> findEsuOutputsToResend(Date olderThan);
}
