package moscow.ptnl.contingent.sysop.repository;

import moscow.ptnl.contingent.sysop.entity.Sysop;
import org.springframework.data.repository.NoRepositoryBean;

import java.util.Optional;

@NoRepositoryBean
public interface SysopRepository {

    Sysop save(Sysop sysop);

    Sysop getOne(Long sysopId);

    Optional<Sysop> findById(long sysop);
}
