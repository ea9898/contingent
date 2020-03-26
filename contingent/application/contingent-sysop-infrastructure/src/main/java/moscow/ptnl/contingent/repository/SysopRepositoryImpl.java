package moscow.ptnl.contingent.repository;

import moscow.ptnl.contingent.sysop.entity.Sysop;
import moscow.ptnl.contingent.sysop.repository.SysopRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class SysopRepositoryImpl extends BaseRepository implements SysopRepository {

    @Autowired
    private SysopCRUDRepository sysopCRUDRepository;

    @Override
    public Sysop save(Sysop sysop) {
        return sysopCRUDRepository.save(sysop);
    }

    @Override
    public Sysop getOne(Long sysopId) {
        return sysopCRUDRepository.getOne(sysopId);
    }

    @Override
    public Optional<Sysop> findById(long sysop) {
        return sysopCRUDRepository.findById(sysop);
    }
}
