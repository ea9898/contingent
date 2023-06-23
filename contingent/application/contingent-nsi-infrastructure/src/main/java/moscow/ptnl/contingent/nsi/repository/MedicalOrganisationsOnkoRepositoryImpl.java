package moscow.ptnl.contingent.nsi.repository;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.MedicalOrganisationsOnko;
import moscow.ptnl.contingent.repository.BaseRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.util.Optional;

@Repository
@Transactional(propagation= Propagation.MANDATORY)
public class MedicalOrganisationsOnkoRepositoryImpl  extends BaseRepository implements moscow.ptnl.contingent.nsi.domain.repository.MedicalOrganisationsOnkoRepository {

    @Autowired
    private MedicalOrganisationsOnkoCRUDRepository medicalOrganisationsOnkoCRUDRepository;


    @Override
    public Optional<MedicalOrganisationsOnko> findByMoId(Long moId) {
        return medicalOrganisationsOnkoCRUDRepository.findByMoId(moId);
    }
}
