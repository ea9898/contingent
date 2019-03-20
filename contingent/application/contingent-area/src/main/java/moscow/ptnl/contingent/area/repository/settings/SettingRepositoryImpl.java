package moscow.ptnl.contingent.area.repository.settings;

import java.util.List;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.CriteriaQuery;
import javax.persistence.criteria.Root;

import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import moscow.ptnl.contingent.area.entity.settings.Setting;
import moscow.ptnl.contingent.area.entity.settings.Setting_;
import moscow.ptnl.contingent.area.repository.BaseRepository;

@Repository
@Transactional(propagation=Propagation.MANDATORY)
public class SettingRepositoryImpl extends BaseRepository implements SettingRepository<Setting, Long> {

    @Override
    public List<Setting> getSettingByRelease(String value) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Setting> criteria = criteriaBuilder.createQuery(Setting.class);
        Root<Setting> setting = criteria.from(Setting.class);
        criteria.where(criteriaBuilder.equal(setting.get(Setting_.value.getName()), value));
        List<Setting> results = entityManager.createQuery(criteria).getResultList();
        return results;
    }


    @Override
    public Setting getSettingsByKey(String name) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Setting> criteria = criteriaBuilder.createQuery(Setting.class);
        Root<Setting> setting = criteria.from(Setting.class);
        criteria.where(criteriaBuilder.equal(setting.get(Setting_.name.getName()), name));
        Setting result = entityManager.createQuery(criteria).getSingleResult();
        return result;
    }
}
