package moscow.ptnl.contingent.endpoint;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKind;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeRelations;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import moscow.ptnl.contingent.domain.nsi.entity.NsiActionsEnum;
import moscow.ptnl.contingent.domain.nsi.entity.NsiPush;
import moscow.ptnl.contingent.domain.nsi.entity.NsiPushEvent;
import moscow.ptnl.contingent.repository.CommonRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeMedicalPositionsCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeRelationsCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeSpecializationsCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.ClassAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.KindAreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.NsiPushEventCRUDRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.integration.annotation.MessageEndpoint;
import org.springframework.integration.annotation.ServiceActivator;
import org.springframework.messaging.Message;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.io.Serializable;

import static moscow.ptnl.contingent.configuration.EventChannelsConfiguration.NSI_EVENT_CHANNEL_NAME;

/**
 * Точка получения событий из канала NSI_EVENT_CHANNEL_NAME.
 * 
 * @author m.kachalov
 */
@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
@MessageEndpoint
public class NsiEventEndpoint {

    private static final Logger LOG = LoggerFactory.getLogger(NsiEventEndpoint.class);

    @Autowired
    AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    ClassAreaTypesCRUDRepository classAreaTypesCRUDRepository;

    @Autowired
    KindAreaTypesCRUDRepository kindAreaTypesCRUDRepository;

    @Autowired
    AreaTypeMedicalPositionsCRUDRepository areaTypeMedicalPositionsCRUDRepository;

    @Autowired
    AreaTypeRelationsCRUDRepository areaTypeRelationsCRUDRepository;

    @Autowired
    AreaTypeSpecializationsCRUDRepository areaTypeSpecializationsCRUDRepository;

    @Autowired
    NsiPushEventCRUDRepository nsiPushEventCRUDRepository;

    @ServiceActivator(inputChannel = NSI_EVENT_CHANNEL_NAME)
    public void nsiPushConsumer(Message<NsiPush> msg) {
        Object entity = msg.getPayload().getEntity();
        String action = msg.getPayload().getAction();
        try {
            if (entity instanceof AreaType) {
                saveOrDelete(areaTypesCRUDRepository, (AreaType) entity, action);
                return;
            }
            if (entity instanceof AreaTypeClass) {
                saveOrDelete(classAreaTypesCRUDRepository, (AreaTypeClass) entity, action);
                return;
            }
            if (entity instanceof AreaTypeKind) {
                saveOrDelete(kindAreaTypesCRUDRepository, (AreaTypeKind) entity, action);
                return;
            }
            if (entity instanceof AreaTypeMedicalPositions) {
                saveOrDelete(areaTypeMedicalPositionsCRUDRepository, (AreaTypeMedicalPositions) entity, action);
                return;
            }
            if (entity instanceof AreaTypeRelations) {
                saveOrDelete(areaTypeRelationsCRUDRepository, (AreaTypeRelations) entity, action);
                return;
            }
            if (entity instanceof AreaTypeSpecializations) {
                saveOrDelete(areaTypeSpecializationsCRUDRepository, (AreaTypeSpecializations) entity, action);
            }
        } catch (Exception e) {
            NsiPushEvent event = nsiPushEventCRUDRepository.findById(msg.getPayload().getId()).get();
            event.setError(true);
            event.setErrorMessage(e.getMessage());
            nsiPushEventCRUDRepository.save(event);
        }
    }

    private  <T, K extends Serializable> void saveOrDelete(CommonRepository<T, K> repository, T entity, String action) {
        if (action.equalsIgnoreCase(NsiActionsEnum.DELETED.toString())) {
            repository.delete(entity);
        } else {
            repository.save(entity);
        }
    }
}
