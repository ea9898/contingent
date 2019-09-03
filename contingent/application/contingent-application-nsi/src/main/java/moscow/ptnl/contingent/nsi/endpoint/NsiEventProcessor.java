/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.nsi.endpoint;

import java.io.Serializable;
import java.util.concurrent.Future;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKind;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeRelations;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import moscow.ptnl.contingent.domain.Keyable;
import moscow.ptnl.contingent.nsi.domain.NsiActionsEnum;
import moscow.ptnl.contingent.repository.CommonRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeMedicalPositionsCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeRelationsCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeSpecializationsCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesClassCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesKindCRUDRepository;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

/**
 *
 * @author m.kachalov
 */
@Component
public class NsiEventProcessor {
    
    private static final Logger LOG = LoggerFactory.getLogger(NsiEventProcessor.class);
    
    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private AreaTypesClassCRUDRepository areaTypesClassCRUDRepository;

    @Autowired
    private AreaTypesKindCRUDRepository areaTypesKindCRUDRepository;

    @Autowired
    private AreaTypeMedicalPositionsCRUDRepository areaTypeMedicalPositionsCRUDRepository;

    @Autowired
    private AreaTypeRelationsCRUDRepository areaTypeRelationsCRUDRepository;

    @Autowired
    private AreaTypeSpecializationsCRUDRepository areaTypeSpecializationsCRUDRepository;
    
    @Async
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Throwable.class)
    public Future<Void> processMesage(Keyable entity, String action) {
        if (entity instanceof AreaType) {
            saveOrDelete(areaTypesCRUDRepository, (AreaType) entity, action);
        } else if (entity instanceof AreaTypeClass) {
            saveOrDelete(areaTypesClassCRUDRepository, (AreaTypeClass) entity, action);
        } else if (entity instanceof AreaTypeKind) {
            saveOrDelete(areaTypesKindCRUDRepository, (AreaTypeKind) entity, action);
        } else if (entity instanceof AreaTypeMedicalPositions) {
            saveOrDelete(areaTypeMedicalPositionsCRUDRepository, (AreaTypeMedicalPositions) entity, action);
        } else if (entity instanceof AreaTypeRelations) {
            saveOrDelete(areaTypeRelationsCRUDRepository, (AreaTypeRelations) entity, action);
        } else if (entity instanceof AreaTypeSpecializations) {
            saveOrDelete(areaTypeSpecializationsCRUDRepository, (AreaTypeSpecializations) entity, action);
        }
        return new AsyncResult(null);
    }
    
    private <T extends Keyable, K extends Serializable> void saveOrDelete(CommonRepository<T, K> repository, T entity, String action) {
        if (NsiActionsEnum.DELETED.name().equalsIgnoreCase(action)) {
            LOG.info("delete action");
            repository.deleteById(entity.getKey());
        } else {
            LOG.info("save action");
            repository.save(entity);
        }
    }
    
}
