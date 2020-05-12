/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package moscow.ptnl.contingent.nsi.endpoint;

import java.io.Serializable;
import java.util.concurrent.Future;
import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClass;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKind;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.domain.Keyable;
import moscow.ptnl.contingent.nsi.domain.NsiActionsEnum;
import moscow.ptnl.contingent.nsi.domain.area.Gender;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.area.PositionNom;
import moscow.ptnl.contingent.nsi.domain.area.Specialization;
import moscow.ptnl.contingent.nsi.repository.GenderCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.PolicyTypeCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.PositionCodeCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.PositionNomCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.SpecializationCRUDRepository;
import moscow.ptnl.contingent.repository.CommonRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeMedicalPositionsCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeRelationsCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeSpecializationsCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesClassCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesKindCRUDRepository;
import moscow.ptnl.contingent.repository.PagingAndSortingRepository;
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

    @Autowired
    private SpecializationCRUDRepository specializationCRUDRepository;

    @Autowired
    private PositionCodeCRUDRepository positionCodeCRUDRepository;

    @Autowired
    private GenderCRUDRepository genderCRUDRepository;

    @Autowired
    private PolicyTypeCRUDRepository policyTypeCRUDRepository;

    @Autowired
    private PositionNomCRUDRepository positionNomCRUDRepository;

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
        } else if (entity instanceof Specialization) {
            saveOrDelete(specializationCRUDRepository, (Specialization) entity, action);
        } else if (entity instanceof PositionCode) {
            saveOrDelete(positionCodeCRUDRepository, (PositionCode) entity, action);
        } else if (entity instanceof Gender) {
            saveOrDelete(genderCRUDRepository, (Gender) entity, action);
        } else if (entity instanceof PolicyType) {
            saveOrDelete(policyTypeCRUDRepository, (PolicyType) entity, action);
        } else if (entity instanceof PositionNom) {
            saveOrDelete(positionNomCRUDRepository, (PositionNom) entity, action);
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
