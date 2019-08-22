package moscow.ptnl.contingent.nsi.ws;

import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeClass;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeKind;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeRelations;
import moscow.ptnl.contingent.area.entity.nsi.AreaTypeSpecializations;
import moscow.ptnl.contingent.domain.nsi.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper;
import moscow.ptnl.contingent.nsi.pushaccepter.PushAccepter;
import moscow.ptnl.contingent.repository.nsi.AreaTypeMedicalPositionsCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeRelationsCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypeSpecializationsCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesClassCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesKindCRUDRepository;
import moscow.ptnl.contingent.repository.nsi.NsiPushEventCRUDRepository;
import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CacheConfig;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.nsiproduct.nsiservice.v1.types.GetCatalogItemsRequest;
import ru.mos.emias.nsiproduct.nsiservice.v1.types.GetCatalogItemsResponse;
import ru.mos.emias.nsiproduct.nsiserviceasyncfasad.v1.NsiServiceAsyncFasadPortType;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.AdminServicePortType;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.Fault;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.SyncNsiRequest;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.SyncNsiResponse;

import java.util.List;

import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaTypeClasses;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaTypeKinds;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaTypeMedicalPositions;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaTypeRelations;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapAreaTypeSpecializations;

@Service(NsiAdminWebServiceImpl.SERVICE_NAME)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class NsiAdminWebServiceImpl implements AdminServicePortType {

    public static final String SERVICE_NAME = "NSI_ADMIN_V1";

    @Autowired
    PushAccepter pushAccepter;

    @Autowired
    NsiPushEventCRUDRepository nsiPushEventCRUDRepository;

    @Autowired
    NsiServiceAsyncFasadPortType nsiService;

    @Autowired
    AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    AreaTypesClassCRUDRepository areaTypesClassCRUDRepository;

    @Autowired
    AreaTypesKindCRUDRepository areaTypesKindCRUDRepository;

    @Autowired
    AreaTypeMedicalPositionsCRUDRepository areaTypeMedicalPositionsCRUDRepository;

    @Autowired
    AreaTypeRelationsCRUDRepository areaTypeRelationsCRUDRepository;

    @Autowired
    AreaTypeSpecializationsCRUDRepository areaTypeSpecializationsCRUDRepository;


    @Override
    public SyncNsiResponse syncNsi(SyncNsiRequest body) throws Fault {
        body.getCatalogCode().forEach(catalog -> {
            GetCatalogItemsRequest catalogDataRequest = new GetCatalogItemsRequest();
            catalogDataRequest.setIdCatalog(catalog);
            try {
                GetCatalogItemsResponse response = nsiService.getCatalogItems(catalogDataRequest);
                switch (NsiTablesEnum.getByCode(catalog)) {
                    case AREA_TYPE:
                        List<AreaType> areaTypes = NsiEntityMapper.mapAreaTypes(response.getEhdCatalogItems().getRows());
                        areaTypesCRUDRepository.saveAll(areaTypes);
                        break;
                    case AREA_TYPE_CLASS:
                        List<AreaTypeClass> areaTypeClasses = mapAreaTypeClasses(response.getEhdCatalogItems().getRows());
                        areaTypesClassCRUDRepository.saveAll(areaTypeClasses);
                        break;
                    case AREA_TYPE_KIND:
                        List<AreaTypeKind> areaTypeKinds = mapAreaTypeKinds(response.getEhdCatalogItems().getRows());
                        areaTypesKindCRUDRepository.saveAll(areaTypeKinds);
                        break;
                   case AREA_TYPE_MEDICAL_POSITIONS:
                        List<AreaTypeMedicalPositions> areaTypeMedicalPositions = mapAreaTypeMedicalPositions(response.getEhdCatalogItems().getRows());
                        areaTypeMedicalPositionsCRUDRepository.saveAll(areaTypeMedicalPositions);
                        break;
                    case AREA_TYPE_RELATIONS:
                        List<AreaTypeRelations> areaTypeRelations = mapAreaTypeRelations(response.getEhdCatalogItems().getRows());
                        areaTypeRelationsCRUDRepository.saveAll(areaTypeRelations);
                        break;
                    case AREA_TYPE_SPECIALIZATIONS:
                        List<AreaTypeSpecializations> areaTypeSpecializations = mapAreaTypeSpecializations(response.getEhdCatalogItems().getRows());
                        areaTypeSpecializationsCRUDRepository.saveAll(areaTypeSpecializations);
                }
            } catch (ru.mos.emias.nsiproduct.nsiserviceasyncfasad.v1.Fault fault) {
                fault.printStackTrace();
            }

        });
        return new SyncNsiResponse();
    }
}
