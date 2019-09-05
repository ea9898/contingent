package moscow.ptnl.contingent.nsi.ws;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClass;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKind;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.domain.area.Gender;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.area.Specialization;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.error.NsiEhdErrorReason;
import moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper;
import moscow.ptnl.contingent.nsi.pushaccepter.PushAccepter;
import moscow.ptnl.contingent.nsi.transform.SoapExceptionMapper;
import moscow.ptnl.contingent.nsi.repository.AreaTypeMedicalPositionsCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeRelationsCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypeSpecializationsCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesClassCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.AreaTypesKindCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.GenderCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.NsiPushEventCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.PositionCodeCRUDRepository;
import moscow.ptnl.contingent.nsi.repository.SpecializationCRUDRepository;
import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.beans.factory.annotation.Autowired;
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
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapGenders;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapPositionCodes;
import static moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper.mapSpecializations;

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

    @Autowired
    SpecializationCRUDRepository specializationCRUDRepository;

    @Autowired
    PositionCodeCRUDRepository positionCodeCRUDRepository;

    @Autowired
    GenderCRUDRepository genderCRUDRepository;


    @Override
    public SyncNsiResponse syncNsi(SyncNsiRequest body) throws Fault {
        Validation validation = new Validation();

        NsiTablesEnum nsiTablesEnum = NsiTablesEnum.getByName(body.getCatalogCode());
        if (NsiTablesEnum.UNKNOWN.equals(nsiTablesEnum)) {
            validation.error(NsiEhdErrorReason.CATALOG_ID_NOT_FOUND, new ValidationParameter("catalogCode", body.getCatalogCode()));
        }

        if (!validation.isSuccess()) {
            throw SoapExceptionMapper.map(new ContingentException(validation));
        }

        GetCatalogItemsRequest catalogDataRequest = new GetCatalogItemsRequest();
        catalogDataRequest.setIdCatalog(nsiTablesEnum.getCode());

        GetCatalogItemsResponse response = null;
        try {
            response = nsiService.getCatalogItems(catalogDataRequest);
        } catch (Exception e) {
            if (e.getMessage() == null) {
                validation.error(NsiEhdErrorReason.CANNOT_UPDATE_DICT);
            } else {
                validation.error(NsiEhdErrorReason.UNEXPECTED_ERROR, new ValidationParameter("error", e.getMessage()));
            }
        } finally {
            if (response == null && validation.isSuccess()) {
                validation.error(NsiEhdErrorReason.CANNOT_UPDATE_DICT);
            }
        }

        if (!validation.isSuccess()) {
            throw SoapExceptionMapper.map(new ContingentException(validation));
        }

        try {
            switch (nsiTablesEnum) {
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
                    break;
                case SPECIALIZATIONS:
                    List<Specialization> specializations = mapSpecializations(response.getEhdCatalogItems().getRows());
                    specializationCRUDRepository.saveAll(specializations);
                    break;
                case POSITION_CODE:
                    List<PositionCode> positionCodes = mapPositionCodes(response.getEhdCatalogItems().getRows());
                    positionCodeCRUDRepository.saveAll(positionCodes);
                    break;
                case GENDER:
                    List<Gender> genders = mapGenders(response.getEhdCatalogItems().getRows());
                    genderCRUDRepository.saveAll(genders);
                    break;
            }
        } catch (Exception e) {
            validation.error(NsiEhdErrorReason.UPDATE_DICT_ERROR, new ValidationParameter("message", e.getMessage()));
        }

        if (!validation.isSuccess()) {
            throw SoapExceptionMapper.map(new ContingentException(validation));
        }

        return new SyncNsiResponse();
    }
}
