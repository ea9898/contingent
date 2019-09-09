package moscow.ptnl.contingent.nsi.ws;

import moscow.ptnl.contingent.nsi.domain.area.AreaType;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeClass;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeKind;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeMedicalPositions;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeRelations;
import moscow.ptnl.contingent.nsi.domain.area.AreaTypeSpecializations;
import moscow.ptnl.contingent.nsi.domain.area.Gender;
import moscow.ptnl.contingent.nsi.domain.area.PolicyType;
import moscow.ptnl.contingent.nsi.domain.area.PositionCode;
import moscow.ptnl.contingent.nsi.domain.area.Specialization;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.error.NsiEhdErrorReason;
import moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper;
import moscow.ptnl.contingent.nsi.pushaccepter.PushAccepter;
import moscow.ptnl.contingent.nsi.repository.PolicyTypeCRUDRepository;
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
import ru.mos.emias.nsiproduct.core.v1.EhdCatalog;
import ru.mos.emias.nsiproduct.nsiservice.v1.types.GetCatalogItemsRequest;
import ru.mos.emias.nsiproduct.nsiservice.v1.types.GetCatalogItemsResponse;
import ru.mos.emias.nsiproduct.nsiserviceasyncfasad.v1.NsiServiceAsyncFasadPortType;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.AdminServicePortType;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.Fault;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.SyncNsiRequest;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.SyncNsiResponse;

import java.util.List;
import ru.mos.emias.nsiproduct.core.v1.EhdCatalogRow;


@Service(NsiAdminWebServiceImpl.SERVICE_NAME)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class NsiAdminWebServiceImpl implements AdminServicePortType {

    public static final String SERVICE_NAME = "NSI_ADMIN_V1";
    
    @Autowired
    private NsiEntityMapper entityMapper;

    @Autowired
    private PushAccepter pushAccepter;

    @Autowired
    private NsiPushEventCRUDRepository nsiPushEventCRUDRepository;

    @Autowired
    private NsiServiceAsyncFasadPortType nsiService;

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


    @Override
    public SyncNsiResponse syncNsi(SyncNsiRequest body) throws Fault {
        Validation validation = new Validation();

        NsiTablesEnum nsiTablesEnum = NsiTablesEnum.getByName(body.getCatalogName());
        if (NsiTablesEnum.UNKNOWN.equals(nsiTablesEnum)) {
            validation.error(NsiEhdErrorReason.CATALOG_ID_NOT_FOUND, new ValidationParameter("catalogCode", body.getCatalogName()));
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
            
            List<EhdCatalogRow> rows = response.getEhdCatalogItems().getRows();
            
            switch (nsiTablesEnum) {
                case AREA_TYPE:
                    List<AreaType> areaTypes = entityMapper.mapTypedList(rows, AreaType.class);
                    areaTypesCRUDRepository.saveAll(areaTypes);
                    break;
                case AREA_TYPE_CLASS:
                    List<AreaTypeClass> areaTypeClasses = entityMapper.mapTypedList(rows, AreaTypeClass.class);
                    areaTypesClassCRUDRepository.saveAll(areaTypeClasses);
                    break;
                case AREA_TYPE_KIND:
                    List<AreaTypeKind> areaTypeKinds = entityMapper.mapTypedList(rows, AreaTypeKind.class);
                    areaTypesKindCRUDRepository.saveAll(areaTypeKinds);
                    break;
               case AREA_TYPE_MEDICAL_POSITIONS:
                    List<AreaTypeMedicalPositions> areaTypeMedicalPositions = entityMapper.mapTypedList(rows, AreaTypeMedicalPositions.class);
                    areaTypeMedicalPositionsCRUDRepository.saveAll(areaTypeMedicalPositions);
                    break;
               case AREA_TYPE_RELATIONS:
                    List<AreaTypeRelations> areaTypeRelations = entityMapper.mapTypedList(rows, AreaTypeRelations.class);
                    areaTypeRelationsCRUDRepository.saveAll(areaTypeRelations);
                    break;
               case AREA_TYPE_SPECIALIZATIONS:
                    List<AreaTypeSpecializations> areaTypeSpecializations = entityMapper.mapTypedList(rows, AreaTypeSpecializations.class);
                    areaTypeSpecializationsCRUDRepository.saveAll(areaTypeSpecializations);
                    break;
               case SPECIALIZATION:
                    List<Specialization> specializations = entityMapper.mapTypedList(rows, Specialization.class);
                    specializationCRUDRepository.saveAll(specializations);
                    break;
               case POSITION_CODE:
                    List<PositionCode> positionCodes = entityMapper.mapTypedList(rows, PositionCode.class);
                    positionCodeCRUDRepository.saveAll(positionCodes);
                    break;
               case GENDER:
                    List<Gender> genders = entityMapper.mapTypedList(rows, Gender.class);
                    genderCRUDRepository.saveAll(genders);
                    break;
               case POLICY_TYPE:
                    List<PolicyType> policyTypes = entityMapper.mapTypedList(rows, PolicyType.class);
                    policyTypeCRUDRepository.saveAll(policyTypes);
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
