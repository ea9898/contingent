package moscow.ptnl.contingent.nsi.ws;

import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.infrastructure.service.setting.SettingService;
import moscow.ptnl.contingent.nsi.domain.NsiExternalEntity;
import moscow.ptnl.contingent.nsi.domain.NsiFormTablesEnum;
import moscow.ptnl.contingent.nsi.domain.area.*;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import moscow.ptnl.contingent.error.ValidationParameter;
import moscow.ptnl.contingent.nsi.domain.NsiTablesEnum;
import moscow.ptnl.contingent.nsi.error.NsiEhdErrorReason;
import moscow.ptnl.contingent.nsi.pushaccepter.NsiEntityMapper;
import moscow.ptnl.contingent.nsi.pushaccepter.PushAccepter;
import moscow.ptnl.contingent.nsi.repository.*;
import moscow.ptnl.contingent.nsi.service.NsiAdminService;
import moscow.ptnl.contingent.nsi.transform.SoapExceptionMapper;
import moscow.ptnl.contingent.nsi.transform.UpdateAddressByGlobalIdResponseMapper;
import moscow.ptnl.contingent.repository.CommonRepository;
import org.apache.cxf.annotations.SchemaValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.nsiproduct.core.v1.PagingOptions;
import ru.mos.emias.nsiproduct.nsiservice.v1.types.GetCatalogItemsRequest;
import ru.mos.emias.nsiproduct.nsiservice.v1.types.GetCatalogItemsResponse;
import ru.mos.emias.nsiproduct.nsiserviceasyncfasad.v1.NsiServiceAsyncFasadPortType;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.AdminServicePortType;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.Fault;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.EditAddressRequest;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.EditAddressResponse;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.SyncNsiRequest;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.SyncNsiResponse;

import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import ru.mos.emias.nsiproduct.core.v1.EhdCatalogRow;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.UpdateAddressByGlobalIdRequest;
import ru.mos.emias.pushaccepterproduct.adminservice.v1.types.UpdateAddressByGlobalIdResponse;


@Service(NsiAdminWebServiceImpl.SERVICE_NAME)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class NsiAdminWebServiceImpl implements AdminServicePortType {

    private final static Logger LOG = LoggerFactory.getLogger(NsiAdminWebServiceImpl.class);

    public static final String SERVICE_NAME = "NSI_ADMIN_V1";

    private static final String NSI_ENTITY_SOURCE = "syncNsi";

    @Autowired
    private NsiEntityMapper entityMapper;

    @Autowired
    private PushAccepter pushAccepter;

    @Autowired
    private SettingService settingService;

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

    @Autowired
    private PositionNomCRUDRepository positionNomCRUDRepository;

    @Autowired
    private AreaTypeProfileCRUDRepository areaTypeProfileCRUDRepository;

    @Autowired
    private PositionSuppCRUDRepository positionSuppCRUDRepository;

    @Autowired
    private MappingPositionCodeToOtherPositionCRUDRepository mappingPositionCodeToOtherPositionCRUDRepository;

    @Autowired
    private AreaService areaServiceDomain;

    @Autowired
    private NsiAdminService nsiAdminService;

    @Autowired
    private UpdateAddressByGlobalIdResponseMapper updateAddressByGlobalIdResponseMapper;

    @Override
    @Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
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
        PagingOptions pagingOptions = new PagingOptions();
        pagingOptions.setPageSize(100);
        int pageNum = 1;
        List<EhdCatalogRow> rows = new ArrayList<>();

        do {
            pagingOptions.setPageNumber(pageNum);
            catalogDataRequest.setPagingOptions(pagingOptions);
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
            pageNum++;
            rows.addAll(response.getEhdCatalogItems().getRows());
        } while (!response.getEhdCatalogItems().getRows().isEmpty());

        try {
            switch (nsiTablesEnum) {
                case AREA_TYPE:
                    List<AreaType> areaTypes = entityMapper.mapTypedList(rows, AreaType.class);
                    saveAll(areaTypesCRUDRepository, areaTypes, true);
                    break;
                case AREA_TYPE_CLASS:
                    List<AreaTypeClass> areaTypeClasses = entityMapper.mapTypedList(rows, AreaTypeClass.class);
                    saveAll(areaTypesClassCRUDRepository, areaTypeClasses, true);
                    break;
                case AREA_TYPE_KIND:
                    List<AreaTypeKind> areaTypeKinds = entityMapper.mapTypedList(rows, AreaTypeKind.class);
                    saveAll(areaTypesKindCRUDRepository, areaTypeKinds, true);
                    break;
                case AREA_TYPE_MEDICAL_POSITIONS:
                    List<AreaTypeMedicalPositions> areaTypeMedicalPositions = entityMapper.mapTypedList(rows, AreaTypeMedicalPositions.class);
                    saveAll(areaTypeMedicalPositionsCRUDRepository, areaTypeMedicalPositions, true);
                    break;
                case AREA_TYPE_RELATIONS:
                    List<AreaTypeRelations> areaTypeRelations = entityMapper.mapTypedList(rows, AreaTypeRelations.class);
                    saveAll(areaTypeRelationsCRUDRepository, areaTypeRelations, true);
                    break;
                case AREA_TYPE_SPECIALIZATIONS:
                    List<AreaTypeSpecializations> areaTypeSpecializations = entityMapper.mapTypedList(rows, AreaTypeSpecializations.class);
                    saveAll(areaTypeSpecializationsCRUDRepository, areaTypeSpecializations, true);
                    break;
                case SPECIALIZATION:
                    List<Specialization> specializations = entityMapper.mapTypedList(rows, Specialization.class);
                    saveAll(specializationCRUDRepository, specializations, true);
                    break;
                case POSITION_CODE:
                    List<PositionCode> positionCodes = entityMapper.mapTypedList(rows, PositionCode.class);
                    saveAll(positionCodeCRUDRepository, positionCodes, true);
                    break;
                case GENDER:
                    List<Gender> genders = entityMapper.mapTypedList(rows, Gender.class);
                    saveAll(genderCRUDRepository, genders, true);
                    break;
                case POLICY_TYPE:
                    List<PolicyType> policyTypes = entityMapper.mapTypedList(rows, PolicyType.class);
                    saveAll(policyTypeCRUDRepository, policyTypes, true);
                    break;
                case D_POSITION_NOM:
                    List<PositionNom> positionNoms = entityMapper.mapTypedList(rows, PositionNom.class);
                    saveAll(positionNomCRUDRepository, positionNoms, true);
                    break;
                case AREA_TYPE_PROFILE:
                    List<AreaTypeProfile> areaTypeProfiles = entityMapper.mapTypedList(rows, AreaTypeProfile.class);
                    saveAll(areaTypeProfileCRUDRepository, areaTypeProfiles, true);
                    break;
                case POSITION_SUPP:
                    List<PositionSupp> positionSupps = entityMapper.mapTypedList(rows, PositionSupp.class);
                    saveAll(positionSuppCRUDRepository, positionSupps, true);
                    break;
                case MAPPING_POSITIONCODE_TO_OTHERPOSITION:
                    List<MappingPositionCodeToOtherPosition> mappingPositionCodeToOtherPositions =
                            entityMapper.mapTypedList(rows, MappingPositionCodeToOtherPosition.class);
                    saveAll(mappingPositionCodeToOtherPositionCRUDRepository, mappingPositionCodeToOtherPositions, true);
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

    @Override
    public UpdateAddressByGlobalIdResponse updateAddressByGlobalId(UpdateAddressByGlobalIdRequest body) throws Fault {
        if (body.getArGlobalId().isEmpty()) {
            return new UpdateAddressByGlobalIdResponse();
        }
        List<Long> unrecognizedAddresses;
        Validation validation = new Validation();

        try {
            long maxIdsNumber = settingService.getSettingProperty(SettingService.UPDATE_ADDRESS_BY_GLOBAL_ID_MAXCOUNT);

            if (body.getArGlobalId().size() > maxIdsNumber) {
                validation.error(NsiEhdErrorReason.TOO_MANY_ADDRESS_IDS, new ValidationParameter(SettingService.UPDATE_ADDRESS_BY_GLOBAL_ID_MAXCOUNT, maxIdsNumber));
            }
            NsiFormTablesEnum entityType = NsiFormTablesEnum.findByName(body.getTableName());

            if (entityType == null) {
                validation.error(NsiEhdErrorReason.INCORRECT_TABLE_NAME);
            }
            if (!validation.isSuccess()) {
                throw SoapExceptionMapper.map(new ContingentException(validation));
            }
            unrecognizedAddresses = nsiAdminService.updateAddressesByGlobalId(body.getFormId(), body.getArGlobalId(), entityType);
        }
        catch (Fault e) {
            throw e;
        }
        catch (Exception e) {
            validation.error(NsiEhdErrorReason.UNEXPECTED_ERROR, new ValidationParameter("message", e.getMessage()));
            throw SoapExceptionMapper.map(new ContingentException(validation));
        }
        return updateAddressByGlobalIdResponseMapper.transform(unrecognizedAddresses);
    }

    @Override
    public EditAddressResponse editAddress(EditAddressRequest body) throws Fault {
        try {
            Map<String, String> mapOpts = new HashMap<>();
            body.getOptions().getEntry().forEach( opt -> mapOpts.put(opt.getKey(), opt.getValue()));
            areaServiceDomain.editAddress(body.getArGlobalId(), mapOpts);
            return new EditAddressResponse();
        } catch (Exception ex) {
            throw mapException(ex);
        }
    }

    private <T extends Serializable, K extends Serializable> void saveAll(CommonRepository<T, K> repository, List<T> entities, boolean archiveAbsent) {
        List<Long> updatedGlobalId = new ArrayList<>();

        entities.forEach(e -> {
            if (e instanceof NsiExternalEntity) {
                ((NsiExternalEntity) e).setUpdateDate(LocalDateTime.now());
                ((NsiExternalEntity) e).setSource(NSI_ENTITY_SOURCE);

                updatedGlobalId.add(((NsiExternalEntity) e).getGlobalId());
            }
        });
        repository.saveAll(entities);

        if (archiveAbsent) {
            repository.findAll().stream()
                    .filter(elem -> !updatedGlobalId.contains(((NsiExternalEntity) elem).getGlobalId()))
                    .forEach(e -> {
                        ((NsiExternalEntity) e).setArchived(true);
                        repository.save(e);
                    });
        }

    }


    private Fault mapException(Exception ex) {
        if (!(ex instanceof ContingentException)) {
            LOG.error(ex.getMessage(), ex);
        }
        return SoapExceptionMapper.map(ex);
    }

}
