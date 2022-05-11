package moscow.ptnl.contingent.area.ws.v3;

import moscow.ptnl.contingent.area.transform.OptionEnum;
import moscow.ptnl.contingent.area.transform.SoapBaseExceptionMapper;
import moscow.ptnl.contingent.area.transform.SoapVersioningMapper;
import moscow.ptnl.contingent.area.transform.v1.model.options.GetAreaListBriefOptions;
import moscow.ptnl.contingent.area.transform.v1.model.sorting.GetAreaListBriefSorting;
import moscow.ptnl.contingent.area.transform.v3.AddMedicalEmployeeMapperV3;
import moscow.ptnl.contingent.area.transform.v3.AddressRegistryToAddressRegistryBaseMapperV3;
import moscow.ptnl.contingent.area.transform.v3.AreaBriefMapperV3;
import moscow.ptnl.contingent.area.transform.v3.AreaMapperV3;
import moscow.ptnl.contingent.area.transform.v3.ChangeMedicalEmployeeMapperV3;
import moscow.ptnl.contingent.area.transform.v3.GetAreaHistoryMapperV3;
import moscow.ptnl.contingent.area.transform.v3.MuAvailableAreaTypes2Mapper;
import moscow.ptnl.contingent.area.transform.v3.MuAvailableAreaTypesInMoMapper;
import moscow.ptnl.contingent.area.transform.v3.SearchAreaAddressMapperV3;
import moscow.ptnl.contingent.area.transform.v3.SoapCustomMapperV3;
import moscow.ptnl.contingent.area.ws.BaseService;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.MoMuService;
import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.MuMuService;
import moscow.ptnl.contingent.domain.area.model.area.AreaFullHistory;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.domain.area.model.area.MedicalEmployee;
import moscow.ptnl.contingent.security.annotation.EMIASSecured;
import moscow.ptnl.metrics.Metrics;
import org.apache.cxf.annotations.SchemaValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.contingent2.area.v3.types.GetAreaListBriefRequest;
import ru.mos.emias.contingent2.area.v3.types.GetAreaListBriefResponse;
import ru.mos.emias.contingent2.area.v3.types.DelMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v3.types.DelMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v3.types.GetMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v3.types.GetMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v3.types.GetMuAvailableAreaTypesInMoRequest;
import ru.mos.emias.contingent2.area.v3.types.GetMuAvailableAreaTypesInMoResponse;
import ru.mos.emias.contingent2.area.v3.types.GetMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v3.types.GetMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v3.types.AddMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v3.types.AddMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v3.AreaPT;
import ru.mos.emias.contingent2.area.v3.Fault;
import ru.mos.emias.contingent2.area.v3.types.AddAreaAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.AddAreaAddressResponse;
import ru.mos.emias.contingent2.area.v3.types.AddMoAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.AddMoAddressResponse;
import ru.mos.emias.contingent2.area.v3.types.AddMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v3.types.AddMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v3.types.ArchiveAreaRequest;
import ru.mos.emias.contingent2.area.v3.types.ArchiveAreaResponse;
import ru.mos.emias.contingent2.area.v3.types.CreateDependentAreaRequest;
import ru.mos.emias.contingent2.area.v3.types.CreateDependentAreaResponse;
import ru.mos.emias.contingent2.area.v3.types.CreateOrderRequest;
import ru.mos.emias.contingent2.area.v3.types.CreateOrderResponse;
import ru.mos.emias.contingent2.area.v3.types.CreatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v3.types.CreatePrimaryAreaResponse;
import ru.mos.emias.contingent2.area.v3.types.DelAreaAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.DelAreaAddressResponse;
import ru.mos.emias.contingent2.area.v3.types.DelMoAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.DelMoAddressResponse;
import ru.mos.emias.contingent2.area.v3.types.DelMoAddressTotalRequest;
import ru.mos.emias.contingent2.area.v3.types.DelMoAddressTotalResponse;
import ru.mos.emias.contingent2.area.v3.types.DelMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v3.types.DelMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v3.types.GetAreaAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.GetAreaAddressResponse;
import ru.mos.emias.contingent2.area.v3.types.GetAreaByIdRequest;
import ru.mos.emias.contingent2.area.v3.types.GetAreaByIdResponse;
import ru.mos.emias.contingent2.area.v3.types.GetAreaHistoryRequest;
import ru.mos.emias.contingent2.area.v3.types.GetAreaHistoryResponse;
import ru.mos.emias.contingent2.area.v3.types.GetMoAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.GetMoAddressResponse;
import ru.mos.emias.contingent2.area.v3.types.GetMoAddressTotalRequest;
import ru.mos.emias.contingent2.area.v3.types.GetMoAddressTotalResponse;
import ru.mos.emias.contingent2.area.v3.types.GetMuAvailableAreaTypes2Request;
import ru.mos.emias.contingent2.area.v3.types.GetMuAvailableAreaTypes2Response;
import ru.mos.emias.contingent2.area.v3.types.GetMuMuServiceRequest;
import ru.mos.emias.contingent2.area.v3.types.GetMuMuServiceResponse;
import ru.mos.emias.contingent2.area.v3.types.GetNewAreaIdRequest;
import ru.mos.emias.contingent2.area.v3.types.GetNewAreaIdResponse;
import ru.mos.emias.contingent2.area.v3.types.InitiateAddAreaAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.InitiateAddAreaAddressResponse;
import ru.mos.emias.contingent2.area.v3.types.InitiateAddMoAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.InitiateAddMoAddressResponse;
import ru.mos.emias.contingent2.area.v3.types.InitiateCreatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v3.types.InitiateCreatePrimaryAreaResponse;
import ru.mos.emias.contingent2.area.v3.types.RestoreAreaRequest;
import ru.mos.emias.contingent2.area.v3.types.RestoreAreaResponse;
import ru.mos.emias.contingent2.area.v3.types.SearchAreaRequest;
import ru.mos.emias.contingent2.area.v3.types.SearchAreaResponse;
import ru.mos.emias.contingent2.area.v3.types.SearchDnAreaRequest;
import ru.mos.emias.contingent2.area.v3.types.SearchDnAreaResponse;
import ru.mos.emias.contingent2.area.v3.types.SearchMoAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.SearchMoAddressResponse;
import ru.mos.emias.contingent2.area.v3.types.SearchMuByAreaAddressRequest;
import ru.mos.emias.contingent2.area.v3.types.SearchMuByAreaAddressResponse;
import ru.mos.emias.contingent2.area.v3.types.SearchOrderRequest;
import ru.mos.emias.contingent2.area.v3.types.SearchOrderResponse;
import ru.mos.emias.contingent2.area.v3.types.SetAreaMuServiceRequest;
import ru.mos.emias.contingent2.area.v3.types.SetAreaMuServiceResponse;
import ru.mos.emias.contingent2.area.v3.types.SetMedicalEmployeeOnAreaRequest;
import ru.mos.emias.contingent2.area.v3.types.SetMedicalEmployeeOnAreaResponse;
import ru.mos.emias.contingent2.area.v3.types.UpdateDependentAreaRequest;
import ru.mos.emias.contingent2.area.v3.types.UpdateDependentAreaResponse;
import ru.mos.emias.contingent2.area.v3.types.UpdateOrderRequest;
import ru.mos.emias.contingent2.area.v3.types.UpdateOrderResponse;
import ru.mos.emias.contingent2.area.v3.types.UpdatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v3.types.UpdatePrimaryAreaResponse;
import ru.mos.emias.contingent2.core.v3.AreaHistory;
import ru.mos.emias.contingent2.core.v3.MuService;

import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

/**
 *
 * @author m.kachalov
 */
@Service(AreaServiceImpl.SERVICE_NAME)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AreaServiceImpl extends BaseService implements AreaPT {
    
    private final static Logger LOG = LoggerFactory.getLogger(AreaServiceImpl.class);
    
    public static final String SERVICE_NAME = "AREA-V3";
    
    @Autowired
    private SoapBaseExceptionMapper<ru.mos.emias.contingent2.area.v3.Fault> exceptionMapper;
    
    @Autowired
    private SoapVersioningMapper versioningMapper;
    
    @Autowired
    private ru.mos.emias.contingent2.area.AreaPT areaServiceV1;
    
    @Autowired
    private ru.mos.emias.contingent2.area.v2.AreaPT areaServiceV2;

    @Autowired
    private SoapCustomMapperV3 soapCustomMapper;

    @Autowired
    private AreaService areaServiceDomain;

    @Autowired
    private MoMuService moMuMuServiceDomain;

    @Autowired
    private AreaMapperV3 areaMapper;

    @Autowired
    private MuAvailableAreaTypes2Mapper muAvailableAreaTypes2Mapper;

    @Autowired
    private AreaBriefMapperV3 areaBriefMapper;

    @Autowired
    private SearchAreaAddressMapperV3 searchAreaAddressMapper;

    @Autowired
    private MuAvailableAreaTypesInMoMapper muAvailableAreaTypesInMoMapper;

    @Autowired
    private GetAreaHistoryMapperV3 getAreaHistoryMapper;

    @Autowired
    private AddMedicalEmployeeMapperV3 addMedicalEmployeeMapper;

    @Autowired
    private ChangeMedicalEmployeeMapperV3 changeMedicalEmployeeMapper;

    @Autowired
    private AddressRegistryToAddressRegistryBaseMapperV3 addressRegistryBaseMapper;

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public InitiateAddMoAddressResponse initiateAddMoAddress(InitiateAddMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.initiateAddMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.InitiateAddMoAddressRequest())),
                    new InitiateAddMoAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchOrderResponse searchOrder(SearchOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.searchOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.SearchOrderRequest())),
                    new SearchOrderResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMoAddressResponse getMoAddress(GetMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.getMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.GetMoAddressRequest())),
                    new GetMoAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelAreaAddressResponse delAreaAddress(DelAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelAreaAddressRequest())),
                    new DelAreaAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMoAddressTotalResponse delMoAddressTotal(DelMoAddressTotalRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.delMoAddressTotal(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.DelMoAddressTotalRequest())),
                    new DelMoAddressTotalResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public UpdateDependentAreaResponse updateDependentArea(UpdateDependentAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.updateDependentArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.UpdateDependentAreaRequest())),
                    new UpdateDependentAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public InitiateAddAreaAddressResponse initiateAddAreaAddress(InitiateAddAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.initiateAddAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.InitiateAddAreaAddressRequest())),
                    new InitiateAddAreaAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public CreatePrimaryAreaResponse createPrimaryArea(CreatePrimaryAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.createPrimaryArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.CreatePrimaryAreaRequest())),
                    new CreatePrimaryAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public UpdatePrimaryAreaResponse updatePrimaryArea(UpdatePrimaryAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.updatePrimaryArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.UpdatePrimaryAreaRequest())),
                    new UpdatePrimaryAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetAreaByIdResponse getAreaById(GetAreaByIdRequest body) throws Fault {
        try {
            AreaInfo area = areaServiceDomain.getAreaByIdV2(body.getAreaId());
            GetAreaByIdResponse response = new GetAreaByIdResponse();
            response.setResult(areaMapper.entityToDtoTransform(area));
            return response;
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public CreateDependentAreaResponse createDependentArea(CreateDependentAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.createDependentArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.CreateDependentAreaRequest())),
                    new CreateDependentAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetAreaListBriefResponse getAreaListBrief(GetAreaListBriefRequest body) throws Fault {
        try {
            Map<GetAreaListBriefOptions, ? extends OptionEnum.OptionValuesEnum> options =
                    soapCustomMapper.mapOptions(body.getOptions(), GetAreaListBriefOptions.class);
            GetAreaListBriefResponse response = new GetAreaListBriefResponse();
            response.setResult(new GetAreaListBriefResponse.Result());
            PageRequest pageRequest = soapCustomMapper.mapPagingOptions(body.getPagingOptions(), EnumSet.allOf(GetAreaListBriefSorting.class));
            GetAreaListBriefOptions.ShowMeValues showMeOption = (GetAreaListBriefOptions.ShowMeValues) options.get(GetAreaListBriefOptions.SHOW_ME);
            Boolean showMe = GetAreaListBriefOptions.ShowMeValues.ALL.equals(showMeOption) ? Boolean.TRUE :
                    (GetAreaListBriefOptions.ShowMeValues.NONE.equals(showMeOption) ? Boolean.FALSE : null);
            Page<AreaInfo> areas = areaServiceDomain.getAreaListBriefV2(body.getAreas().getIds(), showMe, pageRequest);
            soapCustomMapper.mapPagingResults(response.getResult(), areas);

            if (!areas.isEmpty()) {
                response.getResult().getAreas().addAll(areas.stream()
                        .map(a -> areaBriefMapper.entityToDtoTransform(a))
                        .collect(Collectors.toList()));
            }
            return response;
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public UpdateOrderResponse updateOrder(UpdateOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.updateOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.UpdateOrderRequest())),
                    new UpdateOrderResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public ArchiveAreaResponse archiveArea(ArchiveAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.archiveArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.ArchiveAreaRequest())),
                    new ArchiveAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMoAddressTotalResponse getMoAddressTotal(GetMoAddressTotalRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.getMoAddressTotal(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.GetMoAddressTotalRequest())),
                    new GetMoAddressTotalResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetAreaHistoryResponse getAreaHistory(GetAreaHistoryRequest body) throws Fault {
        try {
            GetAreaHistoryResponse response = new GetAreaHistoryResponse();
            AreaFullHistory results = areaServiceDomain.getAreaHistory3(body.getAreaId());
            response.setAreaId(results.getAreaId());

            if (!results.getAreaEvents().isEmpty()) {
                response.setAreaEvents(new AreaHistory.AreaEvents());
                response.getAreaEvents().getEvents().addAll(results.getAreaEvents().stream()
                        .map(getAreaHistoryMapper::entityToDtoTransform)
                        .collect(Collectors.toList()));
            }
            if (!results.getMedicalEmployeeEvents().isEmpty()) {
                response.setMedicalEmployeeEvents(new AreaHistory.MedicalEmployeeEvents());
                response.getMedicalEmployeeEvents().getEvents().addAll(results.getMedicalEmployeeEvents().stream()
                        .map(getAreaHistoryMapper::entityToDtoTransform)
                        .collect(Collectors.toList()));
            }
            return response;
        } catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetAreaAddressResponse getAreaAddress(GetAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.getAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.GetAreaAddressRequest())),
                    new GetAreaAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public RestoreAreaResponse restoreArea(RestoreAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.restoreArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.RestoreAreaRequest())),
                    new RestoreAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddMuAvailableAreaTypesResponse addMuAvailableAreaTypes(AddMuAvailableAreaTypesRequest body) throws Fault {
        try {
            moMuMuServiceDomain.addMuAvailableAreaTypesV3(body.getMoId(), body.getMuId(), body.getAreaTypeCodes().getAreaTypeCodes());

            return new AddMuAvailableAreaTypesResponse();
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public InitiateCreatePrimaryAreaResponse initiateCreatePrimaryArea(InitiateCreatePrimaryAreaRequest body) throws Fault {
        try {
            InitiateCreatePrimaryAreaResponse response = new InitiateCreatePrimaryAreaResponse();
            Long id = areaServiceDomain.initiateCreatePrimaryArea(
                    body.getMoId(),
                    body.getMuId(),
                    body.getNumber(),
                    body.getDescription(),
                    body.getAreaTypeCode(),
                    null,
                    body.getPolicyTypes() == null ? new ArrayList<>() : body.getPolicyTypes().getPolicyTypeCodes(),
                    body.getAgeMin(),
                    body.getAgeMax(),
                    body.getAgeMinM(),
                    body.getAgeMaxM(),
                    body.getAgeMinW(),
                    body.getAgeMaxW(),
                    body.isAutoAssignForAttachment(),
                    body.isAttachByMedicalReason(),
                    body.getAddMedicalEmployees() == null ? Collections.emptyList() : body.getAddMedicalEmployees().getAddMedicalEmployees().stream()
                            .map(addMedicalEmployeeMapper::dtoToEntityTransform).collect(Collectors.toList()),
                    body.getAddresses() == null ? Collections.emptyList() : body.getAddresses().stream()
                            .map(addressRegistryBaseMapper::dtoToEntityTransform).collect(Collectors.toList()));

            response.setId(id);
            return response;
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SetAreaMuServiceResponse setAreaMuService(SetAreaMuServiceRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.setAreaMuService(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.SetAreaMuServiceRequest())),
                    new SetAreaMuServiceResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchDnAreaResponse searchDnArea(SearchDnAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.searchDnArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.SearchDnAreaRequest())),
                    new SearchDnAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SetMedicalEmployeeOnAreaResponse setMedicalEmployeeOnArea(SetMedicalEmployeeOnAreaRequest body) throws Fault {
        try {
            List<Long> assignmentIds = areaServiceDomain.setMedicalEmployeeOnArea(body.getAreaId(),
                    body.getAddMedicalEmployees() == null ? Collections.emptyList() : body.getAddMedicalEmployees().getAddMedicalEmployees().stream().map(
                            addMedicalEmployeeMapper::dtoToEntityTransform).collect(Collectors.toList()),
                    body.getChangeMedicalEmployees() == null ? Collections.emptyList() : body.getChangeMedicalEmployees().getChangeMedicalEmployees().stream().map(
                            changeMedicalEmployeeMapper::dtoToEntityTransform).collect(Collectors.toList()));

            SetMedicalEmployeeOnAreaResponse response = new SetMedicalEmployeeOnAreaResponse();
            response.getAssignmentIds().addAll(assignmentIds);
            return response;
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchAreaResponse searchArea(SearchAreaRequest body) throws Fault {
        try {
            Page<AreaInfo> areas = areaServiceDomain.searchArea(body.getAreaTypeClassCode(), body.getMoId(),
                    body.getMuIds() == null ? Collections.emptyList() : body.getMuIds(),
                    body.getAreaTypeCodes() == null ? Collections.emptyList() : body.getAreaTypeCodes(),
                    null, body.getMuService() != null ? body.getMuService().getMuIds() : Collections.emptyList(),
                    body.getNumber(), body.getDescription(), body.isIsArchived(),
                    body.getMedicalEmployees() == null ? Collections.emptyList() :
                            body.getMedicalEmployees().stream().map(me -> new MedicalEmployee(me.getMedicalEmployeeJobId(), me.getSnils()))
                                    .filter(empl -> empl.getMedicalEmployeeJobId() != null || empl.getSnils()!= null)
                                    .collect(Collectors.toList()),
                    body.getAddresses() == null ? Collections.emptyList() : body.getAddresses().stream().map(searchAreaAddressMapper::dtoToEntityTransform).collect(Collectors.toList()),
                    body.isIsExactAddressMatch(),
                    soapCustomMapper.mapPagingOptions(body.getPagingOptions(), null), true);
            SearchAreaResponse response = new SearchAreaResponse();
            response.setResult(new SearchAreaResponse.Result());
            soapCustomMapper.mapPagingResults(response.getResult(), areas);

            if (!areas.isEmpty()) {
                response.getResult().getAreas().addAll(areas.stream()
                        .map(area -> areaMapper.areaSearchResultTransform(area))
                        .collect(Collectors.toList()));
            }
            return response;
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchMuByAreaAddressResponse searchMuByAreaAddress(SearchMuByAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.searchMuByAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.SearchMuByAreaAddressRequest())),
                    new SearchMuByAreaAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchMoAddressResponse searchMoAddress(SearchMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.searchMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.SearchMoAddressRequest())),
                    new SearchMoAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddMoAddressResponse addMoAddress(AddMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV2.addMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v2.types.AddMoAddressRequest())),
                    new AddMoAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMuAvailableAreaTypesResponse delMuAvailableAreaTypes(DelMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delMuAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelMuAvailableAreaTypesRequest())),
                    new DelMuAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public CreateOrderResponse createOrder(CreateOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.createOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.CreateOrderRequest())),
                    new CreateOrderResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override //@EMIASSecured(faultClass = Fault.class) @Metrics
    public GetNewAreaIdResponse getNewAreaId(GetNewAreaIdRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getNewAreaId(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetNewAreaIdRequest())),
                    new GetNewAreaIdResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddAreaAddressResponse addAreaAddress(AddAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.addAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.AddAreaAddressRequest())),
                    new AddAreaAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMoAddressResponse delMoAddress(DelMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelMoAddressRequest())),
                    new DelMoAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMuAvailableAreaTypes2Response getMuAvailableAreaTypes2(GetMuAvailableAreaTypes2Request body) throws Fault {
        try {
            GetMuAvailableAreaTypes2Response getMuAvailableAreaTypes2Response = new GetMuAvailableAreaTypes2Response();
            getMuAvailableAreaTypes2Response.getMuAvailableAreaTypes().addAll(moMuMuServiceDomain.getMuAvailableAreaTypes2(body.getMuId())
            .stream().map(muAvailableAreaTypes2Mapper::entityToDtoTransform).collect(Collectors.toList()));
            return getMuAvailableAreaTypes2Response;
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddMoAvailableAreaTypesResponse addMoAvailableAreaTypes(AddMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.addMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.AddMoAvailableAreaTypesRequest())),
                    new AddMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMoAvailableAreaTypesResponse delMoAvailableAreaTypes(DelMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelMoAvailableAreaTypesRequest())),
                    new DelMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMoAvailableAreaTypesResponse getMoAvailableAreaTypes(GetMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetMoAvailableAreaTypesRequest())),
                    new GetMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMuAvailableAreaTypesResponse getMuAvailableAreaTypes(GetMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getMuAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetMuAvailableAreaTypesRequest())),
                    new GetMuAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMuMuServiceResponse getMuMuService(GetMuMuServiceRequest body) throws Fault {
        try {
            List<MuMuService> results = moMuMuServiceDomain.getMuMuService(body.getMuId(), body.getAreaTypeCode());

            GetMuMuServiceResponse response = new GetMuMuServiceResponse();
            response.setServiceMu(new MuService());
            response.getServiceMu().getMuIds().addAll(results.stream()
                    .map(MuMuService::getServiceMuId)
                    .sorted()
                    .collect(Collectors.toList())
            );
            return response;
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMuAvailableAreaTypesInMoResponse getMuAvailableAreaTypesInMo(GetMuAvailableAreaTypesInMoRequest body) throws Fault {
        try {
            Page<Long> moIdPage = moMuMuServiceDomain.checkMoIdsInMaat(body.getMoIds(), soapCustomMapper.mapPagingOptions(body.getPagingOptions(), null));
            List<MuAvailableAreaTypes> results = moMuMuServiceDomain.getMuAvailableAreaTypesInMo(moIdPage.getContent());
            GetMuAvailableAreaTypesInMoResponse response = new GetMuAvailableAreaTypesInMoResponse();
            response.setResult(new GetMuAvailableAreaTypesInMoResponse.Result());
            if (moIdPage.getContent().size() > 0) {
                response.getResult().getMuAvailableAreaTypes().addAll(muAvailableAreaTypesInMoMapper.entityToDtoTransform(results));
            }
            soapCustomMapper.mapPagingResults(response.getResult(), moIdPage);
            return response;
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }
}
