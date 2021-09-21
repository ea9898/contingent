package moscow.ptnl.contingent.area.ws.v2;

import moscow.ptnl.contingent.area.transform.OptionEnum;
import moscow.ptnl.contingent.area.transform.SoapVersioningMapper;
import moscow.ptnl.contingent.area.transform.UserContextMapper;
import moscow.ptnl.contingent.area.transform.v1.model.options.GetAreaListBriefOptions;
import moscow.ptnl.contingent.area.transform.v1.model.options.GetAreaListBriefOptions.ShowMeValues;
import moscow.ptnl.contingent.area.transform.v1.model.sorting.GetAreaListBriefSorting;
import moscow.ptnl.contingent.area.transform.v2.AddMedicalEmployeeMapperV2;
import moscow.ptnl.contingent.area.transform.v2.AddressRegistryToAddressRegistryBaseMapperV2;
import moscow.ptnl.contingent.area.transform.v2.AreaBriefMapperV2;
import moscow.ptnl.contingent.area.transform.v2.AreaDnMapperV2;
import moscow.ptnl.contingent.area.transform.v2.AreaMapperV2;
import moscow.ptnl.contingent.area.transform.v2.GetAreaHistoryMapperV2;
import moscow.ptnl.contingent.area.transform.v2.MoAddressAllocationMapper;
import moscow.ptnl.contingent.area.transform.v2.MoAddressInfoMapper;
import moscow.ptnl.contingent.area.transform.v2.SearchAreaAddressMapperV2;
import moscow.ptnl.contingent.area.transform.v2.SoapCustomMapperV2;
import moscow.ptnl.contingent.area.transform.v2.SoapExceptionMapper;
import moscow.ptnl.contingent.area.ws.BaseService;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.MoMuService;
import moscow.ptnl.contingent.domain.area.model.area.AreaHistory;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.domain.area.model.area.MedicalEmployee;
import moscow.ptnl.contingent.domain.area.model.area.MoAddressAllocation;
import moscow.ptnl.contingent.domain.area.model.area.MoAddressWithAddresses;
import moscow.ptnl.contingent.error.ContingentException;

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

import ru.mos.emias.contingent2.area.v2.AreaPT;
import ru.mos.emias.contingent2.area.v2.Fault;

import ru.mos.emias.contingent2.area.v2.types.AddAreaAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.AddAreaAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.AddMoAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.AddMoAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.AddMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v2.types.AddMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v2.types.AddMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v2.types.AddMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v2.types.ArchiveAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.ArchiveAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.AreaHistoryResultPage;
import ru.mos.emias.contingent2.area.v2.types.CreateDependentAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.CreateDependentAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.CreateOrderRequest;
import ru.mos.emias.contingent2.area.v2.types.CreateOrderResponse;
import ru.mos.emias.contingent2.area.v2.types.CreatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.CreatePrimaryAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.DelAreaAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.DelAreaAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.DelMoAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.DelMoAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.DelMoAddressTotalRequest;
import ru.mos.emias.contingent2.area.v2.types.DelMoAddressTotalResponse;
import ru.mos.emias.contingent2.area.v2.types.DelMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v2.types.DelMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v2.types.DelMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v2.types.DelMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v2.types.GetAreaAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.GetAreaAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.GetAreaByIdRequest;
import ru.mos.emias.contingent2.area.v2.types.GetAreaByIdResponse;
import ru.mos.emias.contingent2.area.v2.types.GetAreaHistoryRequest;
import ru.mos.emias.contingent2.area.v2.types.GetAreaHistoryResponse;
import ru.mos.emias.contingent2.area.v2.types.GetAreaListBriefRequest;
import ru.mos.emias.contingent2.area.v2.types.GetAreaListBriefResponse;
import ru.mos.emias.contingent2.area.v2.types.GetMoAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.GetMoAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.GetMoAddressTotalRequest;
import ru.mos.emias.contingent2.area.v2.types.GetMoAddressTotalResponse;
import ru.mos.emias.contingent2.area.v2.types.GetMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v2.types.GetMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v2.types.GetMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v2.types.GetMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v2.types.GetNewAreaIdRequest;
import ru.mos.emias.contingent2.area.v2.types.GetNewAreaIdResponse;
import ru.mos.emias.contingent2.area.v2.types.InitiateAddAreaAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.InitiateAddAreaAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.InitiateAddMoAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.InitiateAddMoAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.InitiateCreatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.InitiateCreatePrimaryAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.RestoreAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.RestoreAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.SearchAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.SearchAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.SearchDnAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.SearchDnAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.SearchMoAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.SearchMoAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.SearchMuByAreaAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.SearchMuByAreaAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.SearchOrderRequest;
import ru.mos.emias.contingent2.area.v2.types.SearchOrderResponse;
import ru.mos.emias.contingent2.area.v2.types.SetAreaMuServiceRequest;
import ru.mos.emias.contingent2.area.v2.types.SetAreaMuServiceResponse;
import ru.mos.emias.contingent2.area.v2.types.SetMedicalEmployeeOnAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.SetMedicalEmployeeOnAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.UpdateDependentAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.UpdateDependentAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.UpdateOrderRequest;
import ru.mos.emias.contingent2.area.v2.types.UpdateOrderResponse;
import ru.mos.emias.contingent2.area.v2.types.UpdatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.UpdatePrimaryAreaResponse;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collections;
import java.util.EnumSet;
import java.util.Map;
import java.util.stream.Collectors;

/**
 *
 * @author sorlov
 */
@Service(AreaServiceImpl.SERVICE_NAME)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AreaServiceImpl extends BaseService implements AreaPT {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    public static final String SERVICE_NAME = "AREA-V2";

    @Autowired
    private ru.mos.emias.contingent2.area.AreaPT areaServiceV1;

    @Autowired
    private SoapExceptionMapper exceptionMapper;

    @Autowired
    private SoapVersioningMapper versioningMapper;

    @Autowired
    private UserContextMapper userContextMapper;

    @Autowired
    private AreaService areaServiceDomain;

    @Autowired
    private MoMuService moMuMuServiceDomain;

    @Autowired
    private SoapCustomMapperV2 soapCustomMapper;

    @Autowired
    private AreaMapperV2 areaMapper;

    @Autowired
    private SearchAreaAddressMapperV2 searchAreaAddressMapper;

    @Autowired
    private AreaBriefMapperV2 areaBriefMapper;

    @Autowired
    private AddMedicalEmployeeMapperV2 addMedicalEmployeeMapper;

    @Autowired
    private AddressRegistryToAddressRegistryBaseMapperV2 addressRegistryBaseMapper;

    @Autowired
    private AreaDnMapperV2 areaDnMapper;

    @Autowired
    private MoAddressAllocationMapper moAddressAllocationMapper;

    @Autowired
    private MoAddressInfoMapper moAddressInfoMapper;

    @Autowired
    private GetAreaHistoryMapperV2 getAreaHistoryMapper;

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public RestoreAreaResponse restoreArea(RestoreAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.restoreArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.RestoreAreaRequest())),
                    new RestoreAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddMuAvailableAreaTypesResponse addMuAvailableAreaTypes(AddMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.addMuAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.AddMuAvailableAreaTypesRequest())),
                    new AddMuAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
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
                    body.getAreaTypeProfileCode(),
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
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddMoAvailableAreaTypesResponse addMoAvailableAreaTypes(AddMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.addMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.AddMoAvailableAreaTypesRequest())),
                    new AddMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMuAvailableAreaTypesResponse getMuAvailableAreaTypes(GetMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getMuAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetMuAvailableAreaTypesRequest())),
                    new GetMuAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchDnAreaResponse searchDnArea(SearchDnAreaRequest body) throws Fault {
        try {
            Page<AreaInfo> areas = areaServiceDomain.searchDnArea(body.getMoId(),
                    body.getMu() == null ? Collections.emptyList() : body.getMu().getMuIds(),
                    body.getAreaTypes() == null ? Collections.emptyList() : body.getAreaTypes().getAreaTypeCodes(),
                    body.getAreaTypeProfileCode(),
                    body.getMuService() == null ? Collections.emptyList() : body.getMuService().getMuIds(),
                    body.getSpecializations() == null ? Collections.emptyList() : body.getSpecializations().getSpecializationCodes(),
                    body.getAreas() == null ? Collections.emptyList() : body.getAreas().getAreaIds(),
                    soapCustomMapper.mapPagingOptions(body.getPagingOptions(), null), true
            );
            SearchDnAreaResponse response = new SearchDnAreaResponse();
            soapCustomMapper.mapPagingResults(response, areas);
            response.getAreas().addAll(areas.stream().map(area -> areaDnMapper.entityToDtoTransform(area)).collect(Collectors.toList()));
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SetMedicalEmployeeOnAreaResponse setMedicalEmployeeOnArea(SetMedicalEmployeeOnAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.setMedicalEmployeeOnArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.SetMedicalEmployeeOnAreaRequest())),
                    new SetMedicalEmployeeOnAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMoAvailableAreaTypesResponse delMoAvailableAreaTypes(DelMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelMoAvailableAreaTypesRequest())),
                    new DelMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchAreaResponse searchArea(SearchAreaRequest body) throws Fault {
        try {
            Page<AreaInfo> areas = areaServiceDomain.searchArea(body.getAreaTypeClassCode(), body.getMoId(),
                    body.getMuIds() == null ? Collections.emptyList() : body.getMuIds(),
                    body.getAreaTypeCodes() == null ? Collections.emptyList() : body.getAreaTypeCodes(),
                    body.getAreaTypeProfileCode(), body.getMuService() != null ? body.getMuService().getMuIds() : Collections.emptyList(),
                    body.getNumber(), body.getDescription(), body.isIsArchived(),
                    body.getMedicalEmployees() == null ? Collections.emptyList() :
                            body.getMedicalEmployees().stream().map(me -> new MedicalEmployee(me.getMedicalEmployeeJobId(), me.getSnils()))
                                    .filter(empl -> empl.getMedicalEmployeeJobId() != null || empl.getSnils()!= null)
                                    .collect(Collectors.toList()),
                    body.getAddresses() == null ? Collections.emptyList() : body.getAddresses().stream().map(searchAreaAddressMapper::dtoToEntityTransform).collect(Collectors.toList()),
                    body.isIsExactAddressMatch(),
                    soapCustomMapper.mapPagingOptions(body.getPagingOptions(), null), true);
            SearchAreaResponse response = new SearchAreaResponse();
            soapCustomMapper.mapPagingResults(response, areas);
            response.getAreas().addAll(areas.stream().map(area -> areaMapper.entityToDtoTransform(area)).collect(Collectors.toList()));
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchMuByAreaAddressResponse searchMuByAreaAddress(SearchMuByAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.searchMuByAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.SearchMuByAreaAddressRequest())),
                    new SearchMuByAreaAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddMoAddressResponse addMoAddress(AddMoAddressRequest body) throws Fault {
        try {
            AddMoAddressResponse response = new AddMoAddressResponse();
            response.getMoAddressIds().addAll(
                    areaServiceDomain.addMoAddress(body.getMoId(), body.getAreaTypeCodes(), body.getOrderId(),
                            body.getAddresses().stream().map(addressRegistryBaseMapper::dtoToEntityTransform).collect(Collectors.toList()),  true));
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public CreateOrderResponse createOrder(CreateOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.createOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.CreateOrderRequest())),
                    new CreateOrderResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override //@EMIASSecured(faultClass = Fault.class) @Metrics
    public GetNewAreaIdResponse getNewAreaId(GetNewAreaIdRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getNewAreaId(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetNewAreaIdRequest())),
                    new GetNewAreaIdResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMuAvailableAreaTypesResponse delMuAvailableAreaTypes(DelMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delMuAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelMuAvailableAreaTypesRequest())),
                    new DelMuAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddAreaAddressResponse addAreaAddress(AddAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.addAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.AddAreaAddressRequest())),
                    new AddAreaAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMoAddressResponse delMoAddress(DelMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelMoAddressRequest())),
                    new DelMoAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public InitiateAddMoAddressResponse initiateAddMoAddress(InitiateAddMoAddressRequest body) throws Fault {
        try {
            Long result = areaServiceDomain.initiateAddMoAddress(
                    body.getMoId(), body.getAreaTypeCodes(), body.getOrderId(),
                    body.getAddresses().stream().map(addressRegistryBaseMapper::dtoToEntityTransform).collect(Collectors.toList()));
            InitiateAddMoAddressResponse response = new InitiateAddMoAddressResponse();
            response.setId(result);
            return response;
        } catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchOrderResponse searchOrder(SearchOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.searchOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.SearchOrderRequest())),
                    new SearchOrderResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMoAddressResponse getMoAddress(GetMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetMoAddressRequest())),
                    new GetMoAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public UpdateDependentAreaResponse updateDependentArea(UpdateDependentAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.updateDependentArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.UpdateDependentAreaRequest())),
                    new UpdateDependentAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public InitiateAddAreaAddressResponse initiateAddAreaAddress(InitiateAddAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.initiateAddAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.InitiateAddAreaAddressRequest())),
                    new InitiateAddAreaAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelAreaAddressResponse delAreaAddress(DelAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelAreaAddressRequest())),
                    new DelAreaAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMoAddressTotalResponse delMoAddressTotal(DelMoAddressTotalRequest body) throws Fault {
        try {
            boolean result = moMuMuServiceDomain.delMoAddressTotal(body.getOrderId(), body.getAddressGlobalIds());

            DelMoAddressTotalResponse response = new DelMoAddressTotalResponse();
            response.setResult(new DelMoAddressTotalResponse.Result());
            response.getResult().setValue(result);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public CreatePrimaryAreaResponse createPrimaryArea(CreatePrimaryAreaRequest body) throws Fault {
        try {
            CreatePrimaryAreaResponse response = new CreatePrimaryAreaResponse();
            Long id = areaServiceDomain.createPrimaryArea(body.getMoId(), body.getMuId(), body.getNumber(), body.getAreaTypeCode(),
                    body.getAreaTypeProfileCode(), body.getPolicyTypes() == null ? new ArrayList<>() : body.getPolicyTypes().getPolicyTypeCodes(),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(), body.getAgeMinW(), body.getAgeMaxW(),
                    body.isAutoAssignForAttachment(), body.isAttachByMedicalReason(), body.getDescription());

            response.setId(id);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public UpdatePrimaryAreaResponse updatePrimaryArea(UpdatePrimaryAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.updatePrimaryArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.UpdatePrimaryAreaRequest())),
                    new UpdatePrimaryAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public CreateDependentAreaResponse createDependentArea(CreateDependentAreaRequest body) throws Fault {
        try {
            CreateDependentAreaResponse response = new CreateDependentAreaResponse();
            Long id = areaServiceDomain.createDependentArea(body.getMoId(), body.getMuId(),
                    body.getNumber(), body.getAreaTypeCode(), body.getAreaTypeProfileCode(),
                    body.getPrimaryAreaTypes().stream().flatMap(pat -> pat.getPrimaryAreaTypeCodes().stream()).collect(Collectors.toList()),
                    body.getPolicyTypes().stream().flatMap(pt -> pt.getPolicyTypeCodes().stream()).collect(Collectors.toList()),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(),
                    body.getAgeMinW(), body.getAgeMaxW(), body.getDescription());

            response.setId(id);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
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
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetAreaListBriefResponse getAreaListBrief(GetAreaListBriefRequest body) throws Fault {
        try {
            Map<GetAreaListBriefOptions, ? extends OptionEnum.OptionValuesEnum> options =
                    soapCustomMapper.mapOptions(body.getOptions(), GetAreaListBriefOptions.class);
            GetAreaListBriefResponse response = new GetAreaListBriefResponse();
            PageRequest pageRequest = soapCustomMapper.mapPagingOptions(body.getPagingOptions(), EnumSet.allOf(GetAreaListBriefSorting.class));
            ShowMeValues showMeOption = (ShowMeValues) options.get(GetAreaListBriefOptions.SHOW_ME);
            Boolean showMe = ShowMeValues.ALL.equals(showMeOption) ? Boolean.TRUE :
                    (ShowMeValues.NONE.equals(showMeOption) ? Boolean.FALSE : null);
            Page<AreaInfo> areas = areaServiceDomain.getAreaListBriefV2(body.getAreas().getIds(), showMe, pageRequest);
            soapCustomMapper.mapPagingResults(response, areas);
            response.getAreas().addAll(areas.stream()
                    .map(a -> areaBriefMapper.entityToDtoTransform(a))
                    .collect(Collectors.toList()));

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public UpdateOrderResponse updateOrder(UpdateOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.updateOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.UpdateOrderRequest())),
                    new UpdateOrderResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public ArchiveAreaResponse archiveArea(ArchiveAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.archiveArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.ArchiveAreaRequest())),
                    new ArchiveAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMoAvailableAreaTypesResponse getMoAvailableAreaTypes(GetMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetMoAvailableAreaTypesRequest())),
                    new GetMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetAreaAddressResponse getAreaAddress(GetAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetAreaAddressRequest())),
                    new GetAreaAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SetAreaMuServiceResponse setAreaMuService(SetAreaMuServiceRequest body) throws Fault {
        try {
            areaServiceDomain.setAreaMuService(body.getAreaId(),
                    body.getAddMuService() == null ? Collections.emptyList() : body.getAddMuService().getMuIds(),
                    body.getCloseMuService() == null ? Collections.emptyList() : body.getCloseMuService().getMuIds());
            SetAreaMuServiceResponse response = new SetAreaMuServiceResponse();
            response.setResult(new SetAreaMuServiceResponse.Result());
            response.getResult().setValue(true);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMoAddressTotalResponse getMoAddressTotal(GetMoAddressTotalRequest body) throws Fault {
        try {
            Page<MoAddressAllocation> results = areaServiceDomain.getMoAddressTotal(body.getAddressGlobalIds(), soapCustomMapper.mapPagingOptions(body.getPagingOptions(), null));
            GetMoAddressTotalResponse response = new GetMoAddressTotalResponse();
            response.getAddressAllocations().addAll(moAddressAllocationMapper.entityToDtoTransform(results.getContent()));
            soapCustomMapper.mapPagingResults(response, results);

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetAreaHistoryResponse getAreaHistory(GetAreaHistoryRequest body) throws Fault {
        try {
            GetAreaHistoryResponse getAreaHistoryResponse = new GetAreaHistoryResponse();
            AreaHistory results = areaServiceDomain.getAreaHistory(body.getAreaId(), soapCustomMapper.mapPagingOptions(body.getPagingOptions(), null));
            AreaHistoryResultPage areaHistoryResultPage = new AreaHistoryResultPage();
            ru.mos.emias.contingent2.core.v2.AreaHistory areaHistory = new ru.mos.emias.contingent2.core.v2.AreaHistory();
            areaHistory.setAreaId(results.getAreaId());
            areaHistory.setDateCreated(results.getDateCreated());
            if (results.getEvents().getTotalElements() > 0L) {
                areaHistory.setEvents(new ru.mos.emias.contingent2.core.v2.AreaHistory.Events());
                areaHistory.getEvents().getEvents().addAll(results.getEvents().get()
                        .map(getAreaHistoryMapper::entityToDtoTransform).collect(Collectors.toList()));
            }
            areaHistoryResultPage.setResult(areaHistory);
            soapCustomMapper.mapPagingResults(areaHistoryResultPage, results.getEvents());
            getAreaHistoryResponse.setResult(areaHistoryResultPage);
            return getAreaHistoryResponse;
        } catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchMoAddressResponse searchMoAddress(SearchMoAddressRequest body) throws Fault {
        try {
            SearchMoAddressResponse response = new SearchMoAddressResponse();
            Page<MoAddressWithAddresses> results = moMuMuServiceDomain.searchMoAddress(body.getMoId(), body.getAddressGlobalIds(),
                    body.getAreaTypeCodes(), body.getOrderDate(), body.getOrderName(), body.getOrderNumber(), body.getOrderOuz(),
                    body.getOrderCreateDate(), soapCustomMapper.mapPagingOptions(body.getPagingOptions(), null));
            response.getAddressInfos().addAll(moAddressInfoMapper.entityToDtoTransform(results.getContent()));
            soapCustomMapper.mapPagingResults(response, results);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    private ru.mos.emias.contingent2.area.v2.Fault mapFault(ru.mos.emias.contingent2.area.Fault ex) {
        return ex.getCause() == null ? new Fault(ex.getMessage(), ex.getFaultInfo()) : new Fault(ex.getMessage(), ex.getFaultInfo(), ex.getCause());
    }

    private ru.mos.emias.contingent2.area.v2.Fault mapException(Exception ex) {
        if (ex instanceof ru.mos.emias.contingent2.area.Fault) {
            //Мапинг исключения предыдущей версии контракта
            return mapFault((ru.mos.emias.contingent2.area.Fault) ex);
        }
        if (!(ex instanceof ContingentException)) {
            LOG.error(ex.getMessage(), ex);
        }
        return exceptionMapper.map(ex, userContextMapper);
    }
}
