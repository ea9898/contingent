package moscow.ptnl.contingent.area.ws.v1;

import moscow.ptnl.contingent.area.transform.AddMedicalEmployeeMapper;
import moscow.ptnl.contingent.area.transform.AddressRegistryToAddressRegistryBaseMapper;
import moscow.ptnl.contingent.area.transform.ChangeMedicalEmployeeMapper;
import moscow.ptnl.contingent.area.transform.SearchAreaAddressMapper;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.MoMuService;
import moscow.ptnl.contingent.domain.area.OrderService;
import moscow.ptnl.contingent.domain.area.entity.AddressAllocationOrders;
import moscow.ptnl.contingent.domain.area.entity.Area;
import moscow.ptnl.contingent.domain.area.entity.AreaAddress;
import moscow.ptnl.contingent.domain.area.entity.MoAddress;
import moscow.ptnl.contingent.area.transform.AreaDnMapper;
import moscow.ptnl.contingent.domain.area.model.area.MedicalEmployee;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.domain.area.model.area.AreaInfo;
import moscow.ptnl.contingent.domain.area.model.area.AreaTypeStateType;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.transform.AddressAllocationOrderMapper;
import moscow.ptnl.contingent.area.transform.AreaMapper;
import moscow.ptnl.contingent.area.transform.AreaTypeShortMapper;
import moscow.ptnl.contingent.area.transform.GetMuAvailableAreaTypesResponseMapper;
import moscow.ptnl.contingent.area.transform.MoAddressMapper;
import moscow.ptnl.contingent.area.transform.PagingOptionsMapper;
import moscow.ptnl.contingent.area.transform.SoapCustomMapper;
import moscow.ptnl.contingent.area.transform.SoapExceptionMapper;
import moscow.ptnl.contingent.area.ws.BaseService;
import org.apache.cxf.annotations.SchemaValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.contingent2.area.Fault;
import ru.mos.emias.contingent2.area.types.AddAreaAddressRequest;
import ru.mos.emias.contingent2.area.types.AddAreaAddressResponse;
import ru.mos.emias.contingent2.area.types.AddMoAddressRequest;
import ru.mos.emias.contingent2.area.types.AddMoAddressResponse;
import ru.mos.emias.contingent2.area.types.AddMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.types.AddMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.types.AddMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.types.AddMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.types.AddressAllocationOrderListResultPage;
import ru.mos.emias.contingent2.area.types.ArchiveAreaRequest;
import ru.mos.emias.contingent2.area.types.ArchiveAreaResponse;
import ru.mos.emias.contingent2.area.types.CreateDependentAreaRequest;
import ru.mos.emias.contingent2.area.types.CreateDependentAreaResponse;
import ru.mos.emias.contingent2.area.types.CreateOrderRequest;
import ru.mos.emias.contingent2.area.types.CreateOrderResponse;
import ru.mos.emias.contingent2.area.types.CreatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.types.CreatePrimaryAreaResponse;
import ru.mos.emias.contingent2.area.types.DelAreaAddressRequest;
import ru.mos.emias.contingent2.area.types.DelAreaAddressResponse;
import ru.mos.emias.contingent2.area.types.DelMoAddressRequest;
import ru.mos.emias.contingent2.area.types.DelMoAddressResponse;
import ru.mos.emias.contingent2.area.types.DelMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.types.DelMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.types.DelMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.types.DelMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.types.GetAreaAddressRequest;
import ru.mos.emias.contingent2.area.types.GetAreaAddressResponse;
import ru.mos.emias.contingent2.area.types.GetAreaByIdRequest;
import ru.mos.emias.contingent2.area.types.GetAreaByIdResponse;
import ru.mos.emias.contingent2.area.types.GetMoAddressRequest;
import ru.mos.emias.contingent2.area.types.GetMoAddressResponse;
import ru.mos.emias.contingent2.area.types.GetMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.types.GetMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.types.GetMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.types.GetMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.types.GetNewAreaIdRequest;
import ru.mos.emias.contingent2.area.types.GetNewAreaIdResponse;
import ru.mos.emias.contingent2.area.types.InitiateAddAreaAddressRequest;
import ru.mos.emias.contingent2.area.types.InitiateAddAreaAddressResponse;
import ru.mos.emias.contingent2.area.types.InitiateCreatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.types.InitiateCreatePrimaryAreaResponse;
import ru.mos.emias.contingent2.area.types.RestoreAreaRequest;
import ru.mos.emias.contingent2.area.types.RestoreAreaResponse;
import ru.mos.emias.contingent2.area.types.SearchAreaRequest;
import ru.mos.emias.contingent2.area.types.SearchAreaResponse;
import ru.mos.emias.contingent2.area.types.SearchDnAreaRequest;
import ru.mos.emias.contingent2.area.types.SearchDnAreaResponse;
import ru.mos.emias.contingent2.area.types.SearchOrderRequest;
import ru.mos.emias.contingent2.area.types.SearchOrderResponse;
import ru.mos.emias.contingent2.area.types.SetMedicalEmployeeOnAreaRequest;
import ru.mos.emias.contingent2.area.types.SetMedicalEmployeeOnAreaResponse;
import ru.mos.emias.contingent2.area.types.UpdateDependentAreaRequest;
import ru.mos.emias.contingent2.area.types.UpdateDependentAreaResponse;
import ru.mos.emias.contingent2.area.types.UpdateOrderRequest;
import ru.mos.emias.contingent2.area.types.UpdateOrderResponse;
import ru.mos.emias.contingent2.area.types.UpdatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.types.UpdatePrimaryAreaResponse;
import ru.mos.emias.contingent2.area.AreaPT;


import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;
import moscow.ptnl.contingent.area.transform.AreaAddressMapper;
import moscow.ptnl.contingent.security.annotation.EMIASSecured;
import moscow.ptnl.metrics.Metrics;
import ru.mos.emias.contingent2.area.types.InitiateAddMoAddressRequest;
import ru.mos.emias.contingent2.area.types.InitiateAddMoAddressResponse;

/**
 *
 * @author mkachalov
 */
@Service(AreaServiceImpl.SERVICE_NAME)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AreaServiceImpl extends BaseService implements AreaPT {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    public static final String SERVICE_NAME = "V1";

    @Autowired
    private AreaServiceInternal areaService;

    @Autowired
    private SoapCustomMapper soapCustomMapper;

    @Autowired
    private PagingOptionsMapper pagingOptionsMapper;

    @Autowired
    private AddressAllocationOrderMapper addressAllocationOrderMapper;

    @Autowired
    private AreaMapper areaMapper;

    @Autowired
    private AreaTypeShortMapper areaTypeShortMapper;

    @Autowired
    private MoAddressMapper moAddressMapper;

    @Autowired
    private AreaAddressMapper areaAddressMapper;

    @Autowired
    private AreaDnMapper areaDnMapper;

    @Autowired
    private SearchAreaAddressMapper searchAreaAddressMapper;

    @Autowired
    private AddressRegistryToAddressRegistryBaseMapper addressRegistryToAddressRegistryBaseMapper;

    @Autowired
    private MoMuService moMuMuServiceDomain;

    @Autowired
    private OrderService orderServiceDomain;

    @Autowired
    private AreaService areaServiceDomain;

    @Autowired
    private AddMedicalEmployeeMapper addMedicalEmployeeMapper;

    @Autowired
    private ChangeMedicalEmployeeMapper changeMedicalEmployeeMapper;

    @Autowired
    private GetMuAvailableAreaTypesResponseMapper getMuAvailableAreaTypesResponseMapper;
    
    @Override @EMIASSecured @Metrics
    public CreatePrimaryAreaResponse createPrimaryArea(CreatePrimaryAreaRequest body) throws Fault {
        try {
            CreatePrimaryAreaResponse response = new CreatePrimaryAreaResponse();
            Long id = areaServiceDomain.createPrimaryArea(body.getMoId(), body.getMuId(), body.getNumber(), body.getAreaTypeCode(),
                    body.getPolicyTypes() == null ? new ArrayList<>() : body.getPolicyTypes().getPolicyTypeCodes(),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(), body.getAgeMinW(), body.getAgeMaxW(),
                    body.isAutoAssignForAttachment(), body.isAttachByMedicalReason(), body.getDescription());

            response.setId(id);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public DelAreaAddressResponse delAreaAddress(DelAreaAddressRequest body) throws Fault {
        try {
            areaServiceDomain.delAreaAddress(body.getAreaId(), body.getAreaAddressIds());
            return new DelAreaAddressResponse();
        } catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public CreateDependentAreaResponse createDependentArea(CreateDependentAreaRequest body) throws Fault {
        try {
            CreateDependentAreaResponse response = new CreateDependentAreaResponse();
            Long id = areaServiceDomain.createDependentArea(body.getMoId(), body.getMuId(),
                    body.getNumber(), body.getAreaTypeCode(),
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

    @Override @EMIASSecured @Metrics
    public UpdatePrimaryAreaResponse updatePrimaryArea(UpdatePrimaryAreaRequest body) throws Fault {
        try {
            areaServiceDomain.updatePrimaryArea(body.getAreaId(), body.getNumber(),
                    body.getPolicyTypesAdd() == null ? Collections.EMPTY_LIST : body.getPolicyTypesAdd().getPolicyTypeCodes(),
                    body.getPolicyTypesDel() == null ? Collections.EMPTY_LIST : body.getPolicyTypesDel().getPolicyTypeCodes(),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(), body.getAgeMinW(), body.getAgeMaxW(),
                    body.isAutoAssignForAttachment(), body.isAttachByMedicalReason(), body.getDescription());

            return new UpdatePrimaryAreaResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public UpdateDependentAreaResponse updateDependentArea(UpdateDependentAreaRequest body) throws Fault {
        try {
            areaServiceDomain.updateDependentArea(body.getAreaId(), body.getMuId(), body.getNumber(), body.getDescription(),
                    body.getPrimaryAreaTypesAdd() == null ? Collections.EMPTY_LIST : body.getPrimaryAreaTypesAdd().getPrimaryAreaTypeCodes(),
                    body.getPrimaryAreaTypesDel() == null ? Collections.EMPTY_LIST : body.getPrimaryAreaTypesDel().getPrimaryAreaTypeCodes(),
                    body.getPolicyTypesAdd() == null ? Collections.EMPTY_LIST : body.getPolicyTypesAdd().getPolicyTypeCodes(),
                    body.getPolicyTypesDel() == null ? Collections.EMPTY_LIST : body.getPolicyTypesDel().getPolicyTypeCodes(),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(), body.getAgeMinW(), body.getAgeMaxW());

            return new UpdateDependentAreaResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public InitiateAddAreaAddressResponse initiateAddAreaAddress(InitiateAddAreaAddressRequest body) throws Fault {
        try {
            InitiateAddAreaAddressResponse response = new InitiateAddAreaAddressResponse();
            Long id = areaService.initiateAddAreaAddress(
                    body.getAreaId(),
                    body.getAddresses().stream().map(addressRegistryToAddressRegistryBaseMapper::dtoToEntityTransform).collect(Collectors.toList()));
            response.setId(id);
            return response;
        } catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public CreateOrderResponse createOrder(CreateOrderRequest body) throws Fault {
        try {
            
            Long id = orderServiceDomain.createOrder(body.getNumber(), body.getDate(), body.getOuz(), body.getName());

            CreateOrderResponse response = new CreateOrderResponse();
            response.setId(id);

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public UpdateOrderResponse updateOrder(UpdateOrderRequest body) throws Fault {
        try {
            orderServiceDomain.updateOrder(body.getId(), body.getNumber(), body.getDate(), body.getOuz(), body.getName());

            return new UpdateOrderResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public SearchOrderResponse searchOrder(SearchOrderRequest body) throws Fault {
        try {
            Page<AddressAllocationOrders> results = orderServiceDomain.searchOrder(body.getId(), body.getNumber(), body.getDate(),
                    body.getName(), pagingOptionsMapper.dtoToEntityTransform(body.getPagingOptions()));
            SearchOrderResponse response = new SearchOrderResponse();
            AddressAllocationOrderListResultPage resultPage = new AddressAllocationOrderListResultPage();
            soapCustomMapper.mapPagingResults(resultPage, results);
            resultPage.getAddressAllocationOrders().addAll(results.get().map(addressAllocationOrderMapper::entityToDtoTransform).collect(Collectors.toList()));
            response.setResult(resultPage);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public GetAreaByIdResponse getAreaById(GetAreaByIdRequest body) throws Fault {
        try {
            AreaInfo area = areaServiceDomain.getAreaById(body.getAreaId());
            GetAreaByIdResponse response = new GetAreaByIdResponse();
            response.setResult(areaMapper.entityToDtoTransform(area));
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public SetMedicalEmployeeOnAreaResponse setMedicalEmployeeOnArea(SetMedicalEmployeeOnAreaRequest body) throws Fault {
        try {
            List<Long> assignmentIds = areaServiceDomain.setMedicalEmployeeOnArea(body.getAreaId(),
                    body.getAddMedicalEmployees() == null ? Collections.EMPTY_LIST : body.getAddMedicalEmployees().getAddMedicalEmployees().stream().map(
                            addMedicalEmployeeMapper::dtoToEntityTransform).collect(Collectors.toList()),
                    body.getChangeMedicalEmployees() == null ? Collections.EMPTY_LIST : body.getChangeMedicalEmployees().getChangeMedicalEmployees().stream().map(
                            changeMedicalEmployeeMapper::dtoToEntityTransform).collect(Collectors.toList()));
            SetMedicalEmployeeOnAreaResponse response = new SetMedicalEmployeeOnAreaResponse();
            response.getAssignmentIds().addAll(assignmentIds);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public AddAreaAddressResponse addAreaAddress(AddAreaAddressRequest body) throws Fault {
        try {
            AddAreaAddressResponse response = new AddAreaAddressResponse();
            response.getAreaAddressIds().addAll(areaServiceDomain.addAreaAddress(body.getAreaId(),
                    body.getAddresses().stream().map(addressRegistryToAddressRegistryBaseMapper::dtoToEntityTransform).collect(Collectors.toList()), true));
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public RestoreAreaResponse restoreArea(RestoreAreaRequest body) throws Fault {
        try {
            areaServiceDomain.restoreArea(body.getAreaId());

            return new RestoreAreaResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public AddMoAddressResponse addMoAddress(AddMoAddressRequest body) throws Fault {
        try {
            AddMoAddressResponse response = new AddMoAddressResponse();
            response.getMoAddressIds().addAll(
                    areaServiceDomain.addMoAddress(body.getMoId(), body.getAreaTypeCode(), body.getOrderId(),
                            body.getAddresses().stream().map(addressRegistryToAddressRegistryBaseMapper::dtoToEntityTransform).collect(Collectors.toList()),  true));
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public SearchAreaResponse searchArea(SearchAreaRequest body) throws Fault {
        try {
            Page<AreaInfo> areas = areaServiceDomain.searchArea(body.getAreaTypeClassCode(), body.getMoId(),
                    body.getMuIds() == null ? Collections.EMPTY_LIST : body.getMuIds(),
                    body.getAreaTypeCodes() == null ? Collections.EMPTY_LIST : body.getAreaTypeCodes(),
                    body.getNumber(), body.getDescription(), body.isIsArchived(),
                    body.getMedicalEmployees() == null ? Collections.EMPTY_LIST :
                            body.getMedicalEmployees().stream().map(me -> new MedicalEmployee(me.getMedicalEmployeeJobId(), me.getSnils()))
                                    .filter(empl -> empl.getMedicalEmployeeJobId() != null || empl.getSnils()!= null)
                                    .collect(Collectors.toList()),
                    body.getAddresses() == null ? Collections.EMPTY_LIST : body.getAddresses().stream().map(searchAreaAddressMapper::dtoToEntityTransform).collect(Collectors.toList()),
                    body.isIsExactAddressMatch(),
                    pagingOptionsMapper.dtoToEntityTransform(body.getPagingOptions()));
            SearchAreaResponse response = new SearchAreaResponse();
            soapCustomMapper.mapPagingResults(response, areas);
            response.getAreas().addAll(areas.stream().map(area -> areaMapper.entityToDtoTransform(area)).collect(Collectors.toList()));
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public GetMoAddressResponse getMoAddress(GetMoAddressRequest body) throws Fault {
        try {
            GetMoAddressResponse response = new GetMoAddressResponse();
            Page<MoAddress> addresses = moMuMuServiceDomain.getMoAddress(body.getMoId(), body.getAreaTypes(),
                    pagingOptionsMapper.dtoToEntityTransform(body.getPagingOptions()));
            response.setMoId(body.getMoId());
            response.getAreaTypes().addAll(addresses.getContent().stream()
                    .map(MoAddress::getAreaType)
                    .distinct()
                    .map(areaTypeShortMapper::entityToDtoTransform)
                    .collect(Collectors.toSet()));
            response.getOrders().addAll(addresses.getContent().stream()
                    .map(MoAddress::getAddressAllocationOrder)
                    .distinct()
                    .map(addressAllocationOrderMapper::entityToDtoTransform)
                    .collect(Collectors.toSet()));
            response.getMoAddresses().addAll(addresses.getContent().stream()
                    .map(moAddressMapper::entityToDtoTransform)
                    .collect(Collectors.toList())
                    .stream().sorted(Comparator.comparing(ru.mos.emias.contingent2.core.MoAddress::getMoAddressId)).collect(Collectors.toList()));
            soapCustomMapper.mapPagingResults(response, addresses);

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public DelMoAddressResponse delMoAddress(DelMoAddressRequest body) throws Fault {
        try {
            moMuMuServiceDomain.delMoAddress(body.getMoAddressIds(), body.getOrderId());

            return new DelMoAddressResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public GetAreaAddressResponse getAreaAddress(GetAreaAddressRequest body) throws Fault {
        try {
            GetAreaAddressResponse response = new GetAreaAddressResponse();
            
            Page<AreaAddress> areaAddresses = areaServiceDomain.getAreaAddress(body.getMoId(),
                    body.getAreas().getAreaIds(), pagingOptionsMapper.dtoToEntityTransform(body.getPagingOptions()));
            response.getAreaAddresses().addAll(
                areaAddresses.getContent().stream()
                .map(areaAddressMapper::entityToDtoTransform)
                        .collect(Collectors.toList()));
            soapCustomMapper.mapPagingResults(response, areaAddresses);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public InitiateCreatePrimaryAreaResponse initiateCreatePrimaryArea(InitiateCreatePrimaryAreaRequest body) throws Fault {
        try {
            InitiateCreatePrimaryAreaResponse response = new InitiateCreatePrimaryAreaResponse();
            Long id = areaService.initiateCreatePrimaryArea(
                    body.getMoId(),
                    body.getMuId(),
                    body.getNumber(),
                    body.getDescription(),
                    body.getAreaTypeCode(),
                    body.getPolicyTypes() == null ? new ArrayList<>() : body.getPolicyTypes().getPolicyTypeCodes(),
                    body.getAgeMin(),
                    body.getAgeMax(),
                    body.getAgeMinM(),
                    body.getAgeMaxM(),
                    body.getAgeMinW(),
                    body.getAgeMaxW(),
                    body.isAutoAssignForAttachment(),
                    body.isAttachByMedicalReason(),
                    body.getAddMedicalEmployees() == null ? Collections.EMPTY_LIST : body.getAddMedicalEmployees().getAddMedicalEmployees().stream().map(addMedicalEmployeeMapper::dtoToEntityTransform).collect(Collectors.toList()),
                    body.getAddresses() == null ? Collections.EMPTY_LIST : body.getAddresses().stream()
                            .map(addressRegistryToAddressRegistryBaseMapper::dtoToEntityTransform).collect(Collectors.toList()));

            response.setId(id);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public ArchiveAreaResponse archiveArea(ArchiveAreaRequest body) throws Fault {
        try {
            areaServiceDomain.archiveArea(body.getAreaId());

            return new ArchiveAreaResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @Metrics
    public GetNewAreaIdResponse getNewAreaId(GetNewAreaIdRequest body) throws Fault {
        try {
            GetNewAreaIdResponse response = new GetNewAreaIdResponse();
            response.setNewAreaId(areaServiceDomain.getNewAreaId());

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public AddMoAvailableAreaTypesResponse addMoAvailableAreaTypes(AddMoAvailableAreaTypesRequest body) throws Fault {
        try {
            moMuMuServiceDomain.addMoAvailableAreaTypes(body.getMoId(), body.getAreaTypeCodes().getAreaTypeCodes());

            return new AddMoAvailableAreaTypesResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public DelMoAvailableAreaTypesResponse delMoAvailableAreaTypes(DelMoAvailableAreaTypesRequest body) throws Fault {
        try {
            moMuMuServiceDomain.delMoAvailableAreaTypes(body.getMoId(), body.getAreaTypeCodes().getAreaTypeCodes());

            return new DelMoAvailableAreaTypesResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public GetMoAvailableAreaTypesResponse getMoAvailableAreaTypes(GetMoAvailableAreaTypesRequest body) throws Fault {
        try {
            GetMoAvailableAreaTypesResponse result = new GetMoAvailableAreaTypesResponse();
            result.getMoAvailableAreaTypes().addAll(moMuMuServiceDomain.getMoAvailableAreaTypes(body.getMoId()).stream()
                    .map(areaTypeShortMapper::entityToDtoTransform)
                    .collect(Collectors.toList())
            );
            return result;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public AddMuAvailableAreaTypesResponse addMuAvailableAreaTypes(AddMuAvailableAreaTypesRequest body) throws Fault {
        try {
            moMuMuServiceDomain.addMuAvailableAreaTypes(body.getMoId(), body.getMuId(), body.getAreaTypeCodes().getAreaTypeCodes());

            return new AddMuAvailableAreaTypesResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public SearchDnAreaResponse searchDnArea(SearchDnAreaRequest body) throws Fault {
        try {
            Page<Area> areas = areaServiceDomain.searchDnArea(body.getMoId(),
                    body.getMu() == null ? Collections.emptyList() : body.getMu().getMuIds(),
                    body.getAreaTypes() == null ? Collections.emptyList() : body.getAreaTypes().getAreaTypeCodes(),
                    body.getSpecializations() == null ? Collections.emptyList() : body.getSpecializations().getSpecializationCodes(),
                    body.getAreas() == null ? Collections.emptyList() : body.getAreas().getAreaIds(),
                    pagingOptionsMapper.dtoToEntityTransform(body.getPagingOptions())
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

    @Override @EMIASSecured @Metrics
    public DelMuAvailableAreaTypesResponse delMuAvailableAreaTypes(DelMuAvailableAreaTypesRequest body) throws Fault {
        try {
            moMuMuServiceDomain.delMuAvailableAreaTypes(body.getMuId(), body.getAreaTypeCodes().getAreaTypeCodes());

            return new DelMuAvailableAreaTypesResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public GetMuAvailableAreaTypesResponse getMuAvailableAreaTypes(GetMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return getMuAvailableAreaTypesResponseMapper.entityToDtoTransform(moMuMuServiceDomain.getMuAvailableAreaTypes(
                    body.getMoId(), body.getMuId(), AreaTypeStateType.getByValue(body.getAreaTypeState())));
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override @EMIASSecured @Metrics
    public InitiateAddMoAddressResponse initiateAddMoAddress(InitiateAddMoAddressRequest body) throws Fault {
        try {
            Long result = areaService.initiateAddMoAddress(
                    body.getMoId(), body.getAreaTypeCode(), body.getOrderId(),
                    body.getAddresses().stream().map(addressRegistryToAddressRegistryBaseMapper::dtoToEntityTransform).collect(Collectors.toList()));
            InitiateAddMoAddressResponse response = new InitiateAddMoAddressResponse();
            response.setId(result);
            return response;            
        } catch (Exception ex) {
            throw mapException(ex);
        }
    }

    private Fault mapException(Exception ex) {
        if (!(ex instanceof ContingentException)) {
            LOG.error(ex.getMessage(), ex);
        }
        return SoapExceptionMapper.map(ex);
    }
}
