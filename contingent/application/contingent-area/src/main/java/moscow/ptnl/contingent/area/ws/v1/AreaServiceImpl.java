package moscow.ptnl.contingent.area.ws.v1;

import moscow.ptnl.contingent.area.entity.area.AddressAllocationOrders;
import moscow.ptnl.contingent.area.entity.area.MoAddress;
import moscow.ptnl.contingent.area.entity.area.MoAvailableAreaTypes;
import moscow.ptnl.contingent.area.entity.nsi.AreaType;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.model.area.AreaInfo;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.transform.AddressAllocationOrderMapper;
import moscow.ptnl.contingent.area.transform.AreaMapper;
import moscow.ptnl.contingent.area.transform.AreaTypeShortMapper;
import moscow.ptnl.contingent.area.transform.MoAddressMapper;
import moscow.ptnl.contingent.area.transform.NotNsiAddressMapper;
import moscow.ptnl.contingent.area.transform.NsiAddressMapper;
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
import ru.mos.emias.contingent2.area.types.GetAreaAddressRequest;
import ru.mos.emias.contingent2.area.types.GetAreaAddressResponse;
import ru.mos.emias.contingent2.area.types.GetAreaByIdRequest;
import ru.mos.emias.contingent2.area.types.GetAreaByIdResponse;
import ru.mos.emias.contingent2.area.types.GetMoAddressRequest;
import ru.mos.emias.contingent2.area.types.GetMoAddressResponse;
import ru.mos.emias.contingent2.area.types.GetMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.types.GetMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.types.GetNewAreaIdRequest;
import ru.mos.emias.contingent2.area.types.GetNewAreaIdResponse;
import ru.mos.emias.contingent2.area.types.RestoreAreaRequest;
import ru.mos.emias.contingent2.area.types.RestoreAreaResponse;
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
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;
import moscow.ptnl.contingent.area.transform.AreaAddressMapper;
import ru.mos.emias.contingent2.core.PrimaryAreaTypeCodes;

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
    private NsiAddressMapper nsiAddressMapper;

    @Autowired
    private NotNsiAddressMapper notNsiAddressMapper;

    @Override
    public CreatePrimaryAreaResponse createPrimaryArea(CreatePrimaryAreaRequest body) throws Fault {
        try {
            CreatePrimaryAreaResponse response = new CreatePrimaryAreaResponse();
            Long id = areaService.createPrimaryArea(body.getMoId(), body.getMuId(), body.getNumber(), body.getAreaTypeCode(),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(), body.getAgeMinW(), body.getAgeMaxW(),
                    body.isAutoAssignForAttachment(), body.isAttachByMedicalReason(), body.getDescription());

            response.setId(id);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public DelAreaAddressResponse delAreaAddress(DelAreaAddressRequest body) throws Fault {
        try {
            areaService.delAreaAddress(body.getAreaId(), body.getAreaAddressIds());
            return new DelAreaAddressResponse();
        } catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public CreateDependentAreaResponse createDependentArea(CreateDependentAreaRequest body) throws Fault {
        try {
            CreateDependentAreaResponse response = new CreateDependentAreaResponse();
            Long id = areaService.createDependentArea(body.getMoId(), body.getMuId(),
                    body.getNumber(), body.getAreaTypeCode(),
                    body.getPrimaryAreaTypes().stream().flatMap(pat -> pat.getPrimaryAreaTypeCodes().stream()).collect(Collectors.toList()),
                    body.getPolicyTypes().stream().flatMap(pt -> pt.getPolicyTypeCodes().stream()).collect(Collectors.toList()),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(),
                    body.getAgeMinW(), body.getAgeMaxW(), body.isAutoAssignForAttachment(), body.getDescription());

            response.setId(id);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public UpdatePrimaryAreaResponse updatePrimaryArea(UpdatePrimaryAreaRequest body) throws Fault {
        try {
            areaService.updatePrimaryArea(body.getAreaId(), body.getNumber(),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(), body.getAgeMinW(), body.getAgeMaxW(),
                    body.isAutoAssignForAttachment(), body.isAttachByMedicalReason(), body.getDescription());

            return new UpdatePrimaryAreaResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public UpdateDependentAreaResponse updateDependentArea(UpdateDependentAreaRequest body) throws Fault {
        try {
            areaService.updateDependentArea(body.getAreaId(), body.getMuId(),
                    body.getMuTypes() == null ? Collections.EMPTY_LIST : body.getMuTypes().getMuTypes(),
                    body.getNumber(),
                    body.getPrimaryAreaTypesAdd() == null ? Collections.EMPTY_LIST : body.getPrimaryAreaTypesAdd().getPrimaryAreaTypeCodes(),
                    body.getPrimaryAreaTypesDel() == null ? Collections.EMPTY_LIST : body.getPrimaryAreaTypesDel().getPrimaryAreaTypeCodes(),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(), body.getAgeMinW(), body.getAgeMaxW(),
                    body.isAutoAssignForAttachment(), body.getDescription());

            return new UpdateDependentAreaResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public CreateOrderResponse createOrder(CreateOrderRequest body) throws Fault {
        try {
            
            Long id = areaService.createOrder(body.getNumber(), body.getDate(), body.getOuz(), body.getName());

            CreateOrderResponse response = new CreateOrderResponse();
            response.setId(id);

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public UpdateOrderResponse updateOrder(UpdateOrderRequest body) throws Fault {
        try {
            areaService.updateOrder(body.getId(), body.getNumber(), body.getDate(), body.getOuz(), body.getName());

            return new UpdateOrderResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public SearchOrderResponse searchOrder(SearchOrderRequest body) throws Fault {
        try {
            Page<AddressAllocationOrders> results = areaService.searchOrder(body.getId(), body.getNumber(), body.getDate(),
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

    @Override
    public GetAreaByIdResponse getAreaById(GetAreaByIdRequest body) throws Fault {
        try {
            AreaInfo area = areaService.getAreaById(body.getAreaId());
            GetAreaByIdResponse response = new GetAreaByIdResponse();
            response.setResult(areaMapper.entityToDtoTransform(area));
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public SetMedicalEmployeeOnAreaResponse setMedicalEmployeeOnArea(SetMedicalEmployeeOnAreaRequest body) throws Fault {
        try {
            List<Long> assignmentIds = areaService.setMedicalEmployeeOnArea(body.getAreaId(),
                    body.getAddMedicalEmployees() == null ? Collections.EMPTY_LIST : body.getAddMedicalEmployees().getAddMedicalEmployees(),
                    body.getChangeMedicalEmployees() == null ? Collections.EMPTY_LIST : body.getChangeMedicalEmployees().getChangeMedicalEmployees());
            SetMedicalEmployeeOnAreaResponse response = new SetMedicalEmployeeOnAreaResponse();
            response.getAssignmentIds().addAll(assignmentIds);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public AddAreaAddressResponse addAreaAddress(AddAreaAddressRequest body) throws Fault {
        try {
            AddAreaAddressResponse response = new AddAreaAddressResponse();
            response.getAreaAddressIds().addAll(areaService.addAreaAddress(body.getAreaId(),
                    body.getNsiAddresses() == null ? Collections.EMPTY_LIST : body.getNsiAddresses().stream().map(nsiAddressMapper::dtoToEntityTransform).collect(Collectors.toList()),
                    body.getNotNsiAddresses() == null ? Collections.EMPTY_LIST : body.getNotNsiAddresses().stream().map(notNsiAddressMapper::dtoToEntityTransform).collect(Collectors.toList())));
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public RestoreAreaResponse restoreArea(RestoreAreaRequest body) throws Fault {
        try {
            areaService.restoreArea(body.getAreaId());

            return new RestoreAreaResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public AddMoAddressResponse addMoAddress(AddMoAddressRequest body) throws Fault {
        try {
            AddMoAddressResponse response = new AddMoAddressResponse();
            response.getMoAddressIds().addAll(
                    areaService.addMoAddress(body.getMoId(), body.getAreaTypeCode(), body.getOrderId(),
                            body.getNsiAddresses() == null ? Collections.EMPTY_LIST : body.getNsiAddresses().stream().map(nsiAddressMapper::dtoToEntityTransform).collect(Collectors.toList()),
                            body.getNotNsiAddresses() == null ? Collections.EMPTY_LIST : body.getNotNsiAddresses().stream().map(notNsiAddressMapper::dtoToEntityTransform).collect(Collectors.toList())));

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetMoAddressResponse getMoAddress(GetMoAddressRequest body) throws Fault {
        try {
            GetMoAddressResponse response = new GetMoAddressResponse();
            Page<MoAddress> addresses = areaService.getMoAddress(body.getMoId(), body.getAreaTypes(),
                    body.getPagingOptions() == null ? null : pagingOptionsMapper.dtoToEntityTransform(body.getPagingOptions()));
            response.setMoId(body.getMoId());
            response.getAreaTypes().addAll(addresses.getContent().stream()
                    .map(MoAddress::getAreaType)
                    .map(areaTypeShortMapper::entityToDtoTransform)
                    .collect(Collectors.toSet()));
            response.getOrders().addAll(addresses.getContent().stream()
                    .map(MoAddress::getAddressAllocationOrder)
                    .map(addressAllocationOrderMapper::entityToDtoTransform)
                    .collect(Collectors.toSet()));
            response.getMoAddresses().addAll(addresses.getContent().stream()
                    .map(moAddressMapper::entityToDtoTransform)
                    .collect(Collectors.toSet()));
            soapCustomMapper.mapPagingResults(response, addresses);

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public DelMoAddressResponse delMoAddress(DelMoAddressRequest body) throws Fault {
        try {
            areaService.delMoAddress(body.getMoAddressIds(), body.getOrderId());

            return new DelMoAddressResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetAreaAddressResponse getAreaAddress(GetAreaAddressRequest body) throws Fault {
        try {
            GetAreaAddressResponse response = new GetAreaAddressResponse();
            
            Page<moscow.ptnl.contingent.area.model.area.AddressArea> areaAddresses = areaService.getAreaAddress(body.getAreaId(), body.getPagingOptions() != null ?
                    pagingOptionsMapper.dtoToEntityTransform(body.getPagingOptions()) : null);
            response.getAreaAddresses().addAll(
                areaAddresses.getContent().stream()
                .map(areaAddressMapper::entityToDtoTransform)
                        .collect(Collectors.toList()));
            
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public ArchiveAreaResponse archiveArea(ArchiveAreaRequest body) throws Fault {
        try {
            areaService.archiveArea(body.getAreaId());

            return new ArchiveAreaResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetNewAreaIdResponse getNewAreaId(GetNewAreaIdRequest body) throws Fault {
        try {
            GetNewAreaIdResponse response = new GetNewAreaIdResponse();
            response.setNewAreaId(areaService.getNewAreaId());

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public AddMoAvailableAreaTypesResponse addMoAvailableAreaTypes(AddMoAvailableAreaTypesRequest body) throws Fault {
        try {
            areaService.addMoAvailableAreaTypes(body.getMoId(), body.getAreaTypeCodes().getAreaTypeCodes());

            return new AddMoAvailableAreaTypesResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public DelMoAvailableAreaTypesResponse delMoAvailableAreaTypes(DelMoAvailableAreaTypesRequest body) throws Fault {
        try {
            areaService.delMoAvailableAreaTypes(body.getMoId(), body.getAreaTypeCodes().getAreaTypeCodes());

            return new DelMoAvailableAreaTypesResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetMoAvailableAreaTypesResponse getMoAvailableAreaTypes(GetMoAvailableAreaTypesRequest body) throws Fault {
        try {
            GetMoAvailableAreaTypesResponse result = new GetMoAvailableAreaTypesResponse();
            result.getMoAvailableAreaTypes().addAll(areaService.getMoAvailableAreaTypes(body.getMoId()).stream()
                    .map(areaTypeShortMapper::entityToDtoTransform)
                    .collect(Collectors.toList())
            );
            return result;
        }
        catch (Exception ex) {
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
