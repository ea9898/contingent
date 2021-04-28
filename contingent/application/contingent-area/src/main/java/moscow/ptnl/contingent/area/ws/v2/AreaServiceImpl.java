package moscow.ptnl.contingent.area.ws.v2;

import moscow.ptnl.contingent.area.transform.SoapVersioningMapper;
import moscow.ptnl.contingent.area.transform.UserContextMapper;
import moscow.ptnl.contingent.area.transform.v2.SoapExceptionMapper;
import moscow.ptnl.contingent.area.ws.BaseService;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.error.ContingentException;

import org.apache.cxf.annotations.SchemaValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
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
import ru.mos.emias.contingent2.area.v2.types.DelMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v2.types.DelMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v2.types.DelMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v2.types.DelMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v2.types.GetAreaAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.GetAreaAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.GetAreaByIdRequest;
import ru.mos.emias.contingent2.area.v2.types.GetAreaByIdResponse;
import ru.mos.emias.contingent2.area.v2.types.GetAreaListBriefRequest;
import ru.mos.emias.contingent2.area.v2.types.GetAreaListBriefResponse;
import ru.mos.emias.contingent2.area.v2.types.GetMoAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.GetMoAddressResponse;
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
import ru.mos.emias.contingent2.area.v2.types.SearchMuByAreaAddressRequest;
import ru.mos.emias.contingent2.area.v2.types.SearchMuByAreaAddressResponse;
import ru.mos.emias.contingent2.area.v2.types.SearchOrderRequest;
import ru.mos.emias.contingent2.area.v2.types.SearchOrderResponse;
import ru.mos.emias.contingent2.area.v2.types.SetMedicalEmployeeOnAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.SetMedicalEmployeeOnAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.UpdateDependentAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.UpdateDependentAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.UpdateOrderRequest;
import ru.mos.emias.contingent2.area.v2.types.UpdateOrderResponse;
import ru.mos.emias.contingent2.area.v2.types.UpdatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.UpdatePrimaryAreaResponse;

import java.lang.invoke.MethodHandles;

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

    @Override
    public RestoreAreaResponse restoreArea(RestoreAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.restoreArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.RestoreAreaRequest())),
                    new RestoreAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public AddMuAvailableAreaTypesResponse addMuAvailableAreaTypes(AddMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.addMuAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.AddMuAvailableAreaTypesRequest())),
                    new AddMuAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public InitiateCreatePrimaryAreaResponse initiateCreatePrimaryArea(InitiateCreatePrimaryAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.initiateCreatePrimaryArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.InitiateCreatePrimaryAreaRequest())),
                    new InitiateCreatePrimaryAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public AddMoAvailableAreaTypesResponse addMoAvailableAreaTypes(AddMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.addMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.AddMoAvailableAreaTypesRequest())),
                    new AddMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetMuAvailableAreaTypesResponse getMuAvailableAreaTypes(GetMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getMuAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetMuAvailableAreaTypesRequest())),
                    new GetMuAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public SearchDnAreaResponse searchDnArea(SearchDnAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.searchDnArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.SearchDnAreaRequest())),
                    new SearchDnAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public SetMedicalEmployeeOnAreaResponse setMedicalEmployeeOnArea(SetMedicalEmployeeOnAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.setMedicalEmployeeOnArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.SetMedicalEmployeeOnAreaRequest())),
                    new SetMedicalEmployeeOnAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public DelMoAvailableAreaTypesResponse delMoAvailableAreaTypes(DelMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelMoAvailableAreaTypesRequest())),
                    new DelMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public SearchAreaResponse searchArea(SearchAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.searchArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.SearchAreaRequest())),
                    new SearchAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public SearchMuByAreaAddressResponse searchMuByAreaAddress(SearchMuByAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.searchMuByAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.SearchMuByAreaAddressRequest())),
                    new SearchMuByAreaAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public AddMoAddressResponse addMoAddress(AddMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.addMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.AddMoAddressRequest())),
                    new AddMoAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public CreateOrderResponse createOrder(CreateOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.createOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.CreateOrderRequest())),
                    new CreateOrderResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetNewAreaIdResponse getNewAreaId(GetNewAreaIdRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getNewAreaId(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetNewAreaIdRequest())),
                    new GetNewAreaIdResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public DelMuAvailableAreaTypesResponse delMuAvailableAreaTypes(DelMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delMuAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelMuAvailableAreaTypesRequest())),
                    new DelMuAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public AddAreaAddressResponse addAreaAddress(AddAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.addAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.AddAreaAddressRequest())),
                    new AddAreaAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public DelMoAddressResponse delMoAddress(DelMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelMoAddressRequest())),
                    new DelMoAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public InitiateAddMoAddressResponse initiateAddMoAddress(InitiateAddMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.initiateAddMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.InitiateAddMoAddressRequest())),
                    new InitiateAddMoAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public SearchOrderResponse searchOrder(SearchOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.searchOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.SearchOrderRequest())),
                    new SearchOrderResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetMoAddressResponse getMoAddress(GetMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetMoAddressRequest())),
                    new GetMoAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public UpdateDependentAreaResponse updateDependentArea(UpdateDependentAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.updateDependentArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.UpdateDependentAreaRequest())),
                    new UpdateDependentAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public InitiateAddAreaAddressResponse initiateAddAreaAddress(InitiateAddAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.initiateAddAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.InitiateAddAreaAddressRequest())),
                    new InitiateAddAreaAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public DelAreaAddressResponse delAreaAddress(DelAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.delAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.DelAreaAddressRequest())),
                    new DelAreaAddressResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public CreatePrimaryAreaResponse createPrimaryArea(CreatePrimaryAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.createPrimaryArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.CreatePrimaryAreaRequest())),
                    new CreatePrimaryAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public UpdatePrimaryAreaResponse updatePrimaryArea(UpdatePrimaryAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.updatePrimaryArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.UpdatePrimaryAreaRequest())),
                    new UpdatePrimaryAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public CreateDependentAreaResponse createDependentArea(CreateDependentAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.createDependentArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.CreateDependentAreaRequest())),
                    new CreateDependentAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetAreaByIdResponse getAreaById(GetAreaByIdRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getAreaById(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetAreaByIdRequest())),
                    new GetAreaByIdResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetAreaListBriefResponse getAreaListBrief(GetAreaListBriefRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getAreaListBrief(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetAreaListBriefRequest())),
                    new GetAreaListBriefResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public UpdateOrderResponse updateOrder(UpdateOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.updateOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.UpdateOrderRequest())),
                    new UpdateOrderResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public ArchiveAreaResponse archiveArea(ArchiveAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.archiveArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.ArchiveAreaRequest())),
                    new ArchiveAreaResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetMoAvailableAreaTypesResponse getMoAvailableAreaTypes(GetMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetMoAvailableAreaTypesRequest())),
                    new GetMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetAreaAddressResponse getAreaAddress(GetAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetAreaAddressRequest())),
                    new GetAreaAddressResponse());
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
