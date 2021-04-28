package moscow.ptnl.contingent.area.ws.v2;

import moscow.ptnl.contingent.area.transform.SoapVersioningMapper;
import moscow.ptnl.contingent.area.transform.UserContextMapper;
import moscow.ptnl.contingent.area.transform.v2.SoapExceptionMapper;
import moscow.ptnl.contingent.area.ws.BaseService;

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
 * @author m.kachalov
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
        return null;
    }

    @Override
    public InitiateCreatePrimaryAreaResponse initiateCreatePrimaryArea(InitiateCreatePrimaryAreaRequest body) throws Fault {
        return null;
    }

    @Override
    public AddMoAvailableAreaTypesResponse addMoAvailableAreaTypes(AddMoAvailableAreaTypesRequest body) throws Fault {
        return null;
    }

    @Override
    public GetMuAvailableAreaTypesResponse getMuAvailableAreaTypes(GetMuAvailableAreaTypesRequest body) throws Fault {
        return null;
    }

    @Override
    public SearchDnAreaResponse searchDnArea(SearchDnAreaRequest body) throws Fault {
        return null;
    }

    @Override
    public SetMedicalEmployeeOnAreaResponse setMedicalEmployeeOnArea(SetMedicalEmployeeOnAreaRequest body) throws Fault {
        return null;
    }

    @Override
    public DelMoAvailableAreaTypesResponse delMoAvailableAreaTypes(DelMoAvailableAreaTypesRequest body) throws Fault {
        return null;
    }

    @Override
    public SearchAreaResponse searchArea(SearchAreaRequest body) throws Fault {
        return null;
    }

    @Override
    public SearchMuByAreaAddressResponse searchMuByAreaAddress(SearchMuByAreaAddressRequest body) throws Fault {
        return null;
    }

    @Override
    public AddMoAddressResponse addMoAddress(AddMoAddressRequest body) throws Fault {
        return null;
    }

    @Override
    public CreateOrderResponse createOrder(CreateOrderRequest body) throws Fault {
        return null;
    }

    @Override
    public GetNewAreaIdResponse getNewAreaId(GetNewAreaIdRequest body) throws Fault {
        return null;
    }

    @Override
    public DelMuAvailableAreaTypesResponse delMuAvailableAreaTypes(DelMuAvailableAreaTypesRequest body) throws Fault {
        return null;
    }

    @Override
    public AddAreaAddressResponse addAreaAddress(AddAreaAddressRequest body) throws Fault {
        return null;
    }

    @Override
    public DelMoAddressResponse delMoAddress(DelMoAddressRequest body) throws Fault {
        return null;
    }

    @Override
    public InitiateAddMoAddressResponse initiateAddMoAddress(InitiateAddMoAddressRequest body) throws Fault {
        return null;
    }

    @Override
    public SearchOrderResponse searchOrder(SearchOrderRequest body) throws Fault {
        return null;
    }

    @Override
    public GetMoAddressResponse getMoAddress(GetMoAddressRequest body) throws Fault {
        return null;
    }

    @Override
    public UpdateDependentAreaResponse updateDependentArea(UpdateDependentAreaRequest body) throws Fault {
        return null;
    }

    @Override
    public InitiateAddAreaAddressResponse initiateAddAreaAddress(InitiateAddAreaAddressRequest body) throws Fault {
        return null;
    }

    @Override
    public DelAreaAddressResponse delAreaAddress(DelAreaAddressRequest body) throws Fault {
        return null;
    }

    @Override
    public CreatePrimaryAreaResponse createPrimaryArea(CreatePrimaryAreaRequest body) throws Fault {
        return null;
    }

    @Override
    public UpdatePrimaryAreaResponse updatePrimaryArea(UpdatePrimaryAreaRequest body) throws Fault {
        return null;
    }

    @Override
    public CreateDependentAreaResponse createDependentArea(CreateDependentAreaRequest body) throws Fault {
        return null;
    }

    @Override
    public GetAreaByIdResponse getAreaById(GetAreaByIdRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV1.getAreaById(versioningMapper.map(body, new ru.mos.emias.contingent2.area.types.GetAreaByIdRequest())),
                    new GetAreaByIdResponse());
        }
        catch (ru.mos.emias.contingent2.area.Fault ex) {
            //Мапинг исключения предыдущей версии контракта
            throw mapFault(ex);
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Override
    public GetAreaListBriefResponse getAreaListBrief(GetAreaListBriefRequest body) throws Fault {
        return null;
    }

    @Override
    public UpdateOrderResponse updateOrder(UpdateOrderRequest body) throws Fault {
        return null;
    }

    @Override
    public ArchiveAreaResponse archiveArea(ArchiveAreaRequest body) throws Fault {
        return null;
    }

    @Override
    public GetMoAvailableAreaTypesResponse getMoAvailableAreaTypes(GetMoAvailableAreaTypesRequest body) throws Fault {
        return null;
    }

    @Override
    public GetAreaAddressResponse getAreaAddress(GetAreaAddressRequest body) throws Fault {
        return null;
    }

    private ru.mos.emias.contingent2.area.v2.Fault mapFault(ru.mos.emias.contingent2.area.Fault ex) {
        return ex.getCause() == null ? new Fault(ex.getMessage(), ex.getFaultInfo()) : new Fault(ex.getMessage(), ex.getFaultInfo(), ex.getCause());
    }

    private ru.mos.emias.contingent2.area.v2.Fault mapException(Exception ex) {
        if (!(ex instanceof ContingentException)) {
            LOG.error(ex.getMessage(), ex);
        }
        return exceptionMapper.map(ex, userContextMapper);
    }
}
