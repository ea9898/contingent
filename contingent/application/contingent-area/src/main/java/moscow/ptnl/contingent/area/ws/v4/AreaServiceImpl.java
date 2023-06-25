package moscow.ptnl.contingent.area.ws.v4;

import moscow.ptnl.contingent.area.transform.OptionEnum;
import moscow.ptnl.contingent.area.transform.SoapBaseExceptionMapper;
import moscow.ptnl.contingent.area.transform.SoapVersioningMapper;
import moscow.ptnl.contingent.area.transform.v3.SoapCustomMapperV3;
import moscow.ptnl.contingent.area.transform.v4.AreaMapperV4;
import moscow.ptnl.contingent.area.ws.BaseService;
import moscow.ptnl.contingent.domain.area.AreaService;
import moscow.ptnl.contingent.domain.area.MoMuService;
import moscow.ptnl.contingent.domain.area.entity.MuAvailableAreaTypes;
import moscow.ptnl.contingent.domain.area.entity.MuMuService;
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
import ru.mos.emias.contingent2.area.v4.AreaPT;
import ru.mos.emias.contingent2.area.v4.Fault;
import ru.mos.emias.contingent2.area.v4.types.AddAreaAddressRequest;
import ru.mos.emias.contingent2.area.v4.types.AddAreaAddressResponse;
import ru.mos.emias.contingent2.area.v4.types.AddMoAddressRequest;
import ru.mos.emias.contingent2.area.v4.types.AddMoAddressResponse;
import ru.mos.emias.contingent2.area.v4.types.AddMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v4.types.AddMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v4.types.AddMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v4.types.AddMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v4.types.ArchiveAreaRequest;
import ru.mos.emias.contingent2.area.v4.types.ArchiveAreaResponse;
import ru.mos.emias.contingent2.area.v4.types.CreateDependentAreaRequest;
import ru.mos.emias.contingent2.area.v4.types.CreateDependentAreaResponse;
import ru.mos.emias.contingent2.area.v4.types.CreateOrderRequest;
import ru.mos.emias.contingent2.area.v4.types.CreateOrderResponse;
import ru.mos.emias.contingent2.area.v4.types.CreatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v4.types.CreatePrimaryAreaResponse;
import ru.mos.emias.contingent2.area.v4.types.DelAreaAddressRequest;
import ru.mos.emias.contingent2.area.v4.types.DelAreaAddressResponse;
import ru.mos.emias.contingent2.area.v4.types.DelMoAddressRequest;
import ru.mos.emias.contingent2.area.v4.types.DelMoAddressResponse;
import ru.mos.emias.contingent2.area.v4.types.DelMoAddressTotalRequest;
import ru.mos.emias.contingent2.area.v4.types.DelMoAddressTotalResponse;
import ru.mos.emias.contingent2.area.v4.types.DelMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v4.types.DelMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v4.types.DelMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v4.types.DelMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v4.types.GetAreaAddressRequest;
import ru.mos.emias.contingent2.area.v4.types.GetAreaAddressResponse;
import ru.mos.emias.contingent2.area.v4.types.GetAreaByIdRequest;
import ru.mos.emias.contingent2.area.v4.types.GetAreaByIdResponse;
import ru.mos.emias.contingent2.area.v4.types.GetAreaHistoryRequest;
import ru.mos.emias.contingent2.area.v4.types.GetAreaHistoryResponse;
import ru.mos.emias.contingent2.area.v4.types.GetAreaListBriefRequest;
import ru.mos.emias.contingent2.area.v4.types.GetAreaListBriefResponse;
import ru.mos.emias.contingent2.area.v4.types.GetMoAddressRequest;
import ru.mos.emias.contingent2.area.v4.types.GetMoAddressResponse;
import ru.mos.emias.contingent2.area.v4.types.GetMoAddressTotalRequest;
import ru.mos.emias.contingent2.area.v4.types.GetMoAddressTotalResponse;
import ru.mos.emias.contingent2.area.v4.types.GetMoAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v4.types.GetMoAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v4.types.GetMuAvailableAreaTypes2Request;
import ru.mos.emias.contingent2.area.v4.types.GetMuAvailableAreaTypes2Response;
import ru.mos.emias.contingent2.area.v4.types.GetMuAvailableAreaTypesInMoRequest;
import ru.mos.emias.contingent2.area.v4.types.GetMuAvailableAreaTypesInMoResponse;
import ru.mos.emias.contingent2.area.v4.types.GetMuAvailableAreaTypesRequest;
import ru.mos.emias.contingent2.area.v4.types.GetMuAvailableAreaTypesResponse;
import ru.mos.emias.contingent2.area.v4.types.GetMuMuServiceRequest;
import ru.mos.emias.contingent2.area.v4.types.GetMuMuServiceResponse;
import ru.mos.emias.contingent2.area.v4.types.GetNewAreaIdRequest;
import ru.mos.emias.contingent2.area.v4.types.GetNewAreaIdResponse;
import ru.mos.emias.contingent2.area.v4.types.GetServicingMURequest;
import ru.mos.emias.contingent2.area.v4.types.GetServicingMUResponse;
import ru.mos.emias.contingent2.area.v4.types.InitiateAddAreaAddressRequest;
import ru.mos.emias.contingent2.area.v4.types.InitiateAddAreaAddressResponse;
import ru.mos.emias.contingent2.area.v4.types.InitiateAddMoAddressRequest;
import ru.mos.emias.contingent2.area.v4.types.InitiateAddMoAddressResponse;
import ru.mos.emias.contingent2.area.v4.types.InitiateCreatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v4.types.InitiateCreatePrimaryAreaResponse;
import ru.mos.emias.contingent2.area.v4.types.RestoreAreaRequest;
import ru.mos.emias.contingent2.area.v4.types.RestoreAreaResponse;
import ru.mos.emias.contingent2.area.v4.types.SearchAreaRequest;
import ru.mos.emias.contingent2.area.v4.types.SearchAreaResponse;
import ru.mos.emias.contingent2.area.v4.types.SearchDnAreaRequest;
import ru.mos.emias.contingent2.area.v4.types.SearchDnAreaResponse;
import ru.mos.emias.contingent2.area.v4.types.SearchMoAddressRequest;
import ru.mos.emias.contingent2.area.v4.types.SearchMoAddressResponse;
import ru.mos.emias.contingent2.area.v4.types.SearchMuByAreaAddressRequest;
import ru.mos.emias.contingent2.area.v4.types.SearchMuByAreaAddressResponse;
import ru.mos.emias.contingent2.area.v4.types.SearchOrderRequest;
import ru.mos.emias.contingent2.area.v4.types.SearchOrderResponse;
import ru.mos.emias.contingent2.area.v4.types.SetAreaEmployeeRequest;
import ru.mos.emias.contingent2.area.v4.types.SetAreaEmployeeResponse;
import ru.mos.emias.contingent2.area.v4.types.SetAreaMuServiceRequest;
import ru.mos.emias.contingent2.area.v4.types.SetAreaMuServiceResponse;
import ru.mos.emias.contingent2.area.v4.types.UpdateDependentAreaRequest;
import ru.mos.emias.contingent2.area.v4.types.UpdateDependentAreaResponse;
import ru.mos.emias.contingent2.area.v4.types.UpdateOrderRequest;
import ru.mos.emias.contingent2.area.v4.types.UpdateOrderResponse;
import ru.mos.emias.contingent2.area.v4.types.UpdatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v4.types.UpdatePrimaryAreaResponse;

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

    public static final String SERVICE_NAME = "AREA-V4";

    @Autowired
    private SoapBaseExceptionMapper<ru.mos.emias.contingent2.area.v4.Fault> exceptionMapper;

    @Autowired
    private SoapVersioningMapper versioningMapper;

    @Autowired
    private ru.mos.emias.contingent2.area.v3.AreaPT areaServiceV3;

    @Autowired
    private ru.mos.emias.contingent2.area.v4.AreaPT areaServiceV4;

    @Autowired
    private SoapCustomMapperV3 soapCustomMapper;

    @Autowired
    private AreaService areaServiceDomain;

    @Autowired
    private MoMuService moMuServiceDomain;

    @Autowired
    private AreaMapperV4 areaMapper;


    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public InitiateAddMoAddressResponse initiateAddMoAddress(InitiateAddMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.initiateAddMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.InitiateAddMoAddressRequest())),
                    new InitiateAddMoAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchOrderResponse searchOrder(SearchOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.searchOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.SearchOrderRequest())),
                    new SearchOrderResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMoAddressResponse getMoAddress(GetMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.getMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.GetMoAddressRequest())),
                    new GetMoAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelAreaAddressResponse delAreaAddress(DelAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.delAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.DelAreaAddressRequest())),
                    new DelAreaAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMoAddressTotalResponse delMoAddressTotal(DelMoAddressTotalRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.delMoAddressTotal(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.DelMoAddressTotalRequest())),
                    new DelMoAddressTotalResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public UpdateDependentAreaResponse updateDependentArea(UpdateDependentAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.updateDependentArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.UpdateDependentAreaRequest())),
                    new UpdateDependentAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public InitiateAddAreaAddressResponse initiateAddAreaAddress(InitiateAddAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.initiateAddAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.InitiateAddAreaAddressRequest())),
                    new InitiateAddAreaAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public CreatePrimaryAreaResponse createPrimaryArea(CreatePrimaryAreaRequest body) throws Fault {
        try {
            ru.mos.emias.contingent2.area.v4.types.CreatePrimaryAreaResponse response = new ru.mos.emias.contingent2.area.v4.types.CreatePrimaryAreaResponse();
            Long id = areaServiceDomain.createPrimaryArea(body.getMoId(), body.getMuId(), body.getNumber(), body.getAreaTypeCode(),
                    body.getAreaTypeCode(), body.getPolicyTypes() == null ? new ArrayList<>() : body.getPolicyTypes().getPolicyTypeCodes(),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(), body.getAgeMinW(), body.getAgeMaxW(),
                    body.isAutoAssignForAttachment(), body.getAttInfoLimit(), body.getAttInfoLimit(), body.isAttachByMedicalReason(), body.getDescription());

            response.setId(id);
            return response;
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public UpdatePrimaryAreaResponse updatePrimaryArea(UpdatePrimaryAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.updatePrimaryArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.UpdatePrimaryAreaRequest())),
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
            ru.mos.emias.contingent2.area.v4.types.GetAreaByIdResponse response = new ru.mos.emias.contingent2.area.v4.types.GetAreaByIdResponse();
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
            return versioningMapper.map(areaServiceV3.createDependentArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.CreateDependentAreaRequest())),
                    new CreateDependentAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetAreaListBriefResponse getAreaListBrief(GetAreaListBriefRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.getAreaListBrief(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.GetAreaListBriefRequest())),
                    new GetAreaListBriefResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public UpdateOrderResponse updateOrder(UpdateOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.updateOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.UpdateOrderRequest())),
                    new UpdateOrderResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public ArchiveAreaResponse archiveArea(ArchiveAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.archiveArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.ArchiveAreaRequest())),
                    new ArchiveAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMoAddressTotalResponse getMoAddressTotal(GetMoAddressTotalRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.getMoAddressTotal(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.GetMoAddressTotalRequest())),
                    new GetMoAddressTotalResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics // После создания 4-й версии нужно удалить
    public GetAreaHistoryResponse getAreaHistory(GetAreaHistoryRequest body) throws Fault {
//        try {
//
//        } catch (Exception ex) {
//            throw exceptionMapper.mapException(ex);
//        }
        throw new RuntimeException("Not implemented yet");
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetAreaAddressResponse getAreaAddress(GetAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.getAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.GetAreaAddressRequest())),
                    new GetAreaAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public RestoreAreaResponse restoreArea(RestoreAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.restoreArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.RestoreAreaRequest())),
                    new RestoreAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddMuAvailableAreaTypesResponse addMuAvailableAreaTypes(AddMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.addMuAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.AddMuAvailableAreaTypesRequest())),
                    new AddMuAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SetAreaEmployeeResponse setAreaEmployee(SetAreaEmployeeRequest body) throws Fault {
//        try {
//
//        } catch (Exception ex) {
//            throw exceptionMapper.mapException(ex);
//        }
        throw new RuntimeException("Not implemented yet");
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public InitiateCreatePrimaryAreaResponse initiateCreatePrimaryArea(InitiateCreatePrimaryAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.initiateCreatePrimaryArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.InitiateCreatePrimaryAreaRequest())),
                    new InitiateCreatePrimaryAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SetAreaMuServiceResponse setAreaMuService(SetAreaMuServiceRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.setAreaMuService(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.SetAreaMuServiceRequest())),
                    new SetAreaMuServiceResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchDnAreaResponse searchDnArea(SearchDnAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.searchDnArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.SearchDnAreaRequest())),
                    new SearchDnAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchAreaResponse searchArea(SearchAreaRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.searchArea(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.SearchAreaRequest())),
                    new SearchAreaResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchMuByAreaAddressResponse searchMuByAreaAddress(SearchMuByAreaAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.searchMuByAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.SearchMuByAreaAddressRequest())),
                    new SearchMuByAreaAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public SearchMoAddressResponse searchMoAddress(SearchMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.searchMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.SearchMoAddressRequest())),
                    new SearchMoAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddMoAddressResponse addMoAddress(AddMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.addMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.AddMoAddressRequest())),
                    new AddMoAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMuAvailableAreaTypesResponse delMuAvailableAreaTypes(DelMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.delMuAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.DelMuAvailableAreaTypesRequest())),
                    new DelMuAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public CreateOrderResponse createOrder(CreateOrderRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.createOrder(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.CreateOrderRequest())),
                    new CreateOrderResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @Metrics
    public GetNewAreaIdResponse getNewAreaId(GetNewAreaIdRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.getNewAreaId(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.GetNewAreaIdRequest())),
                    new GetNewAreaIdResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddAreaAddressResponse addAreaAddress(AddAreaAddressRequest body) throws Fault {

        try {
            return versioningMapper.map(areaServiceV3.addAreaAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.AddAreaAddressRequest())),
                    new AddAreaAddressResponse());
        } catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMoAddressResponse delMoAddress(DelMoAddressRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.delMoAddress(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.DelMoAddressRequest())),
                    new DelMoAddressResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMuAvailableAreaTypes2Response getMuAvailableAreaTypes2(GetMuAvailableAreaTypes2Request body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.getMuAvailableAreaTypes2(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.GetMuAvailableAreaTypes2Request())),
                    new GetMuAvailableAreaTypes2Response());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public AddMoAvailableAreaTypesResponse addMoAvailableAreaTypes(AddMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.addMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.AddMoAvailableAreaTypesRequest())),
                    new AddMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public DelMoAvailableAreaTypesResponse delMoAvailableAreaTypes(DelMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.delMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.DelMoAvailableAreaTypesRequest())),
                    new DelMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMoAvailableAreaTypesResponse getMoAvailableAreaTypes(GetMoAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.getMoAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.GetMoAvailableAreaTypesRequest())),
                    new GetMoAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMuAvailableAreaTypesResponse getMuAvailableAreaTypes(GetMuAvailableAreaTypesRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.getMuAvailableAreaTypes(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.GetMuAvailableAreaTypesRequest())),
                    new GetMuAvailableAreaTypesResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMuMuServiceResponse getMuMuService(GetMuMuServiceRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.getMuMuService(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.GetMuMuServiceRequest())),
                    new GetMuMuServiceResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetMuAvailableAreaTypesInMoResponse getMuAvailableAreaTypesInMo(GetMuAvailableAreaTypesInMoRequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.getMuAvailableAreaTypesInMo(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.GetMuAvailableAreaTypesInMoRequest())),
                    new GetMuAvailableAreaTypesInMoResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }

    @Override @EMIASSecured(faultClass = Fault.class) @Metrics
    public GetServicingMUResponse getServicingMU(GetServicingMURequest body) throws Fault {
        try {
            return versioningMapper.map(areaServiceV3.getServicingMU(versioningMapper.map(body, new ru.mos.emias.contingent2.area.v3.types.GetServicingMURequest())),
                    new GetServicingMUResponse());
        }
        catch (Exception ex) {
            throw exceptionMapper.mapException(ex);
        }
    }
}
