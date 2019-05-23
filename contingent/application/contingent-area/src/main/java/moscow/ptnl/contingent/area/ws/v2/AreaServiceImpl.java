package moscow.ptnl.contingent.area.ws.v2;

import moscow.ptnl.contingent.area.ws.BaseService;
import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.contingent2.area.v2.AreaPT;
import ru.mos.emias.contingent2.area.v2.Fault;
import ru.mos.emias.contingent2.area.v2.types.CreateDependentAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.CreateDependentAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.CreateOrderRequest;
import ru.mos.emias.contingent2.area.v2.types.CreateOrderResponse;
import ru.mos.emias.contingent2.area.v2.types.CreatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.CreatePrimaryAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.GetAreaByIdRequest;
import ru.mos.emias.contingent2.area.v2.types.GetAreaByIdResponse;
import ru.mos.emias.contingent2.area.v2.types.GetProfileMURequest;
import ru.mos.emias.contingent2.area.v2.types.GetProfileMUResponse;
import ru.mos.emias.contingent2.area.v2.types.SearchOrderRequest;
import ru.mos.emias.contingent2.area.v2.types.SearchOrderResponse;
import ru.mos.emias.contingent2.area.v2.types.SetProfileMURequest;
import ru.mos.emias.contingent2.area.v2.types.SetProfileMUResponse;
import ru.mos.emias.contingent2.area.v2.types.UpdateDependentAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.UpdateDependentAreaResponse;
import ru.mos.emias.contingent2.area.v2.types.UpdateOrderRequest;
import ru.mos.emias.contingent2.area.v2.types.UpdateOrderResponse;
import ru.mos.emias.contingent2.area.v2.types.UpdatePrimaryAreaRequest;
import ru.mos.emias.contingent2.area.v2.types.UpdatePrimaryAreaResponse;

/**
 *
 * @author m.kachalov
 */
@Service(AreaServiceImpl.SERVICE_NAME)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AreaServiceImpl extends BaseService implements AreaPT {
    
    public static final String SERVICE_NAME = "V2";
    
    public AreaServiceImpl(){}

    @Override
    public UpdateOrderResponse updateOrder(UpdateOrderRequest body) throws Fault {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public GetProfileMUResponse getProfileMU(GetProfileMURequest body) throws Fault {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public SetProfileMUResponse setProfileMU(SetProfileMURequest body) throws Fault {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public SearchOrderResponse searchOrder(SearchOrderRequest body) throws Fault {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public UpdateDependentAreaResponse updateDependentArea(UpdateDependentAreaRequest body) throws Fault {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public CreateOrderResponse createOrder(CreateOrderRequest body) throws Fault {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public CreatePrimaryAreaResponse createPrimaryArea(CreatePrimaryAreaRequest body) throws Fault {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public UpdatePrimaryAreaResponse updatePrimaryArea(UpdatePrimaryAreaRequest body) throws Fault {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public CreateDependentAreaResponse createDependentArea(CreateDependentAreaRequest body) throws Fault {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public GetAreaByIdResponse getAreaById(GetAreaByIdRequest body) throws Fault {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
