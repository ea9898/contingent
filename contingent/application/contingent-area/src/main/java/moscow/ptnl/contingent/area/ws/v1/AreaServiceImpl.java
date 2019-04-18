package moscow.ptnl.contingent.area.ws.v1;

import moscow.ptnl.contingent.area.entity.area.MuProfile;
import moscow.ptnl.contingent.area.error.ContingentException;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.transform.SoapCustomMapper;
import moscow.ptnl.contingent.area.transform.SoapExceptionMapper;
import moscow.ptnl.contingent.area.ws.BaseService;
import org.apache.cxf.annotations.SchemaValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ru.gov.emias2.contingent.v1.area.AreaPT;
import ru.gov.emias2.contingent.v1.area.CreateDependentAreaRequest;
import ru.gov.emias2.contingent.v1.area.CreateDependentAreaResponse;
import ru.gov.emias2.contingent.v1.area.CreateOrderRequest;
import ru.gov.emias2.contingent.v1.area.CreateOrderResponse;
import ru.gov.emias2.contingent.v1.area.CreatePrimaryAreaRequest;
import ru.gov.emias2.contingent.v1.area.CreatePrimaryAreaResponse;
import ru.gov.emias2.contingent.v1.area.GetProfileMURequest;
import ru.gov.emias2.contingent.v1.area.GetProfileMUResponse;
import ru.gov.emias2.contingent.v1.area.SetProfileMURequest;
import ru.gov.emias2.contingent.v1.area.SetProfileMUResponse;
import ru.gov.emias2.contingent.v1.area.UpdateDependentAreaRequest;
import ru.gov.emias2.contingent.v1.area.UpdateDependentAreaResponse;
import ru.gov.emias2.contingent.v1.area.UpdatePrimaryAreaRequest;
import ru.gov.emias2.contingent.v1.area.UpdatePrimaryAreaResponse;
import ru.gov.emias2.contingent.v1.common.ContingentFault;

import java.lang.invoke.MethodHandles;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 *
 * @author mkachalov
 */
@Service(AreaServiceImpl.SERVICE_NAME) 
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AreaServiceImpl extends BaseService implements AreaPT {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    public static final String SERVICE_NAME = "V1";

    @Autowired
    private AreaServiceInternal areaService;

    @Autowired
    private SoapCustomMapper soapCustomMapper;

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public GetProfileMUResponse getProfileMU(GetProfileMURequest body) throws ContingentFault {
        try {
            List<MuProfile> profiles = areaService.getProfileMU(body.getMuId());
            GetProfileMUResponse response = new GetProfileMUResponse();
            response.getProfileAreaTypes().addAll(profiles.stream()
                    .map(soapCustomMapper::mapMuProfileToAreaTypeShort).collect(Collectors.toList()));

            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public SetProfileMUResponse setProfileMU(SetProfileMURequest body) throws ContingentFault {
        try {
            areaService.setProfileMU(body.getMuId(), body.getMuTypeId(),
                    body.getAreaTypesAdd() == null ? null : body.getAreaTypesAdd().getAreaTypeCodes(),
                    body.getAreaTypesDel() == null ? null : body.getAreaTypesDel().getAreaTypeCodes());
            return new SetProfileMUResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public CreatePrimaryAreaResponse createPrimaryArea(CreatePrimaryAreaRequest body) throws ContingentFault {
        try {
            CreatePrimaryAreaResponse response = new CreatePrimaryAreaResponse();
            Long id = areaService.createPrimaryArea(body.getMoId(), body.getMuId(), null, body.getNumber(), body.getAreaTypeCode(),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(), body.getAgeMinW(), body.getAgeMaxW(),
                    body.isAutoAssignForAttachment(), body.isAttachByMedicalReason(), body.getDescription());

            response.setId(id);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public CreateDependentAreaResponse createDependentArea(CreateDependentAreaRequest body) throws ContingentFault {
        try {
            CreateDependentAreaResponse response = new CreateDependentAreaResponse();
            Long id = areaService.createDependentArea(body.getMoId(), body.getMuId(), null, body.getNumber(), body.getAreaTypeCode(),
                    body.getPrimaryAreaTypeCodes(), body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(),
                    body.getAgeMinW(), body.getAgeMaxW(), body.isAutoAssignForAttachment(), body.getDescription());

            response.setId(id);
            return response;
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public UpdatePrimaryAreaResponse updatePrimaryArea(UpdatePrimaryAreaRequest body) throws ContingentFault {
        try {
            areaService.updatePrimaryArea(body.getAreaId(), null, body.getNumber(),
                    body.getAgeMin(), body.getAgeMax(), body.getAgeMinM(), body.getAgeMaxM(), body.getAgeMinW(), body.getAgeMaxW(),
                    body.isAutoAssignForAttachment(), body.isAttachByMedicalReason(), body.getDescription());

            return new UpdatePrimaryAreaResponse();
        }
        catch (Exception ex) {
            throw mapException(ex);
        }
    }

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public UpdateDependentAreaResponse updateDependentArea(UpdateDependentAreaRequest body) throws ContingentFault {
        try {
            areaService.updateDependentArea(body.getAreaId(), body.getMuId(), null, body.getNumber(),
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

    @Transactional(propagation = Propagation.REQUIRED)
    @Override
    public CreateOrderResponse createOrder(CreateOrderRequest body) throws ContingentFault {
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

    private ContingentFault mapException(Exception ex) {
        if (!(ex instanceof ContingentException)) {
            LOG.error(ex.getMessage(), ex);
        }
        return SoapExceptionMapper.map(ex);
    }
}
