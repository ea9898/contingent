package moscow.ptnl.contingent.area.ws.v1;

import moscow.ptnl.contingent.area.entity.area.MuProfile;
import moscow.ptnl.contingent.area.service.AreaServiceInternal;
import moscow.ptnl.contingent.area.transform.SoapCustomMapper;
import moscow.ptnl.contingent.area.transform.SoapExceptionMapper;
import moscow.ptnl.contingent.area.ws.BaseService;
import org.apache.cxf.annotations.SchemaValidation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import ru.gov.emias2.contingent.v1.area.AreaPT;
import ru.gov.emias2.contingent.v1.area.GetProfileMURequest;
import ru.gov.emias2.contingent.v1.area.GetProfileMUResponse;
import ru.gov.emias2.contingent.v1.area.SetProfileMURequest;
import ru.gov.emias2.contingent.v1.area.SetProfileMUResponse;
import ru.gov.emias2.contingent.v1.common.ContingentFault;

import java.util.List;
import java.util.stream.Collectors;

/**
 *
 * @author mkachalov
 */
@Service(AreaServiceImpl.SERVICE_NAME) 
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AreaServiceImpl extends BaseService implements AreaPT {
    
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
            throw SoapExceptionMapper.map(ex);
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
            throw SoapExceptionMapper.map(ex);
        }
    }
}
