package moscow.ptnl.contingent.area.transform;

import moscow.ptnl.contingent.security.UserContextHolder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import ru.mos.emias.contingent2.area.Fault;
import ru.mos.emias.errors.domain.UnauthorizedException;
import ru.mos.emias.system.v1.faults.FaultTypes;
import ru.mos.emias.system.v1.faults.SecurityExceptionTypes;
import ru.mos.emias.system.v1.faults.SecurityFault;
import ru.mos.emias.system.v1.faults.UnauthorizedRequestSecurityException;

import java.lang.invoke.MethodHandles;

public class SoapUnauthorizedExceptionMapper {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    public static <T extends Exception> Exception map(UnauthorizedException e, UserContextMapper userContextMapper, Class<T> faultClass) {
        UnauthorizedRequestSecurityException.RequiredRights rights = new UnauthorizedRequestSecurityException.RequiredRights();
        rights.getUserRightIds().addAll(e.getUserRights());

        UnauthorizedRequestSecurityException urse = new UnauthorizedRequestSecurityException();
        urse.setType(SecurityExceptionTypes.UNAUTHORIZED_REQUEST_EXCEPTION);
        urse.setRequiredRights(rights);

        SecurityFault fault = new SecurityFault();
        fault.setType(FaultTypes.SECURITY);
        fault.setUserContext(userContextMapper.entityToDtoTransform(UserContextHolder.getUserContext()));
        fault.setUnauthorizedRequestSecurityException(urse);

        try {
            return faultClass.getConstructor(String.class, ru.mos.emias.system.v1.faults.BaseFault.class)
                    .newInstance(e.getMessage(), fault);
        } catch (Exception ex) {
            LOG.error("Unexpected error while Unauthorized exception mapping", ex);
            return new Fault(e.getMessage(), fault);
        }
    }
}
