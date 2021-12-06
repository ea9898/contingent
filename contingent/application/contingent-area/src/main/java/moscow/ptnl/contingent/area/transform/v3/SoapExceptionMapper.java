package moscow.ptnl.contingent.area.transform.v3;

import moscow.ptnl.contingent.area.transform.SoapBaseExceptionMapper;
import moscow.ptnl.contingent.area.transform.UserContextMapper;
import moscow.ptnl.contingent.error.ContingentException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.area.v3.Fault;
import ru.mos.emias.system.v1.faults.BaseFault;

@Component("V3")
public class SoapExceptionMapper extends SoapBaseExceptionMapper<Fault> {
    
    private final static Logger LOG = LoggerFactory.getLogger(SoapExceptionMapper.class);
    
    @Autowired
    private UserContextMapper userContextMapper;

    @Override
    public Fault newFault(String message, BaseFault fault) {
        return new Fault(message, fault);
    }

    @Override
    public Fault newFault(String message, BaseFault fault, Throwable cause) {
        return new Fault(message, fault, cause);
    }
    
    @Override
    public Fault mapFault(ru.mos.emias.contingent2.area.Fault ex) {
        return ex.getCause() == null ? new Fault(ex.getMessage(), ex.getFaultInfo()) : new Fault(ex.getMessage(), ex.getFaultInfo(), ex.getCause());
    }
    
    public Fault mapFault(ru.mos.emias.contingent2.area.v2.Fault ex) {
        return ex.getCause() == null ? new Fault(ex.getMessage(), ex.getFaultInfo()) : new Fault(ex.getMessage(), ex.getFaultInfo(), ex.getCause());
    }

    @Override
    public Fault mapException(Exception ex) {
        //Мапинг исключения предыдущей версии контракта
        if (ex instanceof ru.mos.emias.contingent2.area.Fault) {            
            return mapFault((ru.mos.emias.contingent2.area.Fault) ex);
        } else if (ex instanceof ru.mos.emias.contingent2.area.v2.Fault) {
            return mapFault((ru.mos.emias.contingent2.area.v2.Fault) ex);
        }
        if (!(ex instanceof ContingentException)) {
            LOG.error(ex.getMessage(), ex);
        }
        return map(ex, userContextMapper);
    }
}
