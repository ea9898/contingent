package moscow.ptnl.contingent.area.transform.v2;

import moscow.ptnl.contingent.area.transform.SoapBaseExceptionMapper;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.area.v2.Fault;
import ru.mos.emias.system.v1.faults.BaseFault;

@Component("SoapExceptionMapperV2")
public class SoapExceptionMapper extends SoapBaseExceptionMapper<Fault> {

    @Override
    public Fault newFault(String message, BaseFault fault) {
        return new Fault(message, fault);
    }

    @Override
    public Fault newFault(String message, BaseFault fault, Throwable cause) {
        return new Fault(message, fault, cause);
    }
}
