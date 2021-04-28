package moscow.ptnl.contingent.area.transform.v1;

import moscow.ptnl.contingent.area.transform.SoapBaseExceptionMapper;
import org.springframework.stereotype.Component;
import ru.mos.emias.contingent2.area.Fault;
import ru.mos.emias.system.v1.faults.BaseFault;

@Component("SoapExceptionMapperV1")
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
