package moscow.ptnl.contingent.area.ws.v1;

import moscow.ptnl.contingent.area.ws.BaseService;
import org.apache.cxf.annotations.SchemaValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.contingent2.area.composit.AreaCompositPT;
import ru.mos.emias.contingent2.area.composit.types.ArchiveAreaCompositRequest;
import ru.mos.emias.contingent2.area.composit.types.ArchiveAreaCompositResponse;

import java.lang.invoke.MethodHandles;

@Service(AreaCompositServiceImpl.SERVICE_NAME)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AreaCompositServiceImpl extends BaseService implements AreaCompositPT {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    public static final String SERVICE_NAME = "COMPOSIT_V1";

    @Override
    public ArchiveAreaCompositResponse archiveAreaComposit(ArchiveAreaCompositRequest body) throws ru.mos.emias.contingent2.area.composit.Fault {
        return new ArchiveAreaCompositResponse();
    }
}