package moscow.ptnl.contingent.area.ws.v1;

import moscow.ptnl.contingent.area.ws.BaseService;
import org.apache.cxf.annotations.SchemaValidation;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import ru.mos.emias.contingent2.area.composit.AreaCompositePT;
import ru.mos.emias.contingent2.area.composit.Fault;
import ru.mos.emias.contingent2.area.composit.types.ArchiveAreaCompositeRequest;
import ru.mos.emias.contingent2.area.composit.types.ArchiveAreaCompositeResponse;

import java.lang.invoke.MethodHandles;

@Service(AreaCompositeServiceImpl.SERVICE_NAME)
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
@SchemaValidation(type = SchemaValidation.SchemaValidationType.BOTH)
public class AreaCompositeServiceImpl extends BaseService implements AreaCompositePT {

    private final static Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    public static final String SERVICE_NAME = "COMPOSITE";

    @Override
    public ArchiveAreaCompositeResponse archiveAreaComposite(ArchiveAreaCompositeRequest body) throws Fault {
        return null;
    }

}
