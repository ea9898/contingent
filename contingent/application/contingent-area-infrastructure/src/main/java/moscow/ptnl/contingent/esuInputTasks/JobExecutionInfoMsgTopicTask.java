package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.repository.area.AreaCRUDRepository;
import moscow.ptnl.contingent.repository.area.AreaRepository;
import moscow.ptnl.contingent.repository.esu.EsuInputCRUDRepository;
import moscow.ptnl.contingent.repository.esu.EsuInputRepository;
import moscow.ptnl.contingent.repository.nsi.AreaTypesCRUDRepository;
import moscow.ptnl.contingent.service.esu.EsuService;
import moscow.ptnl.contingent.util.EsuTopicsEnum;
import moscow.ptnl.contingent2.rmr.event.JobExecutionInfoMsg;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.lang.invoke.MethodHandles;

/**
 * К_УУ_ЕСУ_2
 */
@Component
@Qualifier("jobExecutionInfoMsgTopicTask")
public class JobExecutionInfoMsgTopicTask extends BaseTopicTask<JobExecutionInfoMsg> {

    private static final Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    private static final String XSD_PATH = "META-INF/xsd/esu/jobExecutionInfoMsg.xsd";

    @Autowired
    private EsuInputRepository esuInputRepository;

    @Autowired
    private EsuInputCRUDRepository esuInputCRUDRepository;

    @Autowired
    private AreaTypesCRUDRepository areaTypesCRUDRepository;

    @Autowired
    private AreaCRUDRepository areaCRUDRepository;

    @Autowired
    private AreaRepository areaRepository;

    @Autowired
    private EsuService esuService;

    public JobExecutionInfoMsgTopicTask() {
        super(EsuTopicsEnum.JOB_EXECUTION_INFO_MSG, XSD_PATH, JobExecutionInfoMsg.class);
    }

    @Override
    protected String getEsuId(JobExecutionInfoMsg event) {
        return event.getActionId(); //Todo не понятно где брать ИД
    }

    @Override
    @Transactional(propagation = Propagation.REQUIRES_NEW)
    public void processMessage(JobExecutionInfoMsg event) {

    }
}