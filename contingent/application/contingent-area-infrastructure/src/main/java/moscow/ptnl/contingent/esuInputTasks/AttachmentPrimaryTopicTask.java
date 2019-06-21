package moscow.ptnl.contingent.esuInputTasks;

import moscow.ptnl.contingent.domain.esu.EsuInput;
import moscow.ptnl.contingent.repository.esu.EsuInputRepository;
import moscow.ptnl.contingent.util.EsuTopicsEnum;
import moscow.ptnl.contingent2.attachment.changeprimarea.event.AttachPrimaryPatientEvent;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.StepContribution;
import org.springframework.batch.core.scope.context.ChunkContext;
import org.springframework.batch.core.step.tasklet.Tasklet;
import org.springframework.batch.repeat.RepeatStatus;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.xml.sax.SAXException;

import javax.xml.XMLConstants;
import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Unmarshaller;
import javax.xml.validation.Schema;
import javax.xml.validation.SchemaFactory;
import java.io.StringReader;
import java.lang.invoke.MethodHandles;
import java.util.List;
import java.util.Objects;

/**
* К_УУ_ЕСУ_4
 * Формирование топика «Создать прикрепление к зависимому участку»
* */
@Component
public class AttachmentPrimaryTopicTask implements Tasklet {

    @Autowired
    private EsuInputRepository esuInputRepository;

    private static final Logger LOG = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass().getName());

    private static final String XSD_PATH = "META-INF/xsd/esu/attachmentprimary.v1.xsd";

    public AttachmentPrimaryTopicTask() {
    }

    public AttachmentPrimaryTopicTask(EsuInputRepository esuInputRepository) {
        this.esuInputRepository = esuInputRepository;
    }

    //@Transactional("contingentTransactionManager")
    public RepeatStatus execute(StepContribution contribution, ChunkContext chunkContext) throws Exception {
        System.out.println("MyTaskOne start..");

        List<EsuInput> messages = esuInputRepository.findByTopic(EsuTopicsEnum.ATTACHMENT_PRIMARY.getName());

        if (!messages.isEmpty()) {
            AttachPrimaryPatientEvent event = convertAttachment(messages.get(0).getMessage());
        }

        System.out.println("MyTaskOne done..");
        return RepeatStatus.FINISHED;
    }

    private AttachPrimaryPatientEvent convertAttachment(String body) {
        JAXBContext jaxbContext;
        AttachPrimaryPatientEvent attachPrimaryPatientEvent = null;
        try {
            jaxbContext = JAXBContext.newInstance(AttachPrimaryPatientEvent.class);
            SchemaFactory sf = SchemaFactory.newInstance(XMLConstants.W3C_XML_SCHEMA_NS_URI);
            Schema schema = sf.newSchema(Objects.requireNonNull(
                    Thread.currentThread().getContextClassLoader().getResource(XSD_PATH)));
            Unmarshaller jaxbUnmarshaller = jaxbContext.createUnmarshaller();
            jaxbUnmarshaller.setSchema(schema);
            StringReader reader = new StringReader(body);
            attachPrimaryPatientEvent = (AttachPrimaryPatientEvent) jaxbUnmarshaller.unmarshal(reader);
        } catch (JAXBException | SAXException e) {
            LOG.error("Некорректные данные; ", e.getMessage());
        }
        return attachPrimaryPatientEvent;
    }
}