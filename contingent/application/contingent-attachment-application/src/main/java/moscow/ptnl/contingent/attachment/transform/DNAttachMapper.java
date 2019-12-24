package moscow.ptnl.contingent.attachment.transform;

import moscow.ptnl.contingent.area.entity.area.Area;
import moscow.ptnl.contingent2.rmr.event.dn.DnAttach;

import javax.xml.datatype.DatatypeFactory;
import java.time.LocalDateTime;

public class DNAttachMapper {

    public static DnAttach map(Long patientId, LocalDateTime operationDate, Area createAttachmentArea, Area closeAttachmentArea) {

        DnAttach dnAttach = new DnAttach();
        dnAttach.setPatientEmiasId(patientId);
        try {
            dnAttach.setOperationDate(DatatypeFactory.newInstance().newXMLGregorianCalendar(operationDate.toString()));
        } catch (Exception ex) {}

        if (createAttachmentArea != null) {
            DnAttach.CreateAttachment createAttachment = new DnAttach.CreateAttachment();
            createAttachment.setAreaId(createAttachment.getAreaId());
            createAttachment.setMoId(createAttachmentArea.getMoId());
            createAttachment.setNotForSelfAppointment(false);
        } else if (closeAttachmentArea != null) {
            DnAttach.CloseAttachment closeAttachment = new DnAttach.CloseAttachment();
            closeAttachment.setAreaId(closeAttachment.getAreaId());
        }

        return dnAttach;
    }
}
