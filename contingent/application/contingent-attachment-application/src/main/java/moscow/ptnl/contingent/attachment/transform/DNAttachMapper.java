package moscow.ptnl.contingent.attachment.transform;

import moscow.ptnl.contingent.domain.area.entity.area.Area;
import moscow.ptnl.contingent.util.XMLGregorianCalendarMapper;
import moscow.ptnl.contingent2.rmr.event.dn.DnAttach;

import java.time.LocalDateTime;

public class DNAttachMapper {

    public static DnAttach map(Long patientId, LocalDateTime operationDate, Area createAttachmentArea, Area closeAttachmentArea) {

        DnAttach dnAttach = new DnAttach();
        dnAttach.setPatientEmiasId(patientId);
        dnAttach.setOperationDate(XMLGregorianCalendarMapper.entityToDtoTransform(operationDate));

        if (createAttachmentArea != null) {
            DnAttach.CreateAttachment createAttachment = new DnAttach.CreateAttachment();
            createAttachment.setAreaId(createAttachmentArea.getId());
            createAttachment.setMoId(createAttachmentArea.getMoId());
            createAttachment.setNotForSelfAppointment(false);
            dnAttach.setCreateAttachment(createAttachment);
        } else if (closeAttachmentArea != null) {
            DnAttach.CloseAttachment closeAttachment = new DnAttach.CloseAttachment();
            closeAttachment.setAreaId(closeAttachmentArea.getId());
            dnAttach.setCloseAttachment(closeAttachment);
        }

        return dnAttach;
    }
}
