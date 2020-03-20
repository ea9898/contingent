package moscow.ptnl.contingent.attachment.service;

import moscow.ptnl.contingent.error.ContingentException;

import java.time.LocalDateTime;

public interface AttachmentServiceInternal {

    public void initiatePersonalAreaAttachment(Long patientEmiasId, LocalDateTime operationDate,
                                               Long createAttachmentJobId, Long closeAttachmentJobId) throws ContingentException;
}
