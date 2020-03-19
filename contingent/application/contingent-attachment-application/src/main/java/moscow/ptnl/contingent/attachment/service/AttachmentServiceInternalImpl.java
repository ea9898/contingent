package moscow.ptnl.contingent.attachment.service;

import moscow.ptnl.contingent.domain.area.entity.area.Area;
import moscow.ptnl.contingent.attachment.AttachmentErrorReason;
import moscow.ptnl.contingent.attachment.helpers.AttachmentAlgoritms;
import moscow.ptnl.contingent.attachment.transform.DNAttachMapper;
import moscow.ptnl.contingent.error.ContingentException;
import moscow.ptnl.contingent.error.Validation;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.List;

@Service
@Transactional(propagation = Propagation.REQUIRED, rollbackFor = Exception.class)
public class AttachmentServiceInternalImpl implements AttachmentServiceInternal {

    @Autowired
    AttachmentAlgoritms attachmentAlgoritms;

    @Autowired
    EsuHelperService esuHelperService;

    @Override
    public void initiatePersonalAreaAttachment(Long patientEmiasId, LocalDateTime operationDate, Long createAttachmentJobId, Long closeAttachmentJobId) throws ContingentException {
        Validation validation = new Validation();

        if (createAttachmentJobId == null && closeAttachmentJobId == null) {
            validation.error(AttachmentErrorReason.NEED_JOB_ID);
        }

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        if (createAttachmentJobId != null) {
            List<Area> createAreasList = attachmentAlgoritms.findPersonalAreasByJobId(createAttachmentJobId);

            if (createAreasList.size() == 0) {
                validation.error(AttachmentErrorReason.AREA_FOR_CREATE_NOT_FOUND);
            } else if (createAreasList.size() > 1) {
                validation.error(AttachmentErrorReason.TOO_MANY_AREA_FOR_CREATE);
            } else {
                esuHelperService.sendEventToESU(DNAttachMapper.map(patientEmiasId, operationDate, createAreasList.get(0), null));
            }
        }

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

        if (closeAttachmentJobId != null) {
            List<Area> closeAreasList = attachmentAlgoritms.findPersonalAreasByJobId(closeAttachmentJobId);

            if (closeAreasList.size() == 0) {
                validation.error(AttachmentErrorReason.AREA_FOR_CLOSE_NOT_FOUND);
            } else if (closeAreasList.size() > 1) {
                validation.error(AttachmentErrorReason.TOO_MANY_AREA_FOR_CLOSE);
            } else {
                esuHelperService.sendEventToESU(DNAttachMapper.map(patientEmiasId, operationDate, null, closeAreasList.get(0)));
            }
        }

        if (!validation.isSuccess()) {
            throw new ContingentException(validation);
        }

    }
}
