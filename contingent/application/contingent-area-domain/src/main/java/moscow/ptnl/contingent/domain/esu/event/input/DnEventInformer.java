package moscow.ptnl.contingent.domain.esu.event.input;

import java.util.List;

/**
 * @author sorlov
 *
 * Описание входящего события DnEventInformer от СИМИ в формате JSON
 */
public class DnEventInformer {

    private String domain;
    private String ehrUid;
    private String operationType;
    private String eventName;
    private List<Results> newResults;
    private List<Results> oldResults;

    public String getDomain() {
        return domain;
    }

    public void setDomain(String value) {
        this.domain = value;
    }

    public String getEhrUid() {
        return ehrUid;
    }

    public void setEhrUid(String value) {
        this.ehrUid = value;
    }

    public String getOperationType() {
        return operationType;
    }

    public void setOperationType(String value) {
        this.operationType = value;
    }

    public String getEventName() {
        return eventName;
    }

    public void setEventName(String value) {
        this.eventName = value;
    }

    public List<Results> getNewResults() {
        return newResults;
    }

    public void setNewResults(List<Results> value) {
        this.newResults = value;
    }

    public List<Results> getOldResults() {
        return oldResults;
    }

    public void setOldResults(List<Results> oldResults) {
        this.oldResults = oldResults;
    }

    public static class Results {

        private String documentId;
        private String compositionId;
        private String patientEmiasId;
        private String jobId;
        private boolean isAttached;
        private boolean isNotForSelfAppointment;

        public String getDocumentId() {
            return documentId;
        }

        public void setDocumentId(String value) {
            this.documentId = value;
        }

        public String getCompositionId() {
            return compositionId;
        }

        public void setCompositionId(String value) {
            this.compositionId = value;
        }

        public String getPatientEmiasId() {
            return patientEmiasId;
        }

        public void setPatientEmiasId(String value) {
            this.patientEmiasId = value;
        }

        public String getJobId() {
            return jobId;
        }

        public void setJobId(String value) {
            this.jobId = value;
        }

        public boolean isIsAttached() {
            return isAttached;
        }

        public void setIsAttached(boolean value) {
            this.isAttached = value;
        }

        public boolean isIsNotForSelfAppointment() {
            return isNotForSelfAppointment;
        }

        public void setIsNotForSelfAppointment(boolean value) {
            this.isNotForSelfAppointment = value;
        }
    }
}
