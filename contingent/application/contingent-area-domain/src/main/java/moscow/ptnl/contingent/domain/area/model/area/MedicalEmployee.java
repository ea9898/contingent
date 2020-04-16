package moscow.ptnl.contingent.domain.area.model.area;

public class MedicalEmployee {

    private Long medicalEmployeeJobId;

    private String snils;

    public MedicalEmployee() {
    }

    public MedicalEmployee(Long medicalEmployeeJobId, String snils) {
        this.medicalEmployeeJobId = medicalEmployeeJobId;
        this.snils = snils;
    }

    public Long getMedicalEmployeeJobId() { return medicalEmployeeJobId; }

    public void setMedicalEmployeeJobId(Long medicalEmployeeJobId) { this.medicalEmployeeJobId = medicalEmployeeJobId; }

    public String getSnils() { return snils; }

    public void setSnils(String snils) { this.snils = snils; }

}
