package moscow.ptnl.contingent.domain.area.model.area;

import java.time.LocalDate;

public class AddMedicalEmployee {

    private long medicalEmployeeJobInfoId;

    private String snils;

    private String positionCode;

    private long subdivisionId;

    private boolean isReplacement;

    private LocalDate startDate;

    private LocalDate endDate;

    public long getMedicalEmployeeJobInfoId() {
        return medicalEmployeeJobInfoId;
    }

    public void setMedicalEmployeeJobInfoId(long medicalEmployeeJobInfoId) {
        this.medicalEmployeeJobInfoId = medicalEmployeeJobInfoId;
    }

    public String getSnils() {
        return snils;
    }

    public void setSnils(String snils) {
        this.snils = snils;
    }

    public String getPositionCode() {
        return positionCode;
    }

    public void setPositionCode(String positionCode) {
        this.positionCode = positionCode;
    }

    public long getSubdivisionId() {
        return subdivisionId;
    }

    public void setSubdivisionId(long subdivisionId) {
        this.subdivisionId = subdivisionId;
    }

    public boolean isReplacement() {
        return isReplacement;
    }

    public void setReplacement(boolean replacement) {
        isReplacement = replacement;
    }

    public LocalDate getStartDate() {
        return startDate;
    }

    public void setStartDate(LocalDate startDate) {
        this.startDate = startDate;
    }

    public LocalDate getEndDate() {
        return endDate;
    }

    public void setEndDate(LocalDate endDate) {
        this.endDate = endDate;
    }
}
