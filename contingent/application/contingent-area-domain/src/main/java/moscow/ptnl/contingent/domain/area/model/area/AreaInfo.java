package moscow.ptnl.contingent.domain.area.model.area;

import moscow.ptnl.contingent.domain.area.entity.area.Area;
import moscow.ptnl.contingent.domain.area.entity.area.AreaMedicalEmployees;

import java.util.List;

// Объект возвращаемый из сервисаного слоя в слой веб-сиервиса
public class AreaInfo {

    private Area area;

    private List<AreaMedicalEmployees> mainAreaMedicalEmployees;

    private List<AreaMedicalEmployees> replacementAreaMedicalEmployees;

    public AreaInfo(Area area, List<AreaMedicalEmployees> mainAreaMedicalEmployees, List<AreaMedicalEmployees> replacementAreaMedicalEmployees) {
        this.area = area;
        this.mainAreaMedicalEmployees = mainAreaMedicalEmployees;
        this.replacementAreaMedicalEmployees = replacementAreaMedicalEmployees;
    }

    public AreaInfo(Area area) {
        this.area = area;
    }

    public Area getArea() {
        return area;
    }

    public void setArea(Area area) {
        this.area = area;
    }

    public List<AreaMedicalEmployees> getMainAreaMedicalEmployees() {
        return mainAreaMedicalEmployees;
    }

    public void setMainAreaMedicalEmployees(List<AreaMedicalEmployees> mainAreaMedicalEmployees) {
        this.mainAreaMedicalEmployees = mainAreaMedicalEmployees;
    }

    public List<AreaMedicalEmployees> getReplacementAreaMedicalEmployees() {
        return replacementAreaMedicalEmployees;
    }

    public void setReplacementAreaMedicalEmployees(List<AreaMedicalEmployees> replacementAreaMedicalEmployees) {
        this.replacementAreaMedicalEmployees = replacementAreaMedicalEmployees;
    }
}
